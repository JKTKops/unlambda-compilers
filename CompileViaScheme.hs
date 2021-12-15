{-# LANGUAGE TemplateHaskell, TupleSections #-}

module CompileViaScheme where

import Data.FileEmbed
import qualified Data.Set as S
import System.FilePath
import System.Environment
import System.Exit
import System.Process

import Language.Unlambda
import Language.Scheme

translate :: Unlambda -> Scheme
translate unl = snd $ translate' hasD unl
  where hasD = D `S.member` unlFvs unl

-- propogate free variables up the call tree so that we don't
-- quadratically recompute them.
-- An application only needs to use force-apply if the head could
-- evaluate to d or a promise. That's only possible if the head
-- contains d or c - c could evaluate to anything, in theory.
-- Slightly smarter would be to first check if the program contains
-- d at all.
type FVs = S.Set Unlambda

mkApp :: Bool -> Unlambda -> Unlambda -> (FVs, Scheme)
mkApp hasD e1 e2
  | hasD && (D `S.member` fvs1 || C `S.member` fvs1)
              = (fvs', mkAppSad   (e1,s1) s2)
  | otherwise = (fvs', mkAppHappy (e1,s1) s2)
  where (fvs1,s1) = translate' hasD e1
        (fvs2,s2) = translate' hasD e2
        fvs' = fvs1 `S.union` fvs2

mkAppSad :: (Unlambda,Scheme) -> Scheme -> Scheme
mkAppSad (u1,s1) s2 = case u1 of
  UnlApp{} -> List [Atom "force-apply", s1, s2]
  D        -> List [Atom "promise", s2]
  _        -> List [s1, s2]

mkAppHappy :: (Unlambda,Scheme) -> Scheme -> Scheme
mkAppHappy (u1,s1) s2 = List [s1, s2]

translate' :: Bool -> Unlambda -> (FVs, Scheme)
translate' _ u | u `elem` [I,V,K,S,C,D,E] = (S.singleton u, Atom $ show u)
translate' _ (Dot '\n') = (S.empty, Atom "r")
translate' _ (Dot c)    = (S.empty, List [Atom "dot", Char c])
translate' hasD (UnlApp e1 e2) = mkApp hasD e1 e2

unlFvs :: Unlambda -> S.Set Unlambda
unlFvs (UnlApp f g) = unlFvs f `S.union` unlFvs g
unlFvs (Dot _)      = S.empty -- defined via a macro, so a Scheme define is never needed
unlFvs f            = S.singleton f

prelude, postlude :: String
prelude  = $(embedStringFile "UnlPrelude.scm")
postlude = ""

type UnlambdaCode = String
type SchemeCode   = String
type BashCode     = String

compile :: UnlambdaCode -> SchemeCode
compile code = unlines [prelude, show scheme, postlude]
  where scheme = translate $ parseUnl code

compileFile :: FilePath -> IO ()
compileFile fp = do
    src <- readFile fp
    let code = compile src
    writeFile scm code
    callCommand $
      "echo '(compile-file \"" ++ scm ++ "\")' | scheme --optimize-level 3 -q"
    writeFile out $ makeExecScript so
  where scm = fp -<.> ".scm"
        so  = fp -<.> ".so"
        out = fp -<.> ""

makeExecScript :: FilePath -> BashCode
makeExecScript so = unlines
  [ "#!/usr/bin/env bash"
  , "petite --script " ++ so
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> compileFile fn
    _ -> die "Error: please provide exactly 1 Unlambda file"