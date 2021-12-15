{-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, LambdaCase #-}

module Language.Unlambda
  (
  -- * Identifiers for lambda terms
    Id, IdSupply, idSupply, newId
  
  -- * Unlambda (+ lambda term) syntax
  , GUnlambda(..), Unlambda, Relambda
  , gunlambdaAsRelambda, unlambdaAsRelambda
  , parseUnl
  ) where

import Control.Monad.State
import Data.Char (isSpace, toLower)

newtype Id = Id Int -- invariant: ints are unique
  deriving (Eq, Ord)
instance Show Id where
  show (Id i) = '_' : show i

newtype IdSupply = IdSupply Int
idSupply :: IdSupply
idSupply = IdSupply 0

newId :: (MonadState IdSupply m) => m Id
newId = state $ \(IdSupply n) -> (Id n, IdSupply (n+1))

-- promoted kind
data HasLambda = NoLambda | YesLambda
data GUnlambda (hasLambda :: HasLambda)
  = I | V | K | S | C | D | E
  | Dot Char
  | UnlApp (GUnlambda hasLambda) (GUnlambda hasLambda)
  | (hasLambda ~ 'YesLambda) => UnlVar Id
  | (hasLambda ~ 'YesLambda) => UnlLam Id (GUnlambda hasLambda)
-- can't use standard deriving syntax for data with constrained constructors
deriving instance Eq (GUnlambda x)  
deriving instance Ord (GUnlambda x)

instance Show (GUnlambda x) where
  showsPrec _ = showsGUnlambda

showsGUnlambda :: GUnlambda x -> ShowS
showsGUnlambda = \case
  I -> ('i':)
  V -> ('v':)
  K -> ('k':)
  S -> ('s':)
  C -> ('c':)
  D -> ('d':)
  E -> ('e':)
  Dot '\n' -> ('r':)
  Dot c    -> (['.', c]++)
  UnlApp e1 e2 -> ('`':) . showsGUnlambda e1 . showsGUnlambda e2
  UnlVar id    -> ('$':) . shows id
  UnlLam id b  -> ('^':) . shows id . showsGUnlambda b

type Unlambda = GUnlambda 'NoLambda
type Relambda = GUnlambda 'YesLambda

gunlambdaAsRelambda :: GUnlambda x -> Relambda
gunlambdaAsRelambda = \case
  I -> I
  V -> V
  K -> K
  S -> S
  C -> C
  D -> D
  E -> E
  Dot c -> Dot c
  UnlApp e1 e2 -> UnlApp (gunlambdaAsRelambda e1) (gunlambdaAsRelambda e2)
  UnlVar i     -> UnlVar i
  UnlLam i b   -> UnlLam i (gunlambdaAsRelambda b)

unlambdaAsRelambda :: Unlambda -> Relambda
unlambdaAsRelambda = gunlambdaAsRelambda

instance Read Unlambda where 
   readsPrec _ str = [runState parseUnlM str]
-- could also define a pretty printer but that's a lot of effort
-- for a compiler that doesn't really generate error messages anyway.

-- | Unlambda Parser
type UP a = State String a

parseUnl :: String -> Unlambda
parseUnl = evalState parseUnlM

nextChar :: UP Char
nextChar = state (\s -> case s of
    (c:cs) -> (c, cs)
    []     -> error "unexpected EOF")

parseUnlM :: UP Unlambda
parseUnlM = do
  op <- nextChar
  case toLower op of
    '`' -> UnlApp <$> parseUnlM <*> parseUnlM
    '#' -> modify (unlines . tail . lines) >> parseUnlM
    'i' -> pure I
    'v' -> pure V
    'k' -> pure K
    's' -> pure S
    'c' -> pure C
    'd' -> pure D
    'e' -> pure E
    '.' -> Dot <$> nextChar
    'r' -> pure $ Dot '\n'
    c | isSpace c -> parseUnlM
    _   -> error $ "Unknown Unlambda operator " ++ [op]