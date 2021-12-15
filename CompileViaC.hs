{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module CompileViaC where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens (makeLenses, use, (%=), (.=), (<<+=))

import Data.Char
import Data.Maybe
import Data.List ((\\), elemIndex, isPrefixOf, partition)
import qualified Data.Set as S

import System.FilePath
import System.Environment
import System.Exit (die)
import System.Process

import Language.Unlambda
import Language.Scheme
import Language.SchemeParser

-- Pick a mode for the tradeoff described in the paper.
-- DelayArg is better for any reasonable program.
data Mode = DuplicateArg | DelayArg

--------------------------------------------------------------
-- Unlambda -> Scheme
--------------------------------------------------------------

unlDefines :: [(Unlambda, LispVal)]
unlDefines = map (second $ head . quoteScheme)
  [ (I, "(define i (lambda (x) x))")
  , (V, "(define v (lambda (_) v))")
  , (K, "(define k (lambda (x) (lambda (_) x)))")
  , (S, "(define s (lambda (x) (lambda (y) (lambda (z)\
        \  (apply (apply x z) (apply y z))))))")
    -- Implicit promise forcing combined with d having a well-defined
    -- identity function closure means we don't need 'apply' here.
  , (C, "(define c call/cc)")
  , (D, "(define d (lambda (x) x))")
  , (E, "(define e (lambda (x) (exit x)))")
  ]
  where second f (a, x) = (a, f x)

-- | Take a full Unlambda program (with UnlApps, presumably) and "flatten"
-- it into a list of the Unlambda functions which are used.
-- For small unlambda programs, the size of the definitions (especially that
-- of 'c') completely dominate the size of the compiled program itself,
-- so not defining the primitives we don't need helps mitigate that.
-- Note that if `d` isn't used, we can also omit 'apply' everywhere.
unlFvs :: Unlambda -> [Unlambda]
unlFvs = S.toList . unlFvs'

-- This version of the runtime doesn't support ?x, @, or |, but they
-- would not be significantly challenging to add.

unl2Scm :: Unlambda -> LispVal
unl2Scm unl = List (Atom "begin" : (unlPrelude ++ [translate unl]))
  where
    unlPrelude = map snd $ filter (\(unl, _) -> unl `S.member` fvs) unlDefines
    fvs = unlFvs' unl

-- updated to use the smarter translator from CompileViaScheme.hs
translate :: Unlambda -> Scheme
translate unl = snd $ translate' hasD unl
  where hasD = D `S.member` unlFvs' unl

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
  UnlApp{} -> List [Atom "apply", s1, s2]
  D        -> List [Atom "delay", s2]
  _        -> List [s1, s2]

mkAppHappy :: (Unlambda,Scheme) -> Scheme -> Scheme
mkAppHappy (u1,s1) s2 = List [s1, s2]

translate' :: Bool -> Unlambda -> (FVs, Scheme)
translate' _ u | u `elem` [I,V,K,S,C,D,E] = (S.singleton u, Atom $ show u)
translate' _ (Dot c)    = (S.empty, List [Atom "dot", Char c])
translate' hasD (UnlApp e1 e2) = mkApp hasD e1 e2

unlFvs' :: Unlambda -> S.Set Unlambda
unlFvs' (UnlApp f g) = unlFvs' f `S.union` unlFvs' g
unlFvs' (Dot _)      = S.empty -- defined via a macro, so a Scheme define is never needed
unlFvs' f            = S.singleton f

--------------------------------------------------------------
-- SCHEME -> C (most of the rest of the file)
--------------------------------------------------------------

--------------------------------------------------------------
-- types
--------------------------------------------------------------

type Symbol = String

data AST = AST { ast_node :: ASTNode
               , ast_subx :: [AST]
               , ast_fvs  :: Maybe (S.Set Binding)
               }

data AbsType = Lambda | Promise
data ImplicitForcing = IFEnabled | IFDisabled

data ASTNode
    = Ref Binding   -- variable reference,  like x
    | Set Binding   -- variable assignment, like (set! x _) (_ is in the head of ast_subx)
    | Cnd           -- conditional,         like (if 1 2 3)
    | Prim Op       -- primop,              like (%halt 1)
    | App ImplicitForcing
                    -- application,         like (f 1 2)
                    -- but maybe an explicit promise force like (g),
                    -- in which case implicit forcing needs to be disabled.
    | Lam [Binding] AbsType
                    -- lambda expression,   like (lambda (x) x)
                    -- or, if the bool is True, a promise
    | Seq           -- sequence,            like (begin 1 2)
    | Lit Literal   -- TODO: remove the unecessary boxing

data Literal = LitInt Int

make_ref       sym = AST (Ref sym)        []     Nothing
make_set  subx sym = AST (Set sym)        subx   Nothing
make_cnd  subx     = AST Cnd              subx   Nothing
make_prim subx op  = AST (Prim op)        subx   Nothing
make_app  subx     = AST (App IFEnabled)  subx   Nothing
make_force subx    = AST (App IFDisabled) subx   Nothing
make_app_gen subx f = AST (App f)         subx   Nothing
make_lam  subx ps  = AST (Lam ps Lambda)  subx   Nothing
make_prom subx ps  = AST (Lam ps Promise) subx   Nothing
make_lam1 subx ps  = AST (Lam ps Lambda)  [subx] Nothing
make_prom1 subx ps = AST (Lam ps Promise) [subx] Nothing
make_closure subx ps b = AST (Lam ps b)   subx   Nothing
make_closure1 subx ps b = AST (Lam ps b)  [subx] Nothing
make_seq  subx     = AST Seq              subx   Nothing
make_lit       n   = AST (Lit n)          []     Nothing

type Op = Symbol -- Ops are just strings with chars like +,-

---------------------------------------------------------------
-- environments
---------------------------------------------------------------

type UID = Symbol

data Binding = Binding { binding_id :: Symbol, binding_case :: BindingCase } deriving Eq
data BindingCase = BindVar UID | BindMacro Expander
type Expander = [LispVal] -> Env -> SC AST

instance Show Binding where
    show (Binding _  (BindVar uid)) = uid
    show (Binding id (BindMacro _)) = id
instance Eq BindingCase where
    BindVar uid1 == BindVar uid2 = uid1 == uid2
    BindMacro _ == BindMacro _ = True -- so that Binding's == will go by the symbols
    _ == _ = False
instance Ord Binding where
   compare (Binding _ (BindVar x)) (Binding _ (BindVar y))
     = x `compare` y
   compare (Binding x (BindMacro _)) (Binding y (BindMacro _)) = x `compare` y
   compare (Binding _ (BindVar _)) _ = LT
   compare _ _ = GT

make_var :: Symbol -> UID -> Binding
make_var id uid = Binding id (BindVar uid)

make_macro :: Symbol -> Expander -> Binding
make_macro id expander = Binding id (BindMacro expander)

isVar :: Binding -> Bool
isVar (Binding _ (BindVar _)) = True
isVar _                       = False

var_uid :: Binding -> UID -- unsafe!
var_uid (Binding _ (BindVar uid)) = uid
var_uid _ = error "var_uid: not a var binding"

isMacro :: Binding -> Bool
isMacro (Binding _ (BindMacro _)) = True
isMacro _                         = False

new_global_var :: Symbol -> Binding
new_global_var id = make_var id id

isGlobalVar :: Binding -> Bool
isGlobalVar (Binding id (BindVar uid)) = id == uid

type Env = [Binding]

extend :: Env -> Env -> Env
extend = (++)

lookup :: Symbol -> Env -> Maybe Binding
lookup id []           = Nothing
lookup id (b:bs)
  | binding_id b == id = Just b
  | otherwise          = lookup id bs

fvLookup :: Symbol -> S.Set Binding -> Maybe Binding
fvLookup id s = lookup id (S.toList s)

--------------------------------------------------------------------
-- compiler monads
--------------------------------------------------------------------

type SC a = State SCState a

data SCState = SCState 
    { _fresh_num    :: Int   -- fresh identifier numbers
    , _global_cte   :: Env   -- list of bindings
    }

type CG a = State CGState a

data CGState = CGState
    { _lambda_count :: Int
    , _lambda_todo  :: [(Int, AST)]
    }
makeLenses ''SCState
makeLenses ''CGState

runSC :: Mode -> SC a -> a
runSC mode = flip evalState (SCState 0 (initialEnv mode))

fresh :: SC Int
fresh = fresh_num <<+= 1

new_var :: Symbol -> SC Binding
new_var id = make_var id . make_uid <$> fresh
  where make_uid :: Int -> UID
        make_uid num = id ++ "." ++ show num

runCG :: CG a -> a
runCG = flip evalState (CGState 0 [])

addLambdaTodo :: AST -> CG Int
addLambdaTodo lam = do
    i <- lambda_count <<+= 1
    lambda_todo %= ((i, lam):)
    return i

popLambdaTodo :: CG (Maybe (Int, AST))
popLambdaTodo = do
    ls <- use lambda_todo
    case ls of
        []     -> return Nothing
        (l:ls) -> lambda_todo .= ls >> return (Just l)

liftMB :: Applicative m => Maybe a -> MaybeT m a
liftMB = MaybeT . pure

--------------------------------------------------------------------
-- xe (eXpand Expression)
--------------------------------------------------------------------

-- the parser tosses (begin ... ) around the whole program, so we can
-- pretend that the whole program is just one expression.

xe :: LispVal -> Env -> SC AST
xe e cte | isConstExpr e = xeConstExpr e cte
         | isIdentExpr e = xeIdentExpr e cte
         | isFormExpr  e = xeFormExpr  e cte
         | otherwise     = error $ "syntax error: " ++ show e

xeLookup :: Symbol -> Env -> SC Binding
xeLookup id cte = fmap fromJust $ runMaybeT $ do
    let v = new_global_var id
        mkNew = global_cte %= (v:) >> return v
    g_cte <- lift $ use global_cte
    liftMB (lookup id cte) <|> liftMB (lookup id g_cte) <|> lift mkNew

isConstExpr :: LispVal -> Bool
isConstExpr Bool{}   = True
isConstExpr Number{} = True
isConstExpr _        = False

isIdentExpr :: LispVal -> Bool
isIdentExpr Atom{} = True
isIdentExpr _      = False

identOf :: LispVal -> Symbol
identOf (Atom sym) = sym
identOf _ = error "identOf: can't get symbol from non-atom"

-- | equivalent to `list?` in Scheme, and (list? '(a . b)) ==> #f
isList :: LispVal -> Bool
isList List{} = True
isList _      = False

isFormExpr = isList

xeConstExpr :: LispVal -> Env -> SC AST
xeConstExpr e cte = return $ make_lit $ litOf e
  where litOf (Number n) = LitInt $ fromInteger n -- no warning if out of bounds

xeIdentExpr :: LispVal -> Env -> SC AST
xeIdentExpr (Atom e) cte = do 
    b <- xeLookup e cte
    if isVar b then return $ make_ref b
               else error $ "can't reference nonvariable: " ++ show e

xeFormExpr :: LispVal -> Env -> SC AST
xeFormExpr (List vs) cte = do
    let h = head vs
    b <- case h of Atom sym -> xeLookup sym cte; _ -> pure $ make_var "" "" -- dummy
    case b of
        Binding _ (BindMacro expander) -> expander vs cte
        _ -> make_app <$> xeExprs vs cte

xeExprs :: [LispVal] -> Env -> SC [AST]
xeExprs le cte = mapM (\x -> xe x cte) le

basicOpMacro :: Symbol -> Binding
basicOpMacro op = make_macro op $ \es cte ->
    if length (tail es) == 2 then make_prim <$> xeExprs (tail es) cte <*> pure ('%':op)
    else error $ op ++ " expects 2 args"

initialEnv :: Mode -> Env
initialEnv mode =
  [ make_macro "dot" $ \es cte ->
      case tail es of
          [Char c] -> do
              x <- new_var "x"
              return $ make_lam [make_prim [] ("%dot:" ++ show c), make_ref x] [x]

          _ -> error "dot expects 1 arg (a character literal)"

  , make_macro "set!" $ \es cte ->
      if length (tail es) == 2
      then if isIdentExpr (second es)
           then do b <- xeLookup (identOf $ second es) []
                   if isVar b then make_set <$> xeExprs (tail (tail es)) cte <*> pure b
                   else error $ "can't set! a macro: " ++ show (List es)
           else error $ "can't set! a non-variable: " ++ show (List es)
      else error "set! expects 2 args"

  , make_macro "define" $ \es cte -> xe (List (Atom "set!" : tail es)) cte

  , make_macro "if" $ \es cte -> case length (tail es) of
      3 -> make_cnd <$> xeExprs (tail es) cte
      _ -> error "if expects 3 args"

  , make_macro "pointer-eq?" $ \es cte ->
      if length (tail es) == 2
      then make_prim <$> xeExprs (tail es) cte <*> pure "%pointer-eq?"
      else error "pointer-eq? expects 2 args"

  , make_macro "apply" $ \es cte -> if length (tail es) == 2
      then flip xe cte $ case mode of
        DelayArg ->
          List [Atom "let", List [ List [Atom "f", second es]
                                 , List [Atom "g", List [Atom "delay", third es]]]
                          , List [Atom "if", List [Atom "pointer-eq?", Atom "d", Atom "f"]
                                           , Atom "g"
                                           , List [Atom "f", List [Atom "force", Atom "g"]]]]
        DuplicateArg ->
          List [Atom "let", List [List [Atom "f", second es]]
                          , List [Atom "if", List [Atom "pointer-eq?", Atom "d", Atom "f"]
                                           , List [Atom "delay", third es]
                                           , List [Atom "f", third es]]]
      else error "apply expects 2 args"

  , make_macro "lambda" $ \es cte ->
      if length (tail es) >= 1
      then if isList (second es)
        then do params <- mapM new_var (map identOf $ extractList $ second es)
                let new_cte = extend params cte
                body <- xe (List (Atom "begin" : tail (tail es))) new_cte
                return $ make_lam1 body params
        else error $ "first argument of lambda must be a list: " ++ show (List es)
      else error "lambda expects at least 1 arg"

  -- form like (let ((x 1) (y 2)) (+ x y)) is broken down and reconstructed
  -- as the form ((lambda (x y) (+ x y)) 1 2)
  , make_macro "let" $ \es cte ->
      if length (tail es) >= 1
      then if isList (second es)
        then xe (List (List ([ Atom "lambda"
                             , List $ map car $ extractList $ second es]
                             ++ drop 2 es)
                      : map cadr (extractList $ second es))) cte
        else error "let expects a binding list"
      else error "let expects at least 1 arg"
  
  , make_macro "delay" $ \es cte ->
      if length (tail es) == 1
      then do body <- xe (second es) cte
              return $ make_prom1 body []
      else error "delay expects 1 arg"

  , make_macro "force" $ \es cte ->
      if length (tail es) == 1
      then do arg <- xe (second es) cte
              return $ make_force [arg]
      else error "force expects 1 arg"

  , make_macro "begin" $ \es cte -> case length (tail es) of
      0 -> xe (Bool False) cte
      1 -> xe (second es) cte
      _ -> make_seq <$> xeExprs (tail es) cte
  
  , make_macro "exit" $ \es cte ->
      if length (tail es) == 1
      then do result <- xe (second es) cte
              return $ make_prim [result] "%halt"
      else error "exit expects 1 arg"
  ]

--------------------------------------------------------------------------------
-- | free variables; we'll need this for CPS and Closure Conversion
--
-- Results are cached in the AST for quick lookup.
--------------------------------------------------------------------------------

fvAnalysis :: AST -> AST
fvAnalysis ast@AST { ast_subx = subx, ast_node = nd } = case nd of
    Ref var -> ast { ast_fvs = Just $ S.singleton var }
    Set var -> let child@AST{ast_fvs = Just fvs} = fvAnalysis $ head subx
               in ast { ast_subx = [child]
                      , ast_fvs = Just $ S.union fvs (S.singleton var)
                      }
    Lam ps _ -> 
      let child@AST{ast_fvs = Just fvs} = fvAnalysis $ head subx
      in ast { ast_subx = [child]
             , ast_fvs = Just $ S.difference fvs (S.fromList ps)
             }
    _       ->
      let fvSubx = map fvAnalysis subx
          fvss = map (fromJust . ast_fvs) fvSubx
      in ast { ast_subx = fvSubx
             , ast_fvs = Just $ S.unions fvss
             }

--------------------------------------------------------------------------------
-- CPS transformation
--------------------------------------------------------------------------------

cpsTransform :: AST -> SC AST
cpsTransform ast = do
    r <- new_var "r"
    let cont_ast = make_lam [make_prim [make_ref r] "%halt"] [r]
    astCPS <- cps ast cont_ast
    case fvLookup "call/cc" (fromJust $ ast_fvs $ fvAnalysis ast) of
        -- if call/cc is used in the program, add this definition
        Just _ -> do dummy <- new_var "_"
                     let [valDefineCallCC] = quoteScheme
                             "(set! call/cc (lambda (k f) (f k (lambda (_ result) (k result)))))"
                     defineCallCC <- xe valDefineCallCC []
                     return $ make_app [make_lam1 astCPS [dummy], defineCallCC]
        Nothing -> return astCPS

cps :: AST -> AST -> SC AST
cps ast@AST{ ast_subx=subx, ast_node=nd } cont_ast = case nd of
    Ref{}    -> pure $ make_app [cont_ast, ast]
    Set var  -> cpsList subx $ \val ->
                  pure $ make_app [cont_ast, make_set val var]
    Cnd      -> let xform xfcont_ast = cpsList [head subx] $ \test ->
                        make_cnd <$> sequence [ pure $ head test
                                              , cps (second subx) xfcont_ast
                                              , cps (third  subx) xfcont_ast ]
                in case ast_node cont_ast of
                    Ref{} -> xform cont_ast -- dont bind a new var to an existing one
                                            -- and ref immediately
                    _ -> do k <- new_var "k"
                            kref <- xform (make_ref k)
                            return $ make_app [make_lam1 kref [k], cont_ast]
    Prim op -> cpsList subx $ \args ->
                 pure $ make_app [cont_ast, make_prim args op]
    App f   -> let fn = head subx
                in case ast_node fn of
                    Lam ps Lambda -> cpsList (tail subx) $ \vals -> do
                        newBody <- cpsSeq (ast_subx fn) cont_ast
                        return $ make_app $ make_lam1 newBody ps : vals
                    _ -> cpsList subx $ \args ->
                        pure $ make_app_gen (head args : cont_ast : tail args) f
    Lam ps b -> do k <- new_var "k"
                   body <- cpsSeq subx (make_ref k)
                   return $ make_app [cont_ast, make_closure1 body (k:ps) b]
    Seq      -> cpsSeq subx cont_ast
                            
cpsList :: [AST] -> ([AST] -> SC AST) -> SC AST
cpsList [] inside = inside []
cpsList (ast:asts) inside = case ast_node ast of
    Lit{} -> body ast
    Ref{} -> body ast
    _     -> do r <- new_var "r"
                contBody <- body (make_ref r)
                cps ast $ make_lam1 contBody [r]
  where body x = cpsList asts $ \new_asts -> inside (x : new_asts)
    
cpsSeq :: [AST] -> AST -> SC AST
-- this case is allowed in Scheme, but Unlambda shouldn't cause it to arise.
cpsSeq []    cont_ast = error "'begin' with no body during compilation"
cpsSeq [ast] cont_ast = cps ast cont_ast
cpsSeq (ast:asts) cont_ast = do
    r <- new_var "r"
    body <- cpsSeq asts cont_ast
    cps ast $ make_lam1 body [r]

--------------------------------------------------------------------------------
-- Free variable analysis (caching results in the AST)
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- closure conversion
--------------------------------------------------------------------------------

closureConvert :: AST -> SC AST
closureConvert ast = make_lam1 <$> convert fvAst (error "Panic! No top-level closure")
                               <*> pure []
  where
    fvAst0@AST{ast_fvs = Just allFvs} = fvAnalysis ast
    fvs = S.filter (not . isGlobalVar) allFvs
    fvAst = fvAst0{ast_fvs = Just fvs}

convert :: AST -> Binding -> SC AST
convert ast@AST{ast_fvs=Just fvs} self = cc ast
  where
    cc :: AST -> SC AST
    cc ast@AST{ ast_subx=subx } = let ccSubx = mapM cc subx in case ast_node ast of
        Ref var -> pure $ case S.lookupIndex var fvs of
            Just i  -> make_prim [make_ref self, make_lit (LitInt (i + 1))] "%closure-ref"
            Nothing -> ast
        Set var -> make_set <$> ccSubx <*> pure var
        Cnd     -> make_cnd <$> ccSubx
        Prim op -> make_prim <$> ccSubx <*> pure op
        App{}    -> ccApp ast
        Lam{}   -> ccLam ast

    ccApp :: AST -> SC AST
    ccApp AST{ ast_subx = (fn : noCCArgs), ast_node = App forcing } = do
        args <- mapM cc noCCArgs
        case ast_node fn of
            Lam ps Lambda -> do
                -- NOTE [CPS Lam subx]: after cps transform, all lams have 1 subx
                body <- cc (head $ ast_subx fn)
                return $ make_app $ make_lam1 body ps : args
            _ -> do
                f <- cc fn
                return $ make_app_gen
                           (make_prim [f, make_lit (LitInt 0)] "%closure-ref"
                             : f : args)
                           forcing
    
    ccLam :: AST -> SC AST -- see note [CPS Lam subx]
    ccLam ast@AST{ ast_subx = [body], ast_node = Lam ps absTy, ast_fvs = Just fvs } = do
        let newFVs = S.filter (not . isGlobalVar) fvs
        newSelf <- new_var "self"
        newBody <- convert body{ast_fvs = Just newFVs} newSelf
        -- use S.toAscList to ensure consistent order of closure fields.
        closureArgs <- mapM (\v -> cc (make_ref v)) $ S.toAscList newFVs
        return $ make_prim (make_closure1 newBody (newSelf : ps) absTy : closureArgs)
                           (case absTy of
                              Lambda  -> "%closure"
                              Promise -> "%promise")

--------------------------------------------------------------------------------
-- perform all the transformations
--------------------------------------------------------------------------------

transformPhase :: Mode -> LispVal -> AST
transformPhase mode val =
  runSC mode $ xe val [] >>= cpsTransform >>= closureConvert

--------------------------------------------------------------------------------
-- code generation
--------------------------------------------------------------------------------

-- TODO maybe?: use ShowS instead of String for output code
type CCode    = String
type Var      = Maybe Binding
type Vars     = [Var]
type StackEnv = [Var]

sconcat :: [ShowS] -> ShowS
sconcat [] = id
sconcat ss = foldr1 (\s1 s2 -> s1 . s2) ss

codeGenerate :: AST -> CCode
codeGenerate ast = unlines $ runCG $ do
    addLambdaTodo ast
    code <- compileAllLambdas
    firstUnusedLabel <- use lambda_count
    return [ "#define NB_GLOBALS " ++ show (length globalVars)
           , "#define MAX_STACK 100"
           , "#define USED_D " ++ show firstUnusedLabel -- dont check if D was really used
           , codePrefix, code "", codeSuffix ]
  where
    globalVars = S.toAscList $ fromJust $ ast_fvs $ fvAnalysis ast
    
    codeGen :: AST -> StackEnv -> CG ShowS
    codeGen ast stackEnv = go ast
      where
        codeGenList :: [AST] -> Vars -> StackEnv -> String
                    -> (ShowS -> StackEnv -> CG ShowS)
                    -> CG ShowS
        codeGenList [] _ stackEnv _ cont = cont id stackEnv
        codeGenList (ast:asts) (var:vars) stackEnv sep cont = do
            astCode <- codeGen ast stackEnv
            codeGenList asts vars (var : stackEnv) sep $ \code se ->
                cont (sconcat [astCode, showString sep, code]) se
    
        codeGenArgs :: [AST] -> StackEnv -> CG ShowS
        codeGenArgs args stackEnv =
            codeGenList args (replicate (length args) Nothing) 
                        stackEnv " " $ \code se -> pure code
        
        accessVar :: Var -> StackEnv -> String
        accessVar Nothing _ = error "Attempt to access non-var during code generation"
        accessVar (Just v@(Binding _ (BindVar uid))) stackEnv
          | isGlobalVar v = let i = unsafeElemIndex v globalVars
                            in concat ["GLOBAL(", show i, "/*", uid, "*/)"]
          | otherwise = let i = length stackEnv - unsafeElemIndex (Just v) stackEnv - 1
                        in concat ["LOCAL(", show i, "/*", uid, "*/)"]

        codeGenPrim :: String -> [AST] -> CG ShowS
        codeGenPrim op args 
          | Just charLit <- dropPrefix "%dot:" op
          = pure $ showString $ "DOT_C(" ++ charLit ++ ");"
          | op `elem` ["%halt", "%pointer-eq?"] =
              sconcat <$>
                sequence [codeGenArgs args stackEnv, pure (showString (op2code op))]         
        codeGenPrim cloOp (closureLam : args)
          | cloOp `elem` ["%closure", "%promise"] = do
            i <- addLambdaTodo closureLam
            let n = length args
                s = concat ["CLOSURE(", show i, ",", show n, ");"]
                e = if cloOp == "%closure" then s
                    else concat ["PROMISE(", show i, ",", show n, ");"]
            argsCode <- codeGenArgs args stackEnv
            return $ sconcat [argsCode, showString "MAKE_", showString e]
        codeGenPrim "%closure-ref" [f, AST{ ast_node = Lit (LitInt i) }] = do
            evalF <- go f
            return $ sconcat 
              [ evalF
              , showString " CLOSURE_REF("
              , shows i
              , showString ");" ]
        codeGenPrim op _ = error $ "Unknown primop during codegen: " ++ op
        
        codeGenApp :: ImplicitForcing -> [AST] -> CG ShowS
        codeGenApp forcing (fn : args) = do
            let n = length args
            case ast_node fn of
                Lam ps Lambda -> codeGenList args (map Just ps) 
                            stackEnv "\n  " $ \code se ->
                              sconcat <$> sequence [pure code, codeGen (head (ast_subx fn)) se]
                _ -> codeGenList args (replicate n Nothing) 
                       stackEnv "\n  " $ \code se ->
                         let start = length stackEnv
                             j = "JUMP(" ++ show n ++ ");"
                             e = case forcing of
                               IFEnabled  -> j
                               -- implicit forcing is disabled when the application is an
                               -- explicit force - that's what FORCE_JUMP is for!
                               IFDisabled -> "FORCE_" ++ j
                         in return $ sconcat [code, showString e]

        go :: AST -> CG ShowS
        go AST{ ast_subx=subx, ast_node=nd } = case nd of
            Ref var -> pure $ showString $
              concat ["PUSH(", accessVar (Just var) stackEnv, ");"]
            -- concat [go (head subx), " ", accessVar ..., " = TOS();"] but the types work
            Set var -> sconcat <$> liftA2 (:) (go (head subx)) 
              (pure $ map showString [" ", accessVar (Just var) stackEnv, " = TOS();"])
            Cnd -> do subxCode <- mapM go subx
                      let [test, t, f] = subxCode
                      return $ sconcat [ test, showString "\n  if (POP()) {\n  "
                                       , t,    showString "\n  } else {\n  "
                                       , f,    showString "\n  }" ]
            Prim op -> codeGenPrim op subx
            App forcing -> codeGenApp forcing subx
            _ -> error "Panic! Impossible Lam or Seq node after CPS transform"

    compileAllLambdas :: CG ShowS
    compileAllLambdas = popLambdaTodo >>= \mp -> case mp of
        Nothing -> pure id
        Just (i, ast@AST{ ast_subx=[body], ast_node=Lam ps _ }) -> do
            bodyCode <- codeGen body (map Just $ reverse ps)
            rest <- compileAllLambdas
            return $ sconcat
              [ showString "FUNCTION("
              , shows i
              , showString ") /* "
              , showString $ limit (show (source ast)) 60
              , showString " */\n  "
              , bodyCode
              , showString "\n\n"
              , rest ]

op2code :: String -> String
op2code op | Just charLit <- dropPrefix "%dot:" op
           = " DOT(" ++ charLit ++ ")"
op2code "%halt" = "HALT();"
op2code "%pointer-eq?" = "PTR_EQ();"
     
codePrefix = "#include \"RTS.h\""

codeSuffix = unlines [
    "  }"
  , "  return POP();"
  , "}"
  , "int main() { execute(); return 0; }"
  ]

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

compile :: Mode -> String -> CCode
compile mode src
  = codeGenerate
  $ transformPhase mode
  $ unl2Scm
  $ parseUnl src

compileFile :: Mode -> FilePath -> IO ()
compileFile mode fp = do
    src <- readFile fp
    let code = compile mode src
    writeFile (fp -<.> ".c") code
    callCommand $ unwords ["gcc -O3 -fwhole-program -I . -o", out, c]
  where c   = fp -<.> ".c"
        out = fp -<.> ""

debug :: Mode -> String -> IO ()
debug mode code = mapM_ putStrLn [
      " * Source Code -------------------------------------------\n"
  , code
  , "\n * Parsed Unlambda ---------------------------------------\n"
  , show unl
  , "\n * Scheme Code -------------------------------------------\n"
  , show scheme
  , "\n * Expanded Code -----------------------------------------\n"
  , show $ source expanded
  , "\n * CPS Transformed ---------------------------------------\n"
  , show $ source cpst
  , "\n * Closure Converted -------------------------------------\n"
  , show $ source ccd
  , "\n * C Code ------------------------------------------------\n"
  , ccode
  ]
  where
    unl = parseUnl code
    scheme = unl2Scm unl
    (expanded, cpst, ccd) = runSC mode $ do
        expanded <- xe scheme []
        cpst     <- cpsTransform expanded
        ccd      <- closureConvert cpst
        return (expanded, cpst, ccd)
    ccode = codeGenerate ccd

main :: IO ()
main = do
  args <- getArgs
  case processArgs args of
    Just (mode, fn) -> compileFile mode fn
    _ -> do name <- getProgName
            die $ "Usage: " ++ name ++ " [-delay-arg|-duplicate-arg] FILE"

processArgs :: [String] -> Maybe (Mode, FilePath)
processArgs args = (,) <$> mode <*> fn
  where
    (flags, strings) = partition ("-" `isPrefixOf`) args
    mode = case flags of
      []                 -> Just DelayArg
      ["-delay-arg"]     -> Just DelayArg
      ["-duplicate-arg"] -> Just DuplicateArg
      _ -> Nothing
    fn = case strings of
      [p] -> Just p
      _   -> Nothing

--------------------------------------------------------------------------------
-- simple utilities
--------------------------------------------------------------------------------

second = (!! 1)
third  = (!! 2)
car = head . extractList
cdr = tail . extractList
cadr = second . extractList
extractList (List l) = l
extractList val = error $ "not a list: " ++ show val

unsafeElemIndex :: Eq a => a -> [a] -> Int
unsafeElemIndex x xs = case elemIndex x xs of
    Just i -> i
    Nothing -> error "unsafeElemIndex: not in list"

limit :: String -> Int -> String
limit [] _  = ""
limit str 0 = "..."
limit (c:cs) n = c : limit cs (n - 1)

dropPrefix :: String -> String -> Maybe String
dropPrefix [] str = Just str
dropPrefix (x:xs) (y:ys)
  | x == y = dropPrefix xs ys
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- "pretty" printer for debugging
-- just converts the AST back to a LispVal which we have a (bad) printer for
--------------------------------------------------------------------------------

source :: AST -> LispVal
source AST { ast_subx = subx, ast_node = nd } = case nd of
    Lit (LitInt  n) -> Number $ fromIntegral n
    Ref (Binding _ (BindVar uid)) -> Atom uid
    Set (Binding _ (BindVar uid)) -> List [Atom "set!", Atom uid, source (head subx)]
    Cnd -> List $ Atom "if" : map source subx
    Prim op -> List $ Atom op : map source subx
    App forcing -> case head subx of
        AST { ast_subx = body, ast_node = inner_nd } -> case inner_nd of
            Lam ps Lambda -> 
                let bindings = map (\(Binding _ (BindVar uid), a) -> List [Atom uid, source a])
                                   (zip ps (tail subx))
                in List $ [Atom "let", List bindings] ++ map source body
            _ -> List $ displayForcing forcing ++ map source subx
      where displayForcing IFEnabled  = [] -- normal case, no need to indicate that
            displayForcing IFDisabled = [Atom "%force"]
    Lam ps Lambda  -> List $ [Atom "lambda", List $ map (Atom . var_uid) ps] ++ map source subx
    Lam ps Promise -> List [Atom "delay", List $ map (Atom . var_uid) ps, source (head subx)]
    Seq -> List $ Atom "begin" : map source subx
