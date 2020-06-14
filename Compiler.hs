{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens (makeLenses, use, uses, (%=), (.=), (<<+=))

import Data.Char
import Data.Maybe
import Data.List ((\\), elemIndex)

import System.IO.Unsafe (unsafePerformIO)
import System.FilePath

import ParserTypes
import Parser

--------------------------------------------------------------
-- Unlambda -> Scheme
--------------------------------------------------------------

data Unlambda
    = UnlApp Unlambda Unlambda
    | I | V | K | S | C | D | E
    | Dot Char
    deriving (Eq, Show)

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

unlDefines :: [(Unlambda, LispVal)]
unlDefines = map (second $ head . quoteScheme)
  [ (I, "(define i (lambda (x) x))")
  , (V, "(define v (lambda (_) v))")
  , (K, "(define k (lambda (x) (lambda (_) x)))")
  , (S, "(define s (lambda (x) (lambda (y) (lambda (z)\
        \  (apply (apply x z) (apply y z))))))")
    -- Implicit promise forcing combined with d having a well-defined
    -- identity function closure means we don't need 'apply' here.
  , (C, "(define c (lambda (f) (call/cc (lambda (k) (f k)))))")
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
unlFvs (UnlApp f g) = unlFvs f ++ unlFvs g
unlFvs (Dot _)      = [] -- defined via a macro, so a Scheme define is never needed
unlFvs f            = [f]

-- This version of the runtime doesn't support ?x, @, or |, but they
-- would not be significantly challenging to add.

unl2Scm :: Unlambda -> LispVal
unl2Scm unl = List (Atom "begin" : (unlPrelude ++ [go unl]))
  where
    unlPrelude = map snd $ filter (\(unl, _) -> unl `elem` fvs) unlDefines
    fvs = unlFvs unl
    mkScmApp | D `elem` fvs = \lv1 lv2 -> List [Atom "apply", lv1, lv2]
             | otherwise    = \lv1 lv2 -> List [lv1, lv2]

    go :: Unlambda -> LispVal
    go (UnlApp D form) = List [Atom "delay", go form]
    go (UnlApp f g)    = mkScmApp (go f) (go g)
    go (Dot char)      = List [Atom "dot", Char char]
    go op = Atom $ map toLower $ show op -- i,v,k,s,c,d,e

--------------------------------------------------------------
-- SCHEME -> C (most of the rest of the file)
--------------------------------------------------------------

--------------------------------------------------------------
-- types
--------------------------------------------------------------

type Symbol = String

data AST = AST { ast_node :: ASTNode, ast_subx :: [AST] }

data ASTNode
    = Ref Binding   -- variable reference,  like x
    | Set Binding   -- variable assignment, like (set! x _) (_ is in the head of ast_subx)
    | Cnd           -- conditional,         like (if 1 2 3)
    | Prim Op       -- primop,              like (%halt 1)
    | App           -- application,         like (f 1 2)
    | Lam [Binding] Bool
                    -- lambda expression,   like (lambda (x) x)
                    -- or, if the bool is True, a promise
    | Seq           -- sequence,            like (begin 1 2)
    | Lit Literal   -- TODO: remove the unecessary boxing

data Literal = LitInt Int

make_ref  subx sym = AST (Ref sym)      subx
make_set  subx sym = AST (Set sym)      subx
make_cnd  subx     = AST Cnd            subx
make_prim subx op  = AST (Prim op)      subx
make_app  subx     = AST App            subx
make_lam  subx ps  = AST (Lam ps False) subx
make_prom subx ps  = AST (Lam ps True)  subx
make_lam1 subx ps  = AST (Lam ps False) [subx]
make_prom1 subx ps = AST (Lam ps True)  [subx]
make_closure subx ps b = AST (Lam ps b) subx
make_closure1 subx ps b = AST (Lam ps b) [subx]
make_seq  subx     = AST Seq     subx
make_lit  subx n   = AST (Lit n) subx

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

--------------------------------------------------------------------
-- compiler monads
--------------------------------------------------------------------

type SC a = State SCState a

data SCState = SCState 
    { _fresh_num    :: Int   -- fresh identifier numbers
    , _global_cte   :: Env   -- 
    }

type CG a = State CGState a

data CGState = CGState
    { _lambda_count :: Int
    , _lambda_todo  :: [(Int, AST)]
    }
makeLenses ''SCState
makeLenses ''CGState

runSC :: SC a -> a
runSC = flip evalState (SCState 0 initialEnv)

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
xeConstExpr e cte = return $ make_lit [] $ litOf e
  where litOf (Number n) = LitInt $ fromInteger n -- no warning if out of bounds

xeIdentExpr :: LispVal -> Env -> SC AST
xeIdentExpr (Atom e) cte = do 
    b <- xeLookup e cte
    if isVar b then return $ make_ref [] b
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

initialEnv :: Env
initialEnv =
  [ make_macro "dot" $ \es cte ->
      case tail es of
          [Char c] -> do
              x <- new_var "x"
              return $ make_lam [make_prim [] ("%dot:" ++ show c), make_ref [] x] [x]

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
      then xe (List [Atom "let", List [List [Atom "f", second es]]
                               , List [Atom "if", List [Atom "pointer-eq?", Atom "d", Atom "f"]
                                                , List [Atom "delay", third es]
                                                , List [Atom "f", third es]]]) cte
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

fv :: AST -> [Binding]
fv AST { ast_subx = subx, ast_node = nd } = case nd of
    Ref var -> [var]
    Set var -> union (fv (head subx)) [var]
    Lam ps _ -> diff (fv (head subx)) ps
    _       -> unionMulti $ map fv subx

--------------------------------------------------------------------------------
-- CPS transformation
--------------------------------------------------------------------------------

cpsTransform :: AST -> SC AST
cpsTransform ast = do
    r <- new_var "r"
    let cont_ast = make_lam [make_prim [make_ref [] r] "%halt"] [r]
    astCPS <- cps ast cont_ast
    case lookup "call/cc" (fv ast) of
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
                            kref <- xform (make_ref [] k)
                            return $ make_app [make_lam1 kref [k], cont_ast]
    Prim op  -> cpsList subx $ \args ->
                  pure $ make_app [cont_ast, make_prim args op]
    App      -> let fn = head subx
                in case ast_node fn of
                    Lam ps False -> cpsList (tail subx) $ \vals -> do
                        newBody <- cpsSeq (ast_subx fn) cont_ast
                        return $ make_app $ make_lam1 newBody ps : vals
                    _ -> cpsList subx $ \args ->
                        pure $ make_app $ head args : cont_ast : tail args
    Lam ps b -> do k <- new_var "k"
                   body <- cpsSeq subx (make_ref [] k)
                   return $ make_app [cont_ast, make_closure1 body (k:ps) b]
    Seq      -> cpsSeq subx cont_ast
                            
cpsList :: [AST] -> ([AST] -> SC AST) -> SC AST
cpsList [] inside = inside []
cpsList (ast:asts) inside = case ast_node ast of
    Lit{} -> body ast
    Ref{} -> body ast
    _     -> do r <- new_var "r"
                contBody <- body (make_ref [] r)
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
-- closure conversion
--------------------------------------------------------------------------------

closureConvert :: AST -> SC AST
closureConvert ast = make_lam1 <$> convert ast (error "Panic! No top-level closure") []
                               <*> pure []

convert :: AST -> Binding -> [Binding] -> SC AST
convert ast self fvs = cc ast
  where
    cc :: AST -> SC AST
    cc ast@AST{ ast_subx=subx } = let ccSubx = mapM cc subx in case ast_node ast of
        Ref var -> pure $ case elemIndex var fvs of
            Just i  -> make_prim [make_ref [] self, make_lit [] (LitInt (i + 1))] "%closure-ref"
            Nothing -> ast
        Set var -> make_set <$> ccSubx <*> pure var
        Cnd     -> make_cnd <$> ccSubx
        Prim op -> make_prim <$> ccSubx <*> pure op
        App     -> ccApp ast
        Lam{}   -> ccLam ast

    ccApp :: AST -> SC AST
    ccApp AST{ ast_subx = (fn : noCCArgs), ast_node = App } = do
        args <- mapM cc noCCArgs
        case ast_node fn of
            Lam ps False -> do
                -- NOTE [CPS Lam subx]: after cps transform, all lams have 1 subx
                body <- cc (head $ ast_subx fn)
                return $ make_app $ make_lam1 body ps : args
            _ -> do
                f <- cc fn
                return $ make_app $ make_prim [f, make_lit [] (LitInt 0)] "%closure-ref"
                                  : f : args
    
    ccLam :: AST -> SC AST -- see note [CPS Lam subx]
    ccLam ast@AST{ ast_subx = [body], ast_node = Lam ps isProm } = do
        let newFVs = filter (not . isGlobalVar) $ fv ast
        newSelf <- new_var "self"
        newBody <- convert body newSelf newFVs
        closureArgs <- mapM (\v -> cc (make_ref [] v)) newFVs
        return $ make_prim (make_closure1 newBody (newSelf : ps) isProm : closureArgs)
                           (if isProm then "%promise" else "%closure")

--------------------------------------------------------------------------------
-- perform all the transformations
--------------------------------------------------------------------------------

transformPhase :: LispVal -> AST
transformPhase val = runSC $ xe val [] >>= cpsTransform >>= closureConvert

--------------------------------------------------------------------------------
-- code generation
--------------------------------------------------------------------------------

type CCode    = String
type Var      = Maybe Binding
type Vars     = [Var]
type StackEnv = [Var]

codeGenerate :: AST -> CCode
codeGenerate ast = unlines $ runCG $ do
    addLambdaTodo ast
    code <- compileAllLambdas
    firstUnusedLabel <- use lambda_count
    return [ "#define NB_GLOBALS " ++ show (length globalVars)
           , "#define MAX_STACK 100"
           , "#define USED_D " ++ show firstUnusedLabel -- dont check if it was really used
           , codePrefix, code, codeSuffix ]
  where
    globalVars = fv ast
    
    codeGen :: AST -> StackEnv -> CG String
    codeGen ast stackEnv = go ast
      where
        codeGenList :: [AST] -> Vars -> StackEnv -> String
                    -> (String -> StackEnv -> CG String)
                    -> CG String
        codeGenList [] _ stackEnv _ cont = cont "" stackEnv
        codeGenList (ast:asts) (var:vars) stackEnv sep cont = do
            astCode <- codeGen ast stackEnv
            codeGenList asts vars (var : stackEnv) sep $ \code se ->
                cont (concat [astCode, sep, code]) se
    
        codeGenArgs :: [AST] -> StackEnv -> CG String
        codeGenArgs args stackEnv =
            codeGenList args (replicate (length args) Nothing) 
                        stackEnv "" $ \code se -> pure code
        
        accessVar :: Var -> StackEnv -> String
        accessVar Nothing _ = error "Attempt to access non-var during code generation"
        accessVar (Just v@(Binding _ (BindVar uid))) stackEnv
          | isGlobalVar v = let i = unsafeElemIndex v globalVars
                            in concat ["GLOBAL(", show i, "/*", uid, "*/)"]
          | otherwise = let i = length stackEnv - unsafeElemIndex (Just v) stackEnv - 1
                        in concat ["LOCAL(", show i, "/*", uid, "*/)"]

        codeGenPrim :: String -> [AST] -> CG String
        codeGenPrim op args 
          | Just charLit <- dropPrefix "%dot:" op = pure $ " DOT_C(" ++ charLit ++ ");"
          | op `elem` ["%halt", "%pointer-eq?"] =
              concat <$> sequence [codeGenArgs args stackEnv, pure (op2code op)]         
        codeGenPrim cloOp (closureLam : args)
          | cloOp `elem` ["%closure", "%promise"] = do
            i <- addLambdaTodo closureLam
            let n = length args
                s = concat ["CLOSURE(", show i, ",", show n, ");"]
                e = if cloOp == "%closure" then s
                    else concat ["PROMISE(", show i, ",", show n, ");"]
                mkINICLO j = " INICLO(" ++ show j ++ ");"
            argsCode <- codeGenArgs args stackEnv
            return $ concat $ [argsCode, " BEGIN_", s]
                           ++ map mkINICLO [n, n-1 .. 1]
                           ++ [" END_", e]
        codeGenPrim "%closure-ref" [f, AST{ ast_node = Lit (LitInt i) }] = do
            evalF <- go f
            return $ concat [evalF, " TOS() = CLOSURE_REF(TOS(),", show i, ");"]
        codeGenPrim op _ = error $ "Unknown primop during codegen: " ++ op
        
        codeGenApp :: [AST] -> CG String
        codeGenApp (fn : args) = do
            let n = length args
            case ast_node fn of
                Lam ps False -> codeGenList args (map Just ps) 
                            stackEnv "\n" $ \code se ->
                              concat <$> sequence [pure code, codeGen (head (ast_subx fn)) se]
                _ -> codeGenList args (replicate n Nothing) 
                       stackEnv "\n" $ \code se ->
                         let start = length stackEnv
                             s = "JUMP(" ++ show n ++ ");"
                             mkPushArg i = concat [" PUSH(LOCAL(", show (i + start), "));"]
                         in return $ concat $ [code, " BEGIN_", s]
                                           ++ map mkPushArg [0 .. n - 1]
                                           ++ [" END_", s]

        go :: AST -> CG String
        go AST{ ast_subx=subx, ast_node=nd } = case nd of
            Ref var -> pure $ concat [" PUSH(", accessVar (Just var) stackEnv, ");"]
            -- concat [go (head subx), " ", accessVar ..., " = TOS();"] but the types work
            Set var -> concat <$> liftA2 (:) (go (head subx)) 
                         (pure [" ", accessVar (Just var) stackEnv, " = TOS();"])
            Cnd -> do subxCode <- mapM go subx
                      let [test, t, f] = subxCode
                      return $ concat [ test, "\n if (POP()) {\n"
                                      , t,    "\n } else {\n"
                                      , f,    "\n }"]
            Prim op -> codeGenPrim op subx
            App -> codeGenApp subx
            _ -> error "Panic! Impossible Lam or Seq node after CPS transform"

    compileAllLambdas :: CG String
    compileAllLambdas = popLambdaTodo >>= \mp -> case mp of
        Nothing -> pure ""
        Just (i, ast@AST{ ast_subx=[body], ast_node=Lam ps _ }) -> do
            bodyCode <- codeGen body (map Just $ reverse ps)
            rest <- compileAllLambdas
            return $ concat ["case ", show i, ": /* ", limit (show (source ast)) 60, " */\n"
                            , bodyCode, "\n\n", rest]

op2code :: String -> String
op2code op | Just charLit <- dropPrefix "%dot:" op
           = " DOT(" ++ charLit ++ ")"
op2code "%halt" = " HALT();"
op2code "%pointer-eq?" = " PTR_EQ();"
     
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

compile :: String -> CCode
compile src = codeGenerate 
            $ transformPhase 
            $ unl2Scm
            $ parseUnl src

compileFile :: FilePath -> IO ()
compileFile fp = do
    src <- readFile fp
    let code = compile src
    writeFile (fp -<.> ".c") code

debug :: String -> IO ()
debug code = mapM_ putStrLn [
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
    (expanded, cpst, ccd) = runSC $ do
        expanded <- xe scheme []
        cpst     <- cpsTransform expanded
        ccd      <- closureConvert cpst
        return (expanded, cpst, ccd)
    ccode = codeGenerate ccd

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

diff :: Eq a => [a] -> [a] -> [a]
diff = (\\)

union :: Eq a => [a] -> [a] -> [a]
union [] s2 = s2
union (x:xs) s2 | x `elem` s2 = union xs s2
                | otherwise   = x : union xs s2

unionMulti :: Eq a => [[a]] -> [a]
unionMulti = foldr union []

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
    App -> case head subx of
        AST { ast_subx = body, ast_node = inner_nd } -> case inner_nd of
            Lam ps False -> 
                let bindings = map (\(Binding _ (BindVar uid), a) -> List [Atom uid, source a])
                                   (zip ps (tail subx))
                in List $ [Atom "let", List bindings] ++ map source body
            _ -> List $ map source subx
    Lam ps False -> List $ [Atom "lambda", List $ map (Atom . var_uid) ps] ++ map source subx
    Lam ps True  -> List [Atom "delay", List $ map (Atom . var_uid) ps, source (head subx)]
    Seq -> List $ Atom "begin" : map source subx
