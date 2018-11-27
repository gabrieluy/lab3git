-- Ivan Aneff 162693 , Gabriel Nuñez 153602 
module Compiler where

import AbsPascal
import LexPascal
import ParPascal
import ErrM
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath.Posix
import Control.Monad
import Control.Monad.Trans.State.Lazy
--import Control.Monad.State

import TypeChecker

-- Environment
data Env = Env {
    code       :: [String]
  , varsMemPos :: Map Ident Int
  , funs       :: Map Ident FunType
  , nextMemPos :: Int
  , label      :: Int
  , maxStack   :: Int   -- para calcular el stack
  }

initialEnv :: Env
initialEnv = Env {
    code       = []
  , varsMemPos = Map.empty
  , funs       = Map.empty
  , nextMemPos = 1       -- posicion 0 es reservada para el argumento del main
  , label      = 0
  , maxStack   = 0
  }

newLabel :: State Env String
newLabel = do
  s <- get
  put $ s { label = label s + 1 }
  return $ "label" ++ show (label s)

emit :: String -> State Env ()
emit c = modify $ \ s -> s { code = c : code s } -- code es generado de forma reversa por eficiencia

extendBuiltinDefs :: State Env ()
extendBuiltinDefs = mapM_ ( \ (f,(argTys,rty)) -> extendFunEnv f $ funJVM f "Runtime" argTys rty) buildInFunctions

extendFunEnv :: Ident -> FunType -> State Env ()
extendFunEnv i ft = modify $ \ s -> s { funs = Map.insert i ft (funs s) }

lookupFun :: Ident -> State Env FunType
lookupFun f = gets (\ s -> fromJust $ Map.lookup f (funs s))

lookupVar :: Ident -> State Env String
lookupVar x = gets (\ s -> show $ fromJust $ Map.lookup x (varsMemPos s))

size :: Type -> Int
size Type_real = 2
size _         = 1

makeCtxVarM :: VarPart -> State Env String
makeCtxVarM VPartEmpty = gets (\ s -> show $ nextMemPos s)
makeCtxVarM (VPart ds) = do {
  mapM_ (\ (VDecl ids t) ->
            mapM (\ id -> modify $ \ s -> s { varsMemPos = Map.insert id (nextMemPos s) (varsMemPos s),
                                              nextMemPos = nextMemPos s + size t   })
            ids)
         ds;
  gets (\ s -> show $ nextMemPos s)
  }

compile :: Program -> [String]
compile p = reverse $ code $ execState (compileProg p) initialEnv

compileProg :: Program -> State Env ()
compileProg (PBlock (Ident name) varPart stms) = do
  emit $ ".class public " ++ name
  emit $ ".super java/lang/Object"
  emit $ ""
  emit $ ".method public static main([Ljava/lang/String;)V"
  extendBuiltinDefs
  varMem <- makeCtxVarM varPart
  emit $ ".limit locals " ++ varMem
  emit $ ".limit stack  1000"  -- aca se puede ser mas preciso
  mapM_ compileStm stms
  emit $ "return"
  emit $ ".end method"
  emit ""

compileStm :: Stm -> State Env () 
compileStm (SAss ident (ETyped exp ty)) = do 
                                            dirMem <- lookupVar ident
                                            compileExp (ETyped exp ty)
                                            case ty of 
                                              Type_integer -> emit $ "istore " ++ dirMem
                                              Type_real ->  emit $ "dstore " ++ dirMem
                                              Type_bool -> emit $ "istore " ++ dirMem
                                              Type_string -> emit $ "astore " ++ dirMem
compileStm (SCall ident exps)           = do
                                            mapM_ compileExp exps
                                            dirMem <- lookupFun ident
                                            emit dirMem
compileStm (SCallEmpty ident)           = do                                            
                                            dirMem <- lookupFun ident
                                            emit dirMem
compileStm (SRepeat stm exp)            = do
                                            test <- newLabel
                                            end <- newLabel
                                            emit $ test ++ ":"
                                            compileStm stm
                                            compileExp exp
                                            emit $ "ifeq " ++ test
                                            emit $ "goto " ++ end
                                            emit $ end ++ ":"
compileStm (SWhile exp stm)             = do
                                            test <- newLabel
                                            end <- newLabel
                                            emit $ test ++ ":"
                                            compileExp exp
                                            emit $ "ifeq " ++ end
                                            compileStm stm
                                            emit $ "goto " ++ test
                                            emit $ end ++ ":"
compileStm (SBlock stms)                = mapM_ compileStm stms;
compileStm (SFor ident exp1 exp2 stm)   = do
                                            test <- newLabel
                                            end <- newLabel
                                            dirMem <- lookupVar ident
                                            compileExp exp1
                                            emit $ "istore " ++ dirMem
                                            emit $ test ++ ":"
                                            emit $ "iload " ++ dirMem
                                            compileExp exp2 
                                            emit $ "if_icmpgt " ++ end
                                            compileStm stm
                                            emit $ "iload " ++ dirMem
                                            emit $ "bipush 1"
                                            emit $ "iadd"
                                            emit $ "istore " ++ dirMem
                                            emit $ "goto " ++ test
                                            emit $ end ++ ":"
compileStm (SIf exp stm1 stm2)          = do
                                            false <- newLabel
                                            true <- newLabel
                                            compileExp exp
                                            emit $ "ifeq " ++ false
                                            compileStm stm1
                                            emit $ "goto " ++ true
                                            emit $ false ++ ":"
                                            compileStm stm2
                                            emit $ true ++ ":"

compileStm (SEmpty)                     = return ()

data Cmp = CEq | CDiff | CLe | CLeq | CGeq | CGt

-- Ranta en la pagin 121 explica la compilacion de comparaciones
showCmpInt :: Cmp -> String
showCmpInt CEq     = "if_icmpeq "
showCmpInt CDiff   = "if_icmpne "
showCmpInt CLe     = "if_icmplt "
showCmpInt CLeq    = "if_icmple "
showCmpInt CGeq    = "if_icmpge "
showCmpInt CGt     = "if_icmpgt "

showCmpReal :: Cmp -> String
showCmpReal CEq     = "ifeq "
showCmpReal CDiff   = "ifne "
showCmpReal CLe     = "iflt "
showCmpReal CLeq    = "ifle "
showCmpReal CGeq    = "ifge "
showCmpReal CGt     = "ifgt "

data OpBin = Plus | Subst | Mul | Div | Div2 | Mod

showOpBinInt :: OpBin -> String
showOpBinInt Plus  = "iadd"
showOpBinInt Subst = "isub"
showOpBinInt Mul   = "imul"
showOpBinInt Div   = error "Caso imposible"
showOpBinInt Div2  = "idiv"
showOpBinInt Mod   = "imod"

showOpBinReal :: OpBin -> String
showOpBinReal Plus  = "dadd"
showOpBinReal Subst = "dsub"
showOpBinReal Mul   = "dmul"
showOpBinReal Div   = "ddiv"
showOpBinReal Div2  = error "Caso imposible"
showOpBinReal Mod   = error "Caso imposible"

data OpBinBool = And | Or
  deriving Eq

showOpBinBool :: OpBinBool -> String
showOpBinBool And = "ifeq "
showOpBinBool Or  = "ifne "

compileExp :: Exp -> State Env ()

compileExp ( ETyped ( EConv exp )  ty )       = do 
                                                  compileExp exp
                                                  emit $ "i2d"
compileExp ( ETyped ( EEq exp1 exp2)  _ )     = compileExpCompare CEq exp1 exp2
compileExp ( ETyped ( EDiff exp1 exp2)  _ )   = compileExpCompare CDiff exp1 exp2
compileExp ( ETyped ( ELe exp1 exp2)  _ )     = compileExpCompare CLe exp1 exp2
compileExp ( ETyped ( ELeq exp1 exp2)  _ )    = compileExpCompare CLeq exp1 exp2
compileExp ( ETyped ( EGeq exp1 exp2)  _ )    = compileExpCompare CGeq exp1 exp2
compileExp ( ETyped ( EGe exp1 exp2)  _ )     = compileExpCompare CGt exp1 exp2
compileExp ( ETyped ( EPlus exp1 exp2)  ty )  = compileExpAritmeticas exp1 exp2 ty Plus
compileExp ( ETyped ( ESubst exp1 exp2)  ty ) = compileExpAritmeticas exp1 exp2 ty Subst
compileExp ( ETyped ( EOr exp1 exp2)  ty )    = compileExpBool Or exp1 exp2
compileExp ( ETyped ( EMul exp1 exp2)  ty )   = compileExpAritmeticas exp1 exp2 ty Mul
compileExp ( ETyped ( EDiv exp1 exp2)  ty )   = compileExpAritmeticas exp1 exp2 ty Div
compileExp ( ETyped ( EAnd exp1 exp2)  ty )   = compileExpBool And exp1 exp2
compileExp ( ETyped ( EMod exp1 exp2)  ty )   = compileExpAritmeticas exp1 exp2 ty Mod
compileExp ( ETyped ( EDiv2 exp1 exp2)  ty )  = compileExpAritmeticas exp1 exp2 ty Div2
compileExp ( ETyped ( ECall ident exps)  ty ) = do 
                                                  mapM_ compileExp exps
                                                  dirMem <- lookupFun ident
                                                  emit dirMem                                       
compileExp ( ETyped ( ECallEmpty ident)  ty ) = do 
                                                  dirMem <- lookupFun ident
                                                  emit dirMem   
compileExp ( ETyped ( ENot exp)  ty )         = compileNotExp exp
compileExp ( ETyped ( ENegNum exp)  ty )      = compileNegNumExp exp
compileExp ( ETyped ( EPlusNum exp)  ty )     = compileExp exp                                             
compileExp ( ETyped ( EIdent ident)  ty )     = do
                                                  dirMem <- lookupVar ident
                                                  case ty of 
                                                    Type_integer -> emit $ "iload " ++ dirMem 
                                                    Type_real ->  emit $ "dload " ++ dirMem
                                                    Type_bool -> emit $ "iload " ++ dirMem
                                                    Type_string -> emit $ "aload " ++ dirMem                                   
compileExp (ETyped (EStr str) _)              = emit $ "ldc " ++ show str
compileExp (ETyped (EInt int) _)              = emit $ "ldc " ++ show int
compileExp (ETyped (EReal real) ty)           = emit $ "ldc2_w " ++ show real
compileExp (ETyped ETrue ty)                  = emit $ "ldc 1"
compileExp (ETyped EFalse ty)                 = emit $ "ldc 0"
 
compileExpAritmeticas :: Exp -> Exp -> Type -> OpBin -> State Env ()
compileExpAritmeticas exp1 exp2 ty op = do
                                          compileExp exp1
                                          compileExp exp2
                                          if (ty == Type_integer) then
                                            emit $ showOpBinInt op
                                          else if (ty == Type_real) then
                                            emit $ showOpBinReal op
                                          else
                                            error "Caso imposible"


compileNegNumExp :: Exp -> State Env ()
compileNegNumExp (ETyped exp ty) = do 
                                    compileExp exp
                                    if (ty == Type_integer) then
                                      emit "ineg"
                                    else if (ty == Type_real) then do
                                      emit "dneg"
                                    else
                                      error "Caso imposible"
compileNotExp :: Exp -> State Env ()
compileNotExp (ETyped exp ty)    = do
                                    false <- newLabel
                                    compileExp exp
                                    emit $ "dup"
                                    emit $ "ifeq " ++ false
                                    emit $ "bipush 1"
                                    emit $ "pop"
                                    emit $ false ++ ":"

compileExpCompare :: Cmp -> Exp -> Exp -> State Env ()
compileExpCompare cmp (ETyped exp1 ty1) exp2 = do -- solo necesito un tipo por el typechecker
                                                  true <- newLabel
                                                  emit $ "bipush 1"
                                                  compileExp (ETyped exp1 ty1)
                                                  compileExp exp2
                                                  if (ty1 == Type_integer) then
                                                    emit $ showCmpInt cmp ++ true
                                                  else if (ty1 == Type_real) then do
                                                    emit $ "dcmpg"
                                                    emit $ showCmpReal cmp ++ true
                                                  else
                                                    error "Caso imposible"
                                                  emit $ "pop"
                                                  emit $ "bipush 0"
                                                  emit $ true ++ ":"

compileExpBool :: OpBinBool -> Exp -> Exp -> State Env ()
compileExpBool Or exp1 exp2   = do 
                                  end <- newLabel
                                  emit $ "bipush 1"
                                  compileExp exp1
                                  emit $ showOpBinBool Or ++ end
                                  compileExp exp2
                                  emit $ showOpBinBool Or ++ end
                                  emit $ "pop"
                                  emit $ "bipush 0"
                                  emit $ end ++ ":"
compileExpBool And exp1 exp2  = do 
                                  end <- newLabel
                                  emit $ "bipush 0"
                                  compileExp exp1
                                  emit $ showOpBinBool And ++ end
                                  compileExp exp2
                                  emit $ showOpBinBool And ++ end
                                  emit $ "pop"
                                  emit $ "bipush 1"
                                  emit $ end ++ ":"                                                 
