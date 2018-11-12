module TypeChecker where

import AbsPascal
import PrintPascal
import ErrM

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State.Lazy
import Data.Maybe
--import Control.Monad.State

type FunType = String

typeJVM :: Maybe Type -> String
typeJVM (Just Type_integer)= "I"
typeJVM (Just Type_real)   = "D"
typeJVM (Just Type_bool)   = "I"
typeJVM (Just Type_string) = "Ljava/lang/String;"
typeJVM Nothing            = "V" -- void

funJVMType :: Ident -> [Type] -> Maybe Type -> FunType
funJVMType (Ident i) typs rty = i ++ "(" ++ (foldr (\ t s -> typeJVM (Just t) ++ s) "" typs) ++ ")" ++ typeJVM rty

funJVM :: Ident -> String -> [Type] -> Maybe Type -> FunType
funJVM i clas argty rty = "invokestatic " ++ clas ++"/" ++ funJVMType i argty rty

buildInFunctions :: [(Ident , ([Type], Maybe Type))]
buildInFunctions = [
  (Ident "writeStr"     , ([Type_string],Nothing)),
  (Ident "writeInt"     , ([Type_integer],Nothing)),
  (Ident "writeReal"    , ([Type_real]  ,Nothing)),
  (Ident "readInt"      , ([]           ,Just Type_integer)),
  (Ident "readReal"     , ([]           ,Just Type_real)),
  (Ident "readStr"      , ([]           ,Just Type_string)),
  (Ident "writeIntBool",  ([Type_integer], Just Type_bool))  -- Para testear and y or cortocicuitados
  ]

-- CHECK Program
typeCheckProgram :: Program -> Err Program
typeCheckProgram (PBlock name varPart stms) = do
  cv      <- makeCtxVar varPart
  stmsChk <- mapM (checkStm cv (Map.fromList buildInFunctions)) stms
  return $ PBlock name varPart stmsChk

type CtxVar = Map Ident Type
type CtxFun = Map Ident ([Type],Maybe Type)

makeCtxVar :: VarPart -> Err CtxVar
makeCtxVar VPartEmpty = return Map.empty
makeCtxVar (VPart ds) = foldM (\ c (VDecl ids t) ->
                                 foldM (\ c id ->
                                          if Map.member id c then
                                            fail "Variable redeclarada"
                                          else
                                            return $ Map.insert id t c)
                                        c ids)
                              Map.empty
                              ds

checkStm :: CtxVar -> CtxFun -> Stm -> Err Stm
checkStm cv cf (SAss x e)      = do
  tx <- typeIdent cv x
  et <- typeCheckExp cv cf e tx
  return $ SAss x et
checkStm cv cf (SCall f es)   = do
  (argTys , Nothing) <- typeFun cf f
  est <- mapM (checkArg cv cf) (zip argTys es)   -- conversion automatica
  return $ SCall f est
checkStm cv cf (SCallEmpty f) = checkStm cv cf (SCall f [])
checkStm cv cf (SRepeat s e) = do
  et <- typeCheckExp cv cf e Type_bool
  st <- checkStm cv cf s
  return $ SRepeat st et
checkStm cv cf (SWhile e s) = do
  et <- typeCheckExp cv cf e Type_bool
  st <- checkStm cv cf s
  return $ SWhile et st
checkStm cv cf (SBlock ss) = liftM SBlock $ mapM (checkStm cv cf) ss
checkStm cv cf (SFor x e1 e2 s) = do
  e1t <- typeCheckExp cv cf e1  Type_integer
  e2t <- typeCheckExp cv cf e2  Type_integer
  typeCheckExp cv cf (EIdent x) Type_integer
  st  <- checkStm cv cf s
  return $ SFor x e1t e2t st
checkStm cv cf (SIf e s1 s2) = do
  et  <- typeCheckExp cv cf e  Type_bool
  s1t <- checkStm cv cf s1
  s2t <- checkStm cv cf s2
  return $ SIf et s1t s2t
checkStm _ _ SEmpty  = return $ SEmpty
-- checkStm _ _ s       = error $ "Caso de instruccion no deninio " ++ show s

numeric :: Type -> Bool
numeric Type_integer = True
numeric Type_real    = True
numeric _            = False

typeCheckExp :: CtxVar  -> CtxFun -> Exp -> Type -> Err Exp
typeCheckExp cv cf e t = do {
  et@(ETyped _ te) <- typeInfExp cv cf e;
  if numeric t && numeric te && te < t then
    return $ convert et t
  else if te == t then
    return $ et
  else
    fail $ "Tipos incompatibles:" ++ show t ++ " y " ++ show te ++ " de la expresion " ++ show e
  }

compInf :: CtxVar -> CtxFun -> Exp -> Exp -> (Exp -> Exp -> Exp) -> Err Exp
compInf cv cf e1 e2 cmp = do {
  et1@(ETyped _ t1) <- typeInfExp cv cf e1;
  et2@(ETyped _ t2) <- typeInfExp cv cf e2;
  let tmax = max t1 t2 in
  if (numeric t1 && numeric t2) then -- comparadores solo para tipos numericos
    return $ ETyped (cmp (convert et1 tmax) (convert et2 tmax)) Type_bool
  else
    fail "Tipos incompatibles en comparacion"
  }

convert :: Exp -> Type -> Exp
convert e@(ETyped _ te) t = if te < t then (ETyped (EConv e) t) else e

opBinInf :: CtxVar -> CtxFun -> Exp -> Exp -> (Exp -> Exp -> Exp) -> Err Exp
opBinInf cv cf e1 e2 c = do {
  et1@(ETyped _ t1) <- typeInfExp cv cf e1;
  et2@(ETyped _ t2) <- typeInfExp cv cf e2;
  let tmax = max t1 t2 in
  if numeric t1 && numeric t2 then
    return $ ETyped (c (convert et1 tmax) (convert et2 tmax)) tmax
  else
    fail "Operador binario aritmetico aplicado a tipos no numericos"
  }

opBinInfInt :: CtxVar -> CtxFun -> Exp -> Exp -> (Exp -> Exp -> Exp) -> Err Exp
opBinInfInt cv cf e1 e2 c = do
  et1 <- typeCheckExp cv cf e1 Type_integer
  et2 <- typeCheckExp cv cf e2 Type_integer
  return $ ETyped (c et1 et2) Type_integer

opBinInfBool :: CtxVar -> CtxFun -> Exp -> Exp -> (Exp -> Exp -> Exp) -> Err Exp
opBinInfBool cv cf e1 e2 c = do
  et1 <- typeCheckExp cv cf e1 Type_bool
  et2 <- typeCheckExp cv cf e2 Type_bool
  return $ ETyped (c et1 et2) Type_bool

opUnInf :: CtxVar -> CtxFun -> Exp -> (Exp -> Exp) -> Err Exp
opUnInf cv cf e c = do {
  ETyped et t <- typeInfExp cv cf e;
  if numeric t then
    return $ ETyped (c et) t
  else
    fail "Operador unario aplicado a un tipo no numerico"
  }

typeInfExp :: CtxVar -> CtxFun -> Exp -> Err Exp
typeInfExp cv cf (EEq    e1 e2) = compInf  cv cf e1 e2 EEq
typeInfExp cv cf (EDiff  e1 e2) = compInf  cv cf e1 e2 EDiff
typeInfExp cv cf (ELe    e1 e2) = compInf  cv cf e1 e2 ELe
typeInfExp cv cf (ELeq   e1 e2) = compInf  cv cf e1 e2 ELeq
typeInfExp cv cf (EGeq   e1 e2) = compInf  cv cf e1 e2 EGeq
typeInfExp cv cf (EGe    e1 e2) = compInf  cv cf e1 e2 EGe
typeInfExp cv cf (EPlus  e1 e2) = opBinInf cv cf e1 e2 EPlus
typeInfExp cv cf (ESubst e1 e2) = opBinInf cv cf e1 e2 ESubst
typeInfExp cv cf (EMul   e1 e2) = opBinInf cv cf e1 e2 EMul
typeInfExp cv cf (EDiv   e1 e2) = do {
  et1 <- typeCheckExp cv cf e1 Type_real;
  et2 <- typeCheckExp cv cf e2 Type_real;
  return $ ETyped (EDiv et1 et2) Type_real
  }
typeInfExp cv cf (EMod   e1 e2) = opBinInfInt cv cf e1 e2 EMod
typeInfExp cv cf (EDiv2  e1 e2) = opBinInfInt cv cf e1 e2 EDiv2
typeInfExp cv cf (ECall  f  es) = do
  (argTys , Just t) <- typeFun cf f
  est <- mapM (checkArg cv cf) (zip argTys es)   -- aca ver casting !
  return $ ETyped (ECall f est) t
typeInfExp cv cf (ECallEmpty f) = typeInfExp cv cf (ECall f [])
typeInfExp cv cf (ENot       e) = typeCheckExp cv cf e Type_bool
typeInfExp cv cf (ENegNum    e) = opUnInf  cv cf e ENegNum
typeInfExp cv cf (EPlusNum   e) = opUnInf  cv cf e EPlusNum
typeInfExp cv cf e@(EStr     _) = return $ ETyped e      Type_string
typeInfExp cv cf e@(EInt     _) = return $ ETyped e      Type_integer
typeInfExp cv cf e@(EReal    _) = return $ ETyped e      Type_real
typeInfExp cv cf ETrue          = return $ ETyped ETrue  Type_bool
typeInfExp cv cf EFalse         = return $ ETyped EFalse Type_bool
typeInfExp cv cf e@(EIdent   x) = do
  t <- typeIdent cv x
  return $ ETyped e t
typeInfExp cv cf (EAnd e1 e2)   = opBinInfBool cv cf e1 e2 EAnd
typeInfExp cv cf (EOr  e1 e2)   = opBinInfBool cv cf e1 e2 EOr

typeIdent :: CtxVar -> Ident -> Err Type
typeIdent cv x =
  case Map.lookup x cv of
    Just t  -> return $ t
    Nothing -> fail "Variable no declarada"

typeFun :: CtxFun -> Ident -> Err ([Type],Maybe Type)
typeFun cf f =
  case Map.lookup f cf of
    Just t  -> return $ t
    Nothing -> fail "Funcion o Procedimiento no declarado"

checkArg :: CtxVar -> CtxFun -> (Type, Exp) -> Err Exp
checkArg cv cf (t, e) = do
          et@(ETyped _ te) <- typeInfExp cv cf e
          if numeric t && numeric te && te < t then return $ convert et t -- conversion automatica
          else if t == te then return $ et
               else fail "Tipos no compatibles en los argumentos de una aplicacion"
