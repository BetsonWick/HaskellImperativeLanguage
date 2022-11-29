module HW3.Evaluator where

import Control.Monad.Trans.Except
import Data.Ratio (denominator, numerator)
import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Void (Void)
import HW3.Base
import Data.Ix
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Foldable (fold, toList)
import Control.Monad (foldM)

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExcept

evalExcept :: Monad m => HiExpr -> ExceptT HiError m HiValue
evalExcept (HiExprValue value) = return value
evalExcept (HiExprApply s list) = do
  evaluated <- evalExcept s
  evalList evaluated list

exprParsed :: Either (ParseErrorBundle String Void) HiExpr -> HiExpr
exprParsed (Right expr) = expr
exprParsed (Left _) = HiExprValue (HiValueNumber 1)

evalList :: Monad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalList f@(HiValueFunction HiFunList) list = do
  evaluatedList <- mapM evalExcept list
  evalMany f evaluatedList
evalList fun [h] = do
  first <- evalExcept h
  evalUnary fun first
evalList fun [h, t] = do
  first <- evalExcept h
  second <- evalExcept t
  evalBinary fun first second
evalList fun [h, t, s] = do
  first <- evalExcept h
  second <- evalExcept t
  third <- evalExcept s
  evalTernary fun first second third
evalList _ _ = throwE HiErrorArityMismatch

evalUnary :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalUnary (HiValueString str) v = case v of
  HiValueNumber n -> do
    toInt <- tryInt n
    return $
      if toInt < 0 || toInt >= T.length str
        then HiValueNull
        else HiValueString $ T.pack [T.unpack str !! toInt]
  _ -> throwE HiErrorInvalidFunction
evalUnary fun (HiValueList list) = case fun of
  HiValueFunction HiFunLength -> return $ HiValueNumber $ toRational $ S.length list
  HiValueFunction HiFunReverse -> return $ HiValueList $ S.reverse list
  _ -> throwE HiErrorInvalidFunction
evalUnary fun (HiValueString str) = case fun of
  HiValueFunction HiFunToLower -> return $ HiValueString $ T.toLower str
  HiValueFunction HiFunToUpper -> return $ HiValueString $ T.toUpper str
  HiValueFunction HiFunLength -> return $ HiValueNumber $ toRational $ T.length str
  HiValueFunction HiFunReverse -> return $ HiValueString $ T.reverse str
  HiValueFunction HiFunTrim -> return $ HiValueString $ T.strip str
  _ -> throwE HiErrorInvalidFunction
evalUnary fun (HiValueBool bool) = case fun of
  HiValueFunction HiFunNot -> return $ HiValueBool $ not bool
  _ -> throwE HiErrorInvalidFunction
evalUnary _ _ = throwE HiErrorInvalidArgument

foldValues :: Monad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
foldValues _ [] = return $ HiValueList S.empty
foldValues f (h:t) = foldM (evalBinary f) h t

wrapNum :: Integer -> HiValue
wrapNum n = HiValueNumber (toRational n)

evalBinary :: Monad m => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalBinary (HiValueString str) first second = trySlice str (first, second)
evalBinary (HiValueFunction HiFunFold) fun (HiValueList list) = foldValues fun (toList list)
evalBinary fun (HiValueString str1) (HiValueString str2) = case fun of
  HiValueFunction HiFunAdd -> return $ HiValueString (T.concat [str1, str2])
  HiValueFunction HiFunDiv -> return $ HiValueString (T.concat [str1, T.pack "/", str2])
  _ -> throwE HiErrorInvalidFunction
evalBinary fun (HiValueString str) (HiValueNumber n) = case fun of
  HiValueFunction HiFunMul -> do
    toInt <- tryInt n
    return $ HiValueString (T.replicate toInt str)
  _ -> throwE HiErrorInvalidFunction
evalBinary fun (HiValueNumber n) (HiValueString str) = case fun of
  HiValueFunction HiFunMul -> do
    toInt <- tryInt n
    return $ HiValueString (T.replicate toInt str)
  _ -> throwE HiErrorInvalidFunction
evalBinary fun (HiValueBool bool1) (HiValueBool bool2) = case fun of
  HiValueFunction HiFunAnd -> return $ HiValueBool $ (&&) bool1 bool2
  HiValueFunction HiFunOr -> return $ HiValueBool $ (||) bool1 bool2
  _ -> throwE HiErrorInvalidFunction
evalBinary fun (HiValueNumber num1) (HiValueNumber num2) = case fun of
  HiValueFunction HiFunAdd -> return $ HiValueNumber (num1 + num2)
  HiValueFunction HiFunDiv -> return $ HiValueNumber (num1 / num2)
  HiValueFunction HiFunSub -> return $ HiValueNumber (num1 - num2)
  HiValueFunction HiFunMul -> return $ HiValueNumber (num1 * num2)
  HiValueFunction HiFunRange -> return $ HiValueList (S.fromList $ map wrapNum (range (ceiling num1, floor num2)))
  _ -> throwE HiErrorInvalidFunction
evalBinary fun a b = case fun of
  HiValueFunction HiFunEquals -> return $ HiValueBool (a == b)
  HiValueFunction HiFunGreaterThan -> return $ HiValueBool (a > b)
  HiValueFunction HiFunLessThan -> return $ HiValueBool (a < b)
  HiValueFunction HiFunNotGreaterThan -> return $ HiValueBool (a >= b)
  HiValueFunction HiFunNotLessThan -> return $ HiValueBool (a >= b)
  _ -> throwE HiErrorInvalidFunction
evalBinary _ _ _ = throwE HiErrorInvalidArgument

evalTernary :: Monad m => HiValue -> HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalTernary fun (HiValueBool bool) x y = case fun of
  HiValueFunction HiFunIf -> return $ if bool then x else y
  _ -> throwE HiErrorInvalidFunction
evalTernary _ _ _ _ = throwE HiErrorInvalidArgument

evalMany :: Monad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
evalMany fun list = case fun of
  HiValueFunction HiFunList -> return $ HiValueList (S.fromList list)
  _ -> throwE HiErrorInvalidFunction

tryInt :: Monad m => Rational -> ExceptT HiError m Int
tryInt rational = case quotRem (numerator rational) (denominator rational) of
  (num, 0) -> return $ fromIntegral num
  _ -> throwE HiErrorInvalidArgument

trySlice :: Monad m => T.Text -> (HiValue, HiValue) -> ExceptT HiError m HiValue
trySlice text bounds = case bounds of
  (HiValueNull, HiValueNull) -> trySlice text (HiValueNumber 0, HiValueNumber (toRational $ T.length text))
  (HiValueNull, HiValueNumber n) -> trySlice text (HiValueNumber 0, HiValueNumber n)
  (HiValueNumber n, HiValueNull) -> trySlice text (HiValueNumber n, HiValueNumber (toRational $ T.length text))
  (HiValueNumber n1, HiValueNumber n2) -> do
    tryFirst <- tryInt n1
    trySecond <- tryInt n2
    return $ HiValueString (T.take (trySecond - tryFirst) $ T.drop tryFirst text)
  _ -> throwE HiErrorInvalidArgument
