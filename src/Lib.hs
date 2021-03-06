{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Lib (
  lineP
) where
import Text.Megaparsec
import Text.Megaparsec.Char       as MC
import Text.Megaparsec.Char.Lexer as MCL
import Data.Char
import Data.Void(Void)
import Data.Decimal
import Debug.Trace
import Control.Monad

type Parser = Parsec Void String

decimalP :: Parser Decimal
decimalP = do intPart <- some digitChar
              _ <- optional . try $ char '.'
              fractPart1 <- optional . try $ some digitChar
              let fractPart2 = case fractPart1 of Just a  -> a
                                                  Nothing -> ""
              let n = strToInt $ intPart ++ fractPart2
                  e = length fractPart2
              return $ Decimal (fromIntegral e) n
           where strToInt :: (Integral n) => String -> n
                 strToInt = foldl (\t v -> 10 * t + v) 0 . map (fromIntegral . subtract (ord '0') . ord)


multP :: Decimal -> Bool -> Parser String
multP m v = do before1 <- some (satisfy $ not . (liftM4 or4 isNumber (== '-') (== '+') (== '=')))
               eq <- single '='
               before2 <- some (satisfy isSpace)
               num <- MCL.signed MC.space decimalP
               after1 <- optional . try $ (some (satisfy $ not . liftM2 (||) isNumber (== '=')) <|> some spaceChar)
               let after2 = case after1 of Just a  -> a
                                           Nothing -> ""
               let ol = concat [before1, [eq], before2, show num, after2]
               let l  = concat [before1, [eq], before2, show (num *. m), after2]
               return (case v of True  -> trace ("Set " ++ show num ++ " to " ++ show (num *. m) ++ " in:\n" ++ ol ++ "\n") l
                                 False -> l)
               where or4 a b c d = ((a || b) || c) || d

lineP :: Decimal -> Bool -> Parser String
lineP m v  = (try (multP m v)) <|> (try genericP) <|> emptyP
             where genericP :: Parser String
                   genericP = some anySingle
                   emptyP :: Parser String
                   emptyP = do MC.space
                               return ""
