module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

parseCharA :: Parser Char
parseCharA = char 'a'

parseCharB :: Parser Char
parseCharB = char 'b'

parseCharAB :: Parser Char
parseCharAB = do 
  parseCharA
  parseCharB


parsePotassium:: Parser Nutrient
parseSugar = do 
  _ <- string "potassium" 
  _ <- space1 
  amount <- L.float 
  return $ potassium amount


parseNutrient :: Parser Nutrient 
parseNutrient = try parsePotassium

someFunc :: IO()
someFunc = putStrLn "someFunc"



