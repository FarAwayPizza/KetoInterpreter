module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L 
import qualified Data.Map.Strict as Map
import Data.Void
import AST(CurrentDay(..))


type Parser = Parsec Void String
type NutrientName = String
type Nutrient = (String, Int)
type Result = [(NutrientName, Nutrient)]

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

integer :: Parser Int
integer = lexeme L.decimal

-- Parse names
name :: Parser String
name = lexeme (some (alphaNumChar <|> char '_'))

-- Parse one food item (for Effects compatibility)
parseFoodData :: Parser (String, Map.Map String Int)
parseFoodData = do
  fName <- name 
  _ <- symbol "{"
  weightVal <- symbol "weight" *> integer <* symbol ","
  fatVal <- symbol "fat" *> integer <* symbol ","
  carbsVal <- symbol "carbs" *> integer <* symbol ","
  potassiumVal <- symbol "potassium" *> integer <* symbol ","
  magnesiumVal <- symbol "magnesium" *> integer <* symbol ","
  sodiumVal <- symbol "sodium" *> integer <* symbol "}"
  
  let nutrients = Map.fromList
        [ ("weight", weightVal)
        , ("fat", fatVal)
        , ("carbs", carbsVal)
        , ("potassium", potassiumVal)
        , ("magnesium", magnesiumVal)
        , ("sodium", sodiumVal)
        ]
  return (fName, nutrients)

-- Parse all ingredients  
parseIngredients :: Parser [(String, Map.Map String Int)]
parseIngredients = do
  _ <- symbol "ingredients" >> symbol "{"
  foods <- many parseFoodData
  _ <- symbol "}"
  return foods

-- Parse a meal
parseMeal :: Parser (String, [(String, Int)])
parseMeal = do
  _ <- symbol "meal"
  mName <- name
  _ <- symbol "{"
  items <- many $ do
    foodName <- name   
    quantity <- integer
    return (foodName, quantity)
  _ <- symbol "}"
  return (mName, items)


parseDay :: Parser (CurrentDay, [(String, [(String, Int)])])
parseDay = do 
  _ <- symbol "{"
  _ <- symbol "day"
  parsedDay <- (Monday <$ symbol "Monday") <|> 
      (Tuesday <$ symbol "Tuesday") <|>
      (Wednesday <$ symbol "Wednesday") <|>
      (Thursday <$ symbol "Thursday") <|>
      (Friday <$ symbol "Friday") <|>
      (Saturday <$ symbol "Saturday") <|>
      (Sunday <$ symbol "Sunday")
  _ <- symbol "{"
  meals <- many parseMeal
  _ <- symbol "}" >> symbol "}"
  return (parsedDay, meals)

 

-- Parse the whole file (Effects-compatible version)
parseKetoDiet :: Parser ([(String, Map.Map String Int)], [(CurrentDay, [(String, [(String, Int)])])])
parseKetoDiet = do
  spaceConsumer
  ingredients <- parseIngredients
  _ <- symbol "mealplan" >> symbol "{"
  days <- many parseDay
  _ <- symbol "}"
  eof
  return (ingredients, days)

-- Run the parser (Effects-compatible)
runParser :: String -> Either String ([(String, Map.Map String Int)], [(CurrentDay, [(String, [(String, Int)])])])
runParser input = case parse parseKetoDiet "input" input of
  Left err -> Left (show err)
  Right result -> Right result



  -- Parse just ingredients from a file
parseIngredientsOnly :: Parser [(String, Map.Map String Int)]
parseIngredientsOnly = do
    spaceConsumer
    result <- parseIngredients
    eof
    return result

-- Parse just meal plan from a file
parseMealPlanOnly :: Parser [(CurrentDay, [(String, [(String, Int)])])]
parseMealPlanOnly = do
    spaceConsumer
    _ <- symbol "mealplan"
    _ <- symbol "{"
    days <- many parseDay
    _ <- symbol "}"
    eof
    return days

  
