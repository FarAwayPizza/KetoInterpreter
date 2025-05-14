module AST where 

import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty) 

newtype Weight = Weight Int
  deriving (Eq, Show)
newtype Sodium = Sodium Int
  deriving (Eq, Show)
newtype Potassium = Potassium Int
  deriving (Eq, Show)
newtype Magnesium = Magnesium Int
  deriving (Eq, Show)
newtype Fat = Fat Int
  deriving (Eq, Show)
newtype Carbs = Carbs Int
  deriving (Eq, Show) 

type IngredientTable = Map.Map String Food 
 
data Food = Food
  { foodName :: String
  , sodium :: Int
  , potassium :: Int
  , magnesium :: Int
  , fat :: Int
  , weight :: Int
  , carbs :: Int 
  }
  deriving (Eq, Show)

data Meal = Meal
  { mealName :: String
  , food :: [(Food, Int)]
  }
  deriving (Eq, Show)

data DailyMealPlan = DailyMealPlan
  { day :: CurrentDay
  , meals :: NonEmpty Meal
  }
  deriving (Eq, Show)

data CurrentDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Show)


data FileAST = FileAST
  { ingredients :: IngredientTable
  , days :: [DailyMealPlan]
  } deriving (Eq, Show)


data ParsedMeal = ParsedMeal
  { parsedMealName :: String
  , parsedFoodItems :: [(String, Int)]
  } deriving (Eq, Show)

data ParsedDailyMealPlan = ParsedDailyMealPlan
  { parsedDay :: CurrentDay
  , parsedMeals :: [ParsedMeal]
  } deriving (Eq, Show)

data ParsedFileAST = ParsedFileAST
  { parsedIngredients :: IngredientTable
  , parsedDays :: [ParsedDailyMealPlan]
  } deriving (Eq, Show)
