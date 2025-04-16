module Evaluator where

import Data.List.NonEmpty

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

data Food = Food
  { foodName :: String
  , weight :: Weight
  , sodium :: Int
  , potassium :: Int
  , magnesium :: Int
  , fat :: Int
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

evaluator :: [a] -> Bool
evaluator = null





-- mealPlan :: [Int] -> MealPlan
-- day :: String -> day
-- substitutions :: ingredient -> [Ingredient]
-- totalCarbs :: [Int] -> Int
-- total Sodium :: [Int] -> Int
-- total potassium :: [Int] -> Int
-- total magnesium :: [Int] -> Int
-- total Fat :: [Int] -> Int
-- maxCarbs = 20
-- maxFat = 40
-- minFat = 20
-- GoalPotassium
-- GoalSodium
-- Goal magnesium

-- allIngredients :: Meal -> [Food]
