{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}


module Evaluator where

import AST (Food(..), IngredientTable, ParsedMeal(..), Meal(..))
 
import Effects
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Error
import Control.Monad.Freer.Writer
import qualified Data.Map.Strict as Map
import Control.Monad (forM)

data Result = Result
    { totalCarbs :: Int
    , totalFat :: Int
    , totalProtein :: Int
    , isKeto :: Bool
    } deriving (Show)

type SimpleEffects = '[State NutritionState, Error String, Writer [Violation]]

resolveItem :: IngredientTable -> (String, Int) -> Either String (Food, Int)
resolveItem ingredientTable (nameStr, qty) =
  case Map.lookup nameStr ingredientTable of
    Just foundFood -> Right (foundFood, qty)
    Nothing   -> Left $ "Unknown ingredient: " ++ nameStr

resolveMeal :: IngredientTable -> ParsedMeal -> Either String Meal
resolveMeal ingredientTable (ParsedMeal mName items) = do
  resolvedItems <- mapM (resolveItem ingredientTable) items
  return $ Meal mName resolvedItems


processItemsWithEffects :: Map.Map String Food -> [(String, [(String, Int)])] -> Eff SimpleEffects [String]
processItemsWithEffects foodMap meals = do
  allErrors <- forM meals $ \(mName, items) -> do
    itemErrors <- forM items $ \(itemName, quantity) -> 
      case Map.lookup itemName foodMap of
        Nothing -> return $ "Unknown food: " ++ itemName
        Just matchedFood -> do
          let scaled = scaleFood matchedFood quantity
          modify (`addNutrition` scaled)
          return $ "Unknown food: " ++ itemName ++ " (in meal: " ++ mName ++ ")"


    return $ filter (not . null) itemErrors

  return $ concat allErrors

checkSimpleViolations :: NutritionState -> DietConstraints -> [Violation]
checkSimpleViolations state cons =
  concat
    [ [CarbsViolation (currentCarbs state) (maxCarbs cons) | currentCarbs state > maxCarbs cons]
    , [FatMinViolation (currentFat state) (minFat cons) | currentFat state < minFat cons]
    , [FatMaxViolation (currentFat state) (maxFat cons) | currentFat state > maxFat cons]
    , [SodiumGoalsMissed (currentSodium state) (goalSodium cons) | abs (currentSodium state - goalSodium cons) > 500]
    , [PotassiumGoalsMissed (currentPotassium state) (goalPotassium cons) | abs (currentPotassium state - goalPotassium cons) > 500]
    , [MagnesiumGoalsMissed (currentMagnesium state) (goalMagnesium cons) | abs (currentMagnesium state - goalMagnesium cons) > 100]
    ]

evaluateDaySimple :: Map.Map String Food -> [(String, [(String, Int)])] -> NutritionState -> DietConstraints -> (Maybe NutritionState, [Violation], [String])
evaluateDaySimple foodMap meals initialState constraints =
  case run $
      runWriter @([Violation]) $
      runError @String $
      runState initialState $
      processItemsWithEffects foodMap meals of
  (Left err, _) -> (Nothing, [], [err])
  (Right (warnings, state), _) ->
    let violations = checkSimpleViolations state constraints
    in (Just state, violations, warnings)



scaleFood :: Food -> Int -> NutritionState
scaleFood (Food _ s p m f c w) grams =
    let scale = (fromIntegral grams :: Double) / 100.0
    in NutritionState
        { currentCarbs = round (fromIntegral c * scale)
        , currentFat = round (fromIntegral f * scale)
        , currentWeight = round (fromIntegral w * scale)
        , currentSodium = round (fromIntegral s * scale)
        , currentPotassium = round (fromIntegral p * scale)
        , currentMagnesium = round (fromIntegral m * scale)
        }

addNutrition :: NutritionState -> NutritionState -> NutritionState
addNutrition n1 n2 = NutritionState
    { currentCarbs = currentCarbs n1 + currentCarbs n2
    , currentFat = currentFat n1 + currentFat n2
    , currentWeight = currentWeight n1 + currentWeight n2
    , currentSodium = currentSodium n1 + currentSodium n2
    , currentPotassium = currentPotassium n1 + currentPotassium n2
    , currentMagnesium = currentMagnesium n1 + currentMagnesium n2
    }
