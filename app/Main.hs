{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)
import Control.Monad (forM_, unless)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Control.Monad.Freer.Reader
import qualified Data.Map.Strict as Map
import Parser
import Effects  
import AST(CurrentDay(..), Food(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parse parseKetoDiet filename content of
        Left err -> putStrLn $ errorBundlePretty err
        Right (ingredients, days) -> runEffects ingredients days

    [ingredFile, mealFile] -> do
      ingredContent <- readFile ingredFile
      case parse parseIngredientsOnly ingredFile ingredContent of
        Left err -> putStrLn $ "Error in ingredients: " ++ errorBundlePretty err
        Right ingredients -> do
          mealContent <- readFile mealFile
          case parse parseMealPlanOnly mealFile mealContent of
            Left err -> putStrLn $ "Error in meals: " ++ errorBundlePretty err
            Right days -> runEffects ingredients days

    _ -> putStrLn "Usage: ketoInterpreter <file> OR ketoInterpreter <ingredients-file> <meals-file>"

-- Run all effects at the top level
runEffects :: [(String, Map.Map String Int)] -> [(CurrentDay, [(String, [(String, Int)])])] -> IO ()
runEffects ingredients days = do
  let constraints = DietConstraints
        { maxCarbs = 20
        , minFat = 100
        , maxFat = 200
        , goalSodium = 2000
        , goalPotassium = 3000
        , goalMagnesium = 400
        }
  
  -- Run effects in correct order  
  result <- runM 
    . runConsoleIO 
    . runDietOps
    . runReader constraints
    . evalState (NutritionState 0 0 0 0 0 0)
    . runWriter @[Violation]
    $ processAllDays ingredients days
  
  -- Handle the result
  case result of
    (_, violations) -> do
      unless (null violations) $ do
        putStrLn "\nOverall violations found:"
        forM_ violations print

-- Process all days
processAllDays :: Members '[Console, DietOps, State NutritionState, Writer [Violation], Reader DietConstraints] r => [(String, Map.Map String Int)] -> [(CurrentDay, [(String, [(String, Int)])])]  -> Eff r ()
processAllDays ingredients days = do
  send $ PrintLine "Parse successful!"
  
  -- Create food map
  let foodMap = Map.fromList
        [ (ingredientName, Food
            { foodName = ingredientName
            , sodium = Map.findWithDefault 0 "sodium" nutrients
            , potassium = Map.findWithDefault 0 "potassium" nutrients
            , magnesium = Map.findWithDefault 0 "magnesium" nutrients
            , fat = Map.findWithDefault 0 "fat" nutrients
            , carbs = Map.findWithDefault 0 "carbs" nutrients
            , weight = Map.findWithDefault 0 "weight" nutrients
            })
        | (ingredientName, nutrients) <- ingredients
        ]
  
  forM_ days $ \daily -> do 
    let (day, meals) = daily  
    put (NutritionState 0 0 0 0 0 0)
    processDay foodMap (day, meals)

-- Process a single day
processDay :: Members '[Console, DietOps, State NutritionState, Writer [Violation], Reader DietConstraints] r => Map.Map String Food -> (CurrentDay, [(String, [(String, Int)])]) -> Eff r ()
processDay foodMap (day, meals) = do
  send $ PrintLine $ "\n=== " ++ show day ++ " ==="
  
  -- Debug output (remove these lines for clean output)
  send $ PrintLine $ "Foods in meals: " ++ show [(fName, quantity) | (_, foods) <- meals, (fName, quantity) <- foods]
  send $ PrintLine $ "Foods available: " ++ show (Map.keys foodMap)
  
  -- Process all foods for the day
  forM_ meals $ \(mealName, foods) -> do
    send $ PrintLine $ "Processing meal: " ++ mealName
    forM_ foods $ \(localFoodName, quantity) -> do
      send $ PrintLine $ "Looking for: " ++ show localFoodName
      case Map.lookup localFoodName foodMap of
        Just food -> do
          send $ PrintLine $ "FOUND: " ++ localFoodName ++ " with " ++ show quantity ++ "g"
          let scaled = scaleFood food quantity
          send $ PrintLine $ "Scaled nutrition: " ++ show scaled
          modify (`addNutrition` scaled)
        Nothing -> send $ PrintLine $ "NOT FOUND: " ++ localFoodName
  
  -- Get final state
  finalState <- get
  
  -- Check constraints (this writes violations)
  checkConstraints
  
  -- Display results
  send $ PrintLine "\n  Daily Totals:"
  send $ PrintLine $ "    Carbs:     " ++ show (currentCarbs finalState) ++ "g"
  send $ PrintLine $ "    Fat:       " ++ show (currentFat finalState) ++ "g"
  send $ PrintLine $ "    Sodium:    " ++ show (currentSodium finalState) ++ "mg"
  send $ PrintLine $ "    Potassium: " ++ show (currentPotassium finalState) ++ "mg"
  send $ PrintLine $ "    Magnesium: " ++ show (currentMagnesium finalState) ++ "mg"

-- Helper function to scale food nutrition by quantity
scaleFood :: Food -> Int -> NutritionState
scaleFood (Food _ sodiumVal potassiumVal magnesiumVal fatVal carbsVal weightVal) grams =
  let scale :: Double = fromIntegral grams / 100.0
  in NutritionState
     { currentCarbs = round (fromIntegral carbsVal * scale)
     , currentFat = round (fromIntegral fatVal * scale)
     , currentWeight = round (fromIntegral weightVal * scale)
     , currentSodium = round (fromIntegral sodiumVal * scale)
     , currentPotassium = round (fromIntegral potassiumVal * scale)
     , currentMagnesium = round (fromIntegral magnesiumVal * scale)
     }

-- Helper function to add two nutrition states
addNutrition :: NutritionState -> NutritionState -> NutritionState
addNutrition n1 n2 = NutritionState
  { currentCarbs = currentCarbs n1 + currentCarbs n2
  , currentFat = currentFat n1 + currentFat n2
  , currentWeight = currentWeight n1 + currentWeight n2
  , currentSodium = currentSodium n1 + currentSodium n2
  , currentPotassium = currentPotassium n1 + currentPotassium n2
  , currentMagnesium = currentMagnesium n1 + currentMagnesium n2
  }
