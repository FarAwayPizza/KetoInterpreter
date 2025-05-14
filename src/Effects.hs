{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}

module Effects where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Error
import Control.Monad.Freer.Writer
import Control.Monad (when)
import qualified Data.Map.Strict as Map  -- Keep Map functions qualified
import Prelude 
import AST (Food(..))

data NutritionState = NutritionState
  { currentCarbs :: Int
  , currentSodium :: Int
  , currentPotassium :: Int
  , currentMagnesium :: Int
  , currentFat :: Int
  , currentWeight :: Int
  } deriving (Eq, Show)

data DietConstraints = DietConstraints
  { maxCarbs :: Int
  , minFat :: Int
  , maxFat :: Int
  , goalSodium :: Int
  , goalPotassium :: Int
  , goalMagnesium :: Int
  } deriving (Eq, Show)

data DietError
  = ExceedingCarbLimit Int Int
  | InvalidIngredient String
  | BelowFatMinimum Int Int
  | ExceededFatLimit Int Int
  | MissingNutrients [String]    
  | InvalidNutrientValue String Int 
  deriving (Eq, Show)

data Violation
  = CarbsViolation Int Int
  | FatMinViolation Int Int
  | FatMaxViolation Int Int
  | SodiumGoalsMissed Int Int
  | PotassiumGoalsMissed Int Int
  | MagnesiumGoalsMissed Int Int
  deriving (Eq, Show)

-- Remove any duplicate DietOps declaration - keep only one
data DietOps r where
  CreateFood :: String -> Map.Map String Int -> DietOps (Either DietError Food)
  ValidateNutrients :: Map.Map String Int -> DietOps (Either DietError ())
  LookupFood :: String -> Map.Map String Food -> DietOps (Maybe Food)

data Console r where
  PrintLine :: String -> Console ()


type HasNutritionState r = Member (State NutritionState) r
type HasDietConstraints r = Member (Reader DietConstraints) r
type HasDietError r = Member (Error DietError) r
type HasViolationLog r = Member (Writer [Violation]) r
type HasDietOps r = Member DietOps r

type DietEffects r = (HasNutritionState r, HasDietConstraints r, HasDietError r, HasViolationLog r, HasDietOps r)

-- Interpreter for DietOps
runDietOps :: Eff (DietOps ': r) a -> Eff r a
runDietOps = interpret $ \case
  CreateFood name nutrients -> do
    let requiredNutrients = ["protein", "fat", "carbs", "sodium", "potassium", "magnesium"]
    let missing = filter (`Map.notMember` nutrients) requiredNutrients
    
    if not (null missing)
      then return $ Left $ MissingNutrients missing
      else do
        -- Check for negative values
        let negatives = Map.filter (< 0) nutrients
        if not (Map.null negatives)
          then return $ Left $ InvalidNutrientValue (fst $ Map.findMin negatives) (snd $ Map.findMin negatives)
          else return $ Right $ Food
            { foodName = name
            , sodium = Map.findWithDefault 0 "sodium" nutrients
            , potassium = Map.findWithDefault 0 "potassium" nutrients
            , magnesium = Map.findWithDefault 0 "magnesium" nutrients
            , fat = Map.findWithDefault 0 "fat" nutrients
            , weight = Map.findWithDefault 0 "weight" nutrients
            , carbs = Map.findWithDefault 0 "carbs" nutrients 
            }
  
  ValidateNutrients nutrients -> do
    let negatives = Map.filter (< 0) nutrients
    if not (Map.null negatives)
      then return $ Left $ InvalidNutrientValue (fst $ Map.findMin negatives) (snd $ Map.findMin negatives)
      else return $ Right ()
  
  LookupFood name foodMap ->
    return $ Map.lookup name foodMap

-- IO interpreter (for production)
runConsoleIO :: Member IO r => Eff (Console ': r) a -> Eff r a
runConsoleIO = interpret $ \case
  PrintLine s -> send $ putStrLn s

-- Pure interpreter (for testing)
runConsolePure :: Eff (Console ': r) a -> Eff r (a, [String])
runConsolePure = runWriter . reinterpret (\case
  PrintLine s -> tell [s])


-- Existing effect handlers (unchanged)
getNutritionState :: Member (State NutritionState) r => Eff r NutritionState
getNutritionState = get

putNutritionState :: Member (State NutritionState) r => NutritionState -> Eff r ()
putNutritionState = put

modifyNutritionState :: Member (State NutritionState) r => (NutritionState -> NutritionState) -> Eff r ()
modifyNutritionState = modify

getMaxCarbs :: Member (Reader DietConstraints) r => Eff r Int
getMaxCarbs = asks maxCarbs

getMinFat :: Member (Reader DietConstraints) r => Eff r Int
getMinFat = asks minFat

getGoalSodium :: Member (Reader DietConstraints) r => Eff r Int
getGoalSodium = asks goalSodium

getGoalPotassium :: Member (Reader DietConstraints) r => Eff r Int
getGoalPotassium = asks goalPotassium

getGoalMagnesium :: Member (Reader DietConstraints) r => Eff r Int
getGoalMagnesium = asks goalMagnesium

getDietConstraints :: Member (Reader DietConstraints) r => Eff r DietConstraints
getDietConstraints = ask

throwDietError :: Member (Error DietError) r => DietError -> Eff r a
throwDietError = throwError

catchDietError :: Member (Error DietError) r => Eff r a -> (DietError -> Eff r a) -> Eff r a
catchDietError = catchError

logViolation :: Member (Writer [Violation]) r => Violation -> Eff r ()
logViolation violation = tell [violation]

logViolations :: Member (Writer [Violation]) r => [Violation] -> Eff r ()
logViolations = tell

-- Updated checkConstraints to work with Food objects (unchanged)
checkConstraints :: (Member (State NutritionState) r, Member (Reader DietConstraints) r, Member (Writer [Violation]) r) => Eff r ()
checkConstraints = do
  state <- getNutritionState
  constraints <- getDietConstraints 
  when (currentCarbs state > maxCarbs constraints) $ 
    logViolation $ CarbsViolation (currentCarbs state) (maxCarbs constraints)
  when (currentFat state < minFat constraints) $ 
    logViolation $ FatMinViolation (currentFat state) (minFat constraints)
  when (currentFat state > maxFat constraints) $ 
    logViolation $ FatMaxViolation (currentFat state) (maxFat constraints)
  when (currentSodium state < goalSodium constraints) $ 
    logViolation $ SodiumGoalsMissed (currentSodium state) (goalSodium constraints)
  when (currentPotassium state < goalPotassium constraints) $ 
    logViolation $ PotassiumGoalsMissed (currentPotassium state) (goalPotassium constraints)
  when (currentMagnesium state < goalMagnesium constraints) $ 
    logViolation $ MagnesiumGoalsMissed (currentMagnesium state) (goalMagnesium constraints)

checkCarbLimit :: (Member (State NutritionState) r, Member (Reader DietConstraints) r) => Eff r Bool
checkCarbLimit = do
  state <- getNutritionState
  constraints <- ask
  return (currentCarbs state <= maxCarbs constraints)

-- Add these helper functions to work with parsed data
processIngredients :: (Member DietOps r, Member (Error DietError) r) => [(String, Map.Map String Int)] -> Eff r (Map.Map String Food)
processIngredients ingredients  = do
  foods <- mapM processIngredient ingredients 
  return $ Map.fromList [(foodName f, f) | f <- foods]
  where
    processIngredient (name, nutrients) = do
      result <- send $ CreateFood name nutrients
      case result of
        Left err -> throwError err
        Right food -> return food


