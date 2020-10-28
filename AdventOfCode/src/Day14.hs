{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ViewPatterns #-}

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (isAlpha)
import Data.HashMap.Strict (HashMap, singleton)
import qualified Data.HashMap.Strict as H
import Debug.Trace

data Ingredient = Ingredient {
                                count :: Int,
                                element :: Text
                             }
                deriving Show

data Recipe = Recipe [Ingredient] Ingredient
                deriving Show

-- 2 NZGK, 9 XQJK, 18 WRKJ => 9 KTWV

-- 2 NZGK
-- [] => 

-- 5 sdf, 2 sdf

-- parseOnly (parseRecipe >> endOfInput) "5 sdf, 2 sdf"
-- 
-- parseOnly (parseRecipe <* endOfInput) "5 sdf, 2 sdf"

-- [Either String a] -> Either String [a]
-- f (m a) -> m (f a)
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- mapM :: (a -> Maybe b) -> [a] -> Maybe [b]

readRecipes :: FilePath -> IO [Recipe]
readRecipes file = do
    recipesIo <- T.readFile file
    case parseRecipes recipesIo of
        Left err -> error err
        Right recipes -> return recipes

parseRecipes :: Text -> Either String [Recipe] 
parseRecipes input = mapM (parseOnly $ parseRecipe <* endOfInput) $ T.lines input

parseIngredient :: Parser Ingredient
parseIngredient = do 
    skipSpace
    n <- decimal <?> "count"
    skipSpace
    chemical <- takeWhile1 isAlpha <?> "element"
    return $ Ingredient n chemical

parseRecipe :: Parser Recipe
parseRecipe = do
    ingredients <- parseIngredient `sepBy` (char ',')
    skipSpace
    _ <- string "=>"
    reaction <- parseIngredient
    return $ Recipe ingredients reaction

recipeFor :: Text -> [Recipe] -> Recipe
recipeFor result recipes = case foundRecipes of
                            [recipe] -> recipe
                            _ ->  error "404 Recipe not found"
    where
        foundRecipes =  filter (\(Recipe _ (Ingredient { element })) -> element == result) recipes

-- foldr (co-recursive) style:
-- go [] = []
-- go (x:xs) = interesting x ++ go xs

-- foldl style:
-- go acc [] = acc
-- go acc (x:xs) = go (interesting x acc) xs

-- quicksort [] = []
-- quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
--     where
--         lesser = filter (< p) xs
--         greater = filter (>= p) xs

walk :: [Recipe] -> Int
walk recipes = go recipes $ singleton "FUEL" 1
    where 
        go rs (traceShowId -> toCook)
            | H.null toCook = error "404 ORE not found"
            | [("ORE", n)] <- H.toList toCook = n
            | otherwise = cookNext
                
            where
                (cooked@(Ingredient demandN result), newBag) = removeFromBag rs toCook
                remainingRecipes = filter (\(Recipe _ (Ingredient _ e)) -> e /= result) rs
                cookNext = go remainingRecipes $ addToBag (map (scale timesToCook) ingredients) newBag
                Recipe ingredients (Ingredient producesN _) = recipeFor result rs
                timesToCook = demandN `div` producesN + (if demandN `mod` producesN > 0 then 1 else 0)
                scale m (Ingredient n e) = Ingredient (n * m) e

-- Ingredient {count = 1, element = "E"},
-- Ingredient {count = 10, element = "ORE"},
-- Ingredient {count = 7, element = "A"},
-- Ingredient {count = 1, element = "D"},
-- Ingredient {count = 10, element = "ORE"},
-- Ingredient {count = 7, element = "A"},
-- Ingredient {count = 1, element = "C"},
-- Ingredient {count = 10, element = "ORE"},
-- Ingredient {count = 7, element = "A"},
-- Ingredient {count = 1, element = "B"},
-- Ingredient {count = 10, element = "ORE"},
-- Ingredient {count = 1, element = "ORE"}]

-- 1 FUEL
-- Remove 1 FUEL, add 7 A, 1 E
-- 1 FUEL -> 7 A, 1 E
-- Remove 1 E, add 7 A, 1 D
-- 7 A, 1 E -> 14 A, 1 D
-- Remove 1 E, add 7 A, 1 C
-- 14 A, 1 D -> 21 A, 1 C
-- 21 A, 1 C -> 28 A, 1 B


-- 28 A, 1 B -> 29 A
-- 29 A -> 30 ORE
-- "FUEL" -> Recipe [7 A, 1 E] (1, FUEL)
-- "A" -> Recipe [10 ORE] (10, A)

-- mapM parseRecipes eg1 :: Either String [[Recipe]]
-- fmap :: Monad m => (a -> b) -> m a -> m b
-- Substituting "ma" = "Either String" and "m b" = "Either String":
-- fmap :: (a -> b) -> Either String a -> Either String b
-- Subtituting "a" = "[[Recipe]]" and "b" = "[Recipe]":
-- fmap :: ([[Recipe]] -> [Recipe]) -> Either String [[Recipe]] -> Either String [Recipe]
exampleRecipe :: [Recipe]
exampleRecipe  = case fmap concat $ mapM parseRecipes eg2 of
        Left err -> error err
        Right recipes -> recipes
    where
        eg1 :: [Text]
        eg1 =
            [
                "10 ORE => 10 A",
                "1 ORE => 1 B",
                "7 A, 1 B => 1 C",
                "7 A, 1 C => 1 D",
                "7 A, 1 D => 1 E",
                "7 A, 1 E => 1 FUEL"
            ]
        eg2 =
            [   "9 ORE => 2 A",
                "8 ORE => 3 B",
                "7 ORE => 5 C",
                "3 A, 4 B => 1 AB",
                "5 B, 7 C => 1 BC",
                "4 C, 1 A => 1 CA",
                "2 AB, 3 BC, 4 CA => 1 FUEL"
            ]

-- 10 ORE => 10 A
-- 1 ORE => 1 B
-- 7 A, 1 B => 1 C
-- 7 A, 1 C => 1 D
-- 7 A, 1 D => 1 E
-- 7 A, 1 E => 1 FUEL

-- Calculate how much ore is required to produce a given amount of an item.
--
--   Initialise a collection of ingredients needed with one entry, e.g. 1 FUEL.
--   Loop:
--     Choose an ingredient from the bag that does not appear on the LHS of any recipe.
--     Look up its recipe and add (the required multiple) of its ingredients to the collection.
--     Remove the recipe from the recipe book.
--     Stop once the collection shows how much ORE is needed.
-- solve :: HashMap Text Recipe -> Int -> Text -> Int
-- solve recipes0 qty0 item0 = go recipes0 [(item0, qty0)]
--   where
--     go :: HashMap Text Recipe -> HashMap Text Int -> Int
--     go recipes requests
--       | [("ORE", qty)] <- HashMap.toList requests = qty
--       | Just (item, qty) <- chooseNextIngredient,
--         Just (Recipe consumes (_, producesQty)) <- HashMap.lookup item recipes =
--           let recipeQty = qty `ceildiv` producesQty
--               request   = HashMap.map (* recipeQty) consumes
--               requests' = HashMap.delete item . HashMap.unionWith (+) request $ requests
--               recipes'  = HashMap.delete item recipes
--           in  go recipes' requests'
--       | otherwise = error $ "No makeable ingredients in " ++ show requests
--       where
--         chooseNextIngredient = listToMaybe $ HashMap.toList $ HashMap.filterWithKey isReadyItem requests
--         isReadyItem item _ = not . any (`recipeConsumes` item) $ recipes

-- ceildiv :: Integral a => a -> a -> a
-- a `ceildiv` b = (a + b - 1) `div` b
