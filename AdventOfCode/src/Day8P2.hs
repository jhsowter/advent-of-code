{-# LANGUAGE OverloadedStrings #-}

-- import qualified Data.Text as T
import Data.List (minimumBy, foldl1')
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

eg :: Text
eg = "123456789012"
eg1 :: Text
eg1 = "0222112222120000"

-- width = 2
-- height = 2

mergePixel :: Char -> Char -> Char
mergePixel '2' c = c
mergePixel a _ = a

-- zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text

mergeLayer :: Text -> Text -> Text
mergeLayer = Text.zipWith mergePixel 

mergeLayers :: [Text]  -> Text
mergeLayers layers = foldl1' mergeLayer layers

draw :: Text -> Int -> IO ()
draw layer w = mapM_ Text.putStrLn lines
    where
        lines = Text.chunksOf w (Text.map drawPixel layer)
        drawPixel :: Char -> Char
        drawPixel '0' = '█'
        drawPixel '1' = ' '

part2 :: IO ()
part2 = do
    input <- Text.readFile "day8p2input.txt"
    draw (mergeLayers $ layers input width height) width
    where 
        width = 25
        height = 6

--mapM :: (Foldable t, Monad m) => (a -> m b) -> t a -> m [b]
--mapM :: (Text -> IO ()) -> [Text] -> IO [()]

--mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
--mapM_ :: (Text -> IO ()) -> [Text] -> IO ()


-- chunksOf
layers :: Text -> Int -> Int -> [Text]
layers input w h = Text.chunksOf (w*h) input


-- █
-- :set -XOverloadedStrings