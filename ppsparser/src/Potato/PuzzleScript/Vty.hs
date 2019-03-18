module Potato.PuzzleScript.Vty (
  renderLevel
) where

import qualified Data.Map as Map
import Data.List.Split
import Data.List.Index
import Graphics.Vty
import Potato.PuzzleScript.Types
import Potato.PuzzleScript.Engine

import Lens.Micro.Platform

renderLevel :: ObjectMap -> Point -> LevelState -> Picture
renderLevel om size@(sx,sy,sz) level = picForLayers final where
  foldxfn :: (Int, Int, Int) -> String -> String
  foldxfn pt rowstr = entry:rowstr where
     entry = case Map.lookup pt level of
        Nothing -> ' '
        Just ((_, obj):_) -> '#' -- TODO render colors
        --Just [] -> " " --this should never happen
  foldyfn :: (Int, Int) -> [String] -> [String]
  foldyfn (y,z) flat = foldr foldxfn "" (map (\x' -> (x',y,z))[0..(sx-1)]) : flat
  foldzfn :: Int -> [[String]] -> [[String]]
  foldzfn z pic = foldr foldyfn [] (map (\y' -> (y',z))[0..(sy-1)]) : pic
  result :: [[String]]
  result = foldr foldzfn [] [0..(sz-1)]

  rowtoimage :: String -> Image
  rowtoimage row = foldl foldtoimage emptyImage splitRow where
    splitRow = split (onSublist " ") row
    foldtoimage img str = case str of
      " " -> translateX 1 img
      x -> string defAttr x <|> img

  imagerowstoimage :: [Image] -> Image
  imagerowstoimage imgs = ifoldl foldfn emptyImage imgs where
    foldfn acc i img = translateX i img <-> acc

  imageflatstolayers :: [Image] -> [Image]
  imageflatstolayers imgs = ifoldr foldfn [] imgs where
    foldfn i img acc = acc ++ [translateY i img]

  final :: [Image]
  final = imageflatstolayers $ map (imagerowstoimage . map rowtoimage) result
