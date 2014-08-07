module Divan.Syllable
( syllablize, isVowel, isLongVowel,
  Syllable, Sentence ) where

import Data.Maybe (isNothing, fromJust)
import Data.Char (isAlpha, toLower)
import Data.List (findIndex)

type Syllable = String
type Sentence = String

-- charAt xs i = Just x, where x character at index i
--               Nothing, is i is out of bounds
charAt :: String -> Int -> Maybe Char
charAt xs i = if length xs > i then Just (xs !! i) else Nothing

-- isVowel x = True if x is a vowel,
--             False otherwise
isVowel :: Char -> Bool
isVowel x = x `elem` "aeiıoöuüâêîû"

-- isLongVowel x = True if x is a long vowel,
--                 False otherwise
isLongVowel :: Char -> Bool
isLongVowel x = x `elem` "âêîû"

-- substring x y xs = Java's substring
substring :: Int -> Int -> String -> String
substring x y = drop x . take y

-- syllablize xs = list of Turkish syllables of xs
-- syllables dont contain apostrophes, they are all in lower case
syllablize :: String -> [Syllable]
syllablize s
  | null s = []
  | '\'' `elem` tail s = concatMap syllablize [takeWhile (/='\'') s, tail $ dropWhile (/='\'') s]
  | isNothing firstVowelIndex = [xs]

  | any isNothing [afterVowel 1] = [xs]
  | isVowel(fromJust $ afterVowel 1) =
      substring 0 (fVI + 1) xs : syllablize(substring (fVI + 1) len xs)

  | any isNothing [afterVowel 2] = [xs]
  | isVowel(fromJust $ afterVowel 2) =
      substring 0 (fVI + 1) xs : syllablize(substring (fVI + 1) len xs)

  | any isNothing [afterVowel 3] = [xs]
  | isVowel(fromJust $ afterVowel 3) =
      substring 0 (fVI + 2) xs : syllablize(substring (fVI + 2) len xs)

  | lastPart `elem` ["str", "ktr", "ntr", "nsp"] =
      substring 0 (fVI + 2) xs : syllablize(substring (fVI + 2) len xs)
  | otherwise =
      substring 0 (fVI + 3) xs : syllablize(substring (fVI + 3) len xs)
  where xs = (filter isAlpha . map toLower) s
        firstVowelIndex = findIndex isVowel xs
        fVI = fromJust firstVowelIndex
        len = length xs
        lastPart = substring (len + 1) (len + 4) xs
        afterVowel i = fromJust $ fmap (charAt xs . (+i)) firstVowelIndex
