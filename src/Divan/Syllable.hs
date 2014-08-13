module Divan.Syllable
( syllabalize, isVowel, isLongVowel,
  Syllable, Sentence ) where

import           Data.Char  (isAlpha, toLower)
import           Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T

type Syllable = T.Text
type Sentence = T.Text

-- charAt xs i = Just x, where x character at index i
--               Nothing, is i is out of bounds
-- charAt :: T.Text -> Int -> Maybe Char
-- charAt xs i = if length xs > i then Just (xs !! i) else Nothing

-- isVowel x = True if x is a vowel,
--             False otherwise
isVowel :: Char -> Bool
isVowel x = x `elem` "aeiıoöuüâêîû"

-- isLongVowel x = True if x is a long vowel,
--                 False otherwise
isLongVowel :: Char -> Bool
isLongVowel x = x `elem` "âêîû"

-- substring x y xs = Java's substring
substring :: Int -> Int -> T.Text -> T.Text
substring x y = T.drop x . T.take y

elemT :: Char -> T.Text -> Bool
elemT c t = isJust $ T.find (==c) t

lastPartChecks :: [T.Text]
lastPartChecks = ["str", "ktr", "ntr", "nsp"]

-- syllablize xs = list of Turkish syllables of xs
-- syllables dont contain apostrophes, they are all in lower case
syllabalize :: T.Text -> [Syllable]
syllabalize s
  | T.null s = []

  | '\'' `elemT` T.tail s =
      concatMap
      syllabalize
      [T.takeWhile (/='\'') s,
       T.tail $ T.dropWhile (/='\'') s]

  | isNothing firstVowelIndex = [xs]

  | any isNothing [afterVowel 1] = [xs]

  | isVowel(fromJust $ afterVowel 1) =
      substring 0 (fVI + 1) xs : syllabalize(substring (fVI + 1) len xs)

  | any isNothing [afterVowel 2] = [xs]

  | isVowel (fromJust $ afterVowel 2) =
      substring 0 (fVI + 1) xs : syllabalize(substring (fVI + 1) len xs)

  | any isNothing [afterVowel 3] = [xs]

  | isVowel (fromJust $ afterVowel 3) =
      substring 0 (fVI + 2) xs : syllabalize(substring (fVI + 2) len xs)

  | lastPart `elem` lastPartChecks =
      substring 0 (fVI + 2) xs : syllabalize(substring (fVI + 2) len xs)

  | otherwise =
      substring 0 (fVI + 3) xs : syllabalize(substring (fVI + 3) len xs)

  where xs = (T.filter isAlpha . T.map toLower) s
        firstVowelIndex = T.findIndex isVowel xs
        fVI = fromJust firstVowelIndex
        len = T.length xs
        lastPart = substring (len + 1) (len + 4) xs
        afterVowel i = fmap (T.index xs . (+i)) firstVowelIndex
