module Divan.Vezin
where

import Divan.Syllable
import Data.Char (isSpace)

data SyllableType = Open | Closed deriving (Show, Eq)

-- readSyllableType x = the syllable type x symbolizes
--                      . for Open, - for Closed
readSyllableType :: Char -> Maybe SyllableType
readSyllableType '.' = Just Open
readSyllableType '-' = Just Closed
readSyllableType _   = Nothing

-- showSyllableType x = the symbol for the syllable type x
--                      . for Open, - for Closed
showSyllableType :: SyllableType -> Char
showSyllableType Open   = '.'
showSyllableType Closed = '-'

type Vezin   = [SyllableType]
type Symbols = String          -- such as "--..--..-.-"

-- readVezin xs = the vezin for a symbol string like "..-"
readVezin :: Symbols -> Maybe Vezin
readVezin = mapM readSyllableType . filter (not . isSpace)

-- showVezin v = symbol string for the vezin
showVezin :: Vezin -> Symbols
showVezin = map showSyllableType

-- detectSyllableVezin xs = vezin for the given syllable
--  We cannot directly map each syllable to SyllableType,
--  because some long syllables have 1.5 value.
detectSyllableVezin :: Syllable -> Vezin
detectSyllableVezin xs
  | length xs == 3         && any isLongVowel xs = [Closed, Open] -- "dâh"
  | length xs == 3         && isVowel (xs !! 1)  = [Closed]       -- "kaç", "gül"
  | length xs `elem` [1,2] && any isLongVowel xs = [Closed]       -- "fâ", "û"
  | length xs `elem` [1,2] && isVowel (last xs)  = [Open]         -- "bu", "sa"
  | otherwise = [Closed]                                          -- "alt", "üst"

-- detectSyllablesVezin xs = syllable list to vezin
detectSyllablesVezin :: [Syllable] -> Vezin
detectSyllablesVezin = concatMap detectSyllableVezin

-- detectStringVezin xs = string to vezin
detectStringVezin :: String -> Vezin
detectStringVezin = detectSyllablesVezin . syllablize

-- detectSentenceVezin xs = sentence to vezin (can be used for verses)
--                          always ends with a closed syllable as an aruz rule
detectSentenceVezin :: Sentence -> Vezin
detectSentenceVezin = (++ [Closed]) . init . detectStringVezin
