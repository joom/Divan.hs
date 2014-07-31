module Divan.Vezin
where

import Divan.Syllable
import Data.Char (isSpace)

data SyllableType = Open | Closed deriving (Show, Eq)

readSyllableType :: Char -> Maybe SyllableType
readSyllableType '.' = Just Open
readSyllableType '-' = Just Closed
readSyllableType _   = Nothing

showSyllableType :: SyllableType -> Char
showSyllableType Open   = '.'
showSyllableType Closed = '-'

type Vezin = [SyllableType]

readVezin :: String -> Maybe Vezin
readVezin = mapM readSyllableType . filter (not . isSpace)

showVezin :: Vezin -> String
showVezin = map showSyllableType

vezinNameMap :: [(String, String)]
vezinNameMap = [
  (".-.-" , "mefâilün"),
  ("--.-" , "müstef’ilün"),
  ("--.--", "müstef’ilâtün"),
  ("-.--" , "fâilâtün"),
  ("..--" , "feilâtün"),
  ("-.-"  , "fâilün"),
  (".--"  , "feûlün")]

vezinName :: Vezin -> Maybe String
vezinName v = lookup (showVezin v) vezinNameMap

detectSyllableVezin :: Syllable -> Vezin
detectSyllableVezin xs
  | length xs `elem` [1,2] && any isLongVowel xs = [Closed]
  | length xs == 3 && any isLongVowel xs         = [Closed, Open]
  | length xs `elem` [1,2] && isVowel (last xs)  = [Open]
  | length xs == 3 && isVowel (xs !! 1)          = [Closed]
  | otherwise = [Closed]

detectSyllablesVezin :: [Syllable] -> Vezin
detectSyllablesVezin = concatMap detectSyllableVezin

detectSentenceVezin :: Sentence -> Vezin
detectSentenceVezin = detectSyllablesVezin . syllablize
