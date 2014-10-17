module Divan.Vezin
where

import           Data.Char      (isSpace)
import qualified Data.Text as T
import           Guguk.Syllabification
import           Guguk.TurkishAlphabet

type Sentence = T.Text
data SyllableType = Open | Closed deriving (Show, Eq)

-- | The syllable type x symbolizes
-- (. for Open, - for Closed)
readSyllableType :: Char -> Maybe SyllableType
readSyllableType '.' = Just Open
readSyllableType '-' = Just Closed
readSyllableType _   = Nothing

-- | The symbol for the syllable type x
-- (. for Open, - for Closed)
showSyllableType :: SyllableType -> Char
showSyllableType Open   = '.'
showSyllableType Closed = '-'

type Vezin   = [SyllableType]
-- | Such as "--..--..-.-"
type Symbols = T.Text

-- | The vezin for a symbol string like "..-"
readVezin :: Symbols -> Maybe Vezin
readVezin = mapM readSyllableType . T.unpack . T.filter (not . isSpace)

-- | Symbols for the vezin
showVezin :: Vezin -> Symbols
showVezin = T.pack . map showSyllableType

-- | Vezin for the given syllable.
--  We cannot directly map each syllable to SyllableType,
--  because some long syllables have 1.5 value.
detectSyllableVezin :: Syllable -> Vezin
detectSyllableVezin xs
  | T.length xs == 3         && T.any isLongVowel xs   = [Closed, Open] -- "dâh"
  -- "kaç", "gül"
  | T.length xs == 3         && isVowel (T.index xs 1) = [Closed]
  -- "fâ", "û"
  | T.length xs `elem` [1,2] && T.any isLongVowel xs = [Closed]
  -- "bu", "sa"
  | T.length xs `elem` [1,2] && isVowel (T.last xs)  = [Open]
  -- "alt", "üst"
  | otherwise = [Closed]

-- | Syllable list to vezin
detectSyllablesVezin :: [Syllable] -> Vezin
detectSyllablesVezin = concatMap detectSyllableVezin

-- | Text to vezin
detectTextVezin :: T.Text -> Vezin
detectTextVezin = detectSyllablesVezin . syllabify

-- | Sentence to vezin (can be used for verses)
-- Always ends with a closed syllable as an aruz rule.
detectSentenceVezin :: Sentence -> Vezin
detectSentenceVezin = (++ [Closed]) . init . detectTextVezin
