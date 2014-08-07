import Divan.Syllable
import Divan.Vezin
import Divan.Tefile

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Applicative

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  input <- readFile (head args)
  putStr (outputString input)

outputString :: String -> String
outputString input = unlines $ map f (lines input)
  where f x = if null x
              then x
              else unlines [x, syllableLine x, symbolsLine x, tefileLine x]
        syllableLine = intercalate " - " . syllablize
        symbolsLine = showVezin . detectSentenceVezin
        tefileLine x = unicodeShow $ tefileName <$> detectSymbolsTefile (symbolsLine x)
        unicodeShow = fromMaybe "Nothing found."
