import Guguk.Syllabification
import Divan.Tefile
import Divan.Vezin

import Control.Applicative
import Data.Maybe          (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Environment  (getArgs)

main :: IO ()
main = do
  args <- getArgs
  input <- TIO.readFile (head args)
  TIO.putStr (outputString input)

outputString :: T.Text -> T.Text
outputString input = T.unlines $ map f (T.lines input)
  where f x = if T.null x
              then x
              else T.unlines [x, syllableLine x, symbolsLine x, tefileLine x]
        syllableLine = T.intercalate " - " . syllabify
        symbolsLine = showVezin . detectSentenceVezin
        tefileLine x = unicodeShow $ tefileName <$>
                       detectSymbolsTefile (symbolsLine x)
        unicodeShow = fromMaybe "Nothing found."
