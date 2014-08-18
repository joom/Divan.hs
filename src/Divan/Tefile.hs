module Divan.Tefile
where

import           Data.Maybe
import qualified Data.Text as T
import           Data.Tuple  (swap)
import           Divan.Vezin

-- | Such as "mefâilün"
type Tefile = T.Text

-- | Association list for vezin symbol strings and tefile names.
tefileMap :: [(Symbols, Tefile)]
tefileMap = [
  ("."    , "fa"),
  ("-"    , "fâ"),
  (".-"   , "feûl"),
  ("--"   , "fa'lün"),
  ("..-"  , "feilün"),
  (".--"  , "feûlün"),
  ("--."  , "mef'ûlü"),
  ("-.-"  , "fâilün"),
  (".-.-" , "mefâilün"),
  (".---" , "mefâîlün"),
  (".--." , "mefâîlü"),
  ("-..-" , "müfteilün"),
  ("-.--" , "fâilâtün"),
  ("--.-" , "müstef'ilün"),
  ("---." , "mef'ûlâtü"),
  ("..--" , "feilâtün"),
  (".-..-", "müfâ'aletün"),
  ("--.--", "müstef'ilâtün"),
  ("..-.-", "mütefâ'ilün")]

-- | Tefile name for the given vezin.
tefileLookup :: Vezin -> Maybe Tefile
tefileLookup = tefileSymbolsLookup . showVezin

-- | Tefile name for the given Symbols.
tefileSymbolsLookup :: Symbols -> Maybe Tefile
tefileSymbolsLookup s = lookup s tefileMap

-- | Symbols for the given tefile.
inverseLookup :: Tefile -> Maybe Symbols
inverseLookup t = lookup t (map swap tefileMap)

-- | The shortest tefile list for the vezin.
detectTefile :: Vezin -> Maybe [Tefile]
detectTefile = detectSymbolsTefile . showVezin

-- | Find the shortest tefile list
-- for the Symbols by breadth-first search.
-- Note that we have a preference to find
-- the longest possible tefile name at a time
-- because we don't want to end up with a tefile list
-- with one syllable names.
detectSymbolsTefile :: Symbols -> Maybe [Tefile]
detectSymbolsTefile sy = runThrough sy 1
  where runThrough s i = case looked of
                           Just x  -> if i > T.length s  then Just [x] else
                                      if isJust fallback then fallback else shortest
                                        where shortest = fmap (x:) (runThrough (T.drop i s) 1)
                           Nothing -> if chars == s
                                      then Nothing
                                      else fallback
          where chars    = T.take i s
                looked   = tefileSymbolsLookup chars
                fallback = runThrough s (i + 1)

-- | Returns a string containing all tefiles separated by /
tefileName :: [Tefile] -> T.Text
tefileName = T.intercalate " / "

-- | A function to determine if two tefile lists
-- correspond to the same vezin.
-- This function is necesssary because this program
-- tries to find the longest tefile names that fit the vezin.
-- Sometimes you might be looking to see if a tefile list
-- fits a verse, this is the function to use then.
equivalent :: [Tefile] -> [Tefile] -> Bool
equivalent x y = xs == ys
  where [xs, ys] = map (fmap T.concat . mapM inverseLookup) [x, y]
