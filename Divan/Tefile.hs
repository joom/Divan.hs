module Divan.Tefile
where

import Divan.Vezin
import Data.List (intercalate)
import Data.Tuple (swap)
import Data.Maybe

type Tefile  = String          -- such as "mefâilün"

-- tefileMap = Association list for vezin symbol strings
--                              and tefile names
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

-- tefileLookup v = tefile name for the given vezin v
tefileLookup :: Vezin -> Maybe Tefile
tefileLookup = tefileSymbolsLookup . showVezin

-- tefileSymbolsLookup s = tefile name for the given symbol string s
tefileSymbolsLookup :: Symbols -> Maybe Tefile
tefileSymbolsLookup s = lookup s tefileMap

-- inverseLookup t = symbols for the given tefile t
inverseLookup :: Tefile -> Maybe Symbols
inverseLookup t = lookup t (map swap tefileMap)

-- detectTefile v = the shortest tefile list for the vezin v
detectTefile :: Vezin -> Maybe [Tefile]
detectTefile = detectSymbolsTefile . showVezin

-- detectSymbolsTefile sy = find the shortest tefile list for the symbol string sy i
--                          by breadth-first search
--                          note that we have a preference to find
--                            the longest possible tefile name at a time
--                            because we don't want to end up with a tefile list
--                            with one syllable names
detectSymbolsTefile :: Symbols -> Maybe [Tefile]
detectSymbolsTefile sy = runThrough sy 1
  where runThrough s i = case looked of
                           Just x  -> if i > length s then Just [x] else
                                      if isJust fallback then fallback else shortest
                                        where shortest = fmap (x:) (runThrough (drop i s) 1)
                           Nothing -> if chars == s
                                      then Nothing
                                      else fallback
          where chars    = take i s
                looked   = tefileSymbolsLookup chars
                fallback = runThrough s (i + 1)

-- tefileName ts = a string containing all tefiles separated by /
tefileName :: [Tefile] -> String
tefileName = intercalate " / "

-- equivalent x y = a function to determine if two tefile lists
--                  correspond to the same vezin
--                  this function is necesssary because this program
--                  tries to find the longest tefile names that fit the vezin
--                  sometimes you might be looking to see if a tefile list
--                  fits a verse, this is the function to use then.
equivalent :: [Tefile] -> [Tefile] -> Bool
equivalent x y = xs == ys
  where [xs, ys] = map (fmap concat . mapM id . map inverseLookup) [x,y]
