module Divan.Tefile
where

import Divan.Vezin
import Data.List (intercalate)

type Tefile  = String          -- such as "mefâilün"

-- tefileMap = Association list for vezin symbol strings
--                              and tefile names
tefileMap :: [(Symbols, Tefile)]
tefileMap = [
  ("."    , "fa'"),
  ("-"    , "fâ"),
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
tefileLookup v = lookup (showVezin v) tefileMap

-- TODO !!!
-- detectTefile v = find the shortest tefile list by breadth-first search
detectTefile :: Vezin -> Maybe [Tefile]
detectTefile = undefined

tefileName :: [Tefile] -> String
tefileName = intercalate " / "
