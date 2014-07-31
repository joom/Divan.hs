module DivanTest where

import Divan.Syllable
import Divan.Vezin
import Test.HUnit

tests ::  Test
tests = TestList $ map TestCase
  [
  -- Syllable tests
    assertEqual "Syllablize \"birbirlerine\""       ["bir", "bir", "le", "ri", "ne"]         (syllablize "birbirlerine")
  , assertEqual "Syllablize \"kafiyelendirmiştir\"" ["ka","fi","ye","len","dir","miş","tir"] (syllablize "kafiyelendirmiştir")
  , assertEqual "Syllablize \"hayatında\""          ["ha","ya","tın","da"]                   (syllablize "hayatında")
  , assertEqual "Syllablize \"ece\""                ["e", "ce"]                              (syllablize "ece")
  , assertEqual "Syllablize \"mef'ûlü\""            ["mef","û","lü"]                         (syllablize "mef'ûlü")
  , assertEqual "Syllablize \"müfte'ilün\""         ["müf","te","i","lün"]                   (syllablize "müfte'ilün")
  -- Vezin tests
  , assertEqual "Read Vezin \".-.-\""       (Just [Open,Closed,Open,Closed])   (readVezin ".-.-")
  , assertEqual "Read Vezin \"--.-\""       (Just [Closed,Closed,Open,Closed]) (readVezin "--.-")
  , assertEqual "Show Vezin Name \".--\""   (Just "feûlün")                    (vezinName [Open,Closed,Closed])
  , assertEqual "Show Vezin Name \"-.--\""  (Just "fâilâtün")                  (vezinName [Closed,Open,Closed,Closed])
  , assertEqual "Detect Syl. Vezin \"gön\"" [Closed]                           (detectSyllableVezin "gön")
  , assertEqual "Detect Syl. Vezin \"ne\""  [Open]                             (detectSyllableVezin "ne")
  , assertEqual "Detect Syl. Vezin \"a\""   [Open]                             (detectSyllableVezin "a")
  , assertEqual "Detect Syl. Vezin \"her\"" [Closed]                           (detectSyllableVezin "her")
  , assertEqual "Detect Syl. Vezin \"fâ\""  [Closed]                           (detectSyllableVezin "fâ")
  , assertEqual "Detect Syl. Vezin \"dâh\"" [Closed,Open]                      (detectSyllableVezin "dâh")

  -- Poem line vezin tests
  , assertEqual "Detect Sent.Vezin \"Nedir bu gizli gizli âhlar çâk-i girîbanlar\""
                ".-.-.-.---..---"   ((showVezin . detectSentenceVezin) "Nedir bu gizli gizli âhlar çâk-i girîbanlar")
  , assertEqual "Detect Sent.Vezin \"Aceb bir şûha sen de âşık-ı nâlân mısın kâfir\""
                ".---.-.-..--..---" ((showVezin . detectSentenceVezin) "Acebbir şûha sen de âşık-ı nâlân mısın kâfir")
  , assertEqual "Detect Sent.Vezin \"Yaraşır kim seni ser-defter-i hûban yazalar\""
                "..--..--..--..-"   ((showVezin . detectSentenceVezin) "Yaraşır kim seni ser-defter-i hûban yazalar")
  , assertEqual "Detect Sent.Vezin \"Nâme-i hüsnün için bir yeni unvan yazalar\""
                "-..-..--..--..-"   ((showVezin . detectSentenceVezin) "Nâme-i hüsnün için bir yeni unvan yazalar")
  ]

runTests ::  IO ()
runTests = do
  _ <- runTestTT tests
  return ()

-- | For now, main will run our tests.
main :: IO ()
main = runTests
