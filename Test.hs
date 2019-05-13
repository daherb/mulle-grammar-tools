module Test (testLat,testSwe,testEng,xmlToRules) where
import PGF
import Tool.Tree
import System.Process( system )
import qualified Data.Map.Lazy as M
import qualified Text.XML.Expat.SAX as X
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Char
import Debug.Trace

cplex = "/home/herb/opt/cplex/cplex/bin/x86-64_linux/cplex"

sentsLat = 
  [
    "imperium Romanum magnum est",
    "imperium imperatorem habet",
    "imperator imperium tenet",
    "Caesar Augustus imperator Romanus est",
    "imperium Romanum tenet",
    "multas civitates externas vincit",
    "saepe civitates victae provinciae deveniunt",
    "Gallia provincia Romana est",
    "Africa provincia Romana est",
    "Gallia et Africa provinciae Romanae sunt",
    "Germanus hostis est",
    "imperator Romanus dicit",
    "Germani hostes sunt"
  ]

testLat =
  runTest "data/test-corpus/la/Prima.pgf" sentsLat "Prima" "PrimaCut" "Lat"
  
sentsSwe =
  [-- "förstår vi då våra grannspråk",
   "\"rolig\" betyder nämligen \"lugnt\" där",
   "övriga nordbor förstår inte finska",
   -- "svårast är det för danskar och svenskar",
   -- "men \"by\" betyder stad på danska och norska",
   "idag är den nordiska gemenskapen på många sätt stark",
   "språkligt har de flesta nordbor också mycket gemensamt",
   "de flesta i Skandinavien talade detta språk för 1500 år sedan",
   "han menar helt enkelt att han inte har möjlighet att gå på festen",
   "historisk sett har de nordiska länderna starka politiska och kulturella band",
   "Sverige och Norge bildade en ny union som varade till 1905 när Norge blev självständigt",
   "om en norrman säger att han har inte någon anledning att gå på din fest , ska du inte bli ledsen"]

testSwe =
  runTest "data/test-corpus/sv/Rivstart.pgf" sentsSwe "Rivstart" "RivstartCut" "Swe"

sentsEng =
  [ "I will not buy this record , it is scratched",
    "I will not buy this tobacconist's , it is scratched",
    "my hovercraft is full of eels"
  ]

testEng =
  runTest "data/test-corpus/en/Hungarian.pgf" sentsEng "Hungarian" "HungarianCut" "Eng"

runTest :: String -> [String] -> String -> String -> String -> IO ()
runTest pgfFile sentences grammarName restGrammarName lang =
  do
    putStrLn ">>> Read PGF"
    pgf <- readPGF pgfFile
    putStrLn ">>> Create problem (Step 1)"
    let tmp = mkMultisetProblem pgf sentences
    putStrLn ">>> Create problem (Step 2)"
    let problem = convertToSetProblem $! tmp
    putStrLn ">>> Create Probelm (Step 3)"
    let lp = printConstraints (LP CPLEX) problem
    putStrLn ">>> Write CSP file"
    grams <- runCPLEX cplex lp grammarName restGrammarName lang

    putStrLn ">>> Write grammar files"
    mapM_ (uncurry writeFile) grams
    putStrLn ">>> Finished"
