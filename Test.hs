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
  -- do
  --   putStrLn ">>> Read PGF"
  --   pgf <- readPGF "data/test-corpus/la/Prima.pgf"
  --   putStrLn ">>> Create problem (Step 1)"
  --   let tmp = mkMultisetProblem pgf sentsLat
  --   putStrLn ">>> Create problem (Step 2)"
  --   let problem = convertToSetProblem tmp
  --   putStrLn ">>> Create Probelm (Step 3)"
  --   let cplex = printConstraints (LP CPLEX) problem
  --   putStrLn ">>> Write file"
  --   writeFile "/tmp/cplex.lp" cplex 
  --   runCPLEX "/tmp/cplex.lp" "Prima" "PrimaCut" "Lat"
  
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
  -- do
  --   pgf <- readPGF "data/test-corpus/sv/Rivstart.pgf"
  --   writeFile "/tmp/cplex.lp" $ printConstraints (LP CPLEX) $ convertToSetProblem $ mkMultisetProblem pgf sentsSwe
  --   runCPLEX "/tmp/cplex.lp" "Rivstart" "RivstartCut" "Swe"

sentsEng =
  [ "I will not buy this record , it is scratched",
    "I will not buy this tobacconist's , it is scratched",
    "my hovercraft is full of eels"
  ]

testEng =
  runTest "data/test-corpus/en/Hungarian.pgf" sentsEng "Hungarian" "HungarianCut" "Eng"
  -- do
  --   pgf <- readPGF "data/test-corpus/en/Hungarian.pgf"
  --   system "rm -f /tmp/cplex.lp"
  --   writeFile "/tmp/cplex.lp" $ printConstraints (LP CPLEX) $ convertToSetProblem $ mkMultisetProblem pgf sentsEng
  --   runCPLEX "/tmp/cplex.lp" "Hungarian" "HungarianCut" "Eng"

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
    let cplex = printConstraints (LP CPLEX) problem
    putStrLn ">>> Write file"
    writeFile "/tmp/cplex.lp" cplex 
    runCPLEX "/tmp/cplex.lp" grammarName restGrammarName lang
    
runCPLEX :: String -> String -> String -> String -> IO ()
runCPLEX fn orig abs lang = 
  do
    putStrLn "+++ Writing problem file..."
    system "rm -f /tmp/cplex.in"
    writeFile "/tmp/cplex.in" $ unlines $
      [ "r " ++ fn
      , "opt"
      , "display solution variables *"
      , "xecute rm -f /tmp/cplex.sol"
      , "write /tmp/cplex.sol all"
      , "quit"
      ]
    putStrLn "+++ Starting CPLEX..."
    system $ cplex ++ " < /tmp/cplex.in" -- " > /tmp/cplex.out"
    putStrLn "+++ Cleaning up solution..."
    system "rm -f /tmp/cplex.var"
    putStrLn "+++ Reading solution..."
    s <- BS.readFile "/tmp/cplex.sol"
    let rs = xmlToRules s
    let a = "abstract " ++ abs ++ " = " ++ orig ++" ; "
    writeFile (abs ++ ".gf") a
    putStrLn a
    mapM_ (\(ct,rs) -> do
             let fn = abs ++ lang ++ show ct
             let c = unlines $ 
                   [ "concrete " ++ fn ++ " of " ++ abs ++ " = Cat" ++ lang ++ ",Grammar" ++ lang ++ "[ListS,ListAP,ListNP] ** open (X=" ++ orig ++ lang ++ ") in {"
                   , "  lin" ] ++
                   [ "    " ++ read r ++ " = X." ++ read r ++ " ; " | r <- rs, isRule r] ++
                   [ " } ; " ]
             writeFile (fn ++ ".gf") c
             putStrLn c
         ) $ M.toList rs

isRule :: String -> Bool
isRule = not . isId
  where
    isId [] = True
    isId ('s':is) = isId is
    isId ('t':is) = isId is
    isId (c  :is) | isDigit c = isId is
    isId _  = False

xmlToRules :: BS.ByteString -> M.Map Int [String]
xmlToRules s =
  saxToRules $ X.parse X.defaultParseOptions s
  where
    saxToRules :: [X.SAXEvent String String] -> M.Map Int [String]
    saxToRules = findSolution
    findSolution :: [X.SAXEvent String String] -> M.Map Int [String]
    findSolution [] = M.empty
    findSolution (X.StartElement "CPLEXSolution" _:es) =
      findHeader es
    findSolution (_:es) =
      findSolution es
    findHeader :: [X.SAXEvent String String] -> M.Map Int [String]
    findHeader (X.StartElement "header" as:es)
      | not $ elem ("solutionName","incumbent") as =
        let
          Just index = read <$> lookup "solutionIndex" as
        in
          findVariable index es
      | otherwise = findSolution es
    findHeader (_:es) =
      findHeader  es
    findVariable :: Int -> [X.SAXEvent String String] -> M.Map Int [String]
    findVariable ct (X.StartElement "variable" as:es)
      | elem ("value","1") as = 
        let 
          rs = findVariable ct es
          Just v = lookup "name" as
        in M.alter (Just . maybe [v] (v:)) ct rs
      | otherwise = findVariable ct es
    findVariable ct (X.EndElement "CPLEXSolution":es) =
      findSolution es
    findVariable ct (e:es) =
      findVariable ct es
