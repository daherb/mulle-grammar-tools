module Test (testLat,testSwe,testEng) where
import PGF
import Tool.Tree
import System.Process( system )

cplex = "/home/herb/opt/cplex/cplex/bin/x86-64_linux/cplex"
sentsLat = 
  [
    "imperium Romanum magnum est",
    "imperium imperatorem habet",
    "imperator imperium tenet",
    "Caesar Augustus imperator Romanus est",
    "imperium Romanum tenet",
    "multas civitates externas vincit",
--    "saepe civitates victae provinciae deveniunt",
    "Gallia provincia Romana est",
    "Africa provincia Romana est",
    "Gallia et Africa provinciae Romanae sunt",
    "Germanus hostis est",
    "imperator Romanus dicit",
    "Germani hostes sunt"
  ]

testLat =
  do
    pgf <- readPGF "data/test-corpus/la/Prima.pgf"
    writeFile "/tmp/cplex.lp" $ printConstraints (LP CPLEX) $ convertToSetProblem $ mkMultisetProblem pgf sentsLat
    runCPLEX "/tmp/cplex.lp" "Prima" "PrimaCut" "Lat"
  
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
  do
    pgf <- readPGF "data/test-corpus/sv/Rivstart.pgf"
    writeFile "/tmp/cplex.lp" $ printConstraints (LP CPLEX) $ convertToSetProblem $ mkMultisetProblem pgf sentsSwe
    runCPLEX "/tmp/cplex.lp" "Rivstart" "RivstartCut" "Swe"

sentsEng =
  [ "I will not buy this record , it is scratched",
    "I will not buy this tobacconist's , it is scratched",
    "my hovercraft is full of eels"
  ]

testEng =
  do
    pgf <- readPGF "data/test-corpus/en/Hungarian.pgf"
    system "rm -f /tmp/cplex.lp"
    writeFile "/tmp/cplex.lp" $ printConstraints (LP CPLEX) $ convertToSetProblem $ mkMultisetProblem pgf sentsEng
    runCPLEX "/tmp/cplex.lp" "Hungarian" "HungarianCut" "Eng"

runCPLEX :: String -> String -> String -> String -> IO ()
runCPLEX fn orig abs lang = 
  do
    putStrLn "+++ Writing problem file..."
    system "rm -f /tmp/cplex.in"
    writeFile "/tmp/cplex.in" $ unlines $
      [ "r " ++ fn
      , "opt"
      , "display solution variables *"
--      , "xecute rm -f /tmp/cplex.sol"
      , "write cplex.sol"
      , "quit"
      ]
    putStrLn "+++ Starting CPLEX..."
    system $ cplex ++ " < /tmp/cplex.in" -- " > /tmp/cplex.out"
    putStrLn "+++ Cleaning up solution..."
    system "rm -f /tmp/cplex.var"
    system "xml sel -t -v '//variable[@value=\"1\"]/@name' /tmp/cplex.sol | sed s/\\\"//g | egrep -v 's[[:digit:]]+t[[:digit:]]' > /tmp/cplex.var"
    putStrLn "+++ Reading solution..."
    s <- readFile "/tmp/cplex.var"
    let a = "abstract " ++ abs ++ " = " ++ orig ++" ; "
    writeFile (abs ++ ".gf") a
    putStrLn a
    let c = unlines $ 
            [ "concrete " ++ abs ++ lang ++ " of " ++ abs ++ " = Cat" ++ lang ++ " ** open (X=" ++ orig ++ lang ++ ") in {"
            , "  lin" ] ++
            [ "    " ++ r ++ " = X." ++ r ++ " ; " | r <- lines s] ++
            [ " } ; " ]
    writeFile (abs ++ lang ++ ".gf") c
    putStrLn c
