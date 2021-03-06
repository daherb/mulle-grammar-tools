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
    grams <- createGrammars grammarName restGrammarName lang =<< runCPLEX cplex lp 
    putStrLn ">>> Write grammar files"
    mapM_ (uncurry writeFile) grams
    putStrLn ">>> Finished"

-- multiSent = [("I eat bread", "minä syön leipää", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron i_Pron) (ComplV2 eat_V2 (MassNP (UseN bread_N)))))) NoVoc"),
-- ("I don't eat bread", "minä en syö leipää", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron i_Pron) (ComplV2 eat_V2 (MassNP (UseN bread_N)))))) NoVoc"),
-- ("eat bread", "syö leipää", "PhrUtt NoPConj (UttImpSg PPos (ImpVP (ComplSlash (SlashV2a eat_V2) (MassNP (UseN bread_N))))) NoVoc"),
-- ("eat bread", "syökää leipää", "PhrUtt NoPConj (UttImpPl PPos (ImpVP (ComplSlash (SlashV2a eat_V2) (MassNP (UseN bread_N))))) NoVoc"),
-- ("don't eat bread", "älä syö leipää", "PhrUtt NoPConj (UttImpSg PNeg (ImpVP (AdvVP (PassV2 eat_V2) (PrepNP part_Prep (MassNP (UseN bread_N)))))) NoVoc"),
-- ("don't eat bread", "älkää syökö leipää", "PhrUtt NoPConj (UttImpPl PNeg (ImpVP (ComplSlash (SlashV2a eat_V2) (MassNP (UseN bread_N))))) NoVoc"),
-- ("I sing a song", "minä laulan laulun", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron i_Pron) (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N)))))) NoVoc"),
-- ("I don't sing a song", "minä en laula laulua", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron i_Pron) (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N)))))) NoVoc"),
-- ("sing a song", "laula laulu", "PhrUtt NoPConj (UttImpSg PPos (ImpVP (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N))))) NoVoc"),
-- ("sing a song", "laulakaa laulu", "PhrUtt NoPConj (UttImpPl PPos (ImpVP (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N))))) NoVoc"),
-- ("don't sing a song", "älä laula laulua", "PhrUtt NoPConj (UttImpSg PNeg (ImpVP (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N))))) NoVoc"),
-- ("don't sing a song", "älkää laulako laulua", "PhrUtt NoPConj (UttImpPl PNeg (ImpVP (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N))))) NoVoc"),
-- ("I want to eat bread", "minä haluan syödä leipää", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron i_Pron) (ComplVV want_2_VV (ComplSlash (SlashV2a eat_V2) (MassNP (UseN bread_N))))))) NoVoc"),
-- ("I don't want to eat bread", "minä en halua syödä leipää", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron i_Pron) (ComplVV want_2_VV (ComplSlash (SlashV2a eat_V2) (MassNP (UseN bread_N))))))) NoVoc"),
-- ("I want to sing a song in the shower", "minä haluan laulaa laulun suihkussa", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron i_Pron) (ComplVV want_2_VV (AdvVP (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N))) (PrepNP in_Prep (DetCN (DetQuant DefArt NumSg) (UseN shower_N)))))))) NoVoc"),
-- ("I don't want to sing a song in the shower", "minä en halua laulaa laulua suihkussa", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron i_Pron) (ComplVV want_2_VV (AdvVP (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N))) (PrepNP in_Prep (DetCN (DetQuant DefArt NumSg) (UseN shower_N)))))))) NoVoc"),
-- ("I have to eat bread", "minun täytyy syödä leipää", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron i_Pron) (ComplVV must_VV (ComplSlash (SlashV2a eat_V2) (MassNP (UseN bread_N))))))) NoVoc"),
-- ("I don't have to eat bread", "minun ei täydy syödä leipää", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron i_Pron) (ComplVV must_VV (ComplSlash (SlashV2a eat_V2) (MassNP (UseN bread_N))))))) NoVoc"),
-- ("I have to sing a song", "minun täytyy laulaa laulu", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron i_Pron) (ComplVV must_VV (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N))))))) NoVoc"),
-- ("I don't have to sing a song", "minun ei täydy laulaa laulua", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron i_Pron) (ComplVV must_VV (ComplSlash (SlashV2a sing_V2) (DetCN (DetQuant IndefArt NumSg) (UseN song_N))))))) NoVoc"),
-- ("I eat bread in the kitchen", "minä syön leipää keittiössä ", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron i_Pron) (AdvVP (ComplSlash (SlashV2a eat_V2) (MassNP (UseN bread_N))) (PrepNP in_Prep (DetCN (DetQuant DefArt NumSg) (UseN kitchen_N))))))) NoVoc"),
-- ("I don't eat bread in the kitchen ", "mina en syö leipää keittiössä", "PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron i_Pron) (AdvVP (ComplSlash (SlashV2a eat_V2) (MassNP (UseN bread_N))) (PrepNP in_Prep (DetCN (DetQuant DefArt NumSg) (UseN kitchen_N))))))) NoVoc")]
