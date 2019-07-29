module Inference where

import PGF
import Tool.Tree
import Control.Monad
import System.Random
import System.Process( system )
import System.IO.Temp
import System.FilePath
import Data.List
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Time.Clock.System
import Debug.Trace

cplex = "/home/herb/opt/cplex/cplex/bin/x86-64_linux/cplex"
--grammarPath = "/home/herb/src/own/mulle-grammar-tool/data/mini-"
tmpPath = "/tmp/"

generateTrees :: PGF -> Int -> Int -> IO [Tree]
generateTrees pgf depth ct =
  do
    g <- mkStdGen <$> (\s -> fromIntegral (systemSeconds s) + fromIntegral (systemNanoseconds s)) <$> getSystemTime 
    return $ take ct $ generateRandomDepth g pgf (startCat pgf) (Just depth)

generateTrees' :: StdGen -> PGF -> Int -> Int -> IO [Tree]
generateTrees' g pgf depth ct =
  do
    return $ take ct $ generateRandomDepth g pgf (startCat pgf) (Just depth)
linearizeTrees :: PGF -> [Tree] -> [String]
linearizeTrees pgf =
  map (linearize pgf (head $ languages pgf)) 

restrictGrammar :: PGF -> [String] -> IO (M.Map Int (Float,[String]))
restrictGrammar pgf sentences =
  do
    putStrLn ">>> Create problem"
    let problem = convertToSetProblem $ mkMultisetProblem pgf sentences
--    putStrLn $ show problem
    putStrLn ">>> Convert to CPLEX"
    let lp = printConstraints (LP CPLEX) problem
    putStrLn ">>> Write CSP file"
    runCPLEX cplex lp

restrictMultilingualGrammar :: (PGF,Language) -> (PGF,Language) -> [(String,String)] -> IO (M.Map Int (Float,[String]))
restrictMultilingualGrammar (pgf1,lang1) (pgf2,lang2) sentences =
  do
    putStrLn ">>> Create problem"
    let problem = convertToSetProblem $ mkMultilingMultisetProblem (pgf1,lang1) (pgf2,lang2) sentences
--    putStrLn $ show problem
    putStrLn ">>> Convert to CPLEX"
    let lp = printConstraints (LP CPLEX) problem
    putStrLn ">>> Write CSP file"
    runCPLEX cplex lp

writeGrammar :: PGF -> FilePath -> M.Map Int (Float,[String]) -> IO [FilePath]
writeGrammar pgf grammarPath rg = 
  do
    let absname = (showLanguage $ abstractName pgf)
    grams <- createGrammars absname (absname ++ "Cut") (showLanguage $ head $ languages pgf) rg
    let grams' = map (\(f,g) -> (tmpPath ++ f,"--# -path=" ++ grammarPath ++ "\n" ++ g)) grams
    writeFile (tmpPath ++ absname ++ "CutInc.gf") $ "incomplete concrete " ++ absname ++ "CutInc of " ++ absname ++ " = Cat" ++ (showLanguage $ head $ languages pgf) ++ ";"
    mapM_ (uncurry writeFile) grams'
    return $ map fst grams'

compileGrammar :: [FilePath] -> IO FilePath
compileGrammar fp =
  do
    tf <- emptySystemTempFile "mulle.pgf"
    let (tp, tfn) = splitFileName tf
    let tn = dropExtension tfn
    system $ "gf --make -n " ++ tn ++ " -D " ++ tp ++ " " ++ unwords fp
    return tf

parseSentences :: PGF -> Language -> [String] -> [[Tree]]
parseSentences pgf lang =
  map (parse pgf lang (startCat pgf))

compareTrees :: [Tree] -> [[Tree]] -> IO ()
compareTrees old new =
  let pos = length $ takeWhile (uncurry elem) $ zip old new
  in
    if pos == length old then putStrLn "Trees are covered"
    else
      putStrLn $ "Missmatch after " ++ show pos ++ "trees"

compareGrammar :: PGF -> PGF -> [String] -> (Float,Float)
compareGrammar fpgf pgf funs =
  let
    retfuns = filterLexical fpgf funs
    relfuns = filterLexical fpgf $ map showCId $ functions pgf
    precision = (fromIntegral . length) (relfuns `intersect` retfuns) / (fromIntegral . length) retfuns
    recall = (fromIntegral . length) (relfuns `intersect` retfuns) / (fromIntegral . length) relfuns
  in
    trace (unlines [ "<<< computing precission/recall", show $ sort funs, show $ sort $ map showCId $ functions pgf, show $ sort retfuns, show $ sort relfuns, show $ sort (relfuns `intersect` retfuns)]) (precision,recall)

filterLexical :: PGF -> [String] -> [String]
filterLexical pgf rs =
  [ f | (f,t) <- zip rs $ map (\c -> unType <$> (functionType pgf =<< readCId c)) rs, isJust t, let Just (l,_,_) = t, (not . null) l]
