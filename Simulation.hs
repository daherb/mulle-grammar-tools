module Simulation where

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
    
-- simulate :: PGF -> Int -> IO ()
-- simulate pgf ct =
--   do
--     putStrLn "*** Generate Trees"
--     ts <- generateTrees pgf ct
--     putStrLn $ unlines $ map (showTree) ts
--     putStrLn "*** Linearize Trees"
--     let ss = linearizeTrees pgf ts
--     putStrLn $ unlines ss
--     putStrLn "*** Compile Grammar"
--     npgf <- readPGF =<< compileGrammar =<< writeGrammar pgf =<< restrictGrammar pgf ss
--     putStrLn "*** Compare Result"
--     let tts = parseSentences npgf ss
--     writeFile "/tmp/ts.txt" (unlines $ map showTree ts)
--     writeFile "/tmp/tts.txt" (unlines $ map unlines $ map (map showTree) tts)
--     compareTrees ts $ tts
--     return ()

simulate :: String -> String -> Int -> Int -> Int -> PGF -> PGF -> IO ()
simulate path objName depth ct limit fpgf pgf
  | ct == limit = return ()
  | otherwise = do
      let log = path ++ "/" ++ (showLanguage $ head $ languages pgf) ++ "-" ++ (showLanguage $ head $ languages fpgf) ++ "-" ++ objName ++ "-d" ++ show depth ++ "-" ++ show ct ++"-"++ show limit ++".csv"
      writeFile log "Iteration,GrammarNr,ObjVal,PGFAllFuns,PGFSynFuns,GrammarAllFuns,GrammarSynFuns,Precision,Recall\n"
      putStrLn "*** Generate Trees"
      ts <- generateTrees pgf depth ct
      putStrLn $ unlines $ map (showTree) ts
      putStrLn "*** Linearize Trees"
      let ss = linearizeTrees pgf ts
      putStrLn $ unlines ss
      putStrLn "*** Generate Grammars"
      rg <- restrictGrammar fpgf ss
      putStrLn $ show rg
      writeResult log (length ts) pgf rg
      simulate' log fpgf pgf ts ss depth limit
        where
          simulate' :: FilePath -> PGF -> PGF -> [Tree] -> [String] -> Int -> Int -> IO ()
          simulate' log fpgf pgf ts ss depth limit 
            | length ts >= limit = return ()
            | otherwise =
              do
                putStrLn "*** Generate Trees"
                t <- generateTrees pgf depth 1
                let ts' = ts ++ t
                putStrLn $ unlines $ map (showTree) ts'
                putStrLn "*** Linearize Trees"
                let ss' = ss ++ linearizeTrees pgf t
                putStrLn $ unlines ss'
                putStrLn "*** Generate Grammars"
                rg <- restrictGrammar fpgf ss'
                putStrLn $ show rg
                writeResult log (length ts') pgf rg
                simulate' log fpgf pgf  ts' ss' depth limit
          writeResult :: FilePath -> Int -> PGF -> M.Map Int (Float,[String]) -> IO ()
          writeResult fp ct pgf rg =
            --          mapM_ (\(_,rs) -> putStrLn $ show $ compareGrammar pgf (map read rs :: [String])) rg
            appendFile fp $ unlines [ show ct ++ "," ++ show nr ++ "," ++ show ov ++ "," ++ (show $ length $ functions pgf) ++ "," ++ (show $ length $ filterLexical pgf $ map showCId $ functions pgf) ++ "," ++ (show $ length rs) ++ "," ++ (show $ length $ filterLexical pgf rrs) ++ "," ++ show prec ++ "," ++ show recall
                                    | (nr,(ov,rs)) <- M.toList rg,let rrs = (map read rs :: [String]), let (prec,recall) = compareGrammar fpgf pgf rrs]

showTree :: Expr -> String
showTree = showExpr []

compareTreebankSingle1 :: FilePath -> FilePath -> String -> IO () -- [(Bool,String,[String])]
compareTreebankSingle1 fp pgfp lang = 
  do
    f <- readFile fp
    pgf <- readPGF pgfp
    let tb = read f :: [(String,String,String)]
    let ss = [s | (s,_,_) <- tb ]
    let l = fromJust $ readLanguage lang
--    putStrLn $ show $ map (map showTree) $ parseSentences pgf l ss
    writeGrammar pgf "/home/herb/src/own/mulle-grammar-tool/data/mini-corpus/de/" =<< restrictGrammar pgf ss
--    npgf <- readPGF =<< compileGrammar =<< writeGrammar pgf =<< restrictGrammar pgf ss
    return ()

compareTreebank2 :: FilePath -> FilePath -> String -> IO [(Bool,String,[String])]
compareTreebank2 fp npgf lang =
  do
    f <- readFile fp
    pgf <- readPGF npgf
    let tb = read f :: [(String,String,String)]
    let ss = [s | (s,_,_) <- tb ]
    let ts = [t| (_,_,t) <- tb ]
    let l = fromJust $ readLanguage lang
    return [(t' `elem` ts',t',ts') | (t',ts')<- zip ts $ map (map showTree) $ parseSentences pgf l ss ]
  

compareTreebankMulti1 :: FilePath -> (FilePath,String) -> (FilePath,String) -> IO ()
compareTreebankMulti1 fp (ppgf1,lang1) (ppgf2,lang2) = 
  do
    f <- readFile fp
    pgf1 <- readPGF ppgf1
    pgf2 <- readPGF ppgf2
    let l1 = fromJust $ readLanguage lang1
    let l2 = fromJust $ readLanguage lang2
    let tb = read f :: [(String,String,String)]
    let ss = [(s1,s2) | (s1,s2,_) <- tb ]
    let ts = [t| (_,_,t) <- tb ]
    fn <- writeGrammar pgf1 "/home/herb/src/own/mulle-grammar-tool/data/mini-corpus/de/" =<< restrictMultilingualGrammar (pgf1,l1) (pgf2,l2) ss
    putStrLn $ unlines fn
