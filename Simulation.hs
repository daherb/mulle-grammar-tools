module Simulation where

import Inference
import Tool.Tree
import PGF
import Data.Maybe
import qualified Data.Map.Lazy as M

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

-- | Function to automatically run the compare-with-grammar experiment
compareWithGrammar
  :: String -- ^ path to store the grammar files
  -> String -- ^ name of the objective function
  -> Int    -- ^ depth of the trees
  -> Int    -- ^ current number of sentences to be generated
  -> Int    -- ^ maximum number of sentences to be generated
  -> PGF    -- ^ the full grammar, superset of all other grammars
  -> PGF    -- ^ the grammar to be reconstructed
  -> IO ()
compareWithGrammar path objName depth ct limit fpgf pgf
  -- If we are at the limit, we stop
  | ct == limit = return ()
  | otherwise = do
      -- assemble path for data file
      let dataFile = path ++ "/" ++ (showLanguage $ head $ languages pgf) ++ "-" ++ (showLanguage $ head $ languages fpgf) ++ "-" ++ objName ++ "-d" ++ show depth ++ "-" ++ show ct ++"-"++ show limit ++".csv"
      -- write header
      writeFile dataFile "Iteration,GrammarNr,ObjVal,PGFAllFuns,PGFSynFuns,GrammarAllFuns,GrammarSynFuns,Precision,Recall\n"
      -- do the learning thing
      putStrLn "*** Generate Trees"
      ts <- generateTrees pgf depth ct
      putStrLn $ unlines $ map (showTree) ts
      putStrLn "*** Linearize Trees"
      let ss = linearizeTrees pgf ts
      putStrLn $ unlines ss
      putStrLn "*** Generate Grammars"
      rg <- restrictGrammar fpgf ss objNumTreesNumRules
      putStrLn $ show rg
      -- write results
      writeResult dataFile  (length ts) pgf rg
      -- loop
      simulate' dataFile fpgf pgf ts ss depth limit
        where
          simulate' :: FilePath -> PGF -> PGF -> [Tree] -> [String] -> Int -> Int -> IO ()
          simulate' dataFile fpgf pgf ts ss depth limit 
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
                rg <- restrictGrammar fpgf ss' objNumTreesNumRules
                putStrLn $ show rg
                writeResult dataFile (length ts') pgf rg
                simulate' dataFile fpgf pgf  ts' ss' depth limit
          writeResult :: FilePath -> Int -> PGF -> M.Map Int (Float,[String]) -> IO ()
          writeResult fp ct pgf rg =
            --          mapM_ (\(_,rs) -> putStrLn $ show $ compareGrammar pgf (map read rs :: [String])) rg
            appendFile fp $ unlines [ show ct ++ "," ++ show nr ++ "," ++ show ov ++ "," ++ (show $ length $ functions pgf) ++ "," ++ (show $ length $ filterLexical pgf $ map showCId $ functions pgf) ++ "," ++ (show $ length rs) ++ "," ++ (show $ length $ filterLexical pgf rrs) ++ "," ++ show prec ++ "," ++ show recall
                                    | (nr,(ov,rs)) <- M.toList rg,let rrs = (map read rs :: [String]), let (prec,recall) = compareGrammar fpgf pgf rrs]
            
-- | Function to restrict a grammar using the sentences from a treebank, monolingual version. First part of the compare-with-treebank experiment
restrictWithTreebankSingle
  :: FilePath -- ^ path to treebank
  -> FilePath -- ^ path to grammar
  -> String   -- ^ name of the language
  -> IO () -- [(Bool,String,[String])]
restrictWithTreebankSingle fp pgfp lang = 
  do
    -- read treebank
    f <- readFile fp
    let tb = read f :: [(String,String,String)]
    -- get sentences from treebank
    let ss = [s | (s,_,_) <- tb ]
    -- read grammar and language
    pgf <- readPGF pgfp
    let l = fromJust $ readLanguage lang
    -- restrict grammar and write resulting grammar to temp directory (imported from Inference)
    writeGrammar pgf tmpPath =<< restrictGrammar pgf ss objNumTreesNumRules
--    npgf <- readPGF =<< compileGrammar =<< writeGrammar pgf =<< restrictGrammar pgf ss
    return ()

-- | Function to restrict a grammar using the sentences from a treebank, bilingual version. First part of the compare-with-treebank experiment
restrictWithTreebankMulti
  :: FilePath          -- ^ path to treebank
  -> (FilePath,String) -- ^ path to first grammar and name of first language
  -> (FilePath,String) -- ^ path to second grammar and name of second language
  -> IO ()
restrictWithTreebankMulti fp (ppgf1,lang1) (ppgf2,lang2) = 
  do
    -- read treebank
    f <- readFile fp
    let tb = read f :: [(String,String,String)]
    -- get sentences and trees from treebank
    let ss = [(s1,s2) | (s1,s2,_) <- tb ]
    let ts = [t| (_,_,t) <- tb ]
    -- read grammars and languages
    pgf1 <- readPGF ppgf1
    pgf2 <- readPGF ppgf2
    let l1 = fromJust $ readLanguage lang1
    let l2 = fromJust $ readLanguage lang2
    -- restrict grammar and write resulting grammar to temp directory (imported from Inference)
    fn <- writeGrammar pgf1 tmpPath =<< restrictMultilingualGrammar (pgf1,l1) (pgf2,l2) ss objNumTreesNumRules
    putStrLn $ unlines fn

-- | Function to compare the results of restricting a grammar with a treebank. Second part of the compare with treebank experiment
compareWithTreebankSingle
  :: FilePath -- ^ path to treebank
  -> FilePath -- ^ path to the new grammar
  -> String   -- ^ name of the language
  -> IO [(Bool,String,[String])] -- ^ the result, i.e. if the sentence is contained, the gold standard tree and the trees resulting from the new grammar
compareWithTreebankSingle fp npgf lang =
  do
    -- read treebank
    f <- readFile fp
    let tb = read f :: [(String,String,String)]
    -- get both sentences and trees from treebank
    let ss = [s | (s,_,_) <- tb ]
    let ts = [t| (_,_,t) <- tb ]
    -- read pgf and language 
    pgf <- readPGF npgf
    let l = fromJust $ readLanguage lang
    -- return result of comparison
    return [(t' `elem` ts',t',ts') | (t',ts')<- zip ts $ map (map showTree) $ parseSentences pgf l ss ]
  
-- Helper functions
-- | Function to convert a tree into a string
showTree :: Expr -> String
showTree = showExpr []
