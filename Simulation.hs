module Simulation where

import Inference
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
