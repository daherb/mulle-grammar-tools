{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tool.Lexicon where

import GHC.Generics

import Prelude hiding (words,readFile,LT)
import System.IO.Strict (readFile)
import System.IO (withFile,hPutStrLn,IOMode(..),Handle)
import Data.Char (toLower)
import Data.String (words)
import Data.Text (Text(..),pack,unpack,stripSuffix)
import Data.List (nub,partition,uncons,notElem,filter,sortBy)
import Data.Map.Strict (Map(..),fromList,(!))
import Data.Maybe
import Data.Aeson
import PGF
import Debug.Trace

data LexItem = LI { fun :: String, cat :: String , lemma :: String, analysis :: String} deriving (Generic, Eq) ;
data LexToken = LT { token :: String , analyses :: [LexItem] } deriving (Eq,Generic) ;

type Lexicon = [LexItem]

instance ToJSON LexItem
instance ToJSON LexToken

instance Show LexItem where
  show (LI f c l a) = "\t" ++ f ++ "_" ++ c ++ " : " ++ c ++ " ; -- " ++ a ;
  
instance Show LexToken where
  show (LT token []) = token ++ " undefined";
  show (LT token as) = token ++ " " ++ (unlines $ map show as)

-- List of punctuation characters
puncStr :: String
puncStr = "[].,;:()\""

-- Read a text file and convert it to data of type Text
fileText :: FilePath -> IO String
fileText fp = readFile fp

-- Returns the result category for a function according to a grammar
getCategory :: PGF -> CId -> CId
getCategory pgf fun =  (\(_,c,_) -> c) $ (unType . fromJust . functionType pgf) fun

-- Compute for each token in a text the morphological analysis (include duplicate tokens)
computeFullCoverage :: PGF -> Language -> String -> [LexToken]
computeFullCoverage pgf lang text =
  let
    morpho = buildMorpho pgf lang
    ws = words text
    mas = nub $ map (\w -> LT w (analyseLemma pgf morpho w)) ws
  in
    retryMissing pgf morpho mas

-- Returns all analyses for a lemma
analyseLemma :: PGF -> Morpho -> String -> [LexItem]
analyseLemma pgf morpho w =
  map (\(l,a) ->
     let cat = (showCId $ getCategory pgf l)
         fun = (showCId l)
     in
       LI
         fun                       -- Fun
         cat                       -- Cat
         (takeWhile (/= '_') fun)  -- Lemma
         a                         -- analysis
         )
    (lookupMorpho morpho w)
-- Compute for each type in a text the morphological analysis (remove duplicate tokens)
computeCoverage :: PGF -> Language -> String -> [LexToken]
computeCoverage pgf lang text =
  let
    cvs = computeFullCoverage pgf lang text
  in
    map (\(LT t as) -> LT t ((nub . map (\(LI f c l a) -> LI f c l "")) as)) cvs

dPrint :: Show a => a -> a
dPrint a = trace (show a) a

retryMissing :: PGF -> Morpho -> [LexToken] -> [LexToken]
retryMissing pgf morpho tokens =
  map retry tokens
  where
    retry :: LexToken -> LexToken
    retry t@(LT l _)
      | isMissing t =  maybe (LT l [])  fst $ uncons $ filter isCovered $ [tryLower l,tryPunc l, tryLowerPunc l, tryQueSuffix l, tryLowerQueSuffix l]
      | otherwise = t
    tryLower :: String -> LexToken
    tryLower w = let l = (map toLower w) in LT l (analyseLemma pgf morpho l)
    tryPunc :: String -> LexToken
    tryPunc w = let l = (filter (flip notElem puncStr) w) in LT l (analyseLemma pgf morpho l)
    tryLowerPunc :: String -> LexToken
    tryLowerPunc w = let l = filter (flip notElem puncStr) (map toLower w) in LT l (analyseLemma pgf morpho l)
    tryQueSuffix :: String -> LexToken
    tryQueSuffix w = let l = stripSuffix "que" (pack w) in case l of { Nothing -> LT w [] ; Just p -> LT (unpack p) (analyseLemma pgf morpho (unpack p)) }
    tryLowerQueSuffix :: String -> LexToken
    tryLowerQueSuffix w = let l = stripSuffix "que" (pack $ map toLower w) in case l of { Nothing -> LT w [] ; Just p -> LT (unpack p) (analyseLemma pgf morpho (unpack p)) }
      
-- Check if a token has been successfully analysed
isCovered :: LexToken -> Bool
isCovered (LT _ []) = False
isCovered _ = True

-- Check if a token has been uniquely analysed
isCoverUnique :: LexToken -> Bool
isCoverUnique (LT _ [_]) = True
isCoverUnique _ = False

-- Check if a token has not been analysed
isMissing :: LexToken -> Bool
isMissing = not . isCovered

textToLexTokensFile :: FilePath -> FilePath -> FilePath -> IO ()
textToLexTokensFile pgfFile textFile outFile =
  do
    pgf <- readPGF pgfFile
    text <- readFile textFile
    let lang = head (languages pgf) 
    let full = computeFullCoverage pgf lang text
    withFile outFile WriteMode $ (\h -> hPutStrLn h $ unlines $ map printLT $ sortBy (\(LT t1 _) (LT t2 _) -> compare t1 t2) full)
  where printLT :: LexToken -> String
        printLT (LT t as) = t ++ "\t" ++ (unwords $ nub $ map (\(LI f c _ _) -> f) as)
-- writeLexicon :: PGF -> Language -> Text -> String -> IO ()
-- writeLexicon pgf lang text fname =
--   let
--     full = computeFullCoverage pgf lang text
--     (covered,temp) = partition isCoverUnique full
-- --     (ambig,missing) = partition isCovered temp
--   in
--     withFile (fname ++ "Abs.gf") WriteMode
--     (\abs -> withFile (fname ++ ".gf") WriteMode
--              (\conc -> do
--                  sequence_ $ map (printEntry abs conc) covered
-- -- --                  sequence_ $ map (map (diambiguate abs conc) ambig)
-- -- --                  sequence_ $ map (map (userInput abs conc) missing)
--              )
--     )
--   where
--     printEntry :: Handle -> Handle -> LexToken -> IO ()
--     printEntry abs conc lt =
--       let sAbs = (\(LT _ [l]) -> show l) lt
--           sConc =  (\(LT _ [LI fun cat _ _]) -> "\t" ++ "_" ++ cat ++ " = " ++ fun) lt
--       in
--         do 
--           hPutStrLn abs sAbs
--           hPutStrLn conc sConc

{-
text <- fileText "../pg218.txt"
pgf <- readPGF "../DictLatAbs.pgf"
lang = Data.List.head (languages pgf)
morpho = buildMorpho pgf lang

(text,pgf,lang,morpho) <- do { text <- fileText "../pg218.txt" ; pgf <- readPGF "../DictLatAbs.pgf" ; lang <- return $ Data.List.head (languages pgf) ; morpho <- return $ buildMorpho pgf lang ; return (text,pgf,lang,morpho) }
(lang,morpho) = (Data.List.head (languages pgf),buildMorpho pgf lang)
retryMissing pgf morpho [LT "Is," (analyseLemma pgf morpho "Is,")]
writeFile "missingItems.txt" $ unlines $ map show $ filter isMissing (computeCoverage pgf lang text)
writeFile "missingItems-tmp.txt" $ Data.String.unlines $ Data.List.map show $ Data.List.filter isMissing $ retryMissing pgf morpho (computeCoverage pgf lang text)
-}
