{-# LANGUAGE DeriveGeneric #-}

module Lexicon where

import GHC.Generics

import Prelude hiding (words,readFile,LT)
import System.IO.Strict (readFile)
import System.IO (withFile,hPutStrLn,IOMode(..))
import Data.Text (Text(..),pack,unpack,words)
import Data.List (nub,partition)
import Data.Map.Strict (Map(..),fromList,(!))
import Data.Maybe
import Data.Aeson
import PGF

data LexItem = LI { fun :: String, cat :: String , lemma :: String, analysis :: String} deriving (Generic, Eq) ;
data LexToken = LT { token :: String , analyses :: [LexItem] } deriving (Eq,Generic) ;

instance ToJSON LexItem
instance ToJSON LexToken

instance Show LexItem where
  show (LI c l f a) = "\t" ++ f ++ "_" ++ c ++ " : " ++ c ++ " ; -- " ++ a ;
  
instance Show LexToken where
  show (LT token []) = token ++ " undefined";
  show (LT token as) = token ++ " " ++ (unlines $ map show as)

fileText :: FilePath -> IO Text
fileText fp = readFile fp >>= (return . pack)

getCategory :: PGF -> CId -> CId
getCategory pgf fun =  (\(_,c,_) -> c) $ (unType . fromJust . functionType pgf) fun

computeFullCoverage :: PGF -> Language -> Text -> [LexToken]
computeFullCoverage pgf lang text =
  let
    morpho = buildMorpho pgf lang
    ws = map unpack $ words text
    mas = nub $ map
      (\w -> LT w (map (\(l,a) ->
                         let cat = (showCId $ getCategory pgf l) in
                         LI
                           (showCId l) -- Fun
                           cat         -- Cat
                           "lemma"     -- Lemma
                           a           -- analysis
                           )
                           (lookupMorpho morpho w))
                   )
      ws
  in
    mas

computeCoverage :: PGF -> Language -> Text -> [LexToken]
computeCoverage pgf lang text =
  let
    cvs = computeFullCoverage pgf lang text
  in
    map (\(LT t as) -> LT t ((nub . map (\(LI f c l a) -> LI f c l "")) as)) cvs

isCovered :: LexToken -> Bool
isCovered (LT _ []) = False
isCovered _ = True

isCoverUnique :: LexToken -> Bool
isCoverUnique (LT _ [_]) = True
isCoverUnique _ = False

isMissing :: LexToken -> Bool
isMissing = not . isCovered
