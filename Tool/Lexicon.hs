{-# LANGUAGE DeriveGeneric #-}

module Lexicon where

import GHC.Generics

import Prelude hiding (words,readFile,LT)
import System.IO.Strict (readFile)
import Data.Text (Text(..),pack,unpack,words)
import Data.List (nub)
import Data.Maybe
import Data.Aeson
import PGF

data LexItem = LI { cat :: String , fun :: String } deriving (Generic, Eq) ;
data LexToken = LT { token :: String , analysis :: [LexItem] } deriving (Eq,Generic) ;

instance ToJSON LexItem
instance ToJSON LexToken

instance Show LexItem where
  show (LI c l) = l ++ " : " ++ c ++ " ; "
  
instance Show LexToken where
  show (LT token []) = token ++ " undefined";
  show (LT token as) = token ++ " " ++ (Prelude.concatMap show) as

fileText :: FilePath -> IO Text
fileText fp = readFile fp >>= (return . pack)

getCategory :: PGF -> CId -> CId
getCategory pgf fun =  (\(_,c,_) -> c) $ (unType . fromJust . functionType pgf) fun

computeCoverage :: PGF -> Language -> Text -> [LexToken]
computeCoverage pgf lang text =
  let
    morpho = buildMorpho pgf lang
    ws = map unpack $ words text
    mas = map (\w -> LT w (nub $ map (\(l,a) -> LI (showCId $ getCategory pgf l) (showCId l)) (lookupMorpho morpho w))) ws
  in
    mas

isCovered :: LexToken -> Bool
isCovered (LT _ []) = False
isCovered _ = True

isMissing :: LexToken -> Bool
isMissing = not . isCovered
