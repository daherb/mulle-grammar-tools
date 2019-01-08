module Tree where

import PGF
import PGF.Internal
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List

data SAT a = SVar a | Conj [SAT a] | Dis [SAT a] | Imp (SAT a) (SAT a)

instance Show a => Show (SAT a) where
  show (SVar a) = show a
  show (Conj ts) = intercalate " /\\ " (map show ts)
  show (Dis ts) = intercalate " \\/ " (map show ts)
  show (Imp p c) = show p ++ " => " ++ show c ++ "\n"

sentenceTrees :: PGF -> String -> [Tree]
sentenceTrees pgf sent =
  parse pgf (head $ languages pgf) (startCat pgf) sent
  
sentenceFunctions :: PGF -> String -> [[CId]]
sentenceFunctions pgf sent =
  let
    parses = parse pgf (head $ languages pgf) (startCat pgf) sent
  in
    map treeToFunList parses

sentenceFunctionsCount :: PGF -> String -> [M.Map String Int]
sentenceFunctionsCount pgf sent =
  let
    lstToMap = Prelude.foldl (\m k -> M.alter (maybe (Just 1) (\n -> Just (n + 1))) (showCId k) m) M.empty
    funLists = sentenceFunctions pgf sent
  in
    map lstToMap funLists

sentenceSat :: PGF -> String -> SAT String
sentenceSat pgf sent =
  let
    funLists = sentenceFunctions pgf sent
    mkSat (t,fs) = Imp (SVar t) (Conj $ map (SVar . showCId) fs)
  in
    Dis (map mkSat $ zip ["t" ++ show i | i <- [0..]] (map nub funLists))


treeToFunList :: Tree -> [CId]
treeToFunList (EApp e1 e2) = treeToFunList e1 ++ treeToFunList e2
treeToFunList (EFun f) = [f]
treeToFunList _ = []

solveCount :: [[(CId,Int)]] -> [String]
solveCount = undefined

solveSat :: PGF -> [String] -> [String]
solveSat pgf sents =
  undefined -- map (sentenceSat pgf) sentences

