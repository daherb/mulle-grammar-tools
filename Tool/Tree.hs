module Tree where

import PGF
import PGF.Internal
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List

data SAT a = SVar a | Conj [SAT a] | Dis [SAT a] | Imp (SAT a) (SAT a)

data ConstraintAtom a = Num Int | CVar a | Neg (ConstraintAtom a) | Sum [ConstraintAtom a]

data Constraint a = Equal (ConstraintAtom a) (ConstraintAtom a) | LessEqual (ConstraintAtom a) (ConstraintAtom a) | IsZero (ConstraintAtom a) | IsOne (ConstraintAtom a)

instance Show a => Show (SAT a) where
  show (SVar a) = show a
  show (Conj ts) = intercalate " /\\ " (map show ts)
  show (Dis ts) = intercalate " \\/ " (map show ts)
  show (Imp p c) = show p ++ " => " ++ show c ++ "\n"

instance Show a => Show (ConstraintAtom a) where
  show (Num n) = show n
  show (CVar v) = show v
  show (Neg c) = "-" ++ show c
  show (Sum cs) = (show $ head cs) ++ foldl (\s c -> s ++ case c of {(Neg c) -> "-" ++ show c ; otherwise -> "+" ++ show c }) "" (tail cs)

instance Show a => Show (Constraint a) where
  show (Equal c1 c2) = show c1 ++ "=" ++ show c2
  show (LessEqual c1 c2) = show c1 ++ "≤" ++ show c2
  show (IsZero c) = show c ++ "=0"
  show (IsOne c) = show c ++ "=1"
  
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
  in
    Dis (map mkSatImp $ zip ["t" ++ show i | i <- [0..]] funLists)


treeToFunList :: Tree -> [CId]
treeToFunList (EApp e1 e2) = treeToFunList e1 ++ treeToFunList e2
treeToFunList (EFun f) = [f]
treeToFunList _ = []

solveCount :: [[(CId,Int)]] -> [String]
solveCount = undefined

solveSat :: PGF -> [String] -> [String]
solveSat pgf sents =
  undefined -- map (sentenceSat pgf) sentences

  
mkSatImp (t,fs) = Imp (SVar t) (Conj $ map (SVar . showCId) fs)
