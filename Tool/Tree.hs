module Tree where

import PGF
import PGF.Internal
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List

data SAT a = SVar a | Conj [SAT a] | Dis [SAT a] | Imp (SAT a) (SAT a)


data Constraint a = C { rules :: [a], trees :: [[(a,[a])]] } deriving Show; 

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
  in
    Dis (map mkSatImp $ zip ["t" ++ show i | i <- [0..]] funLists)

multiSentSat :: PGF -> [String] -> SAT String
multiSentSat pgf sents =
  let
    funListsS = map (sentenceFunctions pgf) sents
    listLengths = map length funListsS
  in
    Conj $ map (Dis . map mkSatImp) (splitAts listLengths $ zip ["t" ++ show i | i <- [0..]] (concat funListsS) )

multiSentConstraints :: PGF -> [String] -> Constraint String
multiSentConstraints pgf sents =
  let
    rules = map showCId $ functions pgf
    sats = multiSentSat pgf sents
  in
    C rules $ map (map unImp) $ map unDis $ unConj sats
  where
    unConj (Conj l) = l
    unConj _ = []
    unDis (Dis l) = l
    unDis _ = []
    unImp (Imp (SVar s) c) = (s,map unVar $ unConj c)
    unImp _ = ([],[])
    unVar (SVar v) = v
    unVar _ = undefined

printConstraintsAsAMPL :: Constraint String -> String
printConstraintsAsAMPL (C rules trees) =
  let
    treeVars = concatMap (map fst) trees
    ruleVars = nub $ concatMap (concatMap snd) trees
  in
    unlines $
      ["var " ++ v ++ ", binary;" | v <- ruleVars ] ++
      ["var " ++ v ++ ", binary;" | v <- treeVars ] ++
      ["minimize rules : " ++ intercalate " + " ruleVars ++ ";",
       "minimize trees : " ++ intercalate " + " treeVars ++ ";"] ++
      [ "s.t. cs" ++ show i ++ ": " ++ intercalate " + " c ++ " >= 1 ; " | (i,c) <- zip [0..] (map (map fst) trees)] ++
      [ "s.t. c" ++ show i ++ ": " ++ c ++ "; " | (i,c) <- zip [0..] [show (length st) ++ " * " ++ ft ++ " - (" ++ intercalate " + " st ++ ") <= 0" | s <- trees, (ft,st) <- s]] ++
      ["solve;",
       "display " ++ intercalate ", " ruleVars ++ ";",
       "display " ++ intercalate ", " treeVars ++ ";"
      ]

printConstraintsAsLP :: Constraint String -> String
printConstraintsAsLP (C rules trees) =
  let
    treeVars = concatMap (map fst) trees
  in
    clean $ unlines $
      ["min: " ++ intercalate " + " rules ++ " + " ++ intercalate " + " treeVars ++ ";"] ++
      [ "cs" ++ show i ++ ": " ++ intercalate " + " c ++ " >= 1 ; " | (i,c) <- zip [0..] (map (map fst) trees)] ++
      [ "c" ++ show i ++ ": " ++ c ++ "; " | (i,c) <- zip [0..] [show (length st) ++ " * " ++ ft ++ " - " ++ intercalate " - " st ++ " <= 0" | s <- trees, (ft,st) <- s]] ++
      ["bin " ++ v ++ ";" | v <- rules] ++
      ["bin " ++ v ++ ";" | v <- treeVars ]

printConstraintsAsCPLEXLP :: Constraint String -> String
printConstraintsAsCPLEXLP (C rules trees) =
  let
    treeVars = concatMap (map fst) trees
  in
    clean $ unlines $
      [ "Minimize" ] ++ 
      [ " obj: " ++ intercalate " + " rules ++ " + " ++ intercalate " + " treeVars ] ++
      [ "Subject to"] ++ 
      [ " cs: " ++ show i ++ ": " ++ intercalate " + " c ++ " >= 1 ; " | (i,c) <- zip [0..] (map (map fst) trees)] ++
      [ " c: " ++ show i ++ ": " ++ c ++ "; " | (i,c) <- zip [0..] [show (length st) ++ " * " ++ ft ++ " - " ++ intercalate " - " st ++ " <= 0" | s <- trees, (ft,st) <- s]] ++
      [ "Binary"] ++ 
      [ " " ++ v | v <- rules] ++
      [ " " ++ v | v <- treeVars ] ++
      [ "End" ]
      
-- printConstraintsAsMPS :: String -> Constraint String -> String
-- printConstraintsAsMPS name (C rules trees) =
--   let
--     treeVars = concatMap (map fst) trees
--   in
--      unlines $
--      ["NAME" `addField` " " `addField` name] ++
--      ["ROWS","" `addField` "N" `addField` "COST"] ++
--      [ "" `addField` "G " `addField` ("CS" ++ show i) | (i,c) <- zip [0..] (map (map fst) trees)] ++
--      [ "" `addField` "L" `addField` ("C" ++ show i) | (i,c) <- zip [0..] [show (length st) ++ " * " ++ ft ++ " - " ++ intercalate " - " st ++ " <= 0" | s <- trees, (ft,st) <- s]] ++
     
-- --       ["min: " ++ intercalate " + " rules ++ " + " ++ intercalate " + " treeVars ++ ";"] ++
-- --       [ "cs" ++ show i ++ ": " ++ intercalate " + " c ++ " >= 1 ; " | (i,c) <- zip [0..] (map (map fst) trees)] ++
-- --       [ "c" ++ show i ++ ": " ++ c ++ "; " | (i,c) <- zip [0..] [show (length st) ++ " * " ++ ft ++ " - " ++ intercalate " - " st ++ " <= 0" | s <- trees, (ft,st) <- s]] ++
--      ["BOUNDS"] ++
--      ["" `addField` "BV" `addField` v | v <- rules] ++
--      ["" `addField` "BV" `addField` v | v <- treeVars ] ++
--      ["ENDATA"]


-- Helper functions

treeToFunList :: Tree -> [CId]
treeToFunList (EApp e1 e2) = treeToFunList e1 ++ treeToFunList e2
treeToFunList (EFun f) = [f]
treeToFunList _ = []

solveCount :: [[(CId,Int)]] -> [String]
solveCount = undefined

solveSat :: PGF -> [String] -> [String]
solveSat pgf sents =
  undefined -- map (sentenceSat pgf) sentences

splitAts :: [Int] -> [a] -> [[a]]
splitAts [] _ = []
splitAts _ [] = []
splitAts (i:is) es =
  take i es:splitAts is (drop i es)
  
mkSatImp (t,fs) = Imp (SVar t) (Conj $ map (SVar . showCId) fs)

clean :: String -> String
clean = filter (not . flip elem "_")

addField :: String -> String -> String
addField s1 s2
  | length s2 > 8 = error "value too long"
  | length s1 == 0 = " " ++ s2
  | length s1 < 5  = s1 ++ align 5 (length s1) ++ s2
  | length s1 < 15 = s1 ++ align 15 (length s1) ++ s2
  | length s1 < 25 = s1 ++ align 25 (length s1) ++ s2
  | length s1 < 40 = s1 ++ align 40 (length s1) ++ s2
  | length s1 < 50 = s1 ++ align 50 (length s1) ++ s2
  | otherwise      = s1 ++ "  " ++ s2
  where
    align x y = replicate (x - y - 1) ' '
