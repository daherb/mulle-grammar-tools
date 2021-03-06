module Tool.Tree
  (
  -- * Data types
  SAT(..),
  LPFormat(..),
  ConstraintFormat(..),
  Rules(..),
  TreeRules(..),
  SentenceTrees(..),
  ConstraintProblem(..),
  LabeledConstraintProblem(..),
  -- * Constraint problems
  mkMultisetProblem,
  mkMultilingMultisetProblem,
  convertToSetProblem,
  convertToLabeledProblem,
  printConstraints,
  -- * Tree functions
  sentenceTrees,
  treeFunctions,
  -- * Grammar functions
  createGrammars,
  isRule,
  -- * MiniSat
  convertToMiniSat,
  solveSat,
  satToMiniSat,
  printModel,
  -- * CPLEX
  runCPLEX,
  xmlToRules,
  -- * Objective functions
  ObjFun(..),
  objAvgNumTrees,
  objNumTreesWeightedNumRules,
  objNumTrees,
  objNumRules,
  objNumTreesNumRules,
  -- * Other functions
  funCounts,
  clean
  )
where

import PGF
import PGF.Internal
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import SAT
import SAT.Bool
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import qualified Text.XML.Expat.SAX as X
import qualified Data.ByteString.Lazy as BS
import System.Process( system )
import System.IO.Temp
import Data.Char

-- | Data type for boolean operations, i.e. encoding SAT
data SAT a = SVar a | Conj [SAT a] | Dis [SAT a] | Imp (SAT a) (SAT a)

-- | Data type for LP formats
data LPFormat = LPSolve | CPLEX ;
-- | Data type for Constraint formats
data ConstraintFormat = AMPL | LP LPFormat ; -- MPS String ;

-- | A list of rule names
type Rules a = [a]
-- | A list of trees, i.e. a list of list of rule names
type TreeRules a = [Rules a]
-- | A list of sentences, i.e .a list of a list of rule names
type SentenceTrees a = [TreeRules a]

-- | Data type for a constraint problem
-- Simple way to model our problem:
-- It has the following components:
-- * A grammar has a set of rules
-- * For a set of sentences we can get the for each sentence a list of trees
-- * Each tree is a list of syntax rules
-- The list of syntax rules can be a set or a multiset, i.e. contain elements repeatedly
data ConstraintProblem a = CP { rules :: Rules a, trees :: SentenceTrees a } deriving Show;

-- | Data type for alternate version of a constraint problem. Same a above but labeled on sentence and tree level
data LabeledConstraintProblem a = LCP { ltrees :: [(String,[(String,[a])])] } deriving Show ;

-- | Make SAT show-able
instance Show a => Show (SAT a) where
  show (SVar a) = show a
  show (Conj ts) = "(" ++ intercalate " /\\ " (map show ts) ++ ")"
  show (Dis ts) =  "(" ++ intercalate " \\/ " (map show ts) ++ ")"
  show (Imp p c) = show p ++ " => " ++ show c ++ "\n"

-- | Function to create a constraint problem based on a PGF grammar and a list of (positive)
--   example sentences
mkMultisetProblem :: PGF -> [String] -> ConstraintProblem String
mkMultisetProblem pgf sents =
  let
    rules = [showCId r | r <- functions pgf]
    treeSets = map (sentenceTrees pgf (head $ languages pgf)) sents
    funSets = (map . map) (map showCId . treeFunctions) treeSets
  in
    CP rules funSets

-- | Function to create a contraint problem based on two PGF grammar in different languages
--   and a list of bilingual example sentences
mkMultilingMultisetProblem :: (PGF,Language) -> (PGF,Language) -> [(String,String)] -> ConstraintProblem String
mkMultilingMultisetProblem (pgf1,l1) (pgf2,l2) sents =
  let
    rules = [showCId r | r <- functions pgf1] `intersect` [showCId r | r <- functions pgf2]
    treeSets = let tts = [(sentenceTrees pgf1 l1 s1,sentenceTrees pgf2 l2 s2) |(s1,s2) <- sents] in [ intersect f s | (f,s) <- tts]
    funSets = (map . map) (map showCId . treeFunctions) treeSets
  in
    CP rules funSets
    
-- | Function to convert the lists of syntax rules from multi-sets to sets
convertToSetProblem :: Eq a => ConstraintProblem a -> ConstraintProblem a
convertToSetProblem (CP rules trees) = CP rules $ (map . map) nub trees

-- | Function to add labels to a constraint problem. The labels have the shape
--   \[s_0\dots s_n\] for sentences and \[t_0\dots t_m\] for trees and can be
--   used as variables in the constraint solving
convertToLabeledProblem :: ConstraintProblem a -> LabeledConstraintProblem a
convertToLabeledProblem (CP _ trees) =
  (LCP [("s" ++ show si,[("s" ++ show si ++ "t" ++ show ti,rs) | (ti,rs) <- zip [0..] ts]) | (si,ts) <- zip [0..] trees])
  -- (LCP (zip ["s" ++ show i | i <- [0..]] (label "t" trees)))
  -- where
  --   label :: String -> [[a]] -> [[(String,a)]]
  --   label l ll =
  --     splitAts (map length ll) (zip [l ++ show i | i <- [0..]] (concat ll))

-- | Function to parse a sentence to a list of trees using a PGF grammar
sentenceTrees :: PGF -> Language -> String -> [Tree]
sentenceTrees pgf lang sent =
  parse pgf lang (startCat pgf) sent

-- | Function to convert a tree to a list of syntactic functions
treeFunctions :: Tree -> [CId]
treeFunctions (EApp e1 e2) = treeFunctions e1 ++ treeFunctions e2
treeFunctions (EFun f) = [f]
treeFunctions _ = []

-- | Function to count syntax rules in a constraint problem
funCounts :: ConstraintProblem String -> M.Map String Int
funCounts (CP rules trees) =
  let
    lstToMap :: [String] -> M.Map String Int
    lstToMap = Prelude.foldl (\m k -> M.alter (maybe (Just 1) (\n -> Just (n + 1))) k m) M.empty
  in
    M.unionsWith (+) $ (concatMap . map) lstToMap trees

-- | Function to convert a constraint problem to a SAT formula
convertToMiniSat :: (Eq a,Show a) => ConstraintProblem a -> (SAT String, SAT String)
convertToMiniSat cp =
  let
    (LCP labeled) = convertToLabeledProblem $ convertToSetProblem cp
  in
    (Conj $ map (SVar. fst) labeled, Conj (
        [(Imp (SVar s) (Dis (map (SVar . fst) ts)))|(s,ts) <- labeled] ++
        [ Imp (SVar t) (Conj (map (SVar . show) rs))| (s,ts) <- labeled, (t,rs) <- ts]))

-- | Function to print a constraint problem as AMPL
printConstraints :: ConstraintFormat -> ConstraintProblem String -> ObjFun -> String
printConstraints AMPL cp _ =
  let
    (LCP lts) = convertToLabeledProblem cp
    vars = [(t,rs) | (s,ts) <- lts, (t,rs) <- ts]
    treeVars = map fst vars 
    ruleVars = concatMap snd $ vars
  in
    unlines $
      ["var " ++ v ++ ", binary;" | v <- ruleVars ] ++
      ["var " ++ v ++ ", binary;" | v <- treeVars ] ++
      ["minimize rules : " ++ intercalate " + " ruleVars ++ ";",
       "minimize trees : " ++ intercalate " + " treeVars ++ ";"] ++
      [ "s.t. cs" ++ show i ++ ": " ++ intercalate " + " c ++ " >= 1 ; " | (i,c) <- zip [0..] [map fst ts | (s,ts) <- lts]] ++
      [ "s.t. c" ++ show i ++ ": " ++ c ++ "; " | (i,c) <- zip [0..] [show (length rs) ++ " * " ++ t ++ " - (" ++ intercalate " + " rs ++ ") <= 0" | (s,ts) <- lts, (t,rs) <- ts]] ++
      ["solve;",
       "display " ++ intercalate ", " (nub ruleVars) ++ ";",
       "display " ++ intercalate ", " (nub treeVars) ++ ";"
      ]

-- | Function to print a constraint problem as LPSolve LP
printConstraints (LP LPSolve) cp _ =
  let
    (LCP lts) = convertToLabeledProblem cp
    vars = [(t,rs) | (s,ts) <- lts, (t,rs) <- ts]
    treeVars = map fst vars
    ruleVars = concatMap snd $ vars
  in
    clean $ unlines $
      ["min: " ++ intercalate " + " ruleVars ++ " + " ++ intercalate " + " treeVars ++ ";"] ++
      [ "cs" ++ show i ++ ": " ++ intercalate " + " c ++ " >= 1 ; " | (i,c) <- zip [0..] [map fst ts | (s,ts) <- lts]] ++
      [ "c" ++ show i ++ ": " ++ c ++ "; " | (i,c) <- zip [0..] [show (length rs) ++ " * " ++ t ++ " - " ++ intercalate " - " rs ++ " <= 0" | (s,ts) <- lts, (t,rs) <- ts]] ++
      ["bin " ++ v ++ ";" | v <- nub ruleVars ] ++
      ["bin " ++ v ++ ";" | v <- nub treeVars ]

-- | Function to print a constraint problem as CPLEX LP
printConstraints (LP CPLEX) cp ofun =
  let
    (LCP lts) = convertToLabeledProblem cp
    vars = [(t,rs) | (s,ts) <- lts, (t,rs) <- ts]
    sentVars = map fst lts
    treeVars = map fst vars
    ruleVars = concatMap snd $ vars
    oval = case ofun of { OFDict f' -> f' lts ; OFLists f'' -> f'' treeVars ruleVars}
  in
    unlines $
      oval ++
      [ " cs" ++ show i ++ ": " ++ intercalate " + " c ++ " >= 1" | (i,c) <- zip [0..] [map fst ts | (s,ts) <- lts]] ++
      [ " c" ++ show i ++ ": " ++ c | (i,c) <- zip [0..] [show (length rs) ++ "" ++ t ++ " - \"" ++ intercalate "\" - \"" rs ++ "\" <= 0" | (s,ts) <- lts, (t,rs) <- ts]] ++
      [ "Binary"] ++ 
      [ " \"" ++ v ++ "\"" | v <- nub ruleVars ] ++
      [ " " ++ v | v <- nub treeVars ] ++
      [ "End" ]

-- Partial implementation of the MPS exporter but the format is a pain in the ass
-- printConstraints (MPS name) (C rules trees) =
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


-- -- Solvers

-- solveCount :: M.Map String Int -> Int -> [String]
-- solveCount counts cutoff =
--   M.keys $ M.filter (>= cutoff) counts

-- | Function to solve a SAT provlem using MiniSat. The problem consists
--   of two parts, a conjunction(!) of premises and a SAT formula to be
--   solved
solveSat :: (SAT String,SAT String) -> IO [String]
solveSat sats =
  do
    -- Create solver
    s <- newSolver
    -- Add solver with the given problem
    (m,ls) <- satToMiniSat s sats
    -- Solve with the sentences to be true as a premise
    b <- solve s ls
    -- Extract the result from the module
    if b then return $ M.keys $ M.filter (\l -> unsafePerformIO $ modelValue s l) m
    else return []

-- Helper functions

-- -- | Function to split a list at a list of positions
-- splitAts :: [Int] -> [a] -> [[a]]
-- splitAts [] _ = []
-- splitAts _ [] = []
-- splitAts (i:is) es =
--   take i es:splitAts is (drop i es)

-- | Function to remove underscores because they annoy LPSolve
clean :: String -> String
clean = filter (not . flip elem "_")


-- | Function tp onverts a sat problem into MiniSat. The problem is stated in two parts, the
--   first is the list of premises, the second is the problem to be solved under the
--   premisses. The premises are supposed to be a conjunction
satToMiniSat :: Solver -> (SAT String,SAT String) -> IO (M.Map String Lit, [Lit])
satToMiniSat solv (prem@(Conj ps),cs) = do
  -- convert all variables into literals and store references in a map
  lits <- mkLits solv [prem,cs]
  -- put the problem into the solver
  satToMiniSat' solv lits cs
  -- convert the premises
  nprems <- mapM (satToMiniSat' solv lits) ps
  return (lits,nprems)
  where
    -- Add a SAT term recursively to the solver and return the top-level literal
    satToMiniSat' :: Solver -> M.Map String Lit -> SAT String -> IO Lit
    satToMiniSat' solv lits cs =
      case cs of
        Conj ss ->
          do
            ls <- mapM (satToMiniSat' solv lits) ss              
            c <- andl solv ls
            addClause solv [c]
            return c
        Dis ss ->
          do
            ls <- mapM (satToMiniSat' solv lits) ss
            c <- orl solv ls
            addClause solv [c]
            return c
        Imp a b ->
          do
            a' <- satToMiniSat' solv lits a
            b' <- satToMiniSat' solv lits b
            c <- implies solv a' b'
            addClause solv [c]
            return c
        SVar v ->
          return $ fromJust $ M.lookup v lits -- this can be evil
    -- Convert all variables in a list of SAT terms into literals and remembers the names in a map
    mkLits :: Solver -> [SAT String] -> IO (M.Map String Lit)
    mkLits s sats = M.unions <$> mapM (mkLits' s M.empty) sats
    mkLits' :: Solver -> M.Map String Lit -> SAT String ->  IO (M.Map String Lit)
    mkLits' s lits (Conj ss) = M.unions <$> mapM (mkLits' s lits) ss
    mkLits' s lits (Dis ss) =  M.unions <$> mapM (mkLits' s lits) ss
    mkLits' s lits (Imp a b) = liftM2 M.union (mkLits' s lits a) (mkLits' s lits b)
    mkLits' s lits (SVar v) = do
      l <- newLit s
      return $ M.insert v l lits

-- | Function to print a model given a solver and a map of literals
printModel :: Solver -> M.Map String Lit -> IO ()
printModel s ls =
   putStrLn =<< (return . unlines) =<< sequence [fmap (((v ++ "\t") ++) . show) $ modelValue s l |  (v,l) <- M.toList ls]

-- | Function to run cplex on a LP problem
runCPLEX :: FilePath -> String -> IO (M.Map Int (Float,[String]))
runCPLEX cplex lp = 
  do
    lpfile <- emptySystemTempFile "cplex.lp"
    writeFile lpfile lp
    putStrLn $ "+++ Writing problem file... " ++ lpfile
    infile <- emptySystemTempFile "cplex.in"
    outfile <- emptySystemTempFile "cplex.sol"
    writeFile infile $ unlines $
      [ "r " ++ lpfile
      , "opt"
      , "display solution variables *"
      , "xecute rm -f " ++ outfile
      , "write " ++ outfile ++ " all"
      , "quit"
      ]
    putStrLn $ "+++ Starting CPLEX... " ++ infile
    system $ cplex ++ " < " ++ infile ++ " &> /tmp/cplex.out"
    putStrLn $ "+++ Reading solution..." ++ outfile
    s <- BS.readFile outfile
    return $ xmlToRules s

type Grammar = (FilePath,String) -- name of the grammar and file content

-- | Function to create GF grammars as strings from the results of CPLEX
createGrammars :: String -> String -> String -> M.Map Int (Float,[String]) -> IO ([Grammar])
createGrammars orig abs lang rs =
  do
    let absgram = (abs ++ ".gf","abstract " ++ abs ++ " = " ++ orig ++" ; ")
    let concgrams = map (\(ct,(obj,rs)) -> 
                           let fn = abs ++ show ct ++ lang 
                           in (fn ++ ".gf", unlines $
--                                   [ "concrete " ++ fn ++ " of " ++ abs ++ " = Cat" ++ lang ++ ",Grammar" ++ lang ++ "[ListS,ListAP,ListNP] ** open (X=" ++ orig ++ lang ++ ") in {"
                                [ "concrete " ++ fn ++ " of " ++ abs ++ " = " ++ lang ++ "Car ** open (X=" ++ lang ++ ") in {"
 
                                   , "  lin" ] ++
                                [ "    " ++ read r ++ " = X." ++ read r ++ " ; " | r <- rs, isRule r] ++
                                [ " } ; " ])
                        ) $ M.toList rs
    return (absgram:concgrams)

-- | Function to check if a variable is a rule, i.e. if it is neither a variable for a sentence, a tree or a constraint
isRule :: String -> Bool
isRule = not . isId
  where
    isId [] = True
    isId ('s':is) = isId is
    isId ('t':is) = isId is
    isId ('c':is) = isId is
    isId (c  :is) | isDigit c = isId is
    isId _  = False
    
-- | Function to parse a CPLEX solution from a XML file
xmlToRules :: BS.ByteString -> M.Map Int (Float,[String])
xmlToRules s =
  saxToRules $ X.parse X.defaultParseOptions s
  where
    saxToRules :: [X.SAXEvent String String] -> M.Map Int (Float,[String])
    saxToRules = findSolution
    findSolution :: [X.SAXEvent String String] -> M.Map Int (Float,[String])
    findSolution [] = M.empty
    findSolution (X.StartElement "CPLEXSolution" _:es) =
      findHeader es
    findSolution (_:es) =
      findSolution es
    findHeader :: [X.SAXEvent String String] -> M.Map Int (Float,[String])
    findHeader (X.StartElement "header" as:es)
      | not $ elem ("solutionName","incumbent") as =
        let
          Just index = read <$> lookup "solutionIndex" as
          Just obj = read <$> lookup "objectiveValue" as
        in
          findVariable (index,obj) es
      | otherwise = findSolution es
    findHeader (_:es) =
      findHeader  es
    findVariable :: (Int,Float) -> [X.SAXEvent String String] -> M.Map Int (Float,[String])
    findVariable (ct,obj) (X.StartElement "variable" as:es)
      | elem ("value","1") as = 
        let 
          rs = findVariable (ct,obj) es
          Just v = lookup "name" as
        in if isRule v then M.alter (Just . maybe (obj,[v]) (\(o,l) -> (o,v:l))) ct rs else rs
      | otherwise = findVariable (ct,obj) es
    findVariable _ (X.EndElement "CPLEXSolution":es) =
      findSolution es
    findVariable p (e:es) =
      findVariable p es

-- Objective functions

data ObjFun = OFDict ([(String,[(String,[String])])] -> [String]) | OFLists ([String] -> [String] -> [String])

-- | Objective function that computes the average number of resulting trees
objAvgNumTrees :: ObjFun -- [(String,[(String,[String])])] -> [String]
objAvgNumTrees = OFDict
  (\lts -> 
      let
        sts = [(s,map fst ts) | (s,ts) <- lts]
        stcs = [(s,length ts) | (s,ts) <- sts]
      in
        [ "Minimize" ] ++
        [ " obj : " ++ intercalate " + " [show (1 / fromIntegral c) ++ "sts" ++ show i | (i,(_,c)) <- zip [0..] stcs]] ++
        [ "Subject to"] ++
        [ " stc" ++ show i ++ ": " ++ intercalate " + " ts ++ " - sts" ++ show i ++ " = 0" | (i,(_,ts)) <- zip [0..] sts]
        --      [ " obj: " ++ show (1/(fromIntegral .length $ sentVars)) ++ "(" ++ intercalate " + " treeVars ++ ")"] ++ -- average number of trees per sentence
  )
         
-- | Objective trees that sums the count of rules with the weighted sum of the rules
objNumTreesWeightedNumRules :: ObjFun -- [String] -> [String] -> [String]
objNumTreesWeightedNumRules = OFLists
  (\treeVars ruleVars ->
     let
       ruleCounts = Prelude.foldl (\m k -> M.alter (maybe (Just 1) (\n -> Just (n + 1))) k m) M.empty $ ruleVars
       ruleCount = length $ nub ruleVars
     in
       [ "Minimize" ] ++
       [ " obj: " ++ intercalate " + " (map (\r -> (show $ (fromIntegral . floor) (ruleCounts M.! r / fromIntegral ruleCount * 100) / 100) ++ r) ruleVars) ++ " + " ++ intercalate " + " treeVars ] ++ -- weighted rules plus number of trees
       [ "Subject to"]
  )

-- | Objective function that counts the number of trees
objNumTrees :: ObjFun -- [String] -> [String] -> [String]
objNumTrees = OFLists
  (\treeVars ruleVars ->
     [ "Minimize" ] ++
     [ " obj: \"" ++ intercalate " + " (nub treeVars) ++ "" ] ++ -- number of trees
     [ "Subject to"]
  )
-- | Objective function that sums the count of trees and the count of rules
objNumTreesNumRules :: ObjFun -- [String] -> [String] -> [String]
objNumTreesNumRules = OFLists
  (\treeVars ruleVars ->
     [ "Minimize" ] ++
     [ " obj: \"" ++ intercalate "\" + \"" (nub ruleVars) ++ "\" + " ++ intercalate " + " (nub treeVars) ++ "" ] ++ -- number of trees plus number of rules
     [ "Subject to"]
  )

-- | Objective function that counts the number of rules
objNumRules :: ObjFun -- [String] -> [String] -> [String]
objNumRules = OFLists
  (\treeVars ruleVars ->
     [ "Minimize" ] ++
     [ " obj: \"" ++ intercalate "\" + \"" (nub ruleVars) ++ "\"" ] ++ -- number of trees plus number of rules
     [ "Subject to"]
  )
-- [("s1",[("t11",[]),("t12",[]),("t13",[]),("t14",[])]),("s2",[("t21",[]),("t22",[])]),("s3",[("t31",[]),("t32",[]),("t33",[]),("t34",[]),("t35",[])]),("s4",[("t41",[])]),("s5",[("t51",[]),("t52",[]),("t53",[])])]
