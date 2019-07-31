module Inference where

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

-- | The hardcoded path to the cplex executable
cplex = "/home/herb/opt/cplex/cplex/bin/x86-64_linux/cplex"

--grammarPath = "/home/herb/src/own/mulle-grammar-tool/data/mini-"

-- | The path to the temp directory
tmpPath = "/tmp/"

-- | Function to generate a number of random trees of a certain depth
generateTrees
  :: PGF             -- ^ the grammar file to be used
  -> Int             -- ^ the depth of the trees
  -> Int             -- ^ the number of trees to be created 
  -> IO [Tree]       -- ^ a list of trees
generateTrees pgf depth ct =
  do
    g <- mkStdGen <$> (\s -> fromIntegral (systemSeconds s) + fromIntegral (systemNanoseconds s)) <$> getSystemTime 
    return $ take ct $ generateRandomDepth g pgf (startCat pgf) (Just depth)

-- | Function to generate a number of random trees of a certain depth given a 'StdGen' generator
generateTreesWithGen
  :: StdGen          -- ^ the random generator to be used
  -> PGF             -- ^ the grammar file to be used
  -> Int             -- ^ the depth of the trees
  -> Int             -- ^ the number of trees to be created 
  -> IO [Tree]       -- ^ a list of trees
generateTreesWithGen g pgf depth ct =
  do
    return $ take ct $ generateRandomDepth g pgf (startCat pgf) (Just depth)

-- | Function to linearize a list of trees
linearizeTrees
  :: PGF             -- ^ the grammar file to be used
  -> [Tree]          -- ^ a list of trees
  -> [String]        -- ^ a list of sentences
linearizeTrees pgf =
  map (linearize pgf (head $ languages pgf)) 

-- | Function to transform a 'PGF' grammar into an restricted representation using a list of example sentences.
--   
--   The result is a representation of one or several grammars containing the objective value and the list of
--   syntax rules.
restrictGrammar
  :: PGF                             -- ^ the grammar file to be restricted
  -> [String]                        -- ^ a list of example sentences
  -> ObjFun                          -- ^ the objective funtion to be used
  -> IO (M.Map Int (Float,[String])) -- ^ the resulting grammars, i.e. lists of syntax rules
restrictGrammar pgf sentences ofun =
  do
    putStrLn ">>> Create problem"
    let problem = convertToSetProblem $ mkMultisetProblem pgf sentences
--    putStrLn $ show problem
    putStrLn ">>> Convert to CPLEX"
    let lp = printConstraints (LP CPLEX) problem ofun
    putStrLn ">>> Write CSP file"
    runCPLEX cplex lp

-- | Function to transform a 'PGF' grammar into an restricted representation using a list of example sentences.
--
--   To restrict the grammar two languages and bilingual examples are used by restricting the syntax trees to
--   the interesction of the sets of syntax trees for both languages
--   
--   The result is a representation of one or several grammars containing the objective value and the list of
--   syntax rules.
restrictMultilingualGrammar
  :: (PGF,Language)                  -- ^ the first language, i.e. pgf and language name
  -> (PGF,Language)                  -- ^ the second language, i.e. pgf and language name
  -> [(String,String)]               -- ^ list of pairs of examples
  -> ObjFun                          -- ^ the objective funtion to be used
  -> IO (M.Map Int (Float,[String])) -- ^ the resulting grammars, i.e. lists of syntax rules
restrictMultilingualGrammar (pgf1,lang1) (pgf2,lang2) sentences ofun =
  do
    putStrLn ">>> Create problem"
    let problem = convertToSetProblem $ mkMultilingMultisetProblem (pgf1,lang1) (pgf2,lang2) sentences
--    putStrLn $ show problem
    putStrLn ">>> Convert to CPLEX"
    let lp = printConstraints (LP CPLEX) problem ofun
    putStrLn ">>> Write CSP file"
    runCPLEX cplex lp

-- | Function to write a grammar represented by lists of syntax rules to GF syntax files
writeGrammar
  :: PGF                        -- ^ original pgf
  -> FilePath                   -- ^ output path
  -> M.Map Int (Float,[String]) -- ^ representation of the restricted grammars
  -> IO [FilePath]              -- ^ list of files written
writeGrammar pgf grammarPath rg = 
  do
    let absname = (showLanguage $ abstractName pgf)
    grams <- createGrammars absname (absname ++ "Cut") (showLanguage $ head $ languages pgf) rg
    let grams' = map (\(f,g) -> (tmpPath ++ f,"--# -path=" ++ grammarPath ++ "\n" ++ g)) grams
    writeFile (tmpPath ++ absname ++ "CutInc.gf") $ "incomplete concrete " ++ absname ++ "CutInc of " ++ absname ++ " = Cat" ++ (showLanguage $ head $ languages pgf) ++ ";"
    mapM_ (uncurry writeFile) grams'
    return $ map fst grams'

-- | Function to compile a GF grammar to a PGF file
compileGrammar
  :: [FilePath]  -- ^ list of GF syntax files
  -> IO FilePath -- ^ path to compiled PGF file
compileGrammar fp =
  do
    tf <- emptySystemTempFile "mulle.pgf"
    let (tp, tfn) = splitFileName tf
    let tn = dropExtension tfn
    system $ "gf --make -n " ++ tn ++ " -D " ++ tp ++ " " ++ unwords fp
    return tf

-- | Function to parse a sentence in a given language
parseSentences
  :: PGF       -- ^ the grammar
  -> Language  -- ^ the name of the language
  -> [String]  -- ^ the list of sentences
  -> [[Tree]]  -- ^ The list of all syntax trees for all the sentences
parseSentences pgf lang =
  map (parse pgf lang (startCat pgf))

-- | Function to check if a each tree in a list of trees is contained in a separate list of trees, i.e.
-- 
--   with
--   \[T=[t_1,\dots,t_n]\] and \[T'=[T'_1,\dots,t_n]\]
--   \[\Downarrow\]
--   \[[t_1\in T'_1,\dots,t_n\in T'_n]\]
compareTrees
  :: [Tree]     -- ^ the list of trees that should be contained
  -> [[Tree]]   -- ^ lists of lists of trees to compare with
  -> IO () 
compareTrees old new =
  let pos = length $ takeWhile (uncurry elem) $ zip old new
  in
    if pos == length old then putStrLn "Trees are covered"
    else
      putStrLn $ "Missmatch after " ++ show pos ++ "trees"

-- | Function to compare two grammars and compute precision and recall
--
-- The formulas used are:
-- \[precision = \frac{|relevant\_functions\cap retrieved\_functions|}{|retrieved\_functions|}\]
-- \[recall = \frac{|relevant\_functions\cap retrieved\_functions|}{|relevant\_functions|}\]
-- where relevant_functions depends on the restricted pgf and relevant_functions depends on the list of syntactic rules given
compareGrammar
  :: PGF            -- ^ the full pgf
  -> PGF            -- ^ a restricted version of the grammar, defines relevant_functions
  -> [String]       -- ^ a list of syntax rules to be compared to the restricted grammar, defined retrieved_functions
  -> (Float,Float)  -- ^ Precision and Recall
compareGrammar fpgf pgf funs =
  let
    -- compute retrieved functions
    retfuns = filterLexical fpgf funs
    -- compute relevant functions
    relfuns = filterLexical fpgf $ map showCId $ functions pgf
    -- compute precision and recall
    precision = (fromIntegral . length) (relfuns `intersect` retfuns) / (fromIntegral . length) retfuns
    recall = (fromIntegral . length) (relfuns `intersect` retfuns) / (fromIntegral . length) relfuns
  in
    (precision,recall)

-- | Function to remove lexical rules, i.e. constant functions, from a set of syntax rules
filterConstant
  :: PGF -> [String] -> [String]
filterConstant pgf rs =
  [ f | (f,t) <- zip rs $ map (\c -> unType <$> (functionType pgf =<< readCId c)) rs, -- extract the type
        isJust t,             -- check that the type has an actual value
        let Just (h,_,_) = t, -- extract function type -> l is the "hypothesis", i.e. the argument types
        (not . null) h        -- filter out types that do not have arguments
  ]

-- | Function to remove lexical rules, i.e. of lexical categories, from a set of syntax rules
filterLexical
  :: PGF -> [String] -> [String]
filterLexical pgf rs =
  [ f | (f,t) <- zip rs $ map (\c -> unType <$> (functionType pgf =<< readCId c)) rs, -- extract the type
        isJust t,             -- check that the type has an actual value
        let Just (h,c,_) = t, -- extract function type -> l is the "hypothesis", i.e. the argument types
        let s = showCId c,    -- convert category to string
        not (s `elem` cats)   -- filter out types that do not have arguments
  ]
  where
    cats = ["A",       -- one-place adjective
            "A2",      -- two-place adjective
            "ACard",   -- adjective like cardinal
            "AdA",     -- adjective-modifying adverb
            "AdN",     -- numeral-modifying adverb
            "AdV",     -- adverb directly attached to verb
            "Adv",     -- verb-phrase-modifying adverb
            "DAP",     -- determiner with adjective
            "Interj",  -- interjection
            "N",       -- common noun
            "N2",      -- relational noun
            "N3",      -- three-place relational noun
            "PN",      -- proper name
            "V",       -- one-place verb
            "V2",      -- two-place verb
            "V2A",     -- verb with NP and AP complement
            "V2Q",     -- verb with NP and Q complement
            "V2S",     -- verb with NP and S complement
            "V2V",     -- verb with NP and V complement
            "V3",      -- three-place verb
            "VA",      -- adjective-complement verb
            "VQ",      -- question-complement verb
            "VS",      -- sentence-complement verb
            "VV"       -- verb-phrase-complement verb
           ]
