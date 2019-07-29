compareTreebankMultiSingle fp pgfp = 
  do
    f <- readFile fp
    pgf <- readPGF pgfp
    let tb = read f :: [(String,String,String)]
    let ss = [s | (s,_,_) <- tb ]
    let ts = [t | (_,_,t) <- tb ]
    let problem = convertToSetProblem $ mkMultisetProblem pgf ss
    let lp = printConstraints (LP CPLEX) problem
    putStrLn ">>> Write CSP file"
    runCPLEX cplex lp
    
compareTreebankMulti fp pgf1 pgf2 = 
  do
    f <- readFile fp
    tb = read f :: [(String,String,String)]
    
