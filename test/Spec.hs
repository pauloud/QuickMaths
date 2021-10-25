module Spec where
    import DocumentParser  
    import QuickMathsParser
    import QuickMathsBackend () 
    import qualified Text.Megaparsec as M

    testSum = M.parse quickMathsParser "Sum" "Sum i=0 à  n" == Right (MathsTree [GreatOp Sum (Interval (MathsIdent "" "i=1")(MathsIdent " " "n"))])
    
    main :: IO ()
    main = putStrLn "Test suite not yet implemented"
