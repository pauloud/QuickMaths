module Main where 
 import DocumentParser(documentParser)
 import QuickMathsBackend(docToLaTeX,LaTeX)
 import Text.Megaparsec 
 import qualified Text.LaTeX.Base.Render as R 

 main :: IO ()
 main = 
    do 
      parseResult <- return $ parse documentParser "test.txt" "sin x = a + b /  2 // (c+2)" 
      (case parseResult of 
         Left err -> print err 
         Right ast -> R.renderFile "output.txt" (docToLaTeX ast :: LaTeX)) 

   