{-#LANGUAGE OverloadedStrings #-}
module DocumentParser(Document,Paragraph(..),documentParser) where
    import Text.Megaparsec
    import QuickMathsParser
     

    type Text = String 
    type Input = Text 
  
    data Paragraph = LaTeX Text | Maths MathsTree deriving Show 
    type Document = [Paragraph]
    singleton x = [x]

    documentParser = singleton <$> Maths <$> quickMathsParser

    
