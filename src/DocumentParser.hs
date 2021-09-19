{-#LANGUAGE OverloadedStrings #-}
module DocumentParser where
    import Text.Megaparsec
    import QuickMaths 

  type Text = String 
  type Input = Text 
  
  data Paragraph = LaTeX Text | Maths MathsTree 
  type Document = [Paragraph]
 