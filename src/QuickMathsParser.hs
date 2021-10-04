module QuickMathsParser (quickMathsParser,MathsTree(..),term) where
  import qualified Control.Monad.Combinators.Expr as E 
  import qualified Text.Megaparsec.Char as C
  --import qualified Text.LaTeX.Base.Syntax as S 
  --import qualified Text.LaTeX.Base.Commands as C 
  --import qualified Text.LaTeX.Base.Math as M
  import Text.Megaparsec
  import Data.Void
  --import QuickMathsLexer 


  type Text = String 
  type Spaces = String 
  --type Input = [InputWord]
  type Parser = Parsec Void String 

  data GreatOp = Sum 
  data MathsTree = MathsTree [MathsTree] | MathsIdent Spaces Text |Frac MathsTree MathsTree | Parens MathsTree
        |Newline|Concat Text MathsTree MathsTree |Interval MathsTree MathsTree 
        |GreatOp GreatOp MathsTree MathsTree  
        |MemberShip MathsTree MathsTree deriving Show


  
  string = C.string--chunk $ map Char s
  char = C.char --single (Char c)


  newline = Newline <$ char '\n'



  operatorTable :: [[E.Operator Parser MathsTree]]
  operatorTable = [ [E.InfixL (try $ Interval <$ (string " à " ]
                    ,[binaryL (char '*')  (Concat "\\times"),E.InfixL (try $ Frac <$ (char '/' <* notFollowedBy (char '/'))) ]
                    ,[binaryL (char '+') (Concat "+"), binaryL (char '-')(Concat "-")]
                    , [binaryL (string "//") Frac]]
  binaryL parser function = E.InfixL $ function <$ parser 
  
  
  --singleChar = single.char

  parens =  Parens  <$> between (char '(' ) ( char ')' ) term

  
  




{-concat p1 p2 = do
    left <- p1
    right <- p2
    return $ Concat mempty left right 
  interval = do
    inf <- term
    char 'à' 
    sup <- term 
    return Interval inf sup -}
  term = let  ident :: Parser MathsTree

              ident =  try $ MathsIdent <$> (many (char ' ' <|> char '\t')>> notFollowedBy string "à") <*> some (noneOf "\t\n+-*/ "))
              {-(some $ satisfy (\t -> case t of
                                                          LowDiv -> False
                                                          Char '+' -> False
                                                          Char '-' -> False 
                                                          Char '*' -> False
                                                          Char '/' -> False
                                                          Char _ -> True))-} 
                in MathsTree <$> ident <?> "term"
  mathsTerm = parens <|> term
  mathsLine = E.makeExprParser mathsTerm operatorTable
  quickMathsParser :: Parsec Void String MathsTree 
  quickMathsParser = MathsTree <$> (many $   mathsLine <|> newline) 


 



  
