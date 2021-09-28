module QuickMathsParser (quickMathsParser,MathsTree(..),term) where
  import qualified Control.Monad.Combinators.Expr as E 
  import qualified Text.Megaparsec.Char as C
  import Text.Megaparsec
  import Data.Void
  --import QuickMathsLexer 


  type Text = String 
  --type Input = [InputWord]
  type Parser = Parsec Void String 

  data MathsTree = MathsTree [MathsTree] | MathsIdent Text | Frac MathsTree MathsTree | Parens MathsTree| -- Interval MathsTree MathsTree |CurlyB MathsTree MathsTree |SpaceL MathsTree| SpaceR MathsTree |Concat {sep :: Text, left :: MathsTree, right::MathsTree}
       Spaces Text |Newline|Concat Text MathsTree MathsTree  deriving Show


  
  string = C.string--chunk $ map Char s
  char = C.char --single (Char c)


  newline = Newline <$ char '\n'
  spaces = Spaces <$> some (char ' ' <|> char '\t')


  operatorTable :: [[E.Operator Parser MathsTree]]
  operatorTable = [[binaryL (char '*')  (Concat "×"),E.InfixL (try $ Frac <$ (char '/' <* notFollowedBy (char '/'))) ]
                    ,[binaryL (char '+') (Concat "+"), binaryL (char '-')(Concat "-")]
                    , [binaryL (string "//") Frac]]
  binaryL parser function = E.InfixL $ function <$ parser 
  
  
  --singleChar = single.char
  parens =  Parens  <$> (between (char '(') (char ')') term)

  
  




  concat p1 p2 = do
    left <- p1
    right <- p2
    return $ Concat mempty left right 
  term = let  ident :: Parser MathsTree 
              ident = MathsIdent <$> some (noneOf "\t\n+-*/")
              {-(some $ satisfy (\t -> case t of
                                                          LowDiv -> False
                                                          Char '+' -> False
                                                          Char '-' -> False 
                                                          Char '*' -> False
                                                          Char '/' -> False
                                                          Char _ -> True))-} 
                in MathsTree <$> (some $ spaces <|> ident) <?> "term"
  mathsTerm = parens <|> term
  mathsLine = E.makeExprParser mathsTerm operatorTable
  quickMathsParser :: Parsec Void String MathsTree 
  quickMathsParser = MathsTree <$> (many $ mathsLine <|> newline) 


 



  
