module QuickMathsParser (quickMathsParser,MathsTree(..),GreatOp(..),term) where
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

  data GreatOp = Sum deriving (Show,Eq) 
  data MathsTree = MathsTree [MathsTree] | MathsIdent Spaces Text |Frac MathsTree MathsTree | Parens MathsTree
        |Newline|Concat Text MathsTree MathsTree |Interval MathsTree MathsTree 
        |GreatOp GreatOp MathsTree  
        |MemberShip MathsTree MathsTree deriving (Show,Eq)


  
  string = C.string--chunk $ map Char s
  char = C.char --single (Char c)


  newline = Newline <$ char '\n'
  spaceChar = C.char ' ' <|> C.char '\t' 
  



  operatorTable :: [[E.Operator Parser MathsTree]]
  operatorTable = [ [E.InfixL (try $ Interval <$ string " à ") ]
                    ,[binaryL (char '*')  (Concat "\\times"),E.InfixL (try $ Frac <$ (char '/' <* notFollowedBy (char '/')))]
                    ,[binaryL (char '+') (Concat "+"), binaryL (char '-')(Concat "-")]
                    , [binaryL (string "//") Frac]
                    ,[E.Prefix (GreatOp Sum <$string "SUM")]]
  binaryL parser function = E.InfixL $ function <$ parser 
  
  
  --singleChar = single.char

  parens =  Parens  <$> between (char '(' ) ( char ')' ) term

  
  




  term = let  withSpaces :: Parser (Text -> MathsTree)
              withSpaces = MathsIdent <$> many spaceChar >>= (\spaces -> 
                notFollowedBy (string "à" >> spaceChar) >> return spaces)
              ident :: Parser MathsTree
              ident =  try $ (withSpaces  
                                  <*> ( (:) <$> oneOf "\t\n/+" <*> many (noneOf "\t\n/-+ ")))
                in ident <?> "term"
  mathsTerm = parens <|> term
  mathsLine = (E.makeExprParser mathsTerm operatorTable)
  quickMathsParser :: Parsec Void String MathsTree 
  quickMathsParser = MathsTree <$> (many $   mathsLine <|> newline) 


 



  
