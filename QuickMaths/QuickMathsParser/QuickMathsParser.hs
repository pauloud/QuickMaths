module QuickMathsParser where
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import Control.Monad.Combinators.Expr
  import qualified Text.Megaparsec.Lexer as L

  type Parser = Parsec Void Text

  operatorTable :: [[Operator Parser MExpression]]
  operatorTable = [[binaryNA  "à" Bornes],[binaryL "*" Concat "*", binaryL "/" Frac],[binaryL "+" Concat "+", binaryL "-" Concat "-"],[binaryL "//" Frac]]
  symbol :: Parser String
  symbol = L.symbol space1


  binaryN,binaryL :: Text -> (MExpression -> MExpression -> MExpression) -> Operator Parser MExpression
  binaryN name f = InfixN (f <$ symbol name)
  binaryL name f = InfixL (f <$ symbol name)


  --data Bornes = Bornes String String
  data MExpression = PureLateX String | Frac MExpression MExpression | Curly MExpression | Parens MExpression|Concat {sep :: String left :: MExpression right::MExpression} deriving Show
  type ShortMaths = MExpression
  --curlyB =  Curly . PureLateX <$> (between (char '{') char ('}') mTermParser
  --normalement l'idée ci dessus est à abandonner
  parens =  Parens . PureLateX <$> (between (char '(') char (')') mTermParser
  term :: Parser MExpression
  term = Expression <$> many noneOf "+/*()"


  mTermParser :: Parser MExpression
  mTermParser = choice [parens, term]
  mExpParser = makeExprParser mTermParser operatorTable

  
