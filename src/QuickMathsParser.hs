module QuickMathsParser (mExpParser,MExpression,term) where
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import Control.Monad.Combinators.Expr
  import qualified Text.Megaparsec.Char.Lexer as L
  --import Data.Text(Text)
  import Data.Void

  type Text = [Char]
  type Parser = Parsec Void Text



  operatorTable :: [[Operator Parser MExpression]]
  

  operatorTable = [[prefix" " SpaceL],[postfix " " SpaceR],[binaryN  "à" Bornes],[binaryL "*" (Concat "*"),binaryL "/" Frac],[binaryL "+" (Concat "+"), binaryL "-" (Concat "-")], [binaryL "#" BigFrac]]
  {-beginWhitesOps = map (\n -> (prefix (take n (repeat ' '))  (BeginWhites n) )) [1..]
  endWhitesOps = map (\n -> (postfix (take n (repeat ' ') ) (EndWhites n) )) [1..] not optimised-}

  binaryN,binaryL :: String -> (MExpression -> MExpression -> MExpression) -> Operator Parser MExpression
  binaryN name f = InfixN (f <$ symbol name)
  binaryL name f = InfixL (f <$ symbol name)
  prefix,postfix :: String -> (MExpression -> MExpression) -> Operator Parser MExpression
  prefix  name f = Prefix  (f <$ symbol name)
  postfix name f = Postfix (f <$ symbol name)
 

  symbol :: String -> Parser String
  symbol = string
 


 
  {- the MExpression is the short name for Mathematical Expression
  -}

  data MExpression =  Expression String|Frac MExpression MExpression |BigFrac MExpression MExpression| Parens MExpression| CurlyB MExpression|
       Bornes MExpression MExpression |SpaceL MExpression| SpaceR MExpression|BeginWhites Int MExpression| EndWhites Int MExpression|Concat {sep :: String, left :: MExpression, right::MExpression} deriving Show
  type ShortMaths = MExpression
  curlyB =  CurlyB <$> (between (char '{') (char '}') mExpParser)
  parens =  Parens  <$> (between (char '(') (char ')') mExpParser)
  term :: Parser MExpression
  term = Expression <$> many (noneOf "+-/*()#{}")


  mTermParser,mExpParser :: Parser MExpression
  mTermParser = choice [curlyB, parens, term]
  mExpParser = makeExprParser mTermParser operatorTable
  replace :: Functor f => a ->  f b -> f a
  replace a = fmap (\_-> a) 
  



  
