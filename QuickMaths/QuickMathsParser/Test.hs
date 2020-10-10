module Test where 
 import qualified QuickMathsParser as Q
 import qualified Text.Megaparsec as M
 main = M.parseTest Q.mExpParser "sin x = a + b /  2 # {c+2}"