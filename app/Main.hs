module Main where 
 import DocumentParser
 import Text.Megaparsec 
 main = 
    parseTest documentParser "sin x = a + b /  2 // (c+2) "
   