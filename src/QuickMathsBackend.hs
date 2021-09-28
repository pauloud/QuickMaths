
module QuickMathsBackend where
    --import qualified Text.LaTeX.Base.Render as R 
    import Text.LaTeX.Base.Syntax
    import qualified Text.LaTeX.Base.Math as M
    import Text.LaTeX.Base.Parser(parseLaTeX)
    import QuickMathsParser
    import DocumentParser
    import Data.Function((&))
    import Data.Foldable(foldMap')
    import Data.Text(pack)
    

    docToLaTex :: Document -> LaTeX 
    docToLaTex = foldMap' paragraphToLaTex

    teXRaw :: String -> LaTeX
    teXRaw = (.) TeXRaw pack  

    paragraphToLaTex (LaTeX t) = case parseLaTeX (pack t) of
        Left _ -> teXRaw t
        Right laTeX -> laTeX
    paragraphToLaTex (Maths mathsTree) = 
        let content mt = case mt of
                        Spaces spaces -> foldMap' spaceToCommand spaces 
                                            where spaceToCommand c 
                                                    |c==' ' = TeXCommS " " 
                                                    |c=='\t' = TeXCommS ";"
                        MathsIdent t -> teXRaw t
                        Frac n d -> M.frac  (content n)  (content d)
                        Parens mt1 -> M.autoParens (content mt1) 
                        Concat sep left right -> content left <> teXRaw sep <> content right
                        MathsTree trees -> foldMap' content trees 
                in M.mathDisplay $ content mathsTree 



    {-toLatex = toLaTeX
    toLaTeX :: MathsTree -> LaTeX
    toLaTeX (LaTeX []) = TexEmpty
    toLaTex (LaTeX t) = 
    toLaTex (Maths t) = t & replaceElemStrictText 
    toLaTex Frac n d = TeXComm "frac" [FixArg $ toLatex n, FixArg $ toLatex d]
    toLaTeX (concat sep left right) = toLatex left <> toLatex.Text sep <> toLatexRight

    type Text = String
    mathsSpaces :: Text -> LaTex-}
    
   

