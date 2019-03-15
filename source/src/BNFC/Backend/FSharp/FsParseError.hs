module BNFC.Backend.FSharp.FsParseError(fsharpParseErrorFile) where
import BNFC.PrettyPrint

-- | Create file with ParseError exception in provided namespace
--
-- >>> fsharpParseErrorFile "TestNamespace"
-- //automatically generated by BNFC
-- namespace TestNamespace
-- open Microsoft.FSharp.Text.Lexing
-- <BLANKLINE>
-- exception ParseError of Position * Position
-- >>> fsharpParseErrorFile "TestNamespace.SubNamespace"
-- //automatically generated by BNFC
-- namespace TestNamespace.SubNamespace
-- open Microsoft.FSharp.Text.Lexing
-- <BLANKLINE>
-- exception ParseError of Position * Position
fsharpParseErrorFile :: String -> Doc
fsharpParseErrorFile namespace = vcat
    [
        "//automatically generated by BNFC",
        text $ "namespace " ++ namespace,
        "open Microsoft.FSharp.Text.Lexing",
        "",
        "exception ParseError of Position * Position"
    ]