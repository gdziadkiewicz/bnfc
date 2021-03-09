module BNFC.Backend.FSharp (makeFSharp) where

import System.FilePath (pathSeparator, (</>))

import BNFC.Backend.Base hiding (Backend)
import BNFC.Backend.Common.Makefile
import BNFC.Backend.FSharp.CFtoFSharpAbs
import BNFC.Backend.FSharp.CFtoFsLex
import BNFC.Backend.FSharp.CFtoFSharpPrinter
import BNFC.Backend.FSharp.CFtoFSharpShow
import BNFC.Backend.FSharp.CFtoFSharpTemplate
import BNFC.Backend.FSharp.CFtoFSharpTest
import BNFC.Backend.FSharp.CFtoFsYacc
import BNFC.Backend.XML
import BNFC.CF
import BNFC.Options
import BNFC.PrettyPrint
import BNFC.Utils

-- naming conventions

noLang :: SharedOptions -> String -> String
noLang _ name = name

withLang :: SharedOptions -> String -> String
withLang opts name = name ++ sanitizedLang opts

mkMod :: (SharedOptions -> String -> String) -> String -> SharedOptions -> String
mkMod addLang name opts =
    pref ++ if inDir opts then sanitizedLang opts ++ "." ++ name else addLang opts name
        where pref = maybe "" (++".") (inPackage opts)

mkFile :: (SharedOptions -> String -> String) -> String -> String -> SharedOptions -> FilePath
mkFile addLang name ext opts =
    pref ++ if inDir opts
       then sanitizedLang opts </> name ++ ext'
       else addLang opts name ++ if null ext then "" else ext'
    where pref = maybe "" (\ p -> pkgToDir p </> "") (inPackage opts)
          ext' = if null ext then "" else "." ++ ext

-- | Turn language name into a valid ocaml module identifier.
sanitizedLang :: SharedOptions -> String
sanitizedLang = camelCase_ . lang


absFile, absFileM, fslexFile, fslexFileM, fsyaccFile, fsyaccFileM,
  utilFile, templateFile, templateFileM, printerFile, printerFileM,
  tFile :: SharedOptions -> String
absFile       = mkFile withLang "Abs" "fs"
absFileM      = mkMod  withLang "Abs"
fslexFile     = mkFile withLang "Lex" "fsl"
fslexFileM    = mkMod  withLang "Lex"
fsyaccFile    = mkFile withLang "Par" "fsy"
fsyaccFileM   = mkMod  withLang "Par"
templateFile  = mkFile withLang "Skel" "fs"
templateFileM = mkMod  withLang "Skel"
printerFile   = mkFile withLang "Print" "fs"
printerFileM  = mkMod  withLang "Print"
showFile      = mkFile  withLang "Show" "fs"
showFileM     = mkMod  withLang "Show"
tFile         = mkFile withLang "Test" "fs"
utilFile      = mkFile noLang   "BNFC_Util" "fs"

makeFSharp :: SharedOptions -> CF -> MkFiles ()
makeFSharp opts cf = do
  let absMod = absFileM opts
      lexMod = fslexFileM opts
      parMod = fsyaccFileM opts
      prMod  = printerFileM opts
      showMod = showFileM opts
  do
    mkfile (absFile opts) $ cf2Abstract absMod cf
    mkfile (fslexFile opts) $ cf2fslex lexMod parMod cf
    mkfile (fsyaccFile opts) $
                 cf2fsyacc parMod absMod lexMod  cf
    mkfile (templateFile opts) $ cf2Template (templateFileM opts) absMod cf
    mkfile (printerFile opts)  $ cf2Printer prMod absMod cf
    mkfile (showFile opts)  $ cf2show showMod absMod cf
    mkfile (tFile opts) $ fsharpTestfile absMod lexMod parMod prMod showMod cf
    mkfile (utilFile opts) utilM
    mkMakefile opts $ makefile opts
    case xml opts of
      2 -> makeXML opts True cf
      1 -> makeXML opts False cf
      _ -> return ()

pkgToDir :: String -> FilePath
pkgToDir = replace '.' pathSeparator

codeDir :: SharedOptions -> FilePath
codeDir opts = let pref = maybe "" pkgToDir (inPackage opts)
                   dir = if inDir opts then sanitizedLang opts else ""
                   sep = if null pref || null dir then "" else [pathSeparator]
                 in pref ++ sep ++ dir

makefile :: SharedOptions -> String -> Doc
makefile opts basename = vcat
    [ mkVar "OCAMLC" "ocamlc"
    , mkVar "OCAMLYACC" $ case ocamlParser opts of
        OCamlYacc -> "ocamlyacc"
        Menhir    -> "menhir"
    , mkVar "OCAMLLEX" "ocamllex"
    , mkVar "OCAMLCFLAGS" ""
    , mkRule "all" []
        [ "$(OCAMLYACC) " ++ fsyaccFile opts
        , "$(OCAMLLEX) "  ++ fslexFile opts
        , "$(OCAMLC) $(OCAMLCFLAGS) -o " ++ mkFile withLang "Test" "" opts +++
                          utilFile opts +++
                          absFile opts +++ templateFile opts +++
                          showFile opts +++ printerFile opts +++
                          mkFile withLang "Par" "mli" opts +++
                          mkFile withLang "Par" "ml" opts +++
                          mkFile withLang "Lex" "ml" opts +++
                          tFile opts ]
    , mkRule "clean" []
        [ "-rm -f " ++ unwords (map (dir++) [ "*.cmi", "*.cmo", "*.o" ]) ]
    , mkRule "distclean" ["clean"]
        [ "-rm -f " ++ unwords [ mkFile withLang "Lex" "*" opts,
                                 mkFile withLang "Par" "*" opts,
                                 mkFile withLang "Layout" "*" opts,
                                 mkFile withLang "Skel" "*" opts,
                                 mkFile withLang "Print" "*" opts,
                                 mkFile withLang "Show" "*" opts,
                                 mkFile withLang "Test" "*" opts,
                                 mkFile withLang "Abs" "*" opts,
                                 mkFile withLang "Test" "" opts,
                                 utilFile opts,
                                 basename ]]
    ]
  where dir = let d = codeDir opts in if null d then "" else d ++ [pathSeparator]

utilM :: String
utilM = unlines
    ["// automatically generated by BNFC ",
     "module BNFC_Util",
     "open FSharp.Text",
     "",
     "exception Parse_error of Lexing.Position * Lexing.Position"
    ]
