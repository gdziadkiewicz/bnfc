{-
    BNF Converter: FSharp main file
    Copyright (C) 2016  Author: Grzegorz Dziadkiewicz

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

-- based on BNFC OCaml backend


module BNFC.Backend.FSharp (makeFSharp) where
import Control.Monad(when)
import System.FilePath (pathSeparator, (</>))

import BNFC.Backend.Base hiding (Backend)
import BNFC.Backend.Common.Makefile(mkMakefile)
import BNFC.Backend.FSharp.FsprojFile(fsprojFile)
import BNFC.Backend.FSharp.CFtoFSharpAbs
import BNFC.Backend.FSharp.CFtoFsLex
import BNFC.Backend.FSharp.CFtoFSharpPrinter
import BNFC.Backend.FSharp.CFtoFSharpShow
import BNFC.Backend.FSharp.CFtoFSharpTemplate
import BNFC.Backend.FSharp.CFtoFSharpTest
import BNFC.Backend.FSharp.CFtoFsYacc
import BNFC.Backend.FSharp.FsMakeFile(makefile)
import BNFC.Backend.FSharp.FsParseError(fsharpParseErrorFile)
import BNFC.Backend.XML
import BNFC.CF
import BNFC.Options
import BNFC.PrettyPrint
import BNFC.Utils

-- naming conventions
withLang :: SharedOptions -> String -> String
withLang opts name = lang opts ++ "." ++ name

mkMod :: (SharedOptions -> String -> String) -> String -> SharedOptions -> String
mkMod addLang name opts =
    pref ++ if inDir opts then lang opts ++ "." ++ name else addLang opts name
        where pref = maybe "" (++".") (inPackage opts)

mkFile :: (SharedOptions -> String -> String) -> String -> String -> SharedOptions -> FilePath
mkFile addLang name ext opts =
    pref ++ if inDir opts
       then lang opts </> name ++ ext'
       else addLang opts name ++  ext'
    where pref = maybe "" (\p->pkgToDir p </> "") (inPackage opts)
          ext' = if null ext then "" else  if null name then ext else "." ++ ext

absFile, absFileM, fslexFile, fslexFileM, fsyaccFile, fsyaccFileM,
  parseErrorFile, parseErrorFileM, templateFile, templateFileM,
  printerFile, printerFileM, projFile, tFile :: SharedOptions -> String
absFile         = mkFile withLang "Abs" "fs"
absFileM        = mkMod  withLang "Abs"
fslexFile       = mkFile withLang "Lex" "fsl"
fslexFileM      = mkMod  withLang "Lex"
fsyaccFile      = mkFile withLang "Par" "fsy"
fsyaccFileM     = mkMod  withLang "Par"
templateFile    = mkFile withLang "Skel" "fs"
templateFileM   = mkMod  withLang "Skel"
printerFile     = mkFile withLang "Print" "fs"
printerFileM    = mkMod  withLang "Print"
showFile        = mkFile withLang "Show" "fs"
showFileM       = mkMod  withLang "Show"
tFileM          = mkMod  withLang "Test"
tFile           = mkFile withLang "Test" "fs"
parseErrorFileM = mkMod  withLang "BnfcUtil" 
parseErrorFile  = mkFile withLang "ParseError" "fs"
projFile        = mkFile withLang "" "fsproj"

makeFSharp :: SharedOptions -> CF -> MkFiles ()
makeFSharp opts cf = do
  let absMod = absFileM opts
      lexMod = fslexFileM opts
      parMod = fsyaccFileM opts
      prMod  = printerFileM opts
      showMod = showFileM opts
      tFileMod = tFileM opts
      dir = let d = codeDir opts in if null d then "." else d ++ [pathSeparator]
      files = [(absFile opts), (templateFile opts), (printerFile opts), (showFile opts), (parseErrorFile opts)]
  do
    mkfile (absFile opts) $ cf2Abstract absMod cf
    mkfile (fslexFile opts) $ cf2fslex lexMod parMod cf
    mkfile (fsyaccFile opts) $ cf2fsyacc parMod absMod lexMod  cf
    mkfile (templateFile opts) $ cf2Template (templateFileM opts) absMod cf
    mkfile (printerFile opts)  $ cf2Printer prMod absMod cf
    mkfile (showFile opts)  $ cf2show showMod absMod cf
    mkfile (tFile opts) $ fsharpTestfile absMod lexMod parMod prMod showMod tFileMod cf
    mkfile (parseErrorFile opts) $ fsharpParseErrorFile (parseErrorFileM opts)
    when (visualStudio opts) $ mkfile (projFile opts) $ fsprojFile "Pies" files (fslexFile opts) (fsyaccFile opts) (tFile opts)
    mkMakefile opts $ makefile dir (projFile opts)
    case xml opts of
      2 -> makeXML opts True cf
      1 -> makeXML opts False cf
      _ -> return ()

pkgToDir :: String -> FilePath
pkgToDir = replace '.' pathSeparator

codeDir :: SharedOptions -> FilePath
codeDir opts = let pref = maybe "" pkgToDir (inPackage opts)
                   dir = if inDir opts then lang opts else ""
                   sep = if null pref || null dir then "" else [pathSeparator]
                 in pref ++ sep ++ dir
