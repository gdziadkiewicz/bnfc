{-# LANGUAGE QuasiQuotes #-}
module BNFC.Backend.FSharp.FsprojFile(fsprojFile) where

import BNFC.PrettyPrint
import ProjectQuote
import Project

fsprojFile:: String -> [String] -> String -> String ->  String -> Doc
fsprojFile assemblyName fsFiles fslexFile fsyaccFile testsFile =
    text $ toXml $ [fsproj|
Project
AssemblyName $assemblyName
Type Exe
Target net472

CompileMany $fsFiles

FsYacc $fsyaccFile
FsLex $fslexFile

Compile $testsFile

Package FsLexYacc 7.0.6
Package FSharp.Core 4.5.0
Package System.ValueTuple 4.5.0
|]