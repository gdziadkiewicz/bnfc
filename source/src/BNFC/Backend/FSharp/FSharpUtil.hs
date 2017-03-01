{-
    BNF Converter: FSharp backend utility module
    Copyright (C) 2016  Author:  Grzegorz Dziadkiewicz

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

module BNFC.Backend.FSharp.FSharpUtil where

import BNFC.CF
import BNFC.Utils
import Data.List(intercalate)

-- Translate Haskell types to F# types
fixType :: Cat -> String
fixType (ListCat c) = fixType c +++ "list"
fixType (TokenCat "Integer") = "int"
fixType (TokenCat "Double") = "float"
fixType (TokenCat "String") = "string"
fixType cat = fixKeywordUse $ show cat

fixKeywordUse:: String -> String
fixKeywordUse s = if s `elem` reservedFSharp then s ++ "T" else s

reservedFSharp :: [String]
reservedFSharp = [
    "abstract","and","as","assert","base","begin","class","default","delegate","do","done",
    "downcast","downto","elif","else","end","exception","extern","false","finally","for",
    "fun","function","global","if","in","inherit","inline","interface","internal","lazy","let",
    "match","member","module","mutable","namespace","new","null","of","open","or",
    "override","private","public","rec","return","sig","static","struct","then","to",
    "true","try","type","upcast","use","val","void","when","while","with","yield",
    --reserved
    "atomic","break","checked","component","const","constraint","constructor",
    "continue","eager","fixed","fori","functor","include",
    "measure","method","mixin","object","parallel","params","process","protected","pure",
    "recursive","sealed","tailcall","trait","virtual","volatile"
    ]

mkTuple :: [String] -> String
mkTuple [] = ""
mkTuple [x] = x
mkTuple xs = "(" ++ intercalate ", " xs ++ ")"

insertBar :: [String] -> [String]
insertBar = map ("| " ++)

fsharpTab:: String
fsharpTab = "    "
indent :: Int -> String -> String
indent n s =
    concat (replicate n fsharpTab) ++ s

mutualDefs :: [String] -> [String]
mutualDefs [] = []
mutualDefs (d:ds) = ("let rec" +++ d) : map ("and" +++) ds
