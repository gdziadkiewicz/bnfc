{-
    BNF Converter: FSharp Abstract Syntax Generator
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

-- based on BNFC OCaml backend

module BNFC.Backend.FSharp.CFtoFSharpAbs (cf2Abstract) where

import Text.PrettyPrint

import BNFC.CF
import BNFC.Utils((+++))
import Data.List(intersperse)
import BNFC.Backend.FSharp.FSharpUtilities

-- to produce an F# module
cf2Abstract :: String -> CF -> String
cf2Abstract absMod cf = unlines $
  "// FSharp module generated by the BNF converter" :
  ("module" +++ absMod) :
  mutualRecDefs (map (prSpecialData cf) (specialCats cf) ++ map prData (cf2data cf))

-- allow mutual recursion so that we do not have to sort the type definitions in
-- dependency order
mutualRecDefs :: [String] -> [String]
mutualRecDefs [] = []
mutualRecDefs (x:xs) = ("type" +++ x)  :  map ("and" +++) xs

prData :: Data -> String
prData (cat, rules) = unlines $
  (fixType cat +++ "=") :
   map ((" | " ++) . prRule) rules

prRule (fun, [])  = fun
prRule (fun,cats) = fun +++ "of" +++ render (mkTupleType cats)

-- | Creates an OCaml type tuple by intercalating * between type names
-- >>> mkTupleType [Cat "A"]
-- A
--
-- >>> mkTupleType [Cat "A", Cat "Abc", Cat "S"]
-- A * Abc * S
mkTupleType :: [Cat] -> Doc
mkTupleType = hsep . intersperse (char '*') . map (text . fixType)

prSpecialData :: CF -> Cat -> String
prSpecialData cf cat = fixType cat +++ "=" +++ show cat +++ "of" +++ contentSpec cf cat

--  unwords ["newtype",cat,"=",cat,contentSpec cf cat,"deriving (Eq,Ord,Show)"]

contentSpec :: CF -> Cat -> String
contentSpec cf cat
    | isPositionCat cf cat = "((int * int) * string)"
    | otherwise = "string"