{-
    BNF Converter: Template Generator
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


module BNFC.Backend.FSharp.CFtoFSharpTemplate (
                    cf2Template
                    ) where

import BNFC.CF
import Data.Char
import BNFC.Backend.FSharp.FSharpUtil


type ModuleName = String
type Constructor = String

cf2Template :: ModuleName -> ModuleName  -> CF -> String
cf2Template skelName absName cf = unlines
  [
  "module "++ skelName ++ " = struct\n",
  "(* OCaml module generated by the BNF converter *)\n",
  "open " ++ absName ++ "\n",
  "type result = string\n",
  "let failure x = failwith \"Undefined case.\" (* x discarded *)\n",
  unlines $ mutualDefs $ map (\(s,xs) -> case_fun s (toArgs xs)) $ specialData cf ++ cf2data cf,
  "end"
  ]
 where toArgs               [] = []
       toArgs ((cons,args):xs)
           = (cons ++ " " ++  (mkTuple $ names (map (checkRes . var) args) (0 :: Int))) : toArgs xs
       names :: [String] -> Int -> [String]
       names [] _ = []
       names (x:xs) n
        | elem x xs = (x ++ show n) : names xs (n+1)
        | otherwise = x : names xs n
       var (ListCat c)      = var c ++ "s"
       var (Cat "Ident")    = "id"
       var (Cat "Integer")  = "n"
       var (Cat "String")   = "str"
       var (Cat "Char")     = "c"
       var (Cat "Double")   = "d"
       var cat              = map toLower (show cat)
       checkRes s
        | elem s reservedOCaml = s ++ "'"
        | otherwise              = s


case_fun :: Cat -> [Constructor] -> String
case_fun cat xs =
 unlines $
         ["trans" ++ show cat ++ " (x : " ++ fixType cat ++ ") : result = match x with",
          unlines $ insertBar $ map (\s -> s ++ " -> " ++ "failure x") xs]
