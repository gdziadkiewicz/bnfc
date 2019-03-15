{-
    TODO: Check if printing fot native fsharp objects like choice types or algebraic types can handle this
    BNF Converter: Non-pretty-printer generator (no "deriving Show" in OCaml...)
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

-- there is no "deriving Show" in OCaml, although there are solutions based
-- on camlp4. Here we generate our own "show module".
module BNFC.Backend.FSharp.CFtoFSharpShow (cf2show) where

import Data.Char(toLower)
import Data.List (intersperse)
import Data.Maybe (fromJust)

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.FSharp.FSharpUtilities

cf2show :: String -> String -> CF -> String
cf2show name absMod cf = unlines [
  prologue name absMod,
  integerRule cf,
  doubleRule cf,
  if hasIdent cf then identRule cf else "",
  unlines [ownPrintRule cf own | (own,_) <- tokenPragmas cf],
  rules cf
  ]

prologue :: String -> String -> String
prologue name absMod = unlines [
  "// show functions generated by the BNF converter",
  "module " ++ name,
  "",
  "open System.Text",
  "open " ++ absMod,
  "",
  "type Showable = StringBuilder -> unit",
  "",
  "let show (s : Showable) : string = ",
  "    let initSize = 16",
  "    let b = StringBuilder initSize",
  "    s b",
  "    b.ToString()",
  "",
  "let emptyS : Showable = ignore",
  "",
  "let c2s (c:char) : Showable = fun buf -> buf.Append c |> ignore",
  "let s2s (s:string) : Showable = fun buf -> buf.Append s |> ignore",
  "",
  "let ( >> ) (s1:Showable) (s2:Showable) : Showable = fun buf ->",
  "    s1 buf",
  "    s2 buf",
  "",
  "let showChar (c:char) : Showable = fun buf -> ",
  "    buf.Append (\"'\" + string c + \"'\") |> ignore",
  "",
  "let showString (s:string) : Showable = fun buf -> ",
  "    buf.Append (\"\\\"\" + s + \"\\\"\") |> ignore",
  "",
  "let showList (showFun : 'a -> Showable) (xs : 'a list) : Showable = fun buf -> ",
  "    let rec f ys =",
  "        match ys with",
  "        | [] -> ()",
  "        | [y] -> showFun y buf",
  "        | y::ys ->",
  "            showFun y buf",
  "            buf.Append \"; \" |> ignore",
  "            f ys ",
  "    buf.Append '[' |> ignore",
  "    f xs;",
  "    buf.Append ']' |> ignore"
  ]

integerRule _ = "let showint (i:int) : Showable = i |> string |> s2s"

doubleRule _ = "let showfloat (f:float) : Showable = f |> string |> s2s"

identRule cf = ownPrintRule cf (Cat "Ident")

ownPrintRule cf own = unlines [
  "let rec" +++ showsFun own +++ "(" ++ show own ++ posn ++ ") : Showable = s2s \""
  ++ show own ++ " \" >> showString i"
  ]
 where
   posn = if isPositionCat cf own then " (_,i)" else " i"

-- copy and paste from CFtoTemplate

rules :: CF -> String
rules cf = unlines $ mutualDefs $
  map (\(s,xs) -> case_fun s (map toArgs xs)) $ cf2data cf -- ++ ifList cf s
 where
   toArgs (cons,args) = ((cons, names (map (checkRes . var) args) (0 :: Int)),
                         ruleOf cons)
   names [] _ = []
   names (x:xs) n
     | x `elem` xs = (x ++ show n) : names xs (n+1)
     | otherwise = x             : names xs n
   var (ListCat c)      = var c ++ "s"
   var (Cat "Ident")    = "id"
   var (Cat "Integer")  = "n"
   var (Cat "String")   = "str"
   var (Cat "Char")     = "c"
   var (Cat "Double")   = "d"
   var cat              = map toLower (show cat)
   checkRes s
        | s `elem` reservedFSharp = s ++ "'"
        | otherwise              = s
   ruleOf s = fromJust $ lookupRule s (cfgRules cf)

case_fun cat xs = unlines [
  showsFun cat +++ "(e:" ++ fixType cat ++ ") : Showable =",
  "    match e with",
  unlines $ map (indent 1) $ insertBar $ map f xs
  ]
  where
   f ((c,xx),r) =
    c +++ mkTuple xx +++ "->" +++
    "s2s" +++ show c +++
    case mkRhs xx (snd r) of
      [] -> []
      str -> ">> c2s ' ' >> " ++ str

mkRhs args its =
  case unwords (intersperse " >> s2s \", \" >> " (mk args its)) of
    [] -> ""
    str -> "c2s '(' >> " ++ str ++ " >> c2s ')'"
 where
  mk args (Left InternalCat : items)      = mk args items
  mk (arg:args) (Left c : items)  = (showsFun c +++ arg)        : mk args items
  mk args       (Right _ : items) = mk args items
  mk _ _ = []

showsFun :: Cat -> String
showsFun c = case c of
    ListCat t -> "showList" +++ showsFun t
    _ -> "show" ++ (fixType $ normCat c)
