{-
    BNF Converter: fslex Generator
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

module BNFC.Backend.FSharp.CFtoFsLex (cf2ocamllex) where

import Control.Arrow ((&&&))
import Data.List
import Data.Char
import Text.PrettyPrint hiding (render)
import qualified Text.PrettyPrint as PP

import BNFC.CF
import AbsBNF
import BNFC.Backend.FSharp.CFtoFsYacc (terminal)
import BNFC.Utils ((+++), cstring, cchar)

cf2ocamllex :: String -> String -> CF -> String
cf2ocamllex _ parserMod cf =
  unlines $ intercalate [""] [
    header parserMod cf,
    definitions cf,
    [PP.render (rules cf)]
   ]

header :: String -> CF -> [String]
header parserMod cf = [
  "(* This ocamllex file was machine-generated by the BNF converter *)",
  "{",
  "open " ++ parserMod,
  "open Lexing",
  "",
  hashtables cf,
  "",
  "let unescapeInitTail (s:string) : string =",
  "  let rec unesc s = match s with",
  "      '\\\\'::c::cs when List.mem c ['\\\"'; '\\\\'; '\\\''] -> c :: unesc cs",
  "    | '\\\\'::'n'::cs  -> '\\n' :: unesc cs",
  "    | '\\\\'::'t'::cs  -> '\\t' :: unesc cs",
  "    | '\\\"'::[]    -> []",
  "    | c::cs      -> c :: unesc cs",
  "    | _         -> []",
  "  (* explode/implode from caml FAQ *)",
  "  in let explode (s : string) : char list =",
  "      let rec exp i l =",
  "        if i < 0 then l else exp (i - 1) (s.[i] :: l) in",
  "      exp (String.length s - 1) []",
  "  in let implode (l : char list) : string =",
  "      let res = Buffer.create (List.length l) in",
  "      List.iter (Buffer.add_char res) l;",
  "      Buffer.contents res",
  "  in implode (unesc (List.tl (explode s)))",
  "",
  "let incr_lineno (lexbuf:Lexing.lexbuf) : unit =",
  "    let pos = lexbuf.lex_curr_p in",
  "        lexbuf.lex_curr_p <- { pos with",
  "            pos_lnum = pos.pos_lnum + 1;",
  "            pos_bol = pos.pos_cnum;",
  "        }",
  "}"
  ]

-- | set up hashtables for reserved symbols and words
hashtables :: CF -> String
hashtables cf = ht "symbol_table" (cfgSymbols cf )  ++ "\n" ++
                ht "resword_table" (reservedWords cf)
    where ht _ syms | null syms = ""
          ht table syms = unlines [
                "let" +++ table +++ "= Hashtbl.create " ++ show (length syms),
                "let _ = List.iter (fun (kwd, tok) -> Hashtbl.add" +++ table
                         +++ "kwd tok)",
                "                  [" ++ concat (intersperse ";" keyvals) ++ "]"
            ]
            where keyvals = map (\(x,y) -> "(" ++ x ++ ", " ++ y ++ ")")
                          (zip (map show syms) (map (terminal cf) syms))



definitions :: CF -> [String]
definitions cf = concat [
        cMacros,
        rMacros cf,
        uMacros cf
    ]


cMacros :: [String]
cMacros = [
  "let l = ['a'-'z' 'A'-'Z' '\\192' - '\\255'] # ['\\215' '\\247']    (*  isolatin1 letter FIXME *)",
  "let c = ['A'-'Z' '\\192'-'\\221'] # ['\\215']    (*  capital isolatin1 letter FIXME *)",
  "let s = ['a'-'z' '\\222'-'\\255'] # ['\\247']    (*  small isolatin1 letter FIXME *)",
  "let d = ['0'-'9']                (*  digit *)",
  "let i = l | d | ['_' '\\'']          (*  identifier character *)",
  "let u = ['\\000'-'\\255']           (* universal: any character *)"
  ]

rMacros :: CF -> [String]
rMacros cf =
  let symbs = cfgSymbols cf
  in
  (if null symbs then [] else [
   "let rsyms =    (* reserved words consisting of special symbols *)",
   "            " ++ unwords (intersperse "|" (map mkEsc symbs))
   ])
 where
  mkEsc s = "\"" ++ concat (map f s) ++ "\""
  f x = if x `elem` ['"','\\'] then  "\\" ++ [x] else [x]

-- user macros, derived from the user-defined tokens
uMacros :: CF -> [String]
uMacros cf = ["let " ++ name ++ " = " ++ rep | (name, rep, _) <- userTokens cf]

-- returns the tuple of (reg_name, reg_representation, token_name)
userTokens :: CF -> [(String, String, String)]
userTokens cf =
  let regName = map toLower . show in
  [(regName name, printRegOCaml reg, show name) | (name, reg) <- tokenPragmas cf]
      


-- | Make OCamlLex rule
-- >>> mkRule "token" [("REGEX1","ACTION1"),("REGEX2","ACTION2"),("...","...")]
-- rule token =
--   parse REGEX1 {ACTION1}
--       | REGEX2 {ACTION2}
--       | ... {...}
--
-- If no regex are given, we dont create a lexer rule:
-- >>> mkRule "empty" []
-- <BLANKLINE>
mkRule :: Doc -> [(Doc,Doc)] -> Doc
mkRule _ [] = empty
mkRule entrypoint (r1:rn) = vcat
    [ "rule" <+> entrypoint <+> "="
    , nest 2 $ hang "parse" 4 $ vcat
        (nest 2 (mkOne r1):map (("|" <+>) . mkOne) rn) ]
  where
    mkOne (regex, action) = regex <+> braces action

-- | Create regex for single line comments
-- >>> mkRegexSingleLineComment "--"
-- "--" (_ # '\n')*
-- >>> mkRegexSingleLineComment "\""
-- "\"" (_ # '\n')*
mkRegexSingleLineComment :: String -> Doc
mkRegexSingleLineComment s = cstring s <+> "(_ # '\\n')*"

-- | Create regex for multiline comments
-- >>> mkRegexMultilineComment "<!--" "-->"
-- "<!--" ((u # ['-']) | '-' (u # ['-']) | "--" (u # ['>']))* '-'* "-->"
--
-- >>> mkRegexMultilineComment "\"'" "'\""
-- "\"'" ((u # ['\'']) | '\'' (u # ['"']))* '\''* "'\""
mkRegexMultilineComment :: String -> String -> Doc
mkRegexMultilineComment b e =
  lit b
  <+> parens ( hsep $ intersperse "|" subregexs ) <> "*"
  <+> lit [head e] <> "*"
  <+> lit e
  where
    lit :: String -> Doc
    lit "" = empty
    lit [c] = cchar c
    lit s = cstring s
    prefix = map (init &&& last) (drop 1 (inits e))
    subregexs = [ lit ss <+> parens ("u #" <+> brackets (lit [s])) | (ss,s) <- prefix]

-- | Uses the function from above to make a lexer rule from the CF grammar
rules :: CF -> Doc
rules cf = mkRule "token" $
    -- comments
    [ (mkRegexSingleLineComment s, "token lexbuf") | s <- singleLineC ]
    ++
    [ (mkRegexMultilineComment b e, "token lexbuf") | (b,e) <- multilineC]
    ++
    -- reserved keywords
    [ ( "rsyms"
      , "let id = lexeme lexbuf in try Hashtbl.find symbol_table id with Not_found -> failwith (\"internal lexer error: reserved symbol \" ^ id ^ \" not found in hashtable\")" )
      | not (null (cfgSymbols cf))]
    ++
    -- user tokens
    [ (text n , tokenAction (text t)) | (n,_,t) <- userTokens cf]
    ++
    -- predefined tokens
    [ ( "l i*", tokenAction "Ident" ) ]
    ++
    -- integers
    [ ( "d+", "let i = lexeme lexbuf in TOK_Integer (int_of_string i)" )
    -- doubles
    , ( "d+ '.' d+ ('e' ('-')? d+)?"
      , "let f = lexeme lexbuf in TOK_Double (float_of_string f)" )
    -- strings
    , ( "'\\\"' ((u # ['\\\"' '\\\\' '\\n']) | ('\\\\' ('\\\"' | '\\\\' | '\\\'' | 'n' | 't')))* '\\\"'"
      , "let s = lexeme lexbuf in TOK_String (unescapeInitTail s)" )
    -- chars
    , ( "'\\'' ((u # ['\\\'' '\\\\']) | ('\\\\' ('\\\\' | '\\\'' | 'n' | 't'))) '\\\''"
      , "let s = lexeme lexbuf in TOK_Char s.[1]")
    -- spaces
    , ( "[' ' '\\t']", "token lexbuf")
    -- new lines
    , ( "'\\n'", "incr_lineno lexbuf; token lexbuf" )
    -- end of file
    , ( "eof", "TOK_EOF" )
    ]
  where
    (multilineC, singleLineC) = comments cf
    tokenAction t = case reservedWords cf of
        [] -> "let l = lexeme lexbuf in TOK_" <> t <>" l"
        _  -> "let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_" <> t <+> "l"

-------------------------------------------------------------------
-- Modified from the inlined version of @RegToAlex@.
-------------------------------------------------------------------

-- modified from pretty-printer generated by the BNF converter

-- the top-level printing method
printRegOCaml :: Reg -> String
printRegOCaml = render . prt 0

-- you may want to change render and parenth

render :: [String] -> String
render = rend 0
    where rend :: Int -> [String] -> String
          rend i ss = case ss of
                        "["      :ts -> cons "["  $ rend i ts
                        "("      :ts -> cons "("  $ rend i ts
                        t  : "," :ts -> cons t    $ space "," $ rend i ts
                        t  : ")" :ts -> cons t    $ cons ")"  $ rend i ts
                        t  : "]" :ts -> cons t    $ cons "]"  $ rend i ts
                        t        :ts -> space t   $ rend i ts
                        _            -> ""

          cons s t  = s ++ t
          space t s = if null s then t else t ++ " " ++ s

parenth :: [String] -> [String]
parenth ss = ["("] ++ ss ++ [")"]

-- the printer class does the job
class Print a where
  prt :: Int -> a -> [String]
  prtList :: [a] -> [String]
  prtList = concat . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ c = [show c]   -- if isAlphaNum c then [[c]] else ['\\':[c]]
  prtList s = [show s] -- map (concat . prt 0) s

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if j<i then parenth else id

instance Print Ident where
  prt _ (Ident i) = [i]

instance Print Reg where
  prt i e = case e of
   RSeq reg0 reg   -> prPrec i 2 (concat [prt 2 reg0 , prt 3 reg])
   RAlt reg0 reg   -> prPrec i 1 (concat [prt 1 reg0 , ["|"] , prt 2 reg])
   RMinus reg0 reg -> prPrec i 1 (concat [prt 2 reg0 , ["#"] , prt 2 reg])
   RStar reg       -> prPrec i 3 (concat [prt 3 reg , ["*"]])
   RPlus reg       -> prPrec i 3 (concat [prt 3 reg , ["+"]])
   ROpt reg        -> prPrec i 3 (concat [prt 3 reg , ["?"]])
   REps            -> prPrec i 3 (["\"\""])  -- special construct for eps in ocamllex?
   RChar c         -> prPrec i 3 (concat [prt 0 c])
   RAlts str       -> prPrec i 3 (concat [["["], [concatMap show str], ["]"]])
   RSeqs str       -> prPrec i 2 (concat (map (prt 0) str))
   RDigit          -> prPrec i 3 (concat [["d"]])
   RLetter         -> prPrec i 3 (concat [["l"]])
   RUpper          -> prPrec i 3 (concat [["c"]])
   RLower          -> prPrec i 3 (concat [["s"]])
   RAny            -> prPrec i 3 (concat [["u"]])
