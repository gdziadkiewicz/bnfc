{-# LANGUAGE NoImplicitPrelude #-}

{-
    BNF Converter: Abstract syntax Generator
    Copyright (C) 2004  Author:  Markus Forberg

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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

module BNFC.Backend.Haskell.CFtoAbstract (cf2Abstract) where

import Prelude'

import BNFC.CF
import BNFC.Options               ( TokenText(..) )
import BNFC.PrettyPrint
import BNFC.Utils                 ( when )

import BNFC.Backend.Haskell.Utils ( catToType, catvars, tokenTextImport, tokenTextType )

-- | Create a Haskell module containing data type definitions for the abstract syntax.

cf2Abstract
  :: TokenText -- ^ Use @ByteString@ or @Text@ instead of @String@?
  -> Bool      -- ^ Derive @Data@, Generic@, @Typeable@?
  -> Bool      -- ^ Make the tree a functor?
  -> String    -- ^ Module name.
  -> CF        -- ^ Grammar.
  -> Doc
cf2Abstract tokenText generic functor name cf = vsep . concat $
    [ [ vcat
        [ "-- Haskell data types for the abstract syntax."
        , "-- Generated by the BNF converter."
        ]
      ]
    , [ vcat . concat $
        [ [ "{-# LANGUAGE DeriveDataTypeable #-}" | gen ]
        , [ "{-# LANGUAGE DeriveGeneric #-}"      | gen ]
        , [ "{-# LANGUAGE GeneralizedNewtypeDeriving #-}" | not $ null $ specialCats cf  ] -- for IsString
        ]
      ]
    , [ hsep [ "module", text name, "where" ] ]
    , [ vcat
        [ text $ "import Prelude (Char, Double, Integer, String" ++ functorImportsUnqual ++ ")"
        , text $ "import qualified Prelude as C (Eq, Ord, Show, Read" ++ functorImportsQual ++ ")"
        , "import qualified Data.String"  -- for IsString
        ]
      ]
    , [ vcat . concat $
        [ map text $ tokenTextImport tokenText
        , [ "import qualified Data.Data    as C (Data, Typeable)" | gen ]
        , [ "import qualified GHC.Generics as C (Generic)"        | gen ]
        ]
      ]
    , map (\ c -> prSpecialData tokenText (isPositionCat cf c) derivingClassesTokenType c) $ specialCats cf
    , concatMap (prData functorName derivingClasses) datas
    , [ "" ] -- ensure final newline
    ]
  where
    datas = cf2data cf
    gen   = generic && not (null datas)
    derivingClasses = map ("C." ++) $ concat
      [ [ "Eq", "Ord", "Show", "Read" ]
      , when generic [ "Data", "Typeable", "Generic" ]
      ]
    derivingClassesTokenType = concat
      [ derivingClasses
      , [ "Data.String.IsString" ]
      ]
    functorImportsUnqual
      | functor   = ", map, fmap"
      | otherwise = ""
    functorImportsQual
      | functor   = ", Functor"
      | otherwise = ""
    functorName
      | functor   = "C.Functor"
      | otherwise = ""

type FunctorName = String

-- |
--
-- >>> vsep $ prData "" ["Eq", "Ord", "Show", "Read"] (Cat "C", [("C1", [Cat "C"]), ("CIdent", [Cat "Ident"])])
-- data C = C1 C | CIdent Ident
--   deriving (Eq, Ord, Show, Read)
--
-- Note that the layout adapts if it does not fit in one line:
-- >>> vsep $ prData "" ["Show"] (Cat "C", [("CAbracadabra",[]),("CEbrecedebre",[]),("CIbricidibri",[]),("CObrocodobro",[]),("CUbrucudubru",[])])
-- data C
--     = CAbracadabra
--     | CEbrecedebre
--     | CIbricidibri
--     | CObrocodobro
--     | CUbrucudubru
--   deriving (Show)
--
-- If the first argument is not null, generate a functor:
-- >>> vsep $ prData "Functor" ["Show"] (Cat "C", [("C1", [Cat "C"]), ("CIdent", [TokenCat "Ident"])])
-- data C a = C1 a (C a) | CIdent a Ident
--   deriving (Show)
-- <BLANKLINE>
-- instance Functor C where
--     fmap f x = case x of
--         C1 a c -> C1 (f a) (fmap f c)
--         CIdent a ident -> CIdent (f a) ident
--
-- The case for lists:
-- >>> vsep $ prData "Functor" ["Show"] (Cat "ExpList", [("Exps", [ListCat (Cat "Exp")])])
-- data ExpList a = Exps a [Exp a]
--   deriving (Show)
-- <BLANKLINE>
-- instance Functor ExpList where
--     fmap f x = case x of
--         Exps a exps -> Exps (f a) (map (fmap f) exps)
--
prData :: FunctorName -> [String] -> Data -> [Doc]
prData functorName derivingClasses (cat,rules) = concat
    [ [ hang ("data" <+> dataType) 4 (constructors rules)
        $+$ nest 2 (deriving_ derivingClasses)
      ]
    , [ genFunctorInstance functorName (cat, rules) | functor ]
    ]
  where
    functor            = not $ null functorName
    prRule (fun, cats) = hsep $ concat [ [text fun], ["a" | functor], map prArg cats ]
    dataType           = hsep $ concat [ [text (show cat)], ["a" | functor] ]
    prArg              = catToType id $ if functor then "a" else empty
    constructors []    = empty
    constructors (h:t) = sep $ ["=" <+> prRule h] ++ map (("|" <+>) . prRule) t

-- | Generate a functor instance declaration:
--
-- >>> genFunctorInstance "Functor" (Cat "C", [("C1", [Cat "C", Cat "C"]), ("CIdent", [TokenCat "Ident"])])
-- instance Functor C where
--     fmap f x = case x of
--         C1 a c1 c2 -> C1 (f a) (fmap f c1) (fmap f c2)
--         CIdent a ident -> CIdent (f a) ident
--
-- >>> genFunctorInstance "Functor" (Cat "SomeLists", [("Ints", [ListCat (TokenCat "Integer")]), ("Exps", [ListCat (Cat "Exp")])])
-- instance Functor SomeLists where
--     fmap f x = case x of
--         Ints a integers -> Ints (f a) integers
--         Exps a exps -> Exps (f a) (map (fmap f) exps)
--
genFunctorInstance :: FunctorName -> Data -> Doc
genFunctorInstance functorName (cat, cons) =
    "instance" <+> text functorName <+> text (show cat) <+> "where"
    $+$ nest 4 ("fmap f x = case x of" $+$ nest 4 (vcat (map mkCase cons)))
  where
    mkCase (f, args) = hsep . concat $
        [ [ text f, "a" ]
        , vars
        , [ "->", text f, "(f a)" ]
        , zipWith recurse vars args
        ]
      where vars = catvars args
    -- We recursively call fmap on non-terminals only if they are not token categories.
    recurse var = \case
      TokenCat{}         -> var
      ListCat TokenCat{} -> var
      ListCat{}          -> parens ("map (fmap f)" <+> var)
      _                  -> parens ("fmap f"       <+> var)


-- | Generate a newtype declaration for Ident types
--
-- >>> prSpecialData StringToken False ["Show","Data.String.IsString"] catIdent
-- newtype Ident = Ident String
--   deriving (Show, Data.String.IsString)
--
-- >>> prSpecialData StringToken True ["Show"] catIdent
-- newtype Ident = Ident ((Int,Int),String)
--   deriving (Show)
--
-- >>> prSpecialData TextToken False ["Show"] catIdent
-- newtype Ident = Ident Data.Text.Text
--   deriving (Show)
--
-- >>> prSpecialData ByteStringToken False ["Show"] catIdent
-- newtype Ident = Ident BS.ByteString
--   deriving (Show)
--
-- >>> prSpecialData ByteStringToken True ["Show"] catIdent
-- newtype Ident = Ident ((Int,Int),BS.ByteString)
--   deriving (Show)
--
prSpecialData
  :: TokenText  -- ^ Format of token content.
  -> Bool       -- ^ If @True@, store the token position.
  -> [String]   -- ^ Derived classes.
  -> TokenCat   -- ^ Token category name.
  -> Doc
prSpecialData tokenText position classes cat = vcat
    [ hsep [ "newtype", text cat, "=", text cat, contentSpec ]
    , nest 2 $ deriving_ classes
    ]
  where
    contentSpec | position    = parens ( "(Int,Int)," <> stringType)
                | otherwise   = stringType
    stringType = text $ tokenTextType tokenText

-- | Generate 'deriving' clause
--
-- >>> deriving_ ["Show", "Read"]
-- deriving (Show, Read)
--
deriving_ :: [String] -> Doc
deriving_ cls = "deriving" <+> parens (hsep $ punctuate "," $ map text cls)
