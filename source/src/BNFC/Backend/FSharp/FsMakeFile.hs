module BNFC.Backend.FSharp.FsMakeFile(makefile) where

import BNFC.PrettyPrint
import BNFC.Backend.Common.Makefile


-- | Create .net xplat tooling based make file
--
-- >>> makefile "src" "src/test.fsproj"
-- run-clean: clean restore build run
-- <BLANKLINE>
-- all: clean restore build
-- <BLANKLINE>
-- clean:
-- 	dotnet clean src
-- <BLANKLINE>
-- restore:
-- 	dotnet restore src
-- <BLANKLINE>
-- build:
-- 	dotnet build src
-- <BLANKLINE>
-- run:
-- 	dotnet run -p src/test.fsproj
-- <BLANKLINE>
makefile :: String -> String -> Doc
makefile dir projFilePath = vcat
    [ 
      mkRule "run-clean" ["clean", "restore", "build", "run"] []
    , mkRule "all"       ["clean", "restore", "build"]        []

    , mkRule "clean"   []  ["dotnet clean " ++ dir]
    , mkRule "restore" []  ["dotnet restore " ++ dir]
    , mkRule "build"   []  ["dotnet build " ++ dir]
    , mkRule "run"     []  ["dotnet run -p " ++ projFilePath]
    ]