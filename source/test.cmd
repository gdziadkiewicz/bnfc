REM cabal sandbox init
cabal build || exit /B 1
REM RD /S /Q .\testDir
.\dist\build\bnfc\bnfc -o testDir --fsharp ../examples/Javalette/JavaletteLight.cf