cabal build
rm -r ./testDir
./dist/build/bnfc/bnfc -o testDir --fsharp ../examples/Javalette/JavaletteLight.cf