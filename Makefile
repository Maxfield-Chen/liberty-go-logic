build:
	nix-shell --run "cabal build"

format:
	nix-shell --run "find src tst -iname \"*.hs\" | xargs stylish-haskell -i"
