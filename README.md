# [haskell-playground][]
 
Playground for Haskell experiments.
Do not use unless you know what you are doing. See LICENSE.

This project consists of seperate subprojects that can either
be built individually or all at once, with the Stack commands
you know and love.

``` sh
# Build, test, generate docs and open them in a browser
stack build --test --haddock --open

# Run and report benchmarks
stack bench --benchmark-arguments '--output=$benchmark.html'
```

## A note about stack ghci

If you just run `stack ghci` in the main directory, you will likely encounter
problems.  Stack tries to load all the subpackages, but doesn't set the correct
language options for the packages. Instead, always specify the package or a
specific module that you want to run ghci in:

``` sh
# Run ghci in the oneoffs subpackage
stack ghci oneoffs

# Run ghci on a specific module in oneoffs
stack ghci oneoffs/src/Ideas/Old/ExtensibleADT.hs
```

## The Makefile

This project includes a Makefile, but just for convenience of developers.
This Makefile is just a thin wrapper around the 'stack' command.

[haskell-playground]: https://github.com/xicesky/haskell-playground
