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

[haskell-playground]: https://github.com/xicesky/haskell-playground
