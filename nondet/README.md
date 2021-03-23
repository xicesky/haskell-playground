# nondet

Benchmarks & experiments with non-determinism, non-det. search, logic
programming and generic problem solving.

## Benchmark comparsion

This project includes a benchmark using criterion for various implementations
of non-deterministic behaviour.

To run the benchmark, use:
``` sh
# Run benchmarks and open report

stack bench nondet --benchmark-arguments '--output=$benchmark.html' \
&& open nondet/nondet-benchmarks.html
```
(The make target `nondet` should do exactly that for you.)
