module BigIntBenchmarks exposing (main)

-- Individual Benchmarks

import AbsBenchmark
import AddBenchmark
import Benchmark exposing (describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import CompareBenchmark
import DivmodBenchmark
import FromIntBenchmark
import FromStringBenchmark
import MulBenchmark
import ToStringBenchmark


main : BenchmarkProgram
main =
    program <|
        describe "BigInt"
            [ AbsBenchmark.benchmark
            , AddBenchmark.benchmark
            , CompareBenchmark.benchmark
            , DivmodBenchmark.benchmark
            , FromIntBenchmark.benchmark
            , FromStringBenchmark.benchmark
            , MulBenchmark.benchmark
            , ToStringBenchmark.benchmark
            ]
