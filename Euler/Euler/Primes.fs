module Primes

let isPrime(n : int64) =
    let rec calc(n : int64, i : int64) =
        match n with
        | 1L -> false
        | n when (i = n) -> true
        | n when (n % i = 0L) -> false
        | _ -> calc(n, i+1L)
    calc(n, 2L)