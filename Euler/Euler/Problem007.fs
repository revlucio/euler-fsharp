module Problem007

open Xunit
open Primes

let nthPrime n = 
    let rec findPrime(n : int64, i : int64, primeIndex : int64) =
        match n with 
        | n when primeIndex = n & isPrime i -> i
        | n when i = 2L -> findPrime(n, (i+1L), (primeIndex+1L))
        | n when isPrime i -> findPrime(n, (i+2L), (primeIndex+1L))
        | _ -> findPrime(n, (i+2L), primeIndex)
    findPrime(n, 2L, 1L)

[<Fact>]
let ``get nth prime`` () =
    Assert.Equal(2L, nthPrime 1L)
    Assert.Equal(3L, nthPrime 2L)
    Assert.Equal(13L, nthPrime 6L)
    Assert.Equal(104_743L, nthPrime 10_001L)
