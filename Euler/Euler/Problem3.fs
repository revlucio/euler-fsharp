module Problem3

open System
open Xunit

let isPrime(x : int64) =
    if (x = 1L) 
    then false
    else
        ([2L..(x-1L)]
        |> List.exists(fun y -> (x % y) = 0L)) = false
        
let primesUnder (x : int64) =
    [1L..(x-1L)]
    |> List.filter(fun y -> isPrime y)

let primeFactors(x : int64) =
    primesUnder x
    |> List.filter(fun y -> (x % y) = 0L)
    
let largestPrimeFactor(x : int64) =
    primeFactors x
    |> List.sortDescending
    |> List.pick(fun y -> Some(y))

[<Fact>]
let ``is Prime`` () =
    Assert.Equal(false, isPrime(1L))
    Assert.Equal(true, isPrime(2L))
    Assert.Equal(false, isPrime(4L))
    Assert.Equal(true, isPrime(5L))
    Assert.Equal(true, isPrime(1009L))
    Assert.Equal(false, isPrime(1000000L))
    
[<Fact>]
let ``primes under`` () =
    Assert.Equal(1570, primesUnder(13195L).Length)

[<Fact>]
let ``largest prime factor`` () =
    Assert.Equal(29L, largestPrimeFactor 131950L)
    