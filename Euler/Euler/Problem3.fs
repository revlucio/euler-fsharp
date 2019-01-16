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
    
    
// take two!

let rec calcFactors(n : int64, i : int64, max: int64, results: List<int64>) =
    match i with 
    | i when (i >= max) -> results
    | i when (n % i = 0L) -> calcFactors(n, (i+1L), (n / i), i :: (n / i) :: results)
    | i when (n % i <> 0L) -> calcFactors(n, (i+1L), (n / i), results)

let factors(n : int64) = 
    calcFactors(n, 1L, n, [])
    |> List.distinct
    |> List.sort

[<Fact>]
let ``find factors of`` () =
    let result = factors 600851475143L
    Assert.Equal<List<int64>>([1L; 2L], factors 2L)    
    Assert.Equal<List<int64>>([1L; 3L], factors 3L)    
    Assert.Equal<List<int64>>([1L; 2L; 4L], factors 4L)
    Assert.Equal<List<int64>>([1L; 2L; 3L; 4L; 6L; 12L], factors 12L)
    
let rec calcIsPrime(n : int64, i : int64) =
    match n with
    | 1L -> false
    | n when (i = n) -> true
    | n when (n % i = 0L) -> false
    | _ -> calcIsPrime(n, i+1L)
    
let isPrimeFast(n : int64) =
    calcIsPrime(n, 2L)
    
[<Fact>]
let ``is Prime fast`` () =
    Assert.Equal(false, isPrimeFast(1L))
    Assert.Equal(true, isPrimeFast(2L))
    Assert.Equal(false, isPrimeFast(4L))
    Assert.Equal(true, isPrimeFast(5L))
    
[<Fact>]
let ``perf test of isPrimeFast`` () =
    Assert.False(isPrimeFast 6008555L)
    