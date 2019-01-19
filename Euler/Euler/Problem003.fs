module Problem3

open System
open Xunit
open Primes

let factors(n : int64) =
    let rec calc(n : int64, i : int64, max: int64, results: List<int64>) =
        match i with 
        | i when (i >= max) -> results
        | i when (n % i = 0L) -> calc(n, (i+1L), (n / i), i :: (n / i) :: results)
        | i when (n % i <> 0L) -> calc(n, (i+1L), (n / i), results) 
    calc(n, 1L, n, [])
    |> List.distinct
    |> List.sort
    
let largestPrimeFactor(x : int64) =
    (factors x |> List.filter isPrime |> List.sortDescending).Head 

[<Fact>]
let ``is Prime`` () =
    Assert.False(isPrime(1L))
    Assert.True(isPrime(2L))
    Assert.False(isPrime(4L))
    Assert.True(isPrime(5L))
    Assert.True(isPrime(1009L))

[<Fact>]
let ``find factors of`` () =
    let result = factors 600851475143L
    Assert.Equal<List<int64>>([1L; 2L], factors 2L)    
    Assert.Equal<List<int64>>([1L; 3L], factors 3L)    
    Assert.Equal<List<int64>>([1L; 2L; 4L], factors 4L)
    Assert.Equal<List<int64>>([1L; 2L; 3L; 4L; 6L; 12L], factors 12L)
    
[<Fact>]
let ``largest prime factor`` () =
    Assert.Equal(29L, largestPrimeFactor 13195L)
    Assert.Equal(6857L, largestPrimeFactor 600851475143L)
    