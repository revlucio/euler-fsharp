module Problem010

open Xunit
open Primes

let sumOfAllPrimesUnder n = 
    let rec loop sum i =
        match i with 
        | i when (i >= n) -> sum
        | i when (isPrime i) -> loop (sum+i) (i+2L)
        | _ -> loop sum (i+2L)
        
    (loop 0L 1L) + 2L

[<Fact>]
let ``sum of all primes under 10`` () =
    Assert.Equal(17L, (sumOfAllPrimesUnder 10L))
    
[<Fact>]
let ``sum of all primes under 100 000`` () =
    Assert.Equal(454396537L, (sumOfAllPrimesUnder 100_000L)) //35.3 seconds to run