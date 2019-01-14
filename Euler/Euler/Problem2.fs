module Problem2

open System
open Xunit

let sumEvens x = 
    x 
    |> List.filter (fun x -> (x % 2) = 0)
    |> List.reduce ( fun acc x -> acc + x )

let calc(arr : int list) =
    if (arr.Length < 2) 
    then [1 ; 1]
    else arr @ [arr.[arr.Length-2] + arr.[arr.Length-1]]
    
let rec getFibUpTo(arr : int list, max) =
    let fib = calc arr
    if (fib.[fib.Length-1] > max)
    then fib.GetSlice(Some 0, Some (fib.Length-2))
    else getFibUpTo(fib, max)

[<Fact>]
let ``Even Fibonacci numbers`` () =
    let result = getFibUpTo([], 5)
    Assert.Equal(5, result.Length)
    Assert.Equal(1, result.[0])
    Assert.Equal(1, result.[1])
    Assert.Equal(2, result.[2])
    Assert.Equal(3, result.[3])
    Assert.Equal(5, result.[4])
    
let rec fib n =
    match n with
    | n when n < 1 -> 0
    | 1 -> 1
    | _ -> fib (n - 1) + fib (n - 2) 
    
[<Fact>]
let ``fibonacci numbers`` () =
    Assert.Equal(1, fib 1)
    Assert.Equal(1, fib 2)
    Assert.Equal(2, fib 3)
    Assert.Equal(3, fib 4)
    Assert.Equal(5, fib 5)
    
[<Fact>]
let ``fibonacci numbers edge cases`` () =
    Assert.Equal(0, fib 0)
    Assert.Equal(0, fib -1)
    
[<Fact>]
let ``sum the evens`` () =
    Assert.Equal(20, sumEvens [1..9]) 
    
[<Fact>]
let ``sum the even fib`` () =  
    Assert.Equal(0, sumEvens (getFibUpTo([], 4_000_000)))  
    
    