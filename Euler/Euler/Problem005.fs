module Problem5

open Xunit

let isDivisibleBy n by = n % by = 0

let isDivisibleByMany n byList = 
    byList 
    |> List.forall(fun by -> isDivisibleBy n by)

[<Fact>]
let ``is divisible by single number`` () =
    Assert.False(isDivisibleBy 9 5)
    Assert.True(isDivisibleBy 9 3)
    
[<Fact>]
let ``is divisible by multiple numbers`` () =
    Assert.True(isDivisibleByMany 15 [3; 5])
    Assert.False(isDivisibleByMany 16 [3; 5])

let smallestDivisibleBy byList =
    let max = byList |> List.max
    let rec calc byList i =
        match i with 
        | i when isDivisibleByMany i byList -> i
        | _ -> calc byList (i+max)
    calc byList max

[<Fact>]
let ``smallest number divisible by a list of numbers`` () =
    Assert.Equal(15, smallestDivisibleBy [3; 5])
    Assert.Equal(2520, smallestDivisibleBy [1..10])
    Assert.Equal(232792560, smallestDivisibleBy [1..20])
    
