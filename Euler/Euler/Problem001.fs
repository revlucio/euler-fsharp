module Tests

open System
open Xunit

let isDivisibleBy n by = (n % by) = 0

let filterMultipleOf3Or5 x =
    x
    |> List.filter ( fun x -> isDivisibleBy x 3 || isDivisibleBy x 5)


[<Fact>]
let ``Multiples of 3 and 5`` () =
    let result = [1..999] |> filterMultipleOf3Or5 |> List.sum
    Assert.Equal(233168, result)
    
