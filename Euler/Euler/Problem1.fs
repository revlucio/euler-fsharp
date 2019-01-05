module Tests

open System
open Xunit

let sum x = 
    x 
    |> List.reduce ( fun acc x -> acc + x )

let filterMultipleOf3Or5 x =
    x
    |> List.filter ( fun x -> (x % 3) = 0 || (x % 5) = 0 )


[<Fact>]
let ``Multiples of 3 and 5`` () =
    let result = [1..999] |> filterMultipleOf3Or5 |> sum
    Assert.Equal(23, result)
    
