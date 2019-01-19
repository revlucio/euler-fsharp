module Problem6

open Xunit

let square n = n * n

let sumTheSquaresOf numbers = 
    numbers |> List.map square |> List.sum

let squareTheSumOf numbers = 
    numbers |> List.sum |> square

[<Fact>]
let ``sum the squares of a list of numbers`` () =
    Assert.Equal(14, sumTheSquaresOf [1; 2; 3])
    Assert.Equal(385, sumTheSquaresOf [1..10])
    
[<Fact>]
let ``square the sum of a list of numbers`` () =
    Assert.Equal(36, squareTheSumOf [1; 2; 3;])
    Assert.Equal(3025, squareTheSumOf [1..10])

[<Fact>]
let ``the difference`` () =
    Assert.Equal(2640, squareTheSumOf [1..10] - sumTheSquaresOf [1..10])
    Assert.Equal(25164150, squareTheSumOf [1..100] - sumTheSquaresOf [1..100])