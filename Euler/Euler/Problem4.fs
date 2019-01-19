module Problem4

open Xunit

let isPalindrome n = 
    n.ToString().ToCharArray() = (n.ToString().ToCharArray() |> Array.rev)

[<Fact>]
let ``is palindrome tests if number is the same if digits are reversed`` () =
    Assert.False(isPalindrome 100)
    Assert.True(isPalindrome 101)
    
let productsOfTwoNumbersOfDigits n = 
    [0..(pown 10 n)-1] |> List.collect (fun x -> List.init(pown 10 n) (fun y -> x * y))
    
let firstPalindrome numbers = numbers |> List.filter isPalindrome |> List.head

[<Fact>]
let ``find all products of two single digit numbers`` () =
    let result = productsOfTwoNumbersOfDigits 1
    Assert.Contains(1, result)
    Assert.Contains(9, result)
    Assert.Equal(81, result |> List.max)
    
[<Fact>]
let ``find all products of two two digit numbers`` () =
    let result = productsOfTwoNumbersOfDigits 2
    Assert.Equal(9801, result |> List.max)
    
[<Fact>]
let ``find all products of two three digit numbers`` () =
    let result = productsOfTwoNumbersOfDigits 3
    Assert.Equal(998001, result |> List.max)
    
[<Fact>]
let ``find first palindrome`` () =
    Assert.Equal(121, firstPalindrome [100; 121; 131])
    
[<Fact>]
let ``find largest palindrome of product of two two digit numbers`` () =
    let result = productsOfTwoNumbersOfDigits(2) |> List.sortDescending |> firstPalindrome
    Assert.Equal(9009, result)
    
[<Fact>]
let ``find largest palindrome of product of two three digit numbers`` () =
    let result = productsOfTwoNumbersOfDigits(3) |> List.sortDescending |> firstPalindrome
    Assert.Equal(906609, result)
    
