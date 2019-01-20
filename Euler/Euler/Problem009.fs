module Problem009

open Xunit

let isPythagTriplet a b c =
    a < b 
    && b < c
    && (a * a) + (b * b) = (c * c)

[<Fact>]
let ``is pythagorean triplet`` () = 
    Assert.False(isPythagTriplet 1 2 3)
    Assert.True(isPythagTriplet 3 4 5)
    
let tripletThatSumsTo n =
    let rec loop a b c =
        match a with 
        | a when ((a + b + c) = n && isPythagTriplet a b c) -> Some [a; b; c]
        | _ when c < n -> loop a b (c+1)
        | _ when b < n -> loop a (b+1) 0
        | _ when a < n -> loop (a+1) 0 0
        | _ -> None
        
    loop 0 0 0
    
[<Fact>]
let ``get triplet that sums to 12`` () =
    Assert.Equal(Some [3; 4; 5], tripletThatSumsTo 12)
    
[<Fact>]
let ``get triplet that sums to 1000`` () =
    Assert.Equal(Some [200; 375; 425], tripletThatSumsTo 1000)
