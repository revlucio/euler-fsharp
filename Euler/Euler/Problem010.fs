module Problem010

open Xunit

let primesUnder n =
    let rec setFactorsToFalse(list : array<bool>, i, n) =
        if (i < list.Length) 
        then 
            list.[i] <- false
            setFactorsToFalse(list, i+n, n)
        else list
 
    let rec loop(list : array<bool>, i) =
        match i with 
        | i when (i = list.Length) -> list
        | 0 -> 
            list.[i] <- false
            loop(list, i+1)
        | 1 -> 
            list.[i] <- false
            loop(list, i+1)
        | i when (list.[i] = true) ->
            loop(setFactorsToFalse(list, (i+i), i), i+1)
        | _ ->
            loop(list, i+1)
    loop((Array.init n (fun _ -> true)), 0)

let getLast(arr :array<bool>) = arr.[arr.Length-1]

[<Fact>]
let ``find primes under 5`` () =
    Assert.Equal(false, getLast(primesUnder 2)) //1
    Assert.Equal(true, getLast(primesUnder 3)) //2
    Assert.Equal(true, getLast(primesUnder 4)) //3
    Assert.Equal(false, getLast(primesUnder 5)) //4
    Assert.Equal(true, getLast(primesUnder 6)) //5
    Assert.Equal(false, getLast(primesUnder 10)) //9
    Assert.Equal(false, getLast(primesUnder 52)) //51
    Assert.Equal(true, getLast(primesUnder 1490)) //1489
    
let sumIndicies(arr : array<bool>) = 
    let rec loop sum i =
        match i with
        | i when i = arr.Length -> sum
        | i when arr.[i] -> loop (sum+int64(i)) (i+1)
        | i when arr.[i] = false -> loop sum (i+1)
        
    loop 0L 0 
    
[<Fact>]
let ``sum of indicies`` () = 
    Assert.Equal(7L, sumIndicies([|false; true; true; false; true|]))
    
[<Fact>]
let ``sum of primes under`` () =
    Assert.Equal(17L, sumIndicies(primesUnder 10))
    Assert.Equal(142913828922L, sumIndicies(primesUnder 2_000_000))
    
