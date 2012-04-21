module FsUtils.Primes
  
let primesTo maxOfSieve = 

    let rec sieve position elements =
        if position = Array.length elements then
            elements
        else
            match Array.get elements position with
            |1 -> sieve (position + 1) elements
            |n -> 
                Seq.iter 
                    (fun i -> Array.set elements (i - 1) 1) 
                    (seq {for i in n..((Array.length elements) / n) -> n * i})
                sieve (position + 1) elements

    sieve 0 [|1..maxOfSieve|] 
    |> Seq.filter (fun i -> i <> 1)