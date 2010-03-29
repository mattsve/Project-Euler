namespace Euler

module Problem10 =
    
    let result =
        PrimeGenerator.primes |> Seq.takeWhile (fun x -> x < 2000000L) |> Seq.sum
            