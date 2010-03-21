namespace Euler
module Problem7 =

    let result = Seq.nth 10000 PrimeGenerator.primes

    let printResult = printfn "Problem 7: %A" result
