namespace Euler

module Problem3 =
    let primeFactors n =

        let inline isFactor n d = n % d = 0L
        
        let rec nextFactor n d =
            let x = if d = 2L then 3L else d + 2L
            if isFactor n x then x else nextFactor n x

        let rec findFactors n d acc =
            if isFactor n d then
                findFactors (n/d) d (d::acc)
            elif n > d then
                findFactors n (nextFactor n d) acc
            else
                acc

        findFactors n 2L [] |> List.rev

    let result =
        List.reduce max (primeFactors 600851475143L)

    let printResult =
        printfn "Problem 3: %d" result