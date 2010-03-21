namespace Euler

module Problem9 =

    let euclidsFormula m n =
        let a = m * m - n * n
        let b = 2 * m * n
        let c = m * m + n * n
        (a, b, c)
        
    let pythagoreanTriplets =
        let generator (m, n) =
            let nextElement = euclidsFormula m n
            let nextState =
                if (m > (n + 1)) then
                    (m, n + 1)
                else
                    (m + 1, 1)
            Some (nextElement, nextState)
        Seq.unfold generator (2, 1)
        
    let sum (a, b, c) =
        a + b + c
    
    let equals x y =
        x = y
        
    let product (a, b, c) =
        a * b * c
        
    let result =
        pythagoreanTriplets |> Seq.find (sum >> equals 1000) |> product
        
    let printResult =
        printfn "Problem 9: %A" result
        