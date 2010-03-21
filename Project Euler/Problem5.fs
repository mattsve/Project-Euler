namespace Euler
module Problem5 =
    open Microsoft.FSharp.Core
    
    let lcm x y =
        if x = 0I || y = 0I then 
            0I
        else
            (x / (bigint.GreatestCommonDivisor (x, y))) * y

    let result =
        List.reduce lcm [1I .. 20I]

    let printResult = 
        printfn "Problem 5: %A" result