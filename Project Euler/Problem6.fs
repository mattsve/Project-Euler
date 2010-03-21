namespace Euler
module Problem6 =
    open Microsoft.FSharp.Core

    let square x =
        x * x

    let squares x =
        List.map square x

    let sum x =
        List.fold (+) 0I x
            
    let numbers = [1I .. 100I]

    let result =
        (numbers |> sum |> square) - (numbers |> squares |> sum)

    let printResult =
        printfn "Problem 6: %A" result