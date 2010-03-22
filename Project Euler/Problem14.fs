namespace Euler

module Problem14 =

    let sequence n =
        let generator num =
            match num with
            | 1           -> None
            | num % 2 = 0 -> let newNum = num / 2
                             Some (newNum, newNum)
            | _           -> let newNum = 3 * num + 1
                             Some (newNum, newNum)
             
    let result =
        "Not done yet"
        
    let printResult =
        printfn "Problem 14: %A" result