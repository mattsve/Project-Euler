namespace Euler
module Problem4 =

    let reverse n =
        let rec loop x res =
            if 
                x = 0 then res
            else
                loop (x / 10) (res*10 + (x%10))
        loop n 0

    let isPalindrome n = 
        n = reverse n
    
    let result =
        [ for x in 100 .. 999 do
            for y in 100 .. 999 do
                if isPalindrome (x * y) then yield x * y] |> List.reduce max
    
    