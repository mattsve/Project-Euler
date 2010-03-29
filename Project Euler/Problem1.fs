namespace Euler
module Problem1 =
    let numbers = seq {for x in 1 .. 999 do if x % 3 = 0 || x % 5 = 0 then yield x}

    let result = Seq.fold (+) 0 numbers
