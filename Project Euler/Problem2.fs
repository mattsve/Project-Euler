namespace Euler
module Problem2 =
    
    let fibonacciSequence = Seq.unfold (fun (x, y) -> Some (x, (y, x + y))) (1,2)

    let limitedFibonacciSequence = Seq.takeWhile (fun x -> x <= 4000000) fibonacciSequence

    let evenSequence = Seq.filter (fun x -> x % 2 = 0) limitedFibonacciSequence

    let result = Seq.fold (+) (0) evenSequence
    