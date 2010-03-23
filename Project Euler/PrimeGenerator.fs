namespace Euler
        
module PrimeGenerator =
    
    let reinsert x table prime =
        let comp = x + prime
        match Dict.tryFind comp table with
        | None         -> table |> Dict.add comp [prime]
        | Some (facts) -> table |> Dict.add comp (prime::facts)
        
    let rec sieve x table =
        seq {
            match Dict.tryFind x table with
            | None ->
                yield x
                yield! sieve (x + 1L) (table |> Dict.add (x * x) [x])
            | Some(factors) ->
                yield! sieve (x + 1L) (factors |> List.fold (reinsert x) (table |> Dict.remove x))
        }
        
    let primes =
        sieve 2L (Dict.empty())
