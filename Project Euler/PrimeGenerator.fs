namespace Euler

module Dict =
    open System.Collections.Generic
    
    let empty() = new Dictionary<_, _>()
    
    let add key value (dict : #IDictionary<'a, 'b>) =
        dict.[key] <- value; dict

    let remove (key : 'a) (dict : #IDictionary<'a, 'b>) =
        dict.Remove(key) |> ignore; dict

    let tryFind key (dict : #IDictionary<'a, 'b>) =
        match dict.TryGetValue(key) with
        | true, value -> Some (value)
        | false, _    -> None
        
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
