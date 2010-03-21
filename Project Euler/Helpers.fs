namespace Euler

module String =
    let toChars (s : string) =
        s.ToCharArray()  |> Array.toList

module Char =
    let toNumber (c : char) =
        int c - int '0'
