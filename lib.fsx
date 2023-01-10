module Adacola.TypicalProblem90

/// a^b (mod m) を求める
let modPow a b m =
  ((1L, a), Seq.init 63 id) ||> Seq.fold (fun (p, q) i ->
    let p = if b &&& (1L <<< i) <> 0 then (p * q) % m else p
    let q = (q * q) % m
    p, q)
  |> fst

let calcGcd a b =
  let rec loop a = function
    | 0L -> a
    | b -> loop b (a % b)
  loop (max a b) (min a b)
