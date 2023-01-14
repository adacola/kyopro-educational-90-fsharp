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

/// 遅延評価セグメント木
/// count: データ数
/// et: データの単位元
/// ft: データに対する演算
/// em: 中間データの単位元
/// fm: 中間データに対する演算
/// ftm: データと中間データに対する演算
type LazySegmentTree<'T, 'M when 'T: equality and 'M: equality>(
  count: int,
  et: 'T,
  ft: ('T -> 'T -> 'T),
  em: 'M,
  fm: 'M -> 'M -> 'M,
  ftm: ('T -> 'M -> 'T)
) =
  let leafCount = Seq.initInfinite (pown 2) |> Seq.find (fun i -> count <= i)
  let nodeCount = leafCount * 2 + 1
  let nodes = Array.create nodeCount et
  let lazyNodes = Array.create nodeCount em
  let leftChildIndex i = 2 * i + 1
  let rightChildIndex i = 2 * i + 2
  let parentIndex i = (i - 1) / 2
  let leafIndex i = i + leafCount - 1
  let eval i =
    if lazyNodes[i] = em then () else
    if i < leafCount - 1 then
      lazyNodes[leftChildIndex i] <- fm lazyNodes[leftChildIndex i] lazyNodes[i]
      lazyNodes[rightChildIndex i] <- fm lazyNodes[rightChildIndex i] lazyNodes[i]
    nodes[i] <- ftm nodes[i] lazyNodes[i]
    lazyNodes[i] <- em
  let rec update a b x k l r =
    eval k
    if a <= l && r <= b then
      lazyNodes[k] <- fm lazyNodes[k] x
      eval k
    elif a < r && l < b then
      update a b x (leftChildIndex k) l ((l + r) / 2)
      update a b x (rightChildIndex k) ((l + r) / 2) r
      nodes[k] <- ft nodes[leftChildIndex k] nodes[rightChildIndex k]
  let rec query a b k l r =
    eval k
    if r <= a || b <= l then et
    elif a <= l && r <= b then nodes[k] else
    let vl = query a b (leftChildIndex k) l ((l + r) / 2)
    let vr = query a b (rightChildIndex k) ((l + r) / 2) r
    ft vl vr
  new(values: 'T list, et, ft, em, fm, ftm) as this =
    LazySegmentTree(List.length values, et, ft, em, fm, ftm)
    then this.Set values
  member private _.Set values =
    values |> List.iteri (fun i v -> nodes[leafIndex i] <- v)
    for i = leafCount - 2 downto 0 do nodes[i] <- ft nodes[leftChildIndex i] nodes[rightChildIndex i]
  member _.Count = count
  member _.Update a b x = update a b x 0 0 leafCount
  member _.Query a b = query a b 0 0 leafCount
