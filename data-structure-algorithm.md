# 問題を解くのに使ったデータ構造とアルゴリズム

## 動的計画法（DP）

データ構造としては多次元配列を使うことが多い。  
メモ化再帰を使っても同様のことが実現可能。

いろいろな問題で使っている。

## 木に対する探索

深さ優先探索（Depth First Search）と幅優先探索（Breaths First Search）がある。  
DFSの方が直感的だが、DFSを使う場合は再帰が深くなりすぎてスタックオーバーフローが発生する可能性がある。  
BFSを末尾再帰になるように実装するとループに最適化できるため、スタックオーバーフローを防ぐことができる。  
どちらを使うべきかは状況次第だが、どちらでもよい場合、深さが深くなりすぎないことが保証されている場合はDFSの方が簡単で、そうでない場合は末尾再帰BFSを使うとよいのではないか。

いろいろな問題で使っている。

## 二分探索

ソート済み配列から目的の値（もしくは目的の値の近傍の値）を見つけるのに使う。  
もしくは二分木を使ってもよい。

問題1

## 木の直径

適当なノードから最も遠いノードを探し、そのノードから最も遠いノードとの距離が木の直径。

問題3

## 累積和

問題10

## Union-Find

連結判定に使う。  
代表的な実装はUnion-Find木。

問題12

## ダイクストラ法

グラフの最短経路を求めるアルゴリズム。

問題13

## Binary Indexed Tree

初期化をO(n)、要素への加算をO(log n)、先頭から指定した要素までの総和を求めるのをO(log n)で行えるデータ構造。

問題17

## 強連結成分分解

強連結しているノード（相互に行き来可能なノード）ごとにグループ分けするアルゴリズム。

問題21

## ユークリッドの互除法

最大公約数を高速に求めるアルゴリズム。

問題22

## 二部グラフ

接続されているノードを別の色にする条件のもと、2色に色分けされたグラフ。  
特に木は必ず二部グラフとなる。  
一般の平面グラフは必ずしも二部グラフではないが、4色に塗り分けることは可能（四色定理）。

問題26

## 二次元いもす法

二次元に対する領域を加算する際に使えるアルゴリズム。  
二次元配列に対してそれぞれ行と列から累積和を取る。

問題28

## セグメント木

区間に対して更新とクエリを高速に行えるデータ構造。  
遅延評価セグメント木を使うことで更新を効率的に行うことができる。

問題29

## エラトステネスの篩

素数を列挙するアルゴリズム。O(n log log n)程度の計算量となる。

問題30