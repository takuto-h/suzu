(*
 * http://itpro.nikkeibp.co.jp/article/COLUMN/20070116/258746/
 *)

module type Complex =
sig
  type t (* 複素数の型 *)
  val make : float * float -> t (* 実部と虚部から複素数を作って返す *)
  val add : t -> t -> t (* 複素数の足し算 *)
  val mul : t -> t -> t (* 複素数の掛け算 *)
  val abs : t -> float (* 複素数の絶対値 *)
end

module Cartesian : Complex = (* CartesianはComplexインタフェースを実装 *)
struct
  type t = float * float
  let make (x, y) = (x, y)
  let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
  let mul (x1, y1) (x2, y2) =
    (x1 *. x2 -. y1 *. y2,
     x1 *. y2 +. x2 *. y1)
  let abs (x, y) = sqrt (x *. x +. y *. y)
end

module Polar : Complex = (* PolarはComplexインタフェースを実装 *)
struct
  type t = float * float
  let c2p (x, y) = (sqrt (x *. x +. y *. y), atan2 y x)
  let p2c (r, t) = (r *. cos t, r *. sin t)
  let make (x, y) = c2p (x, y)
  let add p1 p2 =
    let (x1, y1) = p2c p1 in
    let (x2, y2) = p2c p2 in
    c2p (x1 +. x2, y1 +. y2)
  let mul (r1, t1) (r2, t2) = (r1 *. r2, t1 +. t2)
  let abs (r, t) = r
end

module Mandelbrot (C : Complex) =
struct
  let d = 100
  let n = 100
  let f1 = 100.0
  let f2 = 1.5
  let f3 = 1.0
  let f () = (* 実際にマンデルブロ集合を描画する関数 *)
    let rec converge n c z =
      if n <= 0 then true else
      if C.abs z >= 2.0 then false else
        converge (n - 1) c (C.add c (C.mul z z)) in
    for x = 0 to 2 * d do
      for y = 0 to 2 * d do
        let cx = float x /. f1 -. f2 in
        let cy = float y /. f1 -. f3 in
        let conv = converge n (C.make (cx, cy)) (C.make (0.0, 0.0)) in
        if conv then Printf.printf "%f %f\n" cx cy;
      done
    done
end

module CartesianMandelbrot = Mandelbrot(Cartesian)
module PolarMandelbrot = Mandelbrot(Polar)

let () = CartesianMandelbrot.f ()
(*let () = PolarMandelbrot.f ()*)
