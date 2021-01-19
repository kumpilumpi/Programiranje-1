(* 1-------------------------------------------------- *)

(* A *)

let dot_prod vekt1 vekt2 =
  let (x1, y1, z1) = vekt1 and (x2, y2, z2) = vekt2 in
  x1 *. x2 +. y1 *. y2 +. z1 *. z2

(* B *)

let fix_second (fun2 : 'a -> 'b -> 'c) x =
  let fun1 = fun2 x in
  fun1

(* C *)

let combine_and_filter sez1 sez2 func = 
  let rec aux novi_sez dvoj_sez fun' =
    match dvoj_sez with 
    | ( _ , [] ) -> novi_sez
    | ( [] , _ ) -> novi_sez 
    | (x :: xs, y :: ys) -> aux ( (fun' x y) :: novi_sez ) (xs, ys) fun'
  in
  aux [] ( sez1 , sez2 ) func

  (* Option ????? *)

(* D *)

let conditional_print predikat sez =
  let rec sez_pravilnih predikat novi_sez sez = (* Nov seznam samo pravilnih *)
    match sez with
    | [] -> novi_sez
    | x :: xs -> if (predikat x) then sez_pravilnih predikat (x :: novi_sez) xs 
          else sez_pravilnih predikat novi_sez xs
  in
  let izpisi sez = (* izpise elemente sez kot potrebno *)
    let rec aux sez n_str =
      match sez with
      | [] -> n_str
      | x :: xs -> aux xs ( x ^ ", " ^ n_str)
    in
    aux sez ""
  in
  izpisi ( sez_pravilnih predikat [] sez )




(* 2-------------------------------------------------- *)


(* 3-------------------------------------------------- *)

let func k n =
  let vsota = 0 
  in
  let rec aux k n =
    if n = 0 then 1 else (
      if n < 0 then 0 else (
        for i = 1 to k do(
          vsota = vsota + (aux k (n - i))
        )done  
      )
    )
  in
  aux k n


