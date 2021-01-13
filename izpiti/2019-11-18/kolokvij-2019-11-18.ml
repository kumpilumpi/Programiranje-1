(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = x * x = y

let pack3 x y z = (x, y, z) 

let sum_if_not predikat sez =
  let rec aux predikat vsota sez =
    match sez with
      |[] -> vsota
      | x :: xs -> if not (predikat x) then aux predikat (vsota + x) xs else aux predikat vsota xs
  in
  aux predikat 0 sez  

let apply list_fun sez = 
  let rec aux list_fun element novi =
    match list_fun with 
    |[] -> novi
    |funkcija :: xs -> aux xs element (novi @ [funkcija element])
  in
  let rec aux2 list_fun sez novi2 =
    match sez with 
    | [] -> novi2
    | element :: xs2 -> aux2 list_fun xs2 (novi2 @ [aux list_fun element []])
  in
  aux2 list_fun sez []


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = DopolniMe

type srecanje = DopolniMe'

type urnik = DopolniMe''

let vaje = "dopolni me"
let predavanja = "dopolni me"

let urnik_profesor = "dopolni me"

let je_preobremenjen = "dopolni me"

let bogastvo = "dopolni me"
