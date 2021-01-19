
(* (1)----------------------------------------------------------------- *)

(* A *)
type option = |None |Some of int

let sum = ()

(* (B)  *)

let twostep_map f1 f2 f3 elem =  
  let (x1, x2) = (f1 elem) in
  (f2 x1, f3 x2)

(* (C)  *)

let function_repeat f sez =
  let rec dodaj n x sez = (* dodaj x nekemu seznamu n- krat  *) 
    if n <= 0 then sez else dodaj (n - 1) x  (x :: sez)
  in
  let rec aux f novi_sez = function
    | [] -> novi_sez
    | x :: xs -> aux f (dodaj (f x) x novi_sez ) xs
  in
  aux f [] sez 

(* (D) *)

let rec iterate f pogoj x = 
  if pogoj x then x else iterate f pogoj (f x) 

(* (2)----------------------------------------------------------------- *)

(* A *)

type 'a napredni_sez = 
      | Prazen
      | Tabel of ('a array *  'a napredni_sez)

let test = (Tabel ([|1;2;20|], Tabel([|17;19;20;30|], Tabel ([|100|], Prazen))))

(* B *)

let rec count nap_sez =
    match nap_sez with 
    | Prazen -> 0
    | Tabel ( x , xs ) -> 1 + count xs 

(* C *)

let rec nth nap_sez i =
    match (nap_sez, i) with 
    | ( Prazen , _ ) -> [||]
    | ( Tabel ( x , xs ) , 0 ) ->  x
    | ( Tabel ( x , xs ) , i ) -> nth xs (i - 1)

(* D *)

let rec je_urejen nap_sez vecji = (* vecji je način urejenja za katerega preverjamo ali je sez urejen *)
    match nap_sez with 
    | Prazen -> true
    | Tabel ( _ , Prazen ) -> true
    | Tabel (x1, Tabel (x2 , xs)) -> (
      if (vecji x1 x2) then je_urejen ( Tabel (x2, xs) ) vecji else false
    )

(* E *)

let rec update 