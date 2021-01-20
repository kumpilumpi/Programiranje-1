(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki vrne razliko med produktom in vsoto dveh celih števil.

    razlika_produkta_in_vsote : int -> int -> int

[*----------------------------------------------------------------------------*)

let razlika_produkta_in_vsote x y = x * y - x - y



(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki združi dva para v četverico.
    
    zlimaj_para : 'a * 'b -> 'c * 'd -> 'a * 'b * 'c * 'd

[*----------------------------------------------------------------------------*)

let zlimaj_para par1 par2 =
  let (x1 , y1) = par1 
  in
  let (x2, y2) = par2
  in
  (x1 , y1 , x2 , y2)



(* c *)
(*----------------------------------------------------------------------------*]
  Imamo podatke tipa [int option * int option * int option], ki jih želimo
  grafično predstaviti. Napišite funkcijo [trojica_graficno], ki sprejme takšno
  trojico in vrne niz, kjer so ``manjkajoči'' elementi nadomeščeni z [-].
  Primer vrnjenega niza je ["(1, 2, -)"]

    trojica_graficno : int option * int option * int option -> string

[*----------------------------------------------------------------------------*)

let trojica_graficno trojica = (*int option*) ()
  (* let (x , y , z) = trojica 
  in
  let sez = [Option.to_list x] @ [Option.to_list y] @ [Option.to_list z] 
  in 
  () *)




(* d *)
(*----------------------------------------------------------------------------*]
  Klic funkcije [nedeljivo_do x n] preveri, da število [x] ni deljivo z nobenim
  naravnim številom od 2 do vključno [n]. Število 73859 je praštevilo, torej
  mora [nedeljivo_do 73859 73858] vrniti [true].

    nedeljivo_do : int -> int -> bool

[*----------------------------------------------------------------------------*)

let nedeljivo_do x n =
  let rec aux x n =
    if n = 2 then (
      if (x mod n) = 0  then false else true
    )
    else(
      if (x mod n) = 0  then false else aux x ( n - 1 )
    )
  in
  if n >= x then false (* Deljiv bo sam s sabo*)
  else
  aux x n 

(* e *)
(*----------------------------------------------------------------------------*]
  Seznam elementov tipa ['a option] želimo razdeliti na podsezname glede na
  pojavitve vrednosti [None].

    razcepi_pri_None : 'a option list -> 'a list list.

  Kot primer, funkcija seznam

    [Some 1; None; Some 2; Some 3; None; None; Some 4; None]

  razcepi v [[1]; [2;3]; []; [4]; []]. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)

let  razcepi_pri_None sez =
  let rec aux odgovor akumolator sez =
    match sez with 
      | [] -> odgovor @ [List.rev akumolator]
      | None :: xs -> aux (odgovor @ [List.rev akumolator]) [] xs
      | x :: xs -> aux odgovor ((Option.to_list x) @ akumolator) xs
  in
  aux [] [] sez

