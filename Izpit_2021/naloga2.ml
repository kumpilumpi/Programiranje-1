(*============================================================================*]
  Filip potrebuje pomoč pri organiziranju kuhinje. Posode in omare mu je uspelo
  popisati in ustrezno označiti, sedaj pa mora nad tem izvajati kopico
  arhivskih nalog, kjer nastopite vi.
[*============================================================================*)

type 'a vsebina_kuhinje =
  | Ponev of 'a
  | Lonec of 'a * 'a
  | Omara of 'a list

(* a *)
(*----------------------------------------------------------------------------*]
  Definirajte primer seznama kuhinjskih elementov [kuhinja], kjer ponev vsebuje
  niz "tuna", lonec vsebuje "brokoli" in "mango", omara pa vsebuje "sir",
  "toast", "sok" in "ragu".
[*----------------------------------------------------------------------------*)

let kuhinja = [
  Ponev "tuna";
  Lonec ("brokoli", "mango");
  Omara ["sir"; "toast"; "sok"; "ragu"]
]

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [prestej], ki za podani seznam kuhinjskih elementov vrne
  skupno število vsebinskih elementov. Za zgornji primer [kuhinja] bi tako
  vrnila 7.
[*----------------------------------------------------------------------------*)

let rec prestej =
  function
  | [] -> 0
  | Ponev _ :: xs -> 1 + prestej xs
  | Lonec _ :: xs -> 2 + prestej xs
  | Omara sez :: xs -> List.length sez + prestej xs

(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme funkcijo [f] in kuhinjski element ter
  funkcijo [f] uporabi na celotni vsebini elementa.

    pretvori : (’a -> ’b) -> ’a kuhinjski_element -> ‘b kuhinjski_element

[*----------------------------------------------------------------------------*)

let pretvori f =
  function
  | Ponev x -> Ponev (f x)
  | Lonec (x, y) -> Lonec (f x, f y)
  | Omara sez ->  Omara (List.map f sez)

(* d *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo [pospravi], ki sprejme seznam kuhinjskih elementov in
  vsebino vseh elementov pospravi v eno samo [Omaro]. Vrstni red elementov v
  končni omari je nepomemben. Za vse točke naj bo funkcija repno rekurzivna. 

    pospravi : ’a kuhinjski_element list -> ‘a kuhinjski_element

[*----------------------------------------------------------------------------*)

let pospravi sez =
  let rec aux sez_omara sez =
    match sez with
    | [] -> Omara sez_omara
    | Ponev x :: xs -> aux (x :: sez_omara) xs
    | Lonec (x, y) :: xs -> aux (x :: y :: sez_omara) xs
    | Omara sez :: xs -> aux (sez @ sez_omara) xs
  in
  aux [] sez 

(* e *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [oceni], ki sprejme seznam tipa ['a kuhinjski_element list]
  in cenilko vsebine tipa [‘a -> int]. Funkcija izračuna skupno ceno celotnega
  seznama, kjer je cena vsebine v loncih množena s 3, v omarah pa s 5.

  Ocena testne kuhinje za cenilko [String.length] je 115.
[*----------------------------------------------------------------------------*)

let oceni cenilka sez =
  let rec sum = (* Vsota seznama *)
    function 
    | [] -> 0
    | x :: xs -> x + (sum xs)
  in
  let rec aux cena cenilka sez =
    match sez with
    | [] -> cena
    | Ponev x :: xs -> aux (cena + (cenilka x)) cenilka xs
    | Lonec (x, y) :: xs -> aux (cena + 3 * ((cenilka x) + (cenilka y))) cenilka xs
    | Omara sez :: xs -> aux ( cena +  5 * (sum (List.map cenilka sez)) ) cenilka xs
  in
  aux 0 cenilka sez
