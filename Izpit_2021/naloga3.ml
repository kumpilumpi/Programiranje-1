(*============================================================================*]
 Po koncu karantene načrtuje Rožle planinski pohod po svojem najljubšem
 gorovju. Zamislil si je že pot, ki ima veliko priložnosti za fotografiranje.
 Ker pa uporablja zastarel telefon, ima na pomnilniku prostora za zgolj dve
 fotografiji. Da bi ti dve sliki čim bolj izkoristil, želi da je med lokacijo
 prve fotografije in lokacijo druge fotografije kar se da velik vzpon.

 Kot vhod dobimo seznam nadmorskih višin za vse razgledne točke v takšnem
 vrstnem redu, kot si sledijo po poti. Na primer:

    [350; 230; 370; 920; 620; 80; 520; 780; 630]

 V zgornjem primeru se Rožletu najbolj splača slikati na točki 5 (višina 80m)
 in nato na točki 7 (višina 780m), saj se je med njima vzpel za 700 metrov.
 Čeprav je med točko 3 (višina 920m) in točko 5 (višina 80m) večja višinska
 razlika, se je med točkama spuščal in ne vzpenjal, zato ne prideta v poštev.
[*============================================================================*)

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki v času `O(n log(n))` ali hitreje izračuna največjo
  višinsko razliko med optimalno izbranima točkama. Časovno zahtevnost
  utemeljite v komentarju.
[*----------------------------------------------------------------------------*)

(* setete vse nadmorske visine
 let vzpon sez =
  let rec aux max trenutna sez =
    match sez with
    | x :: [] -> (
      if max > (trenutna + x) then max else trenutna + x
    )
    | x :: y :: xs ->(
      if x < y then aux max (trenutna + x) (y :: xs)
      else( (* se začne spuščati*)
        if max > (trenutna + x) then aux max 0 (y :: xs)
        else aux (trenutna + x) 0 (y :: xs)
      )
    )
    | [] -> failwith ("Napaka")
  in
  aux 0 0 sez  *)

let vzpon sez =
  let rec aux max zacetek sez =
    match sez with 
    | x :: [] -> if max > (x - zacetek) then max else (x - zacetek)
    | x :: y :: xs -> (
      if y > x then aux max zacetek (y :: xs)
      else( (* se začne spuščati*)
        if max > (x - zacetek) then aux max y (y :: xs)
        else aux (x - zacetek) y (y :: xs)
        )
      )
      | [] -> failwith ("Napaka")
      in
      aux 0 (List.hd sez) sez

      (*
      funkcija deluje v času O (n * k), kjer je n dolžina seznama k pa čas posameznega koraka.
      Saj funkcija v vsakem koraku rekurzije naredi samo en korak, v času k, in pri tem zmanjša dolžino
      seznama za en element.
      *)

(* b *)
(*----------------------------------------------------------------------------*]
  Prejšnjo rešitev prilagodite tako, da vrne zgolj indeksa teh dveh točk. Pri
  tem poskrbite, da ne pokvarite časovne zahtevnosti v `O` notaciji.
[*----------------------------------------------------------------------------*)

let vzpon_indeks sez =
  let razlika (x , y, indeksx, indeksy) = abs (x - y)
  in
  let rec aux max zacetek n sez = (* n beleži indeks sez*)
    match sez with 
    | x :: [] -> if (razlika max) > (x - (fst zacetek)) then max else (fst zacetek, x, snd zacetek, n)
    | x :: y :: xs -> (
      if y > x then aux max zacetek (n+1) (y :: xs)
      else( (* se začne spuščati*)
        if (razlika max) > (x - (fst zacetek)) then aux max (y, n) (n+1) (y :: xs)
        else aux (fst zacetek, x, snd zacetek, n) (y, n+1) (n+1) (y :: xs)
        )
      )
      | [] -> failwith ("Napaka")
      in
      let (_,_,a,b) = aux (0,0,0,0) (List.hd sez, 0) 0 sez
      in
      (a+1,b) (*Hmmmm upam, da popravi zadevo. Zadnje sekunde*)