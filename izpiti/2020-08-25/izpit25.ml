(* 2 ---------------------------- *)
(* A *)

type xytree =
  | Xsplit of int * xytree * xytree (* Deli ravnino po x, levo drevo ima manjšo ali enako koordinato, desno večjo *)
  | Ysplit of int * xytree * xytree
  | Elements of (int * int) list

let example = Xsplit (2, Ysplit (3, Elements [ (0, 2) ; (1, 1) ] , Elements []) , Ysplit (2, Elements [ (3, 1) ] , Xsplit (4, Elements [ (4, 3) ], Elements [] )) )

(* B *)

let rec num_of_elements tree =
  match tree with
    | Elements sez -> List.length sez
    | Xsplit ( _ , a , b ) -> (num_of_elements a) + (num_of_elements b)
    | Ysplit ( _ , a , b ) -> (num_of_elements a) + (num_of_elements b)

(* C *)

let insert tree tocka = ()
  (* let (xt , yt) = tocka in
  match tree with *)

(* D *)

let alternates tree =
  (* let xy zadnja nova =()
  in *)
  let rec aux tree zadnja =
    match tree with 
    | Elements ( _ ) -> true
    | Xsplit ( _, a, b) -> if zadnja = 'y' then (aux a 'x') && (aux b 'x') else false
    | Ysplit ( _, a, b) -> if zadnja = 'x' then (aux a 'y') && (aux b 'y') else false
  in
  match tree with 
    | Elements (_) -> true
    | Xsplit (_, a, b) -> (aux a 'x') && (aux b 'x')
    | Ysplit (_, a, b) -> (aux a 'y') && (aux b 'y')

(* D *)

let boxed_correctly tree =
  let preveri sez_pogoj sez_tock = (*sez pogoj je ([pogoji za x] * [pogoji za y])*)
    ()
  in
  let rec aux pogoji tree =
    match tree with
    | Elements ( sez ) -> preveri pogoji sez (* true / false*)
    | Xsplit (delilna, a, b) -> (aux (((<=)delilna :: (fst pogoji)) , (snd pogoji)) a) && (aux (((>)delilna :: (fst pogoji)) , (snd pogoji)) b)
    | Ysplit (delilna, a, b) -> (aux ((fst pogoji) , ((<=)delilna :: (snd pogoji))) a) && (aux ((fst pogoji) , ((<)delilna :: (snd pogoji))) b) 
  in
  ()
