
(* 1 ---------------------------- *)

(* B *)

let list_of_triple =
  function
  | a :: b :: c :: [] -> Some (a , b , c)
  | _ -> None

(* C *)

type counter = {lt : int ; eq : int ; gt : int}

let compare_with sez x =
  let rec aux counter' x =
    function
    | [] -> counter'
    | y :: ys -> (
      if y > x then aux ( {lt = counter'.lt ; eq = counter'.eq ; gt = counter'.gt + 1 } ) x ys
      else(
        if y = x then aux ( {lt = counter'.lt ; eq = counter'.eq + 1 ; gt = counter'.gt } ) x ys 
        else
          aux ( {lt = counter'.lt + 1 ; eq = counter'.eq ; gt = counter'.gt } ) x ys     
      )
    )
    in
    aux { lt = 0 ; eq = 0 ; gt = 0 } x sez

(* C *)

let apply_all x sez_f =
  let rec aux x sez_f =
    match sez_f with
    | [] -> x
    | f :: fs -> aux (f x) fs
  in
  aux x (List.rev sez_f)

let long_test = List.init 1000000 (fun _ -> (+) 1)



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
  (* let preveri sez_pogoj sez_tock = (*sez pogoj je ([pogoji za x] * [pogoji za y])*)
    ()
  in
  let rec aux pogoji tree =
    match tree with
    | Elements ( sez ) -> preveri pogoji sez (* true / false*)
    | Xsplit (delilna, a, b) -> (aux (((<=)delilna :: (fst pogoji)) , (snd pogoji)) a) && (aux (((>)delilna :: (fst pogoji)) , (snd pogoji)) b)
    | Ysplit (delilna, a, b) -> (aux ((fst pogoji) , ((<=)delilna :: (snd pogoji))) a) && (aux ((fst pogoji) , ((<)delilna :: (snd pogoji))) b) 
  in *)
  ()
