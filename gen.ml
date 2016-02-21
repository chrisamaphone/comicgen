(* visual elements *)
type ve = int
  [@@deriving yojson]

(* a frame has a name and a number of holes *)
type frame = {name: string; nholes:int}
  [@@deriving yojson]

  let available_frames =
    [
      {name="whisper"; nholes=2};
      {name="monolog"; nholes=1};
      {name="dialog"; nholes=2};
      {name="touch"; nholes=2};
      {name="blank"; nholes=0};
      {name="walk"; nholes=1};
      {name="posse"; nholes=4};
      {name="carry"; nholes=2};
      {name="aid"; nholes=2};
      {name="fall"; nholes=1}
    ]

type panel = {name:string; elements : ve list}
  [@@deriving yojson]

type transition
  = Moment | Add | Subtract | Meanwhile | RendezVous | End
  [@@deriving yojson]

type comic = (panel * transition) list
  [@@deriving yojson]

  (* initialize the random number generator *)
  let () = Random.full_init [|1; 13123|]

  let rand_range a b =
    (* Random.int is between 0 and bound (excluded) *)
    a + Random.int (b - a + 1)

  let roll () =
    match rand_range 1 6 with
         1 -> Moment
       | 2 -> Add
       | 3 -> Subtract
       | 4 -> Meanwhile
       | 5 -> RendezVous
       | _ -> End

  let rec roll_continue () =
    match roll () with
         End -> roll_continue ()
       | other -> other

  let randElt l =
    List.nth l (Random.int (List.length l))

  let randSplit l =
    let idx = Random.int (List.length l) in
    let rec split li target prefix idx = match li with
        | [] -> invalid_arg "split: empty list"
        | x::xs ->
              if target = idx then ((List.rev prefix)@xs, x)
              else split xs target (x::prefix) (idx+1)
    in
    split l idx [] 0

  let rec random_subset l =
    match l with 
    [] -> []
    | (x::xs) -> if Random.int 2 > 0 
                 then x::(random_subset xs) 
                 else (random_subset xs);;

  let rec rand_subset_of_size l size =
    match size with
      0 -> []
    | n ->
        let (l', x) = randSplit l
        in x::(rand_subset_of_size l' (n-1))

        (* XXX *)

  let hasAtLeastNHoles n {nholes} = nholes >= n
  let hasAtMostNHoles n {nholes} = nholes <= n

  let pickRandomFrame () =
    randElt available_frames

  let pickFrame nVEs =
    randElt (List.filter (hasAtMostNHoles nVEs) available_frames)

  let genElts n = Array.to_list (Array.init n (fun i -> i +1))
  let newElt n = n+1

  let decideNew unused =
    if List.length unused = 0 then true (* make up new if unused is empty *)
    else
      match Random.int 3 with
           0 -> true
         | _ -> false (* bias toward using previous chars *)

  let rec pickRandomVEs' unused n acc counter =
    if n = 0 then (acc, counter)
    else if not (decideNew unused) then
           (* choose a previously-appearing character *)
              let (unused, elt) = randSplit unused in
              pickRandomVEs' unused (n-1) (elt::acc) counter
           (* choose a new character *)
         else pickRandomVEs' unused (n-1) ((newElt counter)::acc) (counter+1)

  let pickRandomVEs unused n counter =
    pickRandomVEs' unused n [] counter

  let removeRandom l =
    let elt = randElt l in
    List.filter (fun x -> not (x = elt)) l

  (*
  (* XXX eventually care about transition type *)
  fun pickVEs n allPriorVEs (t:transition) (({name,elements},_)::_ : comic) =
  let
    val totalNVEs = List.length allPriorVEs
  in
      case t of
           Moment => (elements, totalNVEs)
         | Add => (elements, totalNVEs) (* XXX *)
         | Subtract =>
             if List.length elements > 0
             then (removeRandom elements, totalNVEs)
             else (elements, totalNVEs)
         | Meanwhile =>
             (* XXX eventually exclude current VEs *)
             pickRandomVEs allPriorVEs n totalNVEs
         | RendezVous => (* (randElt available_ves)::elements *)
           (* XXX maybe this should also choose the frame? *)
             pickRandomVEs allPriorVEs n totalNVEs
         | End => ([], totalNVEs)
  end
  *)

  let member x = List.exists (fun y -> x = y)

  let nonmembers l l' = List.filter (fun x -> not (List.mem x l)) l'

  let pickPanel allPrior (current_panel : panel) transition
    : panel * int =
    let justPrior = current_panel.elements in
    let currentNVEs = List.length justPrior in
    let totalNVEs = List.length allPrior in
      match transition with
           Moment ->
           let {name; nholes} = pickFrame currentNVEs in
           ({name; elements=justPrior}, totalNVEs)
         | Add ->
           let unused = nonmembers justPrior allPrior in
           let howManyNew = 1 (* Random.randRange (1,2) rand *) in
           let  {name; nholes} = pickFrame (currentNVEs + howManyNew) in
           let (new_elts, new_total) = pickRandomVEs unused howManyNew totalNVEs in
           let new_elts = new_elts @ justPrior in
             ({name; elements=new_elts}, new_total)
         | Subtract ->
             if List.length justPrior > 0 then
              let nVEs = currentNVEs - 1 in
              let {name; nholes} = pickFrame nVEs in
              let elts = removeRandom justPrior in
                ({name; elements=elts}, totalNVEs)
            else
              let {name; nholes} = pickFrame 0 in
                ({name; elements=[]}, totalNVEs)
         | Meanwhile ->
             let skipVEs = nonmembers justPrior allPrior in
             let {name; nholes} = pickRandomFrame () in
             let (elts, newTotal) = pickRandomVEs skipVEs nholes totalNVEs in
               ({name; elements=elts}, newTotal)
         | RendezVous ->
             (* CRM: changed 2/21/2016: *)
             (* generate a random subset of prior ves
                generate a frame w/at most that many holes
             *)
             let size = rand_range 1 (List.length allPrior) in
             let elts = rand_subset_of_size allPrior size in
             let {name; nholes} = pickFrame size in
               ({name; elements=elts}, totalNVEs)
         | End -> ({name="blank"; elements=[]}, totalNVEs)

  let fillFrame ({name; nholes}: frame) (visual_elts : ve list) : panel =
    {name; elements=visual_elts}

  (*
  let rec gen (soFar:comic) (nves : int) min max : comic =
    match soFar with
         [] ->
         let new_frame = pickFrame nves in
         let visual_elts = genElts nves in
         let panel = fillFrame new_frame visual_elts in
         let transition = roll_continue () in
           gen [(panel, transition)] nves min max
       | ((current_panel, transition)::more) ->
           let current_length = List.length soFar in
           let next_transition =
               if current_length >= max then End
               else
                if current_length < min
                then roll_continue ()
                else roll ()
           in
             begin match transition with
                  End -> List.rev soFar
                | tr ->
                    let allPrior = genElts nves in
                    let (panel, newTotal) =
                        pickPanel allPrior current_panel transition
                    in
                      gen ((panel, next_transition)::soFar) newTotal min max
              end
  *)

  (* first steps of a refactor where transitions are gen'd first,
     then panels. *)

open Cohn

  let rec gen_transitions min max n_so_far : transition list =
    if n_so_far + 1 > max then [End]
    else 
      let t = if n_so_far < min then roll_continue () else roll ()
      in
      match t with
        End -> [t]
      | _ -> t::(gen_transitions min max (n_so_far+1))

  let gen_first_panel (r:role option) nves =
    let new_frame = pickFrame nves in (* XXX take role into acct *)
    let elements = genElts nves in
      fillFrame new_frame elements

  (* Accepts soFar=[], a number of VEs to start with (nves), and
      a list of transitions to follow (ts) *)
  let rec gen_with_transitions 
    (soFar : comic) (nves : int) (ts : transition list) : comic =
      match (ts, soFar) with
        ([], _) -> List.rev soFar
      | (_, (_, End)::_) -> List.rev soFar
      | (t::ts, []) ->
          let first_panel = gen_first_panel None nves
          in gen_with_transitions [(first_panel, t)] nves ts
      | (t'::ts, (current_panel, t)::more) ->
          let allPrior = genElts nves in
          let (panel, newTotal) =
              pickPanel allPrior current_panel t
          in
            gen_with_transitions ((panel, t')::soFar) newTotal ts

  let gen nves min max : comic =
    let ts = gen_transitions (min-1) (max-1) 0 in
    gen_with_transitions [] nves ts


  (* Cohn grammars -> Transitions *)
  (* XXX do i need to "open" or sthg to make this build w/ocamlbuild? *)

  (* possible first panels *)
  let valid_first role =
    match role with
      Establisher -> ["walk"; "posse"; "carry"; "monolog"; "dialog"]
    | Initial -> ["whisper"; "monolog"; "dialog"; "touch"; "walk"; "posse";
        "carry"; "aid"; "fall"]
    | Peak -> ["whisper"; "monolog"; "touch"; "dialog"; "walk"; "posse";
        "carry"; "aid"; "fall"]
    | _ -> []


  (* XXX make these make sense *)
  let valid_transitions roles =
      match roles with
        (Establisher, Initial) -> [Moment; Subtract; Add; RendezVous]
      | (Establisher, Prolongation) -> [Moment; Subtract; Add]
      | (Establisher, Peak) -> [Add; Meanwhile]
      | (Initial, Prolongation) -> [Moment; Subtract; Add]
      | (Prolongation, Prolongation) -> [Moment; Subtract; Add]
      | (Prolongation, Peak) -> [Subtract; Add; RendezVous]
      | (Initial, Peak) -> [Subtract; Add; Meanwhile; RendezVous]
      | (Peak, Release) -> [Subtract; Add; RendezVous; End]
      | _ -> [End] (* error! *)


  let pickConstrainedFrame nves role : frame =
    let candidates =
      List.filter 
      (fun {name;nholes} -> nholes <= nves && member name (valid_first role))
      available_frames
    in
      randElt candidates

  (* Generates a *reversed* transition list based on the role sequence
      "roles" *)
  let rec gen_constrained_transitions prev_role roles acc =
    match (prev_role, roles) with
      (_, []) -> End::acc
    | (None, current_role::roles) -> (* pass the buck *)
        gen_constrained_transitions (Some current_role) roles acc
    | (Some prev_role, current_role::roles) ->
        let t = randElt (valid_transitions (prev_role, current_role))
        in gen_constrained_transitions (Some current_role) roles (t::acc)

  let gen_seq () = gen_role_sequence arc_grammar

  let gen_constrained_by_seq nves seq =
    let ts = gen_constrained_transitions None seq [] in
    let ts = List.rev ts in
      gen_with_transitions [] nves ts

  let gen_constrained nves : comic =
    let seq = gen_seq () in
    gen_constrained_by_seq nves seq

  (*
  let rec gen_constrained (soFar:comic) (nves:int) panelRoles : comic =
    match (soFar, panelRoles) with
        (_, []) -> List.rev soFar
      | ([], role::roles) ->
          let new_frame = pickConstrainedFrame nves role in
          let ves = genElts nves in
          let panel = fillFrame new_frame ves in
          (*XXX pick transition based on what's next *)
          let transition = roll_continue () in
            gen_constrained [(panel,transition)] nves roles
      | ((current_panel, transition)::more, role::roles) ->
          let current_length = 
  *)

