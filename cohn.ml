
let () = Random.full_init [|1; 13123|]

type role =
  Establisher | Initial | Prolongation | Peak | Release

type grammar =
  Seq of grammar * grammar | Optional of grammar | NT of role
  | Repeat of grammar


let arc_grammar
  = Seq (Optional (NT Establisher),
         Seq (Optional (Seq ((NT Initial), Repeat (NT Prolongation))),
           Seq (NT Peak, Optional (NT Release))))

let rec gen_role_sequence g =
  match g with
    Seq (g1, g2) -> (gen_role_sequence g1)@(gen_role_sequence g2)
  | Optional g ->
    (match Random.int 2 with
       0 -> gen_role_sequence g
     | _ -> [])
  | Repeat g ->
    (match Random.int 2 with 
    (* at random, generate nothing...*)
      0 -> []
    | _ ->
      (* ...or generate g followed by g* *)
      (gen_role_sequence g)@(gen_role_sequence (Repeat g))
      )
  | NT nt -> [nt]
      


