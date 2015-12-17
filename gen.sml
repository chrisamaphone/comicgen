structure ComicGen =
struct

(* visual elements *)
type ve = int

(* a frame has a name and a number of holes *)
type frame = {name:string, nholes:int}

  val available_frames =
    [
      {name="whisper", nholes=2},
      {name="monolog", nholes=1},
      {name="dialog", nholes=2},
      {name="touch", nholes=2},
      {name="blank", nholes=0},
      {name="walk", nholes=1},
      {name="posse", nholes=4},
      {name="carry", nholes=2},
      {name="aid", nholes=2},
      {name="fall", nholes=1}
    ]

type panel = {name:string, elements : ve list}

datatype transition
  = Moment | Add | Subtract | Meanwhile | RendezVous | End

type comic = (panel * transition) list

val rand = Random.rand(1,13123)

  fun roll () =
    case Random.randRange (1,6) rand of
         1 => Moment
       | 2 => Add
       | 3 => Subtract
       | 4 => Meanwhile 
       | 5 => RendezVous
       | _ => End

  fun roll_continue () =
    case roll () of 
         End => roll_continue ()
       | other => other

  fun randElt l =
    List.nth (l, Random.randRange (0, (List.length l) - 1) rand)

  fun randSplit l =
  let
    val idx = Random.randRange (0, List.length l - 1) rand
    fun split [] target prefix idx = raise Subscript
      | split (x::xs) target prefix idx =
            if target = idx then ((rev prefix)@xs, x)
            else split xs target (x::prefix) (idx+1)
  in
    split l idx [] 0
  end

  fun hasAtLeastNHoles n {name, nholes} = nholes >= n
  fun hasAtMostNHoles n {name, nholes} = nholes <= n

  fun pickRandomFrame () =
    randElt available_frames

  fun pickFrame nVEs =
    randElt (List.filter (hasAtMostNHoles nVEs) available_frames)

  fun genElts n = List.tabulate (n, fn i => i +1)
  fun newElt n = n+1

  fun decideNew unused =
    if List.length unused = 0 then true (* make up new if unused is empty *)
    else
      case Random.randRange (0,2) rand of
           0 => true
         | _ => false (* bias toward using previous chars *)

  fun pickRandomVEs' unused n acc counter =
    if n = 0 then (acc, counter)
    else if not (decideNew unused) then
           (* choose a previously-appearing character *)
              let
                val (unused, elt) = randSplit unused
              in
                pickRandomVEs' unused (n-1) (elt::acc) counter
              end
           (* choose a new character *)
         else pickRandomVEs' unused (n-1) ((newElt counter)::acc) (counter+1)

  fun pickRandomVEs unused n counter =
    pickRandomVEs' unused n [] counter

  fun removeRandom l =
  let
    val elt = randElt l
  in
    List.filter (fn x => not (x = elt)) l
  end

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

  fun listMember x [] = false
    | listMember x (y::ys) = x = y orelse listMember x ys

  fun nonmembers l l' = List.filter (fn x => not (listMember x l)) l'

  fun pickPanel allPrior (current_panel : panel) transition
    : panel * int =
    let
      val justPrior = #elements current_panel
      val currentNVEs = List.length justPrior
      val totalNVEs = List.length allPrior
    in
      case transition of
           Moment =>
           let
             val {name, nholes} = pickFrame currentNVEs
           in
             ({name=name, elements=justPrior}, totalNVEs) 
           end
         | Add =>
           let
             val unused = nonmembers justPrior allPrior
             val howManyNew = 1 (* Random.randRange (1,2) rand *)
             val {name, nholes} = pickFrame (currentNVEs + howManyNew)
             val (new_elts, new_total) = pickRandomVEs unused howManyNew totalNVEs
             val new_elts = new_elts @ justPrior
           in
             ({name=name, elements=new_elts}, new_total) 
           end
         | Subtract =>
             if List.length justPrior > 0 then
              let
                val nVEs = currentNVEs - 1
                val {name, nholes} = pickFrame nVEs
                val elts = removeRandom justPrior
              in
                ({name=name, elements=elts}, totalNVEs)
              end
            else
              let
                val {name, nholes} = pickFrame 0
              in
                ({name=name, elements=[]}, totalNVEs)
              end
         | Meanwhile =>
             let
               val skipVEs = nonmembers justPrior allPrior
               val {name, nholes} = pickRandomFrame ()
               val (elts, newTotal) = pickRandomVEs skipVEs nholes totalNVEs
             in
               ({name=name, elements=elts}, newTotal)
             end
         | RendezVous =>
             let
               val {name, nholes} = pickRandomFrame ()
               val (elts, newTotal) = pickRandomVEs allPrior nholes totalNVEs
             in
               ({name=name, elements=elts}, newTotal)
             end
         | End => ({name="blank", elements=[]}, totalNVEs)
    end

  fun fillFrame ({name,nholes}: frame) (VEs : ve list) : panel =
    {name=name, elements=VEs}

  fun gen (soFar:comic) (nves : int) min max : comic =
    case soFar of
         [] => 
         let 
           val new_frame = pickFrame nves
           val VEs = genElts nves
           val panel = fillFrame new_frame VEs
           val transition = roll_continue ()
         in
           gen [(panel, transition)] nves min max
         end
       | ((current_panel, transition)::more) =>
           let
             val current_length = List.length soFar
             val next_transition = 
               if current_length >= max then End
               else
                if current_length < min
                then roll_continue () 
                else roll ()
           in
             case transition of
                  End => rev soFar
                | tr =>
                    let
                      val allPrior = genElts nves
                      val (panel, newTotal) = 
                        pickPanel allPrior current_panel transition
                    in
                      gen ((panel, next_transition)::soFar) newTotal min max
                    end

                    (*
                      val currentNVEs = List.length justPrior
                      val {name=cname, elements=ves} = current_panel
                      val {name=fname, nholes} = pickRandomFrame () (* XXX *) 
                      val (new_ves, new_total) = pickVEs nholes allPrior tr soFar
                      val panel = fillFrame {name=fname, nholes=nholes} new_ves
                    in
                      gen ((panel, next_transition)::soFar) new_total
                    end
                    *)

           end

  (* Next steps:
  * - Impose a limit on the number of entities introduced?
  * - port to JS and do image generation?
  *)

end
