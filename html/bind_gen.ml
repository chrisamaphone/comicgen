let () =
  Js.Unsafe.global##.comic := object%js
    method gen nves min max =
      Printf.eprintf "Call: nves=%d; min=%d; max=%d\n%!" nves min max;
      let comic = Gen.gen nves min max in
      Js.string (Yojson.Safe.pretty_to_string (Gen.comic_to_yojson comic))
    
    method gen_constrained nves =
      Printf.eprintf "Call: nves=%d" nves;
      let comic = Gen.gen_constrained nves in
      Js.string (Yojson.Safe.pretty_to_string (Gen.comic_to_yojson comic))

  end
