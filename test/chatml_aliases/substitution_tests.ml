open Core
open Support

(* A bounded source model, independent of the compiler's type representation or
   substitution code. Every shape has a concrete value; invalid cases vary leaf
   types, so parser failure cannot count as successful rejection. *)
module Atom = struct
  type t =
    | Integer
    | Boolean
  [@@deriving equal]

  let name = function
    | Integer -> "int"
    | Boolean -> "bool"
  ;;

  let literal = function
    | Integer -> "7"
    | Boolean -> "true"
  ;;

  let value = function
    | Integer -> L.VInt 7
    | Boolean -> L.VBool true
  ;;
end

module Shape = struct
  type t =
    | First
    | Second
    | Unit
    | Box of t
    | Tag of t
    | Pair of t * t

  let rec render t ~first ~second =
    match t with
    | First -> first
    | Second -> second
    | Unit -> "unit"
    | Box t -> "{ item : " ^ render t ~first ~second ^ " }"
    | Tag t -> "[ `Value(" ^ render t ~first ~second ^ ") ]"
    | Pair (a, b) ->
      "{ left : "
      ^ render a ~first ~second
      ^ "; right : "
      ^ render b ~first ~second
      ^ " }"
  ;;

  let rec literal t (first, second) =
    let sub t = literal t (first, second) in
    match t with
    | First -> Atom.literal first
    | Second -> Atom.literal second
    | Unit -> "()"
    | Box t -> "{ item = " ^ sub t ^ " }"
    | Tag t -> "`Value(" ^ sub t ^ ")"
    | Pair (a, b) -> "{ left = " ^ sub a ^ "; right = " ^ sub b ^ " }"
  ;;

  let rec value t (first, second) =
    let sub t = value t (first, second) in
    match t with
    | First -> Atom.value first
    | Second -> Atom.value second
    | Unit -> L.VUnit
    | Box t -> L.VRecord (String.Map.singleton "item" (sub t))
    | Tag t -> L.VVariant ("Value", [ sub t ])
    | Pair (a, b) -> L.VRecord (String.Map.of_alist_exn [ "left", sub a; "right", sub b ])
  ;;

  let rec compatible t (first, second) (actual_first, actual_second) =
    let sub t = compatible t (first, second) (actual_first, actual_second) in
    match t with
    | First -> Atom.equal first actual_first
    | Second -> Atom.equal second actual_second
    | Unit -> true
    | Box t | Tag t -> sub t
    | Pair (a, b) -> sub a && sub b
  ;;

  let all =
    let seeds = [ First; Second; Unit ] in
    seeds
    @ List.concat_map seeds ~f:(fun seed ->
      [ Box seed; Tag seed; Box (Tag seed); Tag (Box seed) ])
    @ List.concat_map seeds ~f:(fun a -> List.map seeds ~f:(fun b -> Pair (a, b)))
  ;;
end

let%test_unit
    "transparent aliases agree with independent substitution and parameter renaming"
  =
  let pairs =
    [ Atom.Integer, Atom.Integer; Integer, Boolean; Boolean, Integer; Boolean, Boolean ]
  in
  List.iter Shape.all ~f:(fun shape ->
    List.iter pairs ~f:(fun ((first, second) as expected) ->
      List.iter pairs ~f:(fun actual ->
        let valid = Shape.compatible shape expected actual in
        let literal = Shape.literal shape actual in
        let expanded =
          Shape.render shape ~first:(Atom.name first) ~second:(Atom.name second)
        in
        let direct = "let result : " ^ expanded ^ " = " ^ literal in
        require direct valid;
        List.iter
          [ "'a", "'b"; "'item", "'error" ]
          ~f:(fun (a, b) ->
            let body = Shape.render shape ~first:a ~second:b in
            let source =
              Printf.sprintf
                "type (%s, %s) payload = %s\n\
                 type ('x, 'y) forward = ('x, 'y) payload\n\
                 let result : (%s, %s) forward = %s"
                a
                b
                body
                (Atom.name first)
                (Atom.name second)
                literal
            in
            require source valid;
            if valid && not (L.equal_value (run source) (Shape.value shape actual))
            then failwith ("Alias changed runtime value:\n" ^ source)))))
;;

let%test_unit "explicit function types retain unit parameters and exact nary call arity" =
  for arity = 0 to 4 do
    for unit_mask = 0 to (1 lsl arity) - 1 do
      let parameters = List.init arity ~f:(fun i -> "x" ^ Int.to_string i) in
      let units = List.init arity ~f:(fun i -> unit_mask land (1 lsl i) <> 0) in
      let types =
        List.map units ~f:(function
          | true -> "unit"
          | false -> "int")
      in
      let arguments =
        List.map units ~f:(function
          | true -> "()"
          | false -> "7")
      in
      let lambda =
        if arity = 0
        then "fun () -> 9"
        else "fun " ^ String.concat ~sep:" " parameters ^ " -> 9"
      in
      let prefix =
        "let f : fun(" ^ String.concat ~sep:", " types ^ ") -> int = " ^ lambda ^ "\n"
      in
      for supplied = 0 to 5 do
        let actual =
          List.init supplied ~f:(fun i ->
            Option.value (List.nth arguments i) ~default:"7")
        in
        let source = prefix ^ "let result = f(" ^ String.concat ~sep:", " actual ^ ")" in
        require source (supplied = arity);
        if supplied = arity
        then [%test_eq: bool] true (L.equal_value (run source) (L.VInt 9))
      done
    done
  done
;;

module Arrow_argument = struct
  type t =
    | Unit
    | Integer
    | Function of t list

  let rec syntax = function
    | Unit -> "unit"
    | Integer -> "int"
    | Function parameters ->
      "fun(" ^ String.concat ~sep:", " (List.map parameters ~f:syntax) ^ ") -> int"
  ;;

  let literal = function
    | Unit -> "()"
    | Integer -> "7"
    | Function [] -> "(fun () -> 7)"
    | Function parameters ->
      let names = List.mapi parameters ~f:(fun i _ -> "arg" ^ Int.to_string i) in
      "(fun " ^ String.concat ~sep:" " names ^ " -> 7)"
  ;;
end

let%test_unit "generic arrows normalize after transparent parameter substitution" =
  let open Arrow_argument in
  List.iter
    [ Unit; Integer; Function []; Function [ Unit ]; Function [ Integer; Unit ] ]
    ~f:(fun argument ->
      List.iter [ `Input; `Exact_input; `Output; `Exact_output ] ~f:(fun side ->
        let body, expanded, arguments, returned, suffix, result =
          match side, argument with
          | `Input, Unit -> "'a -> int", "unit -> int", [], "7", "", L.VInt 7
          | `Input, _ ->
            ( "'a -> int"
            , "(" ^ syntax argument ^ ") -> int"
            , [ literal argument ]
            , "7"
            , ""
            , L.VInt 7 )
          | `Exact_input, _ ->
            ( "fun('a) -> int"
            , "fun(" ^ syntax argument ^ ") -> int"
            , [ literal argument ]
            , "7"
            , ""
            , L.VInt 7 )
          | `Output, Unit -> "int -> 'a", "int -> unit", [ "7" ], "()", "", L.VUnit
          | `Output, Integer -> "int -> 'a", "int -> int", [ "7" ], "7", "", L.VInt 7
          | `Output, Function parameters ->
            ( "int -> 'a"
            , "int -> (" ^ syntax argument ^ ")"
            , "7" :: List.map parameters ~f:literal
            , "7"
            , ""
            , L.VInt 7 )
          | `Exact_output, _ ->
            let suffix, result =
              match argument with
              | Unit -> "", L.VUnit
              | Integer -> "", L.VInt 7
              | Function parameters ->
                ( "(" ^ String.concat ~sep:", " (List.map parameters ~f:literal) ^ ")"
                , L.VInt 7 )
            in
            ( "fun(int) -> 'a"
            , "fun(int) -> " ^ syntax argument
            , [ "7" ]
            , literal argument
            , suffix
            , result )
        in
        for arity = 0 to 4 do
          let names = List.init arity ~f:(fun i -> "x" ^ Int.to_string i) in
          let lambda =
            "fun "
            ^ (if arity = 0 then "()" else String.concat ~sep:" " names)
            ^ " -> "
            ^ returned
          in
          let direct = "let f : " ^ expanded ^ " = " ^ lambda in
          let aliased =
            "type 'a callback = "
            ^ body
            ^ "\ntype 'b forwarded = 'b callback\nlet f : ("
            ^ syntax argument
            ^ ") forwarded = "
            ^ lambda
          in
          List.iter [ direct; aliased ] ~f:(fun source ->
            let valid = arity = List.length arguments in
            require source valid;
            if valid
            then (
              let source =
                source
                ^ "\nlet result = f("
                ^ String.concat ~sep:", " arguments
                ^ ")"
                ^ suffix
              in
              if not (L.equal_value (run source) result) then failwith source))
        done))
;;
