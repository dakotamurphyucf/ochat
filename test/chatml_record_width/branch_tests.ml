open Core
module L = Chatml.Chatml_lang
module T = Chatml_typechecker

(* An independent finite model: a label is absent, integer-valued or Boolean.
   No checker types, row merging or unification are used by the oracle. *)
module Field = struct
  type t =
    | Absent
    | Integer
    | Boolean
  [@@deriving equal, sexp]

  let all = [ Absent; Integer; Boolean ]

  let source t ~alternate =
    match t with
    | Absent -> None
    | Integer -> Some (if alternate then "2" else "1")
    | Boolean -> Some (if alternate then "false" else "true")
  ;;

  let matches t value ~alternate =
    match t, value with
    | Absent, None -> true
    | Integer, Some (L.VInt actual) -> Int.equal actual (if alternate then 2 else 1)
    | Boolean, Some (L.VBool actual) -> Bool.equal actual (not alternate)
    | _ -> false
  ;;
end

module Shape = struct
  type t = Field.t * Field.t [@@deriving equal, sexp]

  let all = List.cartesian_product Field.all Field.all
  let fields (x, y) = [ "x", x; "y", y ]

  let source t ~alternate =
    fields t
    |> List.filter_map ~f:(fun (label, field) ->
      Option.map (Field.source field ~alternate) ~f:(fun value -> label ^ " = " ^ value))
    |> String.concat ~sep:"; "
    |> fun fields -> "{" ^ fields ^ "}"
  ;;

  let matches t value ~alternate =
    match value with
    | L.VRecord actual ->
      let fields = fields t in
      Map.length actual
      = List.count fields ~f:(fun (_, field) -> not (Field.equal field Absent))
      && List.for_all fields ~f:(fun (label, field) ->
        Field.matches field (Map.find actual label) ~alternate)
    | _ -> false
  ;;
end

(* Each envelope keeps a record reachable through a different type constructor.
   This checks one shape-preservation law, not separate hand-written cases. *)
module Envelope = struct
  type t =
    | Direct
    | Nested
    | Reference
    | Array
    | Function
  [@@deriving sexp]

  let all = [ Direct; Nested; Reference; Array; Function ]

  let wrap t source =
    match t with
    | Direct -> source
    | Nested -> "{inner = " ^ source ^ "}"
    | Reference -> "ref(" ^ source ^ ")"
    | Array -> "[" ^ source ^ "]"
    | Function -> "(fun () -> " ^ source ^ ")"
  ;;

  let unwrap = function
    | Direct -> "chosen"
    | Nested -> "chosen.inner"
    | Reference -> "!chosen"
    | Array -> "chosen[0]"
    | Function -> "chosen()"
  ;;
end

let%test_unit "finite record model: branch compatibility preserves shape and values" =
  List.iter (List.cartesian_product Shape.all Shape.all) ~f:(fun (left, right) ->
    List.iter Envelope.all ~f:(fun envelope ->
      let compatible = Shape.equal left right in
      List.iter [ false; true ] ~f:(fun alternate ->
        let source =
          "let chosen = if "
          ^ Bool.to_string (not alternate)
          ^ " then "
          ^ Envelope.wrap envelope (Shape.source left ~alternate:false)
          ^ " else "
          ^ Envelope.wrap envelope (Shape.source right ~alternate:true)
          ^ "\nlet result = "
          ^ Envelope.unwrap envelope
        in
        let program = Chatml.Chatml_parse.parse_program_exn source in
        let result = T.check_program program in
        if not (Bool.equal compatible (Result.is_ok result))
        then
          raise_s
            [%sexp
              "branch model mismatch"
            , (left : Shape.t)
            , (right : Shape.t)
            , (envelope : Envelope.t)
            , (source : string)];
        match result with
        | Error diagnostic -> [%test_eq: bool] true (Option.is_some diagnostic.span)
        | Ok _ ->
          let env = Chatml_builtin_modules.create_default_env () in
          (match Chatml_resolver.run_program env program with
           | Error _ ->
             raise_s [%sexp "accepted branch failed at runtime", (source : string)]
           | Ok () -> ());
          let value = L.find_var env "result" |> Option.value_exn in
          if not (Shape.matches (if alternate then right else left) value ~alternate)
          then raise_s [%sexp "record shape/value preservation failed", (source : string)];
          ())))
;;

let%test_unit "matching shapes, open helpers and tagged alternatives remain useful" =
  List.iter
    [ {|let choose b = if b then { x = 1; y = true } else { y = false; x = 2 }
        let get r = r.x
        let result = get(choose(true)) + get({ x = 3; extra = "kept" })|}
    ; {|let choose b = if b then `Small({ x = 1 }) else `Large({ x = 2; y = 3 })
        let result = match choose(false) with
          | `Small(r) -> r.x
          | `Large(r) -> r.y|}
    ; {|let choose b left right = if b then left else right
        let result = choose(false, { x = 1; y = 3 }, { x = 2; y = 4 }).y|}
    ]
    ~f:(fun source ->
      match T.check_program (Chatml.Chatml_parse.parse_program_exn source) with
      | Ok _ -> ()
      | Error diagnostic -> failwith diagnostic.message)
;;

let%test_unit "shared-tail override compatibility is independent of its envelope" =
  List.iter Envelope.all ~f:(fun envelope ->
    List.iter
      [ "false", true; "1", false ]
      ~f:(fun (enabled, accepted) ->
        let source =
          "let choose b r = if b then "
          ^ Envelope.wrap envelope "{r with enabled = true}"
          ^ " else "
          ^ Envelope.wrap envelope "r"
          ^ "\nlet chosen = choose(true, {enabled = "
          ^ enabled
          ^ "; x = 3})"
          ^ "\nlet result = ("
          ^ Envelope.unwrap envelope
          ^ ").x"
        in
        let result = T.check_program (Chatml.Chatml_parse.parse_program_exn source) in
        [%test_eq: bool] accepted (Result.is_ok result)))
;;
