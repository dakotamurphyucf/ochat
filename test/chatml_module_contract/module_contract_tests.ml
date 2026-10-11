open Core
module L = Chatml.Chatml_lang

let observe label source =
  printf "%s: " label;
  let program = Chatml.Chatml_parse.parse_program_exn source in
  let env = L.create_env () in
  Chatml_builtin_modules.add_global_builtins env;
  match Chatml_resolver.run_program env program with
  | Error (Type_diagnostic diagnostic) -> printf "type-error %s\n" diagnostic.message
  | Error (Runtime_diagnostic diagnostic) ->
    printf "runtime-error %s\n" diagnostic.message
  | Ok () ->
    (match L.find_var env "result" with
     | Some (VInt n) -> printf "int %d\n" n
     | Some (VBool b) -> printf "bool %b\n" b
     | Some (VString text) -> printf "string %s\n" text
     | Some _ | None -> print_endline "ok")
;;

let%expect_test "modules retain their kind through record consumers" =
  List.iter
    [ "module field", "module M = struct let x = 1 end\nlet result = M.x"
    ; "module update", "module M = struct let x = 1 end\nlet result = {M with x=2}.x"
    ; ( "module exact pattern"
      , "module M = struct let x = 1 end\nlet result = match M with | {x=n} -> n" )
    ; ( "module open pattern"
      , "module M = struct let x = 1 end\nlet result = match M with | {x=n; _} -> n" )
    ; ( "record annotation"
      , "module M = struct let x = 1 end\nlet r : {x:int} = M\nlet result = r.x" )
    ; ( "aliased update"
      , "module M = struct let x = 1 end\nlet r = M\nlet result = {r with x=2}.x" )
    ; ( "higher order update"
      , "let update r = {r with x=2}\n\
         module M = struct let x = 1 end\n\
         let result = update(M).x" )
    ; "literal update", "let r = {x=1}\nlet result = {r with x=2}.x"
    ; "literal pattern", "let result = match {x=1} with | {x=n} -> n"
    ; "module open", "module M = struct let x = 1 end\nopen M\nlet result = x"
    ]
    ~f:(fun (label, source) -> observe label source);
  [%expect
    {|
    module field: int 1
    module update: type-error Cannot unify module{x: int} with {...}
    module exact pattern: type-error Cannot unify module{x: int} with {x: 'a}
    module open pattern: type-error Cannot unify module{x: int} with {x: 'a; ...}
    record annotation: type-error Cannot unify module{x: int} with {x: int}
    aliased update: type-error Cannot unify module{x: int} with {...}
    higher order update: type-error Cannot unify {...} with module{x: int}
    literal update: int 2
    literal pattern: int 1
    module open: int 1
    |}]
;;

let%expect_test "field receivers generalize kinds without losing shared constraints" =
  List.iter
    [ ( "independent accessor calls"
      , "let get r = r.x\n\
         module M = struct let x = 2 end\n\
         let a = get({x=1; extra=true})\n\
         let result = a + get(M)" )
    ; ( "independent keys calls"
      , "module M = struct let x = 1 end\n\
         let keys = record_keys\n\
         let a = Array.length(keys({x=1; y=2}))\n\
         let result = a + Array.length(keys(M))" )
    ; ( "returned receiver retains module kind"
      , "let keep r = let ignored = r.x in r\n\
         module M = struct let x = 1 end\n\
         let r = keep(M)\n\
         let result = {r with x=2}.x" )
    ; ( "returned receiver accepts records"
      , "let keep r = let ignored = r.x in r\n\
         let r = keep({x=1})\n\
         let result = {r with x=2}.x" )
    ; ( "monomorphic receiver kind"
      , "let use r = let a = r.x in let b = record_keys(r) in let update = {r with x=2} \
         in a\n\
         module M = struct let x = 1 end\n\
         let result = use(M)" )
    ; ( "module exported accessor"
      , "module A = struct let get r = r.x end\n\
         module M = struct let x = 3 end\n\
         let a = A.get({x=2})\n\
         let result = a + A.get(M)" )
    ; ( "nested module"
      , "module A = struct module B = struct let x = 4 end let b = B end\n\
         let result = A.b.x" )
    ; ( "module join"
      , "module A = struct let x = 4 end\n\
         module B = struct let x = 5 end\n\
         let result = (if true then A else B).x" )
    ; ( "mixed join"
      , "module M = struct let x = 4 end\nlet result = (if true then M else {x=5}).x" )
    ]
    ~f:(fun (label, source) -> observe label source);
  [%expect
    {|
    independent accessor calls: int 3
    independent keys calls: int 3
    returned receiver retains module kind: type-error Cannot unify module with record
    returned receiver accepts records: int 2
    monomorphic receiver kind: type-error Cannot unify record with module
    module exported accessor: int 5
    nested module: int 4
    module join: int 4
    mixed join: type-error Cannot unify module{x: int} with {x: int}
    |}]
;;

let%expect_test "resolved receiver kinds preserve record coverage and mutable exports" =
  observe
    "projection then exact match"
    {|let read r = let ignored = r.x in match r with | {x=n} -> n
      let result = read({x=3})|};
  observe
    "projection then open match"
    {|let read r = let ignored = r.x in match r with | {x=n; _} -> n
      let result = read({x=3; extra=true})|};
  observe
    "exported mutable alias"
    {|module M = struct let values = [1] end
      let alias = M.values
      alias[0] <- 7
      let result = M.values[0]|};
  [%expect
    {|
    projection then exact match: int 3
    projection then open match: int 3
    exported mutable alias: int 7
    |}]
;;

let%expect_test "internal module admission rejects record open and malformed kinds" =
  let module T = Chatml_typechecker in
  let state = T.create_state () in
  let env = T.add_mono T.Env.empty "R" (T.Record T.Empty_row) in
  (match T.infer_stmt state env T.Type_alias.empty (L.SOpen "R") with
   | _ -> failwith "record admitted by open"
   | exception T.Type_error message -> print_endline message);
  List.iter
    [ T.Shape_kind Record
    ; T.Field_receiver (T.TInt, T.Empty_row)
    ; T.Record (T.Row (T.Env.singleton "x" (T.Shape_kind Module), T.Empty_row))
    ]
    ~f:(fun ty ->
      match T.check_contractive ty with
      | Error message -> print_endline message
      | Ok () -> failwith "malformed internal representation accepted");
  List.iter
    [ T.Field_receiver (T.Shape_kind Record, T.Empty_row)
    ; T.Field_receiver (T.Shape_kind Module, T.Empty_row)
    ; T.Field_receiver (T.Generic { name = "kind"; equality = None }, T.Empty_row)
    ]
    ~f:(fun ty -> T.check_contractive ty |> Result.ok_or_failwith);
  [%expect
    {|
    Cannot open non-module 'R'
    Representation kind is only valid in a field receiver
    Invalid field receiver representation kind
    Representation kind is only valid in a field receiver
    |}]
;;
