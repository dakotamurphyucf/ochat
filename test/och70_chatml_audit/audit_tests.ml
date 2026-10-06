open Core
module L = Chatml.Chatml_lang

(* Observations are intentional audit reproductions, not desired behavior for
   confirmed defects. See design/och-70 for classifications and follow-up owners. *)
let observe label source =
  printf "%s: " label;
  match Chatml.Chatml_parse.parse_program source with
  | Error diagnostic -> printf "parse-error %s\n" diagnostic.message
  | Ok program ->
    let env = L.create_env () in
    Chatml_builtin_modules.BuiltinModules.add_global_builtins env;
    (match Chatml_resolver.run_program env program with
     | Error (Type_diagnostic diagnostic) -> printf "type-error %s\n" diagnostic.message
     | Error (Runtime_diagnostic diagnostic) ->
       printf "runtime-error %s\n" diagnostic.message
     | Ok () ->
       (match L.find_var env "result" with
        | Some (L.VInt n) -> printf "int %d\n" n
        | Some (L.VBool b) -> printf "bool %b\n" b
        | Some (L.VString s) -> printf "string %s\n" s
        | Some _ -> print_endline "value"
        | None -> print_endline "ok"))
;;

let%expect_test "audit nonexecuting compiler ownership entrypoints and diagnostics" =
  let module C = Chatml_compilation in
  Eio_main.run (fun env ->
    let observe_compile ?limits label target source =
      printf "%s: " label;
      match C.compile ?limits ~env ~target ~source () with
      | Ok _ -> print_endline "compiled"
      | Error error ->
        (match error.diagnostic with
         | None -> print_endline error.code
         | Some diagnostic ->
           printf "%s %s" error.code diagnostic.message;
           Option.iter diagnostic.span ~f:(fun span ->
             printf " at %d:%d" span.left.line span.left.column);
           print_endline "")
    in
    observe_compile
      "initializer not executed"
      One_off_v1
      "let poison = fail(\"must not execute\")\nlet main input = Task.pure(input)";
    observe_compile
      "first namespace"
      One_off_v1
      "type private_type = int\nlet private_value = 1\nlet main input = Task.pure(input)";
    observe_compile
      "next namespace private value"
      One_off_v1
      "let main input = let ignored = private_value in Task.pure(input)";
    observe_compile
      "next namespace private type"
      One_off_v1
      "let private_value : private_type = 1\nlet main input = Task.pure(input)";
    observe_compile "valid after failures" One_off_v1 "let main input = Task.pure(input)";
    observe_compile "entry arity" One_off_v1 "let main input extra = Task.pure(input)";
    observe_compile "entry output" One_off_v1 "let main input = Task.pure(true)";
    observe_compile
      "related state bindings"
      Moderator_v1
      "let initial_state = true\nlet on_event ctx state event = Task.pure(1)";
    observe_compile
      "UTF8 CRLF source span"
      One_off_v1
      "let label = \"é\"\r\nlet main input = missing(input)";
    observe_compile
      ~limits:{ C.default_limits with max_source_bytes = 1 }
      "source resource policy"
      One_off_v1
      "let main input = Task.pure(input)");
  [%expect
    {|
    initializer not executed: compiled
    first namespace: compiled
    next namespace private value: chatml.invalid_handler Unknown variable 'private_value' at 1:31
    next namespace private type: chatml.invalid_handler Unknown type 'private_type' at 1:35
    valid after failures: compiled
    entry arity: chatml.invalid_handler Invalid entrypoint 'main': Function arity mismatch
    entry output: chatml.invalid_handler Invalid entrypoint 'main': Cannot unify bool with [`Array(mu __builtin_json. [`Array(__builtin_json array) | `Bool(bool) | `Null | `Number(float) | `Object({key: string; value: __builtin_json} array) | `String(string)] array) | `Bool(bool) | `Null | `Number(float) | `Object({key: string; value: mu __builtin_json. [`Array(__builtin_json array) | `Bool(bool) | `Null | `Number(float) | `Object({key: string; value: __builtin_json} array) | `String(string)]} array) | `String(string)]
    related state bindings: chatml.invalid_handler Invalid entrypoint 'on_event': Cannot unify int with bool
    UTF8 CRLF source span: chatml.invalid_handler Unknown variable 'missing' at 2:17
    source resource policy: chatml.source_limit
    |}]
;;

let%expect_test "audit equality constraints and module record boundaries" =
  List.iter
    [ "direct array equality", "let a = [1]\nlet result = a == a"
    ; ( "abstracted array equality"
      , "let eq x y = x == y\nlet a = [1]\nlet result = eq(a, a)" )
    ; ( "abstracted function equality"
      , "let eq x y = x == y\nlet f x = x\nlet result = eq(f, f)" )
    ; "abstracted record equality", "let eq x y = x == y\nlet result = eq({x=1}, {x=1})"
    ; "module field", "module M = struct let x = 1 end\nlet result = M.x"
    ; "record extension", "let r = {x=1}\nlet result = {r with x=2}.x"
    ; "module extension", "module M = struct let x = 1 end\nlet result = {M with x=2}.x"
    ; "record pattern", "let result = match {x=1} with | {x=n} -> n"
    ; ( "module pattern"
      , "module M = struct let x = 1 end\nlet result = match M with | {x=n} -> n" )
    ; ( "record annotated module"
      , "module M = struct let x = 1 end\nlet r : {x:int} = M\nlet result = r.x" )
    ]
    ~f:(fun (label, source) -> observe label source);
  [%expect
    {|
    direct array equality: type-error Equality is not supported for arrays
    abstracted array equality: bool true
    abstracted function equality: bool true
    abstracted record equality: bool true
    module field: int 1
    record extension: int 2
    module extension: runtime-error Record extension base is not a record
    record pattern: int 1
    module pattern: runtime-error Non-exhaustive pattern match
    record annotated module: int 1
    |}]
;;

let%expect_test "audit levels value restriction annotations and module exports" =
  List.iter
    [ "polymorphic identity", "let id x = x\nlet a = id(1)\nlet result = id(true)"
    ; "escaping lambda monomorphic", "let f x = let y = x in let a = y(1) in y(true)"
    ; ( "mutable array"
      , "let a = [fun x -> x]\na[0] <- (fun x -> x + 1)\nlet result = a[0](true)" )
    ; ( "module mutable array"
      , "module M = struct let a = [fun x -> x] end\n\
         M.a[0] <- (fun x -> x + 1)\n\
         let result = M.a[0](true)" )
    ; "duplicate parameters", "let f x x = x\nlet result = f(1, true)"
    ; "duplicate recursive names", "let rec f x = x + 1 and f x = x\nlet result = f(true)"
    ; "bad annotation", "let result : int = true"
    ; "bad annotated arity", "let f : int -> int = fun x y -> x"
    ; "self application", "let f x = x(x)"
    ; "recursive alias unguarded", "type t = t\nlet result = 1"
    ; ( "module lexical closure"
      , "let x = 1\nmodule M = struct let f () = x end\nlet x = 2\nlet result = M.f()" )
    ; ( "module export rebind"
      , "module M = struct let x = 1 let x = true end\nlet result = M.x" )
    ; ( "module eager self reference"
      , "module M = struct let x = 1 let y = M.x end\nlet result = M.y" )
    ; ( "module delayed self reference"
      , "module M = struct let x = 1 let f () = M.x end\nlet result = M.f()" )
    ; "outer not exported", "let x = 1\nmodule M = struct let y = x end\nlet result = M.x"
    ]
    ~f:(fun (label, source) -> observe label source);
  [%expect
    {|
    polymorphic identity: bool true
    escaping lambda monomorphic: type-error Cannot unify int with bool
    mutable array: type-error Cannot unify int with bool
    module mutable array: type-error Cannot unify int with bool
    duplicate parameters: bool true
    duplicate recursive names: type-error Cannot unify int with bool
    bad annotation: type-error Cannot unify bool with int
    bad annotated arity: type-error Annotated function expects 1 parameter(s), but lambda has 2
    self application: type-error Recursive types
    recursive alias unguarded: type-error Unguarded recursive type variable 't'
    module lexical closure: int 1
    module export rebind: bool true
    module eager self reference: runtime-error No field 'x' in module
    module delayed self reference: int 1
    outer not exported: type-error Row does not contain label 'x'
    |}]
;;

let%expect_test "audit joins exact patterns recursive rows and builtin failures" =
  List.iter
    [ "join common field", "let r = if true then {x=1; y=2} else {x=3}\nlet result = r.x"
    ; "join missing field", "let r = if true then {x=1; y=2} else {x=3}\nlet result = r.y"
    ; ( "join exact pattern"
      , "let r = if true then {x=1; y=2} else {x=3}\n\
         let result = match r with | {x=n} -> n" )
    ; ( "join open pattern"
      , "let r = if true then {x=1; y=2} else {x=3}\n\
         let result = match r with | {x=n; _} -> n" )
    ; ( "lambda exact pattern wider record"
      , "let f r = match r with | {x=n} -> n\nlet result = f({x=1; y=2})" )
    ; ( "lambda exact pattern exact record"
      , "let f r = match r with | {x=n} -> n\nlet result = f({x=1})" )
    ; ( "variant missing payload coverage"
      , "let result = match `A(true) with | `A(true) -> 1" )
    ; ( "variant total payload"
      , "let result = match `A(true) with | `A(x) -> if x then 1 else 2" )
    ; "nullary variant positive", "let result = match `A with | `A -> 1"
    ; "unit payload nullary pattern", "let result = match `A(()) with | `A -> 1"
    ; "unit payload wildcard pattern", "let result = match `A(()) with | `A(x) -> 1"
    ; "unit payload unit pattern", "let result = match `A(()) with | `A(()) -> 1"
    ; ( "variant impossible constructor"
      , "type t = [`A]\nlet v : t = `A\nlet result = match v with | `A -> 1 | `B -> 2" )
    ; ( "guarded recursive record"
      , "let rec make n = {x=n; next=fun () -> make(n+1)}\nlet result = make(1).x" )
    ; "array map positive", "let result = Array.map([1,2], fun x -> x+1)[1]"
    ; "array map negative", "let result = Array.map([1,2], fun x -> x+1)(0)"
    ; "array index failure", "let result = [1][2]"
    ; "json invalid dynamic input", "let result = Json.parse(\"{\")"
    ; "json optional invalid input", "let result = Option.is_none(Json.parse_opt(\"{\"))"
    ; "integer divide zero", "let result = 1 / 0"
    ; "zero arity positive", "let f () = 4\nlet result = f()"
    ; "zero arity negative", "let f () = 4\nlet result = f(())"
    ]
    ~f:(fun (label, source) -> observe label source);
  [%expect
    {|
    join common field: int 1
    join missing field: type-error Row does not contain label 'y'
    join exact pattern: runtime-error Non-exhaustive pattern match
    join open pattern: int 1
    lambda exact pattern wider record: runtime-error Non-exhaustive pattern match
    lambda exact pattern exact record: int 1
    variant missing payload coverage: type-error Non-exhaustive variant match: missing case '`A(_)'
    variant total payload: int 1
    nullary variant positive: int 1
    unit payload nullary pattern: runtime-error Non-exhaustive pattern match
    unit payload wildcard pattern: type-error Non-exhaustive variant match: missing case '`A'
    unit payload unit pattern: type-error Non-exhaustive variant match: missing case '`A'
    variant impossible constructor: type-error Row does not contain label 'B'
    guarded recursive record: int 1
    array map positive: int 3
    array map negative: type-error Cannot unify int array with (int -> 'g)
    array index failure: runtime-error Array index out of bounds
    json invalid dynamic input: runtime-error Json.parse: ("Jsonaf.of_string: parse error" (error "json > object: not enough input")
      (input {))
    json optional invalid input: bool true
    integer divide zero: runtime-error Division by zero
    zero arity positive: int 4
    zero arity negative: type-error Function arity mismatch
    |}]
;;
