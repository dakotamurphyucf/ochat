open Core
module L = Chatml.Chatml_lang

let show label source =
  printf "%s: " label;
  let env = Chatml_builtin_modules.create_default_env () in
  match
    Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
  with
  | Error (Type_diagnostic diagnostic) -> printf "%s\n" diagnostic.message
  | Error (Runtime_diagnostic diagnostic) -> failwith diagnostic.message
  | Ok () ->
    (match L.find_var env "result" with
     | Some (VBool value) -> printf "%b\n" value
     | _ -> failwith "expected Boolean result")
;;

let%expect_test "generic equality compares values and preserves copy-update freedom" =
  let equal = "let equal x y = x == y\n" in
  show "equal(1, 2)" (equal ^ "let result = equal(1, 2)");
  show
    "equal records"
    (equal ^ "let result = equal({x=1; label=\"a\"}, {label=\"a\"; x=1})");
  show "equal([1], [1])" (equal ^ "let result = equal([1], [1])");
  show
    "replace function field, then compare copies"
    {|let compare r = let copy = {r with callback = 1} in copy == copy
      let original = {callback = (fun x -> x); label = "kept"}
      let result = compare(original)|};
  show
    "compare exported values"
    {|module M = struct let x = 1 end
      let equal_field r = r.x == r.x
      let result = equal_field(M)|};
  show
    "compare module identity"
    (equal ^ "module M = struct let x = 1 end\nlet result = equal(M, M)");
  [%expect
    {|
    equal(1, 2): false
    equal records: true
    equal([1], [1]): Equality is not supported for arrays
    replace function field, then compare copies: true
    compare exported values: true
    compare module identity: Equality is not supported for modules
    |}]
;;
