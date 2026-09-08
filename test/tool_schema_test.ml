open Core
module S = Chatmd_shell_spec.Tool_schema

let get = function
  | Ok value -> value
  | Error errors -> raise_s [%sexp (errors : S.diagnostic list)]
;;

let schema = Fn.compose get S.of_string
let accepts schema json = Result.is_ok (S.validate schema json)
let parse = Jsonaf.of_string

let error_code = function
  | Ok _ -> failwith "expected failure"
  | Error (error :: _) -> error.S.code
  | Error [] -> failwith "empty diagnostics"
;;

let%test_unit "boolean schemas and empty rules" =
  List.iter
    [ `Null; `True; `False; `String "text"; `Number "42"; `Array []; `Object [] ]
    ~f:(fun value ->
      assert (accepts (schema "true") value);
      assert (accepts (schema "{}") value);
      assert (not (accepts (schema "false") value)))
;;

let%expect_test "object and array failures identify value paths" =
  let checked =
    schema
      {|{"type":"object","properties":{"rows":{"type":"array","minItems":1,"maxItems":2,"items":{"type":"object","required":["id"],"properties":{"id":{"type":"integer"}},"additionalProperties":false}}},"required":["rows"],"additionalProperties":false}|}
  in
  assert (accepts checked (parse {|{"rows":[{"id":1},{"id":2}]}|}));
  List.iter
    [ {|{"rows":[{"id":"bad"}]}|}
    ; {|{"rows":[{}]}|}
    ; {|{"rows":[{"id":1,"extra":true}]}|}
    ; {|{"rows":[]}|}
    ; {|{"rows":[{"id":1},{"id":2},{"id":3}]}|}
    ; {|{}|}
    ; {|{"rows":[{"id":1}],"extra":true}|}
    ]
    ~f:(fun text ->
      match S.validate checked (parse text) with
      | Ok _ -> failwith "accepted malformed input"
      | Error errors ->
        print_s
          [%sexp (List.map errors ~f:(fun error -> error.S.path) : string list list)]);
  [%expect
    {|
    ((rows 0 id))
    ((rows 0 id))
    ((rows 0 extra))
    ((rows))
    ((rows))
    ((rows))
    ((extra))
    |}]
;;

let%test_unit "nullable types, anyOf and keywords apply to their own instance types" =
  let checked =
    schema
      {|{"anyOf":[{"type":"null"},{"type":"string","minLength":2},{"type":"number","minimum":3}]}|}
  in
  List.iter [ `Null; `String "ok"; `Number "3" ] ~f:(fun v -> assert (accepts checked v));
  List.iter
    [ `True; `String "x"; `Number "2" ]
    ~f:(fun v -> assert (not (accepts checked v)));
  assert (accepts (schema {|{"type":["null","string"]}|}) `Null);
  assert (
    accepts (schema {|{"minimum":3,"minItems":2,"minLength":5,"required":["x"]}|}) `True);
  let extra = schema {|{"type":"object","additionalProperties":{"type":"integer"}}|} in
  assert (accepts extra (parse {|{"x":1,"y":2}|}));
  assert (not (accepts extra (parse {|{"x":"bad"}|})))
;;

let%test_unit "exact decimal bounds do not round large integers or tiny values" =
  let equal text =
    S.compile (`Object [ "minimum", `Number text; "maximum", `Number text ]) |> get
  in
  List.iter
    [ "9007199254740993", [ "9007199254740992"; "9007199254740994" ]
    ; "-9007199254740993", [ "-9007199254740992"; "-9007199254740994" ]
    ; "1e-1000", [ "0"; "2e-1000" ]
    ; "0", [ "-0.1"; "0.1" ]
    ; "1e1000", [ "1e999"; "1e1001" ]
    ]
    ~f:(fun (value, wrong) ->
      let checked = equal value in
      assert (accepts checked (`Number value));
      List.iter wrong ~f:(fun value -> assert (not (accepts checked (`Number value)))));
  assert (accepts (equal "100") (`Number "1.00e2"));
  assert (accepts (equal "-0") (`Number "0.0e10"));
  let integer = schema {|{"type":"integer"}|} in
  List.iter [ "1"; "1.0"; "1e10"; "10e-1"; "-0" ] ~f:(fun text ->
    assert (accepts integer (`Number text)));
  List.iter [ "1.1"; "1e-1000" ] ~f:(fun text ->
    assert (not (accepts integer (`Number text))))
;;

let%test_unit "enum and const use structural and numerical equality" =
  let checked = schema {|{"const":{"x":1,"y":[2,null]}}|} in
  assert (accepts checked (parse {|{"y":[2.0,null],"x":10e-1}|}));
  assert (not (accepts checked (parse {|{"y":[null,2],"x":1}|})));
  assert (accepts (schema {|{"enum":[null,1,{"ok":true}]}|}) (`Number "1.0"));
  assert (Result.is_error (S.of_string {|{"enum":[1,1.0]}|}));
  assert (Result.is_error (S.of_string {|{"enum":[{"x":1,"y":2},{"y":2,"x":1}]}|}))
;;

let%test_unit "string lengths count Unicode scalars and reject invalid encodings" =
  let checked = schema {|{"type":"string","minLength":1,"maxLength":1}|} in
  List.iter [ "é"; "🦀" ] ~f:(fun text -> assert (accepts checked (`String text)));
  List.iter [ ""; "ab"; "é"; "\255" ] ~f:(fun text ->
    assert (not (accepts checked (`String text))));
  assert (accepts (schema {|{"minLength":1.0,"maxLength":1e0}|}) (`String "x"))
;;

let%test_unit "unsupported and malformed schemas fail before exposure" =
  List.iter
    [ "$ref"
    ; "$schema"
    ; "pattern"
    ; "format"
    ; "oneOf"
    ; "allOf"
    ; "not"
    ; "exclusiveMinimum"
    ; "unevaluatedProperties"
    ]
    ~f:(fun keyword ->
      assert (Result.is_error (S.compile (`Object [ keyword, `String "anything" ]))));
  List.iter
    [ {|{"type":[]}|}
    ; {|{"type":["string","string"]}|}
    ; {|{"type":"unknown"}|}
    ; {|{"properties":[]}|}
    ; {|{"required":["x","x"]}|}
    ; {|{"required":[3]}|}
    ; {|{"items":[]}|}
    ; {|{"additionalProperties":0}|}
    ; {|{"enum":[]}|}
    ; {|{"anyOf":[]}|}
    ; {|{"anyOf":[{} ,0]}|}
    ; {|{"minimum":"3"}|}
    ; {|{"minItems":-1}|}
    ; {|{"maxLength":0.1}|}
    ; {|{"description":3}|}
    ; {|{"type":"string","type":"integer"}|}
    ]
    ~f:(fun text -> assert (Result.is_error (S.of_string text)));
  let checked =
    schema
      {|{"title":"tool","description":"data","$comment":"metadata","type":"boolean"}|}
  in
  assert (accepts checked `True)
;;

let%test_unit "malformed runtime JSON is rejected even with a permissive schema" =
  let checked = schema "true" in
  List.iter
    [ "nan"; "Infinity"; "01"; "+1"; "1."; ".1"; "1e"; "1e+"; " 1"; "1 "; "[[]]"; "" ]
    ~f:(fun text -> assert (not (accepts checked (`Number text))));
  assert (not (accepts checked (`Object [ "x", `Null; "x", `True ])));
  assert (not (accepts checked (`Object [ "\255", `Null ])))
;;

let%test_unit "resource limits precede parsing and branch work cannot swallow exhaustion" =
  let deep = String.make 200 '[' ^ "true" ^ String.make 200 ']' in
  assert (String.equal (error_code (S.of_string deep)) "schema.resource_limit");
  let deep_schema =
    List.fold (List.init 200 ~f:Fn.id) ~init:`True ~f:(fun value _ ->
      `Object [ "items", value ])
  in
  assert (String.equal (error_code (S.compile deep_schema)) "schema.resource_limit");
  assert (
    String.equal
      (error_code (S.of_string (String.make ((1024 * 1024) + 1) ' ')))
      "schema.resource_limit");
  assert (
    String.equal
      (error_code
         (S.validate (schema "true") (`Number ("1e" ^ Int.to_string Int.min_value))))
      "schema.resource_limit");
  assert (
    String.equal
      (error_code
         (S.validate (schema "true") (`Array (List.init 100_001 ~f:(fun _ -> `Null)))))
      "schema.resource_limit");
  let alternatives =
    List.init 20 ~f:(fun _ -> `Object [ "const", `String "x" ]) @ [ `True ]
  in
  let checked = S.compile (`Object [ "anyOf", `Array alternatives ]) |> get in
  assert (
    String.equal
      (error_code (S.validate checked (`String (String.make 100_000 'y'))))
      "schema.resource_limit");
  let integer_alternatives =
    List.init 20 ~f:(fun _ -> `Object [ "type", `String "integer" ]) @ [ `True ]
  in
  let checked = S.compile (`Object [ "anyOf", `Array integer_alternatives ]) |> get in
  assert (
    String.equal
      (error_code (S.validate checked (`Number ("0." ^ String.make 100_000 '1'))))
      "schema.resource_limit");
  (* Brackets and escaped quotes inside text are not parser nesting. *)
  ignore
    (schema
       (Jsonaf.to_string
          (`Object [ "description", `String (String.make 200 '{' ^ "\\\"") ]))
     : S.t)
;;

let%test_unit "unsatisfiable constraints compile and predictably reject values" =
  let checked = schema {|{"type":"number","minimum":4,"maximum":2}|} in
  List.iter [ "1"; "3"; "5" ] ~f:(fun number ->
    assert (not (accepts checked (`Number number))));
  let checked = schema {|{"type":"array","minItems":2,"maxItems":1}|} in
  assert (not (accepts checked (`Array [])))
;;
