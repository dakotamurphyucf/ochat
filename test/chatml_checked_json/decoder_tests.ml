open Core
module L = Chatml.Chatml_lang
module Host = Chatml_host_runtime

let require = function
  | Ok value -> value
  | Error diagnostic ->
    failwith
      (Chatml_diagnostic.render diagnostic ~label:"Decoder fixture" ~checkpoint:(fun () ->
         ()))
;;

let error = function
  | L.VVariant ("Error", [ L.VRecord fields ]) -> fields
  | _ -> failwith "expected checked decoder failure"
;;

let%expect_test "checked fields keep absence, null, shape and domain separate" =
  let library = Support.load () in
  List.iter
    [ "{}"; {|{"role":null}|}; {|{"role":false}|}; {|{"role":"other"}|} ]
    ~f:(fun wire ->
      let source =
        Printf.sprintf
          {|let main () = Task.pure(Checked_json.field(Json.parse(%S), "role", fun value ->
              Checked_json.validate(Checked_json.string(value), "Unknown role.",
                fun role -> role == "reviewer")))|}
          wire
      in
      let fields = Support.run library ~source |> require |> error in
      print_endline (Chatml.Chatml_builtin_spec.value_to_string (L.VRecord fields)));
  [%expect
    {|
    { kind = `Missing; path = [|`Field(role)|] }
    { kind = `Null; path = [|`Field(role)|] }
    { kind = `Wrong_shape({ actual = Bool; expected = string }); path = [|`Field(role)|] }
    { kind = `Invalid_value(Unknown role.); path = [|`Field(role)|] }
    |}]
;;

let%test_unit "nested paths and first duplicate lookup follow an independent wire model" =
  let library = Support.load () in
  List.iter
    [ {|{"items":[{"name":"ok"},{}]}|}, "Missing"
    ; {|{"items":[{"name":"ok"},{"name":null}]}|}, "Null"
    ; {|{"items":[{"name":"ok"},{"name":false}]}|}, "Wrong_shape"
    ]
    ~f:(fun (wire, kind) ->
      let source =
        Printf.sprintf
          {|let main () = Task.pure(Checked_json.field(Json.parse(%S), "items", fun values ->
              Checked_json.array(values, fun item -> Checked_json.field(item, "name", Checked_json.string))))|}
          wire
      in
      let fields = Support.run library ~source |> require |> error in
      (match Map.find_exn fields "path" with
       | L.VArray segments ->
         assert (
           Array.equal
             L.equal_value
             segments
             [| L.VVariant ("Field", [ L.VString "items" ])
              ; L.VVariant ("Index", [ L.VInt 1 ])
              ; L.VVariant ("Field", [ L.VString "name" ])
             |])
       | _ -> failwith "expected a path array");
      match Map.find_exn fields "kind" with
      | L.VVariant (actual, _) -> [%test_eq: string] kind actual
      | _ -> assert false);
  let source =
    {|let main () = Task.pure(Checked_json.field(Json.parse("{\"name\":\"first\",\"name\":null}"), "name", Checked_json.string))|}
  in
  [%test_eq: bool]
    true
    (L.equal_value
       (Support.run library ~source |> require)
       (L.VVariant ("Ok", [ L.VString "first" ])))
;;

let%test_unit "companion hides private helpers while keeping public aliases transparent" =
  let library = Support.load () in
  let public =
    {|let decoded : string Checked_json.checked = Checked_json.string(`String("public"))
      let problem : Checked_json.error = {path = []; kind = `Missing}
      let main () = Task.pure(match decoded with
        | `Ok(value) -> if value == "public" then Checked_json.describe(problem) == "$: Missing value." else false
        | `Error(error) -> false)|}
  in
  assert (L.equal_value (Support.run library ~source:public |> require) (L.VBool true));
  let source = "let main () = Task.pure(Checked_json.failure(`Missing))" in
  match Support.compile library ~source ~checkpoint:(fun () -> ()) () with
  | Ok _ -> assert false
  | Error diagnostic ->
    let site = Option.value_exn diagnostic.Chatml_diagnostic.site in
    let captured = Chatml_diagnostic.Site.source site in
    [%test_eq: string]
      "consumer.chatml"
      (Chatml_source.Id.relative_path (Chatml_source.id captured));
    [%test_eq: string] source (Chatml_source.contents captured);
    assert (String.is_substring diagnostic.message ~substring:"failure")
;;
