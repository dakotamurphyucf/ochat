open! Core
open Document_schema

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Error.t)]
;;

let%expect_test "ordered known object merge agrees with extension-bearing fallback" =
  let limits = Limits.default in
  let child = Shape.object_ [ "text", Shape.value ] |> ok |> Shape.nullable in
  let shape = Shape.object_ [ "first", child; "second", child ] |> ok in
  let codec =
    Domain_codec.create
      ~limits
      ~kind:"ordered.example"
      ~version:1
      ~shape
      ~supported_semantics:[]
      ~decode:Result.return
      ~encode:Result.return
    |> ok
  in
  let object_ text ~extension =
    `Object
      ([ "text", `String text ] @ if extension then [ "future", `Number "1.00" ] else [])
  in
  let compare_case ~extension first second ~reorder ~omit =
    let previous =
      [ "first", object_ "before" ~extension; "second", object_ "before" ~extension ]
    in
    let carrier ~fallback =
      let payload = `Object (previous @ if fallback then [ "fallback", `Null ] else []) in
      Document.create ~limits ~kind:"ordered.example" ~version:1 ~payload
      |> ok
      |> Domain_codec.decode codec
      |> ok
    in
    let fields = [ "first", first; "second", second ] in
    let fields = if reorder then List.rev fields else fields in
    let fields =
      if omit
      then List.filter fields ~f:(fun (name, _) -> not (String.equal name "first"))
      else fields
    in
    let encode ~fallback =
      Domain_codec.encode
        codec
        (Extension_carrier.with_value (carrier ~fallback) (`Object fields))
      |> Result.map ~f:(fun document ->
        match Document.payload document with
        | `Object fields ->
          `Object
            (List.filter fields ~f:(fun (name, _) -> not (String.equal name "fallback")))
        | payload -> payload)
    in
    let actual = encode ~fallback:false in
    let reference = encode ~fallback:true in
    assert (Result.equal Jsonaf.exactly_equal Error.equal actual reference);
    Result.is_ok actual
  in
  let cases = ref 0 in
  let successes = ref 0 in
  List.iter [ false; true ] ~f:(fun extension ->
    List.iter
      [ `Null; object_ "after" ~extension:false ]
      ~f:(fun first ->
        List.iter
          [ `Null; object_ "other" ~extension:false ]
          ~f:(fun second ->
            List.iter [ false; true ] ~f:(fun reorder ->
              List.iter [ false; true ] ~f:(fun omit ->
                incr cases;
                if compare_case ~extension first second ~reorder ~omit then incr successes)))));
  print_s [%sexp { cases = (!cases : int); successes = (!successes : int) }];
  [%expect {| ((cases 32) (successes 18)) |}]
;;
