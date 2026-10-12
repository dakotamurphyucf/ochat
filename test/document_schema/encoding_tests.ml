open! Core
module D = Document_schema
module E = D.Encoding

let limits ~bytes ~nodes ~fields ~depth =
  D.Limits.create ~max_bytes:bytes ~max_nodes:nodes ~max_fields:fields ~max_depth:depth
  |> Result.map_error ~f:(fun _ -> "invalid fixture limits")
  |> Result.ok_or_failwith
;;

let%test_unit
    "bounded builder measures complete envelope and raw-subtree siblings before \
     construction"
  =
  (* Independent literal/shape oracle:12 nodes,9 fields, depth5; raw subtree
     extra counts in the same total as items, rather than receiving new limits. *)
  let wire =
    {|{"format":"ochat.document","schema_version":1,"kind":"encoding.test","payload":{"items":[{"n":0},{"n":1}],"extra":{"s":"x"}}}|}
  in
  let bytes = String.length wire in
  let run limits =
    let constructed = ref false in
    let result =
      E.render ~limits ~byte_limit_context:"fixture bytes" ~checkpoint:ignore ~f:(fun t ->
        (match E.mode t with
         | Measure -> ()
         | Construct -> constructed := true);
        E.object_
          t
          [ ("format", fun t -> E.string t "ochat.document")
          ; ("schema_version", fun t -> E.number t 1)
          ; ("kind", fun t -> E.string t "encoding.test")
          ; ( "payload"
            , fun t ->
                E.object_
                  t
                  [ ( "items"
                    , fun t ->
                        E.array t [ 0; 1 ] ~f:(fun t n ->
                          E.object_ t [ ("n", fun t -> E.number t n) ]) )
                  ; ("extra", fun t -> E.raw t (`Object [ "s", `String "x" ]))
                  ] )
          ])
    in
    result, !constructed
  in
  let exact = limits ~bytes ~nodes:12 ~fields:9 ~depth:5 in
  let json, constructed = run exact in
  assert constructed;
  assert (
    String.equal
      (json
       |> Result.map_error ~f:(fun _ -> "exact fixture rejected")
       |> Result.ok_or_failwith
       |> Jsonaf.to_string)
      wire);
  List.iter
    [ limits ~bytes:(bytes - 1) ~nodes:12 ~fields:9 ~depth:5
    ; limits ~bytes ~nodes:11 ~fields:9 ~depth:5
    ; limits ~bytes ~nodes:12 ~fields:8 ~depth:5
    ; limits ~bytes ~nodes:12 ~fields:9 ~depth:4
    ]
    ~f:(fun bounds ->
      let result, constructed = run bounds in
      assert (Result.is_error result && not constructed))
;;

let%test_unit
    "bounded builder admits hex expansion before constructor phase and preserves \
     checkpoint identity"
  =
  let construct = ref false in
  let encoded = {|{"bytes":"00ff"}|} in
  let f t =
    (match E.mode t with
     | Measure -> ()
     | Construct -> construct := true);
    E.object_ t [ ("bytes", fun t -> E.hex t "\000\255") ]
  in
  let exact = limits ~bytes:(String.length encoded) ~nodes:2 ~fields:1 ~depth:2 in
  let run limits checkpoint =
    E.render ~limits ~byte_limit_context:"fixture bytes" ~checkpoint ~f
  in
  let value =
    run exact ignore
    |> Result.map_error ~f:(fun _ -> "exact hex rejected")
    |> Result.ok_or_failwith
  in
  assert (String.equal (Jsonaf.to_string value) encoded);
  construct := false;
  assert (
    Result.is_error
      (run (limits ~bytes:(String.length encoded - 1) ~nodes:2 ~fields:1 ~depth:2) ignore));
  assert (not !construct);
  let exception Stop of int ref in
  let identity = ref 1 in
  match run exact (fun () -> raise (Stop identity)) with
  | exception Stop actual -> assert (phys_equal identity actual)
  | Ok _ | Error _ -> assert false
;;
