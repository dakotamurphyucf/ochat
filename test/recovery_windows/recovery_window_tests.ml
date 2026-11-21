open! Core
module P = Agent_protocol
module W = P.Recovery_window
module C = W.Citation
module V = P.Recovery_item_version
module R = P.History.Content_revision

let get = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let ids = P.Id.Generator.create ~bytes:(fun n -> String.make n 'a')

let session =
  P.Session_ref.create
    ~server_id:(P.Id.Server.create_with ids)
    ~session_id:(P.Id.Session.create_with ids)
;;

let scope generation =
  W.Scope.create ~session ~generation ~window:(P.Id.Context_window.create_with ids) |> get
;;

let here = scope 0
let digest = Chatml_source.Digest.of_hex (String.make 64 'a') |> Or_error.ok_exn
let effective origin = V.effective ~origin ~digest |> get

let occurrence namespace sequence =
  History_entry.Id.create ~namespace ~sequence |> Result.ok_or_failwith
;;

let cite ?(scope = here) ?(namespace = "test") seq version =
  C.create scope ~occurrence:(occurrence namespace seq) ~version |> get
;;

let%expect_test "a replaced occurrence keeps its canonical and effective versions" =
  let occurrence = cite 1 (V.canonical R.zero) in
  let replacement =
    cite 1 (effective (Replaced { incarnation = 7L; change_id = 1; base = R.zero }))
  in
  let window = W.create here [ replacement; occurrence ] |> get in
  List.iter [ occurrence; replacement ] ~f:(fun item ->
    let ordinal = W.ordinal window item |> Option.value_exn in
    printf "%s %d\n" (P.History.Id.to_string (C.occurrence item)) ordinal);
  [%expect
    {|
    4:test:1 1
    4:test:1 0 |}]
;;

let%test_unit "order, lookup and both codecs agree for each rotation of exact evidence" =
  let versions =
    [ V.canonical R.zero
    ; V.canonical (R.succ R.zero |> get)
    ; effective (Unchanged R.zero)
    ; effective (Inserted { incarnation = 0L; change_id = 3 })
    ; effective (Replaced { incarnation = 1L; change_id = 3; base = R.zero })
    ; effective (Replaced { incarnation = 2L; change_id = 3; base = R.zero })
    ; effective (Captured { session_revision = Int64.max_value })
    ]
  in
  let members =
    List.concat_map [ "import"; "another" ] ~f:(fun namespace ->
      List.map versions ~f:(cite ~namespace 0))
  in
  for n = 0 to List.length members do
    let before, after = List.split_n members n in
    let ordered = after @ before in
    let original = W.create here ordered |> get in
    let restored = W.of_json (W.to_json original) |> get in
    assert (W.length restored = List.length ordered);
    List.iteri ordered ~f:(fun ordinal c ->
      let c = C.of_json (C.to_json c) |> get in
      assert (Option.equal C.equal (W.at restored ~ordinal) (Some c));
      assert (Option.equal Int.equal (W.ordinal restored c) (Some ordinal)));
    assert (Option.is_none (W.at restored ~ordinal:(-1)));
    assert (Option.is_none (W.at restored ~ordinal:(List.length ordered)))
  done
;;

let%test_unit "admission enforces scope, exact member uniqueness, bounds and wire shape" =
  let original = cite 1 (V.canonical R.zero) in
  let foreign = cite ~scope:(scope 1) 1 (V.canonical R.zero) in
  assert (Result.is_error (W.create here [ original; original ]));
  assert (Result.is_error (W.create here [ foreign ]));
  assert (Option.is_none (W.ordinal (W.create here [ original ] |> get) foreign));
  assert (
    Result.is_error
      (V.effective ~origin:(Inserted { incarnation = 0L; change_id = -1 }) ~digest));
  let extra =
    `Object
      [ "view", `String "canonical"
      ; "revision", R.to_json R.zero
      ; "sha256", `String "other"
      ]
  in
  assert (Result.is_error (V.of_json extra));
  let invalid_namespace = occurrence (String.make 4097 'a') 0 in
  assert (
    Result.is_error
      (C.create here ~occurrence:invalid_namespace ~version:(V.canonical R.zero)));
  let namespace = String.make 4096 '\001' in
  let members = List.init 350 ~f:(fun i -> cite ~namespace i (V.canonical R.zero)) in
  assert (Result.is_error (W.create here members))
;;

let%test_unit "a recovery range binds preparation, byte count and continuation" =
  let module Range = P.Recovery_text_range in
  let canonical = cite 0 (V.canonical R.zero) in
  let prepared = cite 0 (effective (Unchanged R.zero)) in
  List.iter
    [ canonical, false; prepared, true ]
    ~f:(fun (citation, needs_preparation) ->
      List.iter [ None; Some Int64.max_value ] ~f:(fun preparation_revision ->
        let request =
          Range.create
            citation
            ?preparation_revision
            ~part:1
            ~start:3
            ~stop:7
            ~max_bytes:4
            ()
        in
        assert (
          Bool.equal
            (Result.is_ok request)
            (Bool.equal needs_preparation (Option.is_some preparation_revision)));
        Result.iter request ~f:(fun request ->
          let wire = Range.to_json request in
          assert (
            Document_schema.Json.equal wire (Range.of_json wire |> get |> Range.to_json));
          List.iter
            [ "β", Some 5, true
            ; "β", Some 4, false
            ; "β", None, false
            ; "ββ", None, true
            ; "ββ", Some 7, false
            ; "", Some 3, false
            ; "βββ", None, false
            ]
            ~f:(fun (text, next, expected) ->
              let reply =
                `Object
                  (("text", `String text)
                   :: Option.to_list
                        (Option.map next ~f:(fun n ->
                           "next_offset", `Number (Int.to_string n))))
                |> Range.Reply.of_json
                |> get
              in
              assert (
                Bool.equal expected (Result.is_ok (Range.Reply.validate reply ~request)))))))
;;
