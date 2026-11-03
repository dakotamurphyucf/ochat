open! Core
module R = Inference.Request
module Runtime = Inference_runtime
module T = Runtime_tests

let with_guidance request ~text ~provenance =
  let guidance = R.Guidance.create ~text ~provenance ~limits:T.limits |> T.ok in
  R.with_guidance request ~guidance:(Some guidance) ~limits:T.limits |> T.ok
;;

let%expect_test
    "unsupported adapters reject guidance before ordinary and session preparation"
  =
  let callbacks = ref 0 in
  let prepare ~preparation_id:_ _ =
    incr callbacks;
    Error Runtime.Preparation_error.Invalid_preparation
  in
  let adapter =
    Runtime.Adapter.create
      ~preflight_history:(fun ~target:_ _ -> Ok ())
      ~id:"synthetic"
      ~limits:Runtime.Limits.default
      ~bind:(fun _ -> Ok ())
      ~prepare
      ~open_session:(fun _ ~policy:_ ->
        Ok (Runtime.Adapter.Session_binding.create ~close:ignore ~prepare))
      ()
    |> T.ok
  in
  let original = T.request T.target in
  let request = with_guidance original ~text:"private file" ~provenance:"capture" in
  let rejected context =
    match Runtime.Context.prepare context ~preparation_id:"id" request with
    | Error Unsupported_input -> ()
    | Error _ | Ok _ -> failwith "guidance must be rejected before adapter callback"
  in
  Eio_main.run (fun _ ->
    Eio.Switch.run (fun sw ->
      let context = Runtime.Context.create adapter ~target:T.target |> T.ok in
      rejected context;
      let session = Runtime.Session.create ~sw in
      rejected (Runtime.Context.with_session context session |> T.ok);
      assert (Int.equal !callbacks 0);
      ignore (Runtime.Context.prepare context ~preparation_id:"empty" original);
      assert (Int.equal !callbacks 1)));
  print_endline
    "default deny precedes both callbacks; empty request retains existing path";
  [%expect
    {| default deny precedes both callbacks; empty request retains existing path |}]
;;

let%expect_test "prepared custody includes guidance body, presence and private provenance"
  =
  let original = T.request T.target in
  let selected = with_guidance original ~text:"exact" ~provenance:"capture:1" in
  let changed =
    [ original
    ; with_guidance original ~text:"changed" ~provenance:"capture:1"
    ; with_guidance original ~text:"exact" ~provenance:"capture:2"
    ]
  in
  List.iter changed ~f:(fun substitute ->
    let context =
      T.context
        ~supports_request_guidance:true
        ~prepared_request:(fun _ -> substitute)
        T.no_effect
    in
    match Runtime.Context.prepare context ~preparation_id:"id" selected with
    | Error Invalid_preparation -> ()
    | Error _ | Ok _ -> failwith "changed guidance must fail exact prepared custody");
  let context = T.context ~supports_request_guidance:true T.no_effect in
  assert (Result.is_ok (Runtime.Context.prepare context ~preparation_id:"exact" selected));
  print_endline "body/provenance/removal substitution rejected; exact guidance accepted";
  [%expect {| body/provenance/removal substitution rejected; exact guidance accepted |}]
;;
