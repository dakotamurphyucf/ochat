open! Core
module R = Inference.Request
module P = History_entry.Payload
module Runtime = Inference_runtime
module A = Openai.Inference_adapter
module D = Openai.Responses_driver
module Codec = Openai.Responses.Codec.Request
module T = Driver_tests
module Policy = Provider_runtime_host.Profile_policy

let limits = Document_schema.Limits.default
let ok = T.unwrap

let message role text =
  `Object
    [ "type", `String "message"
    ; "role", `String role
    ; "content", `Array [ `Object [ "type", `String "input_text"; "text", `String text ] ]
    ]
;;

let entry sequence (role : P.Role.t) text =
  let semantic =
    P.Semantic.create
      (Message
         { form = Input
         ; role
         ; content = [ Text { text; annotations = []; logprobs = Absent } ]
         ; phase = Absent
         })
      ~metadata:P.Metadata.empty
    |> ok
  in
  let id = History_entry.Id.create ~namespace:"authored" ~sequence |> ok in
  History_entry.create_with_id ~id (P.authored semantic)
;;

let%expect_test
    "actual OpenAI preparation projects one User guidance prefix without history edits"
  =
  Eio_mock.Backend.run_full (fun env ->
    let net = Eio_mock.Net.make "guidance-must-not-connect" in
    let driver = D.create ~net ~clock:(Eio.Stdenv.clock env) () |> Or_error.ok_exn in
    let credentials = ref 0 in
    let settings =
      [ T.setting "instructions" (Value (`String "authored instructions")) Captured_prompt
      ]
    in
    let history =
      [ entry 0 System "authored system"
      ; entry 1 Developer "authored developer"
      ; entry 2 User "authored user"
      ]
    in
    let wire_history =
      [ message "system" "authored system"
      ; message "developer" "authored developer"
      ; message "user" "authored user"
      ]
    in
    List.iter [ Policy.Public_api; Direct_codex ] ~f:(fun route ->
      let endpoint = Policy.endpoint route in
      let caps = Policy.capabilities route ~endpoint |> Or_error.ok_exn in
      let profile = T.profile ~caps endpoint |> Policy.apply_endpoint_policy ~route in
      let target =
        A.capture_target profile ~profile_revision:None ~model:"future" ~settings ~limits
        |> ok
      in
      let adapter =
        A.create
          driver
          ~profile
          ~profile_revision:None
          ~auth:
            (Static
               (fun ~sw:_ _ ->
                 incr credentials;
                 D.Auth.bearer "not-resolved-during-preparation"))
          ~limits:Runtime.Limits.default
        |> ok
      in
      let context = Runtime.Context.create adapter ~target |> ok in
      let original = R.create ~target ~history ~tools:[] ~assets:[] ~limits |> ok in
      let prepare request =
        Runtime.Context.prepare context ~preparation_id:"guidance" request |> ok
      in
      let request ~text ~provenance =
        let guidance = R.Guidance.create ~text ~provenance ~limits |> ok in
        R.with_guidance original ~guidance:(Some guidance) ~limits |> ok
      in
      let captured = request ~text:"Untrusted file: \"完成\"\n" ~provenance:"capture:1" in
      let actual = prepare captured in
      let expected_history = message "user" "Untrusted file: \"完成\"\n" :: wire_history in
      let expected =
        D.Prepared.of_captured_settings
          ~guidance_provenance:"capture:1"
          profile
          ~model:"future"
          ~history:expected_history
          ~tools:[]
          ~settings
        |> Or_error.ok_exn
      in
      assert (
        String.equal
          (Runtime.Prepared.fingerprint actual)
          (D.Prepared.fingerprint expected));
      let wire = Codec.jsonaf_of_t (D.Prepared.request expected) in
      assert (not (String.is_substring (Jsonaf.to_string wire) ~substring:"capture:1"));
      assert (
        match Document_schema.Json.field wire ~name:"input" with
        | Value (`Array input) -> List.equal Jsonaf.exactly_equal input expected_history
        | Absent | Null | Value _ -> false);
      assert (
        match Document_schema.Json.field wire ~name:"instructions" with
        | Value (`String "authored instructions") -> true
        | Absent | Null | Value _ -> false);
      List.iter
        [ request ~text:"changed" ~provenance:"capture:1"
        ; request ~text:"Untrusted file: \"完成\"\n" ~provenance:"capture:2"
        ; original
        ]
        ~f:(fun changed ->
          assert (
            not
              (String.equal
                 (Runtime.Prepared.fingerprint actual)
                 (Runtime.Prepared.fingerprint (prepare changed)))));
      let old_path =
        D.Prepared.of_captured_settings
          profile
          ~model:"future"
          ~history:wire_history
          ~tools:[]
          ~settings
        |> Or_error.ok_exn
      in
      assert (
        String.equal
          (D.Prepared.fingerprint old_path)
          (Runtime.Prepared.fingerprint (prepare original)));
      let repeated = prepare captured in
      assert (
        String.equal
          (Runtime.Prepared.fingerprint actual)
          (Runtime.Prepared.fingerprint repeated));
      assert (
        List.equal
          History_entry.Id.equal
          (List.map history ~f:History_entry.id)
          (List.map (R.history captured) ~f:History_entry.id));
      let public =
        Inference.Observation.Configuration.to_json
          (Runtime.Prepared.configuration actual)
        |> Jsonaf.to_string
      in
      assert (not (String.is_substring public ~substring:"Untrusted file"));
      assert (not (String.is_substring public ~substring:"capture:1")));
    assert (Int.equal !credentials 0));
  print_endline
    "Public API and Direct Codex: exact prefix, unchanged authored instructions/history, \
     private identity, no I/O";
  [%expect
    {| Public API and Direct Codex: exact prefix, unchanged authored instructions/history, private identity, no I/O |}]
;;
