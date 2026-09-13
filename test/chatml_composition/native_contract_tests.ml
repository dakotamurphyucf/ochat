open Core
open Agent_server_test_support
open Authoring_context_tests
module Catalog = Agent_session.Extensibility_native_tools
module Corpus = Authoring_corpus
module Digest = Chatmd_shell_spec.Source_ref

(* Bind the public description/schema/result convention, not a host-dependent
   implementation revision or a randomly generated reference-service key. *)
let descriptor (registration : Chat_response.Agent_runtime.native_registration) =
  let info = registration.implementation.info in
  let function_ = info.function_ in
  `Object
    [ "type", `String info.type_
    ; "name", `String function_.name
    ; ( "description"
      , Option.value_map function_.description ~default:`Null ~f:(fun s -> `String s) )
    ; "parameters", function_.parameters
    ; ("strict", if function_.strict then `True else `False)
    ; ( "result_contract"
      , `String
          (match registration.result_contract with
           | Native_output -> "native_output"
           | Invocation_v1 -> "invocation_v1") )
    ]
;;

let declarations names =
  List.map names ~f:(fun name -> Prompt.Chat_markdown.Tool (Builtin name))
;;

let all_registrations env host =
  Catalog.registrations
    ~env
    ~elements:(declarations Catalog.names)
    ~one_off_policy:(Some Chat_response.One_off_request.default_policy)
    ~authoring_validation_host:(Some host)
  |> protocol_ok
;;

let documentation name =
  let shared = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ] in
  match name with
  | "run_chatml" -> "runtime.native.requests", shared
  | "agent_create" -> "runtime.delegation.creation", shared
  | "agent_status" | "agent_send" -> "runtime.delegation.submissions", shared
  | "agent_read" | "agent_wait" -> "runtime.delegation.output", shared
  | "agent_stop" -> "runtime.delegation.stop-helper", shared
  | "ochat_validate" -> "runtime.native.requests", shared
  | "ochat_authoring_context" -> "authoring.reference", shared
  | name -> failwith ("native tool needs a reviewed reference mapping: " ^ name)
;;

let%expect_test "runtime native catalog contracts remain paired with reviewed references" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let sources = Authoring_sources.installed () |> Result.ok_or_failwith in
    let corpus = Corpus.runtime_foundation ~sources |> Result.ok_or_failwith in
    let registrations = all_registrations env (host ()) in
    let another_host = all_registrations env (host ~runtime:"different-runtime" ()) in
    List.iter2_exn registrations another_host ~f:(fun left right ->
      require_json (descriptor left) (descriptor right));
    assert (
      List.equal
        String.equal
        Catalog.names
        (List.map registrations ~f:(fun r -> r.implementation.info.function_.name)));
    List.iter registrations ~f:(fun registration ->
      let name = registration.implementation.info.function_.name in
      let topic_id, surface_ids = documentation name in
      let closures =
        List.map surface_ids ~f:(fun surface_id ->
          let digest =
            Corpus.Coverage.topic_contract corpus ~surface_id ~topic_id
            |> Result.ok_or_failwith
          in
          surface_id, digest)
      in
      let api = descriptor registration |> Jsonaf.to_string |> Digest.digest in
      let reference =
        [%sexp (closures : (string * string) list)] |> Sexp.to_string |> Digest.digest
      in
      print_s
        [%sexp (name : string), (topic_id : string), (api : string), (reference : string)]));
  [%expect
    {|
    (run_chatml runtime.native.requests
     cedc80aa5b6589d7acbe45078be963eaecb5058bfe69daf19bf896e668d68985
     b5be857191687d720a8f68514679407a824c6603f01038ca783c9b16180fd16f)
    (agent_create runtime.delegation.creation
     f50060c1bdd22759d368f8962e1f21f2f347fdcc2bb5383d392d99025f5dd664
     6a6783530e01cfe406cedecad6cb9642437cbce0ba98545af12c2504f74aa57f)
    (agent_status runtime.delegation.submissions
     c948e863b277175b367e349d6edd845405843173ab2845a17dd220a0bcbe1273
     2f8d1fd0a800aa31b2795aeecda363415d68bd80fa1bf7928f09a4821d673e25)
    (agent_send runtime.delegation.submissions
     ebd399228691c35671afa1fe353623284f6eabd788c95748e7e6b38f3ebdbf0e
     2f8d1fd0a800aa31b2795aeecda363415d68bd80fa1bf7928f09a4821d673e25)
    (agent_read runtime.delegation.output
     7b7763f30e055fcc0a81a0578b08d75c7d90ccc5b7e524ead6e99b450fe8d946
     f28bd62a783a22ef96071060afdfd1c6e0bf28af60997033cfe5030d60d73f36)
    (agent_wait runtime.delegation.output
     88196ca1f4dc34e34441e8687ffff3557e8e48215205e4cdcd0303b50a2be722
     f28bd62a783a22ef96071060afdfd1c6e0bf28af60997033cfe5030d60d73f36)
    (agent_stop runtime.delegation.stop-helper
     d10ebc8001ca0ea8e709be0a4ee5a3d64de024e9fcff086774aba2463a9d1a6d
     f3eb8929a24eb98735ec67a35507117fe598732b6b2c68ef7bb8a6931e485c89)
    (ochat_validate runtime.native.requests
     f151690e0f4692e8aaf9aa6b4454b1306d8bcf1f4432c6759fecf5cbb1cce4f8
     b5be857191687d720a8f68514679407a824c6603f01038ca783c9b16180fd16f)
    (ochat_authoring_context authoring.reference
     5af75d5c259034e3e5ed36b55716575412a4b6633fc35b771a260e4ef4a3a9f3
     7684a3aa2f943911039a35d1430b4922b9383c015a1293dbaa2c18e370dcb7f7)
    |}]
;;

let json_requests ?(tool = "ochat_authoring_context") text =
  let rec scan acc = function
    | [] -> List.rev acc
    | opening :: rest when String.equal opening ("```json tool=" ^ tool) ->
      let body, rest =
        List.split_while rest ~f:(fun line -> not (String.equal line "```"))
      in
      (match rest with
       | [] -> failwith "unclosed documentation request"
       | _ :: rest -> scan (Jsonaf.of_string (String.concat ~sep:"\n" body) :: acc) rest)
    | _ :: rest -> scan acc rest
  in
  scan [] (String.split_lines text)
;;

let%expect_test "native request guide executes and validates through the actual tools" =
  let sources = Authoring_sources.installed () |> Result.ok_or_failwith in
  let document =
    Authoring_sources.document sources ~path:"guide/chatml-native-requests.md"
    |> Result.ok_or_failwith
  in
  let execution = json_requests ~tool:"run_chatml" document.text in
  let validation = json_requests ~tool:"ochat_validate" document.text in
  let calls =
    List.mapi execution ~f:(fun i request ->
      "execute-" ^ Int.to_string i, "run_chatml", request)
    @ List.mapi validation ~f:(fun i request ->
      "validate-" ^ Int.to_string i, "ochat_validate", request)
  in
  Fixtures.with_daemon
    ~sources:
      [ ( "agent.chatmd"
        , {|<developer>Check the exact installed requests.</developer><tool name="run_chatml"/><tool name="ochat_validate"/>|}
        )
      ]
    ~calls
    (fun state ->
       List.iteri execution ~f:(fun i _ ->
         match Fixtures.result state ("execute-" ^ Int.to_string i) with
         | Agent_protocol.Invocation.Complete (`Number n) ->
           assert (Float.equal (Float.of_string n) 10.)
         | outcome -> raise_s [%sexp (outcome : Agent_protocol.Invocation.outcome)]);
       List.iteri validation ~f:(fun i request ->
         let report =
           match Fixtures.result state ("validate-" ^ Int.to_string i) with
           | Agent_protocol.Invocation.Complete (`String text) -> Jsonaf.of_string text
           | outcome -> raise_s [%sexp (outcome : Agent_protocol.Invocation.outcome)]
         in
         require_json (field request "target") (field report "target");
         require_json `True (field report "valid");
         require_json (`Array []) (field report "diagnostics");
         match field report "validation_id", field report "deferred" with
         | `String _, `Array (_ :: _) -> ()
         | _ -> failwith (Jsonaf.to_string report)));
  printf
    "%d computation request, %d target-specific validation requests, including an inert \
     failing initializer: native paths pass\n"
    (List.length execution)
    (List.length validation);
  [%expect
    {| 1 computation request, 4 target-specific validation requests, including an inert failing initializer: native paths pass |}]
;;

let%expect_test "installed query examples and the native self-reference are usable" =
  let sources = Authoring_sources.installed () |> Result.ok_or_failwith in
  let corpus = Corpus.runtime_foundation ~sources |> Result.ok_or_failwith in
  let topic = Corpus.topic corpus ~id:"authoring.reference" |> Result.ok_or_failwith in
  let source = String.concat ~sep:"\n" (List.map topic.fragments ~f:(fun f -> f.text)) in
  assert (not (String.is_substring source ~substring:"Embedded.start"));
  assert (not (String.is_substring source ~substring:"authoring-default-tokens"));
  let service =
    Q.create ~secret:"native-contract-doc-example-key" () |> Result.ok_or_failwith
  in
  let query =
    Q.query service ~host:(host ()) ~capabilities:(capabilities ()) ~scope:"doc-examples"
  in
  let rec finish remaining response =
    assert (remaining > 0);
    assert (not (has_error response));
    match field response "next_cursor" with
    | `Null -> require_json `True (field response "complete")
    | `String cursor ->
      finish (remaining - 1) (query (request ~cursor ~max_tokens:32000 "continue"))
    | _ -> failwith "invalid continuation response"
  in
  let examples = json_requests source in
  List.iter examples ~f:(fun example -> finish 100 (query example));
  let requested = request ~task:"child_agent" ~topic_id:"authoring.reference" "topic" in
  Fixtures.with_daemon
    ~sources:
      [ ( "agent.chatmd"
        , {|<developer>Learn the reference API.</developer><tool name="ochat_authoring_context"/>|}
        )
      ]
    ~calls:
      [ "self-reference", "ochat_authoring_context", requested
      ; ( "root-capabilities"
        , "ochat_authoring_context"
        , request ~task:"child_agent" ~topic_id:"chatmd.capabilities" "topic" )
      ]
    (fun state ->
       let response =
         match Fixtures.result state "self-reference" with
         | Agent_protocol.Invocation.Complete (`String text) -> Jsonaf.of_string text
         | outcome -> raise_s [%sexp (outcome : Agent_protocol.Invocation.outcome)]
       in
       assert (not (has_error response));
       require_json `True (field response "complete");
       let text = Authoring_compaction_tests.content [ response ] in
       let lines text =
         String.split_lines text
         |> List.filter ~f:(fun line -> not (String.is_empty line))
       in
       (* The service normalizes paragraph separators. Compare the source content,
          including both intact JSON examples, rather than its trailing whitespace. *)
       assert (List.equal String.equal (lines source) (lines text));
       let root_response =
         match Fixtures.result state "root-capabilities" with
         | Agent_protocol.Invocation.Complete (`String text) -> Jsonaf.of_string text
         | outcome -> raise_s [%sexp (outcome : Agent_protocol.Invocation.outcome)]
       in
       assert (not (has_error root_response));
       require_json `True (field root_response "complete");
       let root_text =
         items root_response
         |> List.filter_map ~f:(fun item ->
           match Jsonaf.member "topic_id" item, Jsonaf.member "text" item with
           | Some (`String "chatmd.capabilities"), Some (`String text) -> Some text
           | _ -> None)
         |> String.concat ~sep:"\n"
       in
       let root_source =
         Authoring_sources.document sources ~path:"guide/chatmd-authoring-capabilities.md"
         |> Result.ok_or_failwith
       in
       assert (List.equal String.equal (lines root_source.text) (lines root_text)));
  printf
    "%d installed strict request examples paginate successfully; native lookup returns \
     all three reviewed sections and root capability guidance without resource execution\n"
    (List.length examples);
  [%expect
    {| 2 installed strict request examples paginate successfully; native lookup returns all three reviewed sections and root capability guidance without resource execution |}]
;;
