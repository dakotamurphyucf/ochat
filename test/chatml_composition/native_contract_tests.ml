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
  | "recurrence_create"
  | "recurrence_list"
  | "recurrence_get"
  | "recurrence_edit"
  | "recurrence_pause"
  | "recurrence_resume"
  | "recurrence_cancel"
  | "recurrence_status"
  | "recurrence_history" -> "runtime.recurrence", shared
  | "run_chatml" -> "runtime.native.requests", shared
  | "agent_create" -> "runtime.delegation.creation", shared
  | "agent_status" | "agent_send" -> "runtime.delegation.submissions", shared
  | "agent_read" | "agent_wait" -> "runtime.delegation.output", shared
  | "agent_stop" -> "runtime.delegation.stop-helper", shared
  | "ochat_validate" -> "runtime.native.requests", shared
  | "ochat_authoring_context" -> "authoring.reference", shared
  | "instructions_refresh"
  | "instructions_read"
  | "instructions_activate"
  | "instructions_deactivate"
  | "instructions_status" -> "instructions.sources", shared
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
     f1f3f713b6b4e6e7e1606945763d52931f2ad7a60546e3257bada7ffc62c5643)
    (agent_create runtime.delegation.creation
     f50060c1bdd22759d368f8962e1f21f2f347fdcc2bb5383d392d99025f5dd664
     bc5e9d61e6673a40997087f793e27cfa3afbb0af45899af45902b7b2757d1ff4)
    (agent_status runtime.delegation.submissions
     c948e863b277175b367e349d6edd845405843173ab2845a17dd220a0bcbe1273
     897c182c482a85450eb37608e4be303fee02e650075818822aa8ba0a338bdc67)
    (agent_send runtime.delegation.submissions
     ebd399228691c35671afa1fe353623284f6eabd788c95748e7e6b38f3ebdbf0e
     897c182c482a85450eb37608e4be303fee02e650075818822aa8ba0a338bdc67)
    (agent_read runtime.delegation.output
     7b7763f30e055fcc0a81a0578b08d75c7d90ccc5b7e524ead6e99b450fe8d946
     44e3790eeda33594c0819333e9ffd1cb53d85d342c3baf6473265cbf2c731f4e)
    (agent_wait runtime.delegation.output
     88196ca1f4dc34e34441e8687ffff3557e8e48215205e4cdcd0303b50a2be722
     44e3790eeda33594c0819333e9ffd1cb53d85d342c3baf6473265cbf2c731f4e)
    (agent_stop runtime.delegation.stop-helper
     d10ebc8001ca0ea8e709be0a4ee5a3d64de024e9fcff086774aba2463a9d1a6d
     356a16bf8785d533f9e795ba86f496e0a617d37bebb1dd301161fcfeddf753a1)
    (ochat_validate runtime.native.requests
     f151690e0f4692e8aaf9aa6b4454b1306d8bcf1f4432c6759fecf5cbb1cce4f8
     f1f3f713b6b4e6e7e1606945763d52931f2ad7a60546e3257bada7ffc62c5643)
    (ochat_authoring_context authoring.reference
     ac6b50652da6d40561b29b87dfd1709eafb31d728d863d3f81e02b4590116647
     c9fb4ef833b03ebbb428ba83cf856712054a2cf7dfe1608493d8b303b0f2c268)
    (instructions_refresh instructions.sources
     7e7602e9a7cb3b07316c036607484ab66493d061de0c46b7471657213eb9ecce
     a75d305fe5c228d85ab983e21d5c5d93721b08ba4715d815a9a2da0f3b4ac123)
    (instructions_read instructions.sources
     b5d7c4847415ebd291f9b37e7bdfe38589c599c530ea102cc8ee7e6b063298eb
     a75d305fe5c228d85ab983e21d5c5d93721b08ba4715d815a9a2da0f3b4ac123)
    (instructions_activate instructions.sources
     1a65c90884f6f002b1e2938e144b247f94f6b4af6be21441aaec42031ff92bb8
     a75d305fe5c228d85ab983e21d5c5d93721b08ba4715d815a9a2da0f3b4ac123)
    (instructions_deactivate instructions.sources
     eec6ebbe4b66d02cdd865047b63acf37f5c0b59b431a8ed1ff5f3d876485c12d
     a75d305fe5c228d85ab983e21d5c5d93721b08ba4715d815a9a2da0f3b4ac123)
    (instructions_status instructions.sources
     9aa238203e9443b0a8d51f41421fd455b18291a8b306ff76b209a5ae6e2c9934
     a75d305fe5c228d85ab983e21d5c5d93721b08ba4715d815a9a2da0f3b4ac123)
    (recurrence_create runtime.recurrence
     a640749b03237f585387cee9c6562cc9cc2560af1d4f67cf8de1c115ff7379f7
     02012d0e153775779d3ed4d3e479b6967d9608db5ce5d1b0b629dd7ecde8feef)
    (recurrence_list runtime.recurrence
     e8f0ec7896e58af08b6bfabfda3cecfdd86dd0a0c5f42e90cb55cf4a57d5d373
     02012d0e153775779d3ed4d3e479b6967d9608db5ce5d1b0b629dd7ecde8feef)
    (recurrence_get runtime.recurrence
     1c229dd8219650ff274cc633733ff25ac9d8d554679c4c2f366a3578070f1187
     02012d0e153775779d3ed4d3e479b6967d9608db5ce5d1b0b629dd7ecde8feef)
    (recurrence_edit runtime.recurrence
     e99cde739818601145d3bc52fcd59e78aa887d2cb26057b9e4748403e8b95c04
     02012d0e153775779d3ed4d3e479b6967d9608db5ce5d1b0b629dd7ecde8feef)
    (recurrence_pause runtime.recurrence
     0eb6efea54e64cd2cda3dce586ae6eda25c975729cb1191b7a203c3c965163d9
     02012d0e153775779d3ed4d3e479b6967d9608db5ce5d1b0b629dd7ecde8feef)
    (recurrence_resume runtime.recurrence
     7d67f87b0f58acfe6a244831ae0fbfc313e03b730712a26dc2de64728c90a299
     02012d0e153775779d3ed4d3e479b6967d9608db5ce5d1b0b629dd7ecde8feef)
    (recurrence_cancel runtime.recurrence
     b5956b32ae20efff038163ef6800ea9372dc100907aecc970e18014ff29ccd02
     02012d0e153775779d3ed4d3e479b6967d9608db5ce5d1b0b629dd7ecde8feef)
    (recurrence_status runtime.recurrence
     64d91fd2c4056dd2c6ba89471c6cacb7715ad5df458171c12ab808042591ce23
     02012d0e153775779d3ed4d3e479b6967d9608db5ce5d1b0b629dd7ecde8feef)
    (recurrence_history runtime.recurrence
     341d5ee94a789d80e3aacac5609ba64fccaba4ee5e625a0c3647dea5840a8d33
     02012d0e153775779d3ed4d3e479b6967d9608db5ce5d1b0b629dd7ecde8feef)
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
