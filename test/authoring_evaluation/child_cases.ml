open Core
open Runner
module H = Execution_host
module P = Agent_protocol
module V = Chat_response.Authoring_validation
module Q = Agent_session.Generated_session_request

let field = Execution_cases.field
let creation candidate = field candidate "create"

let definition_contract ~env bundle =
  let module CM = Prompt.Chat_markdown in
  let root_file = Chatmd_source_bundle.root_file bundle in
  let sources = Chatmd_source_bundle.sources bundle in
  let parsed = CM.parse_source_bundle ~dir:(Eio.Stdenv.cwd env) bundle in
  let configured =
    List.exists parsed.root ~f:(function
      | Config { model = Some model; reasoning_effort = Some reasoning; _ } ->
        not (String.is_empty model || String.is_empty reasoning)
      | _ -> false)
  in
  let instructions elements =
    List.filter_map elements ~f:(function
      | CM.Developer _ as item ->
        Some (CM.sexp_of_top_level_elements item |> Sexp.to_string)
      | _ -> None)
  in
  (* Replacing captured companions with harmless whitespace must change the
     effective developer instructions. A merely attached, unused file does not
     demonstrate imported instructions. Parse failures do not count as proof. *)
  let without_companions =
    Chatmd_source_bundle.create
      ~root_file
      ~sources:
        (List.map sources ~f:(fun (path, text) ->
           ( path
           , match String.equal path root_file with
             | true -> text
             | false -> "" )))
      ()
    |> Result.ok_or_failwith
  in
  let imported_instructions =
    match CM.parse_source_bundle ~dir:(Eio.Stdenv.cwd env) without_companions with
    | stripped ->
      (not (List.is_empty (instructions parsed.root)))
      && not
           (List.equal
              String.equal
              (instructions parsed.root)
              (instructions stripped.root))
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception _ -> false
  in
  match configured && imported_instructions with
  | true -> Valid
  | false ->
    Invalid
      (Semantics, "specify model/reasoning and import companion developer instructions")
;;

let validate ~env ~host ~capabilities candidate =
  match candidate with
  | `Object fields
    when List.equal
           String.equal
           (List.map fields ~f:fst |> List.sort ~compare:String.compare)
           [ "create"; "read"; "send" ] ->
    (match Q.decode ~limits:Chatmd_source_bundle.default_limits (creation candidate) with
     | Error error -> Invalid (Semantics, error.message)
     | Ok request ->
       (match request.tools, request.lifetime, request.start_immediately with
        | [ "read_file" ], Owned, true ->
          let validation =
            V.validate
              ~env
              ~host
              ~capabilities
              (`Object
                  [ "version", `Number "1"
                  ; "target", `String "generated_chatmd"
                  ; "root_file", field (creation candidate) "root_file"
                  ; "sources", field (creation candidate) "sources"
                  ; "tools", field (creation candidate) "tools"
                  ])
            |> Reference_backend.classification
          in
          (match validation with
           | Invalid _ -> validation
           | Valid -> definition_contract ~env request.bundle)
        | _ ->
          Invalid (Capability, "expected an owned, running child selecting only read_file")))
  | _ -> Invalid (Semantics, "expected exactly create, send and read fields")
;;

let rec substitute replacements = function
  | `String text as value ->
    List.Assoc.find replacements text ~equal:String.equal |> Option.value ~default:value
  | `Array values -> `Array (List.map values ~f:(substitute replacements))
  | `Object fields ->
    `Object (List.map fields ~f:(fun (key, value) -> key, substitute replacements value))
  | value -> value
;;

let answer ~id text =
  let module R = Openai.Responses in
  let message : R.Output_message.t =
    { role = Assistant
    ; id
    ; status = "completed"
    ; content = [ { annotations = []; text; _type = "output_text" } ]
    ; phase = None
    ; _type = "message"
    }
  in
  let item = R.Response_stream.Item.Output_message message in
  [ R.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Output_text_delta
      { item_id = id
      ; output_index = 0
      ; content_index = 0
      ; delta = text
      ; type_ = "response.output_text.delta"
      }
  ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
  ]
  |> Stdlib.List.to_seq
;;

let parent_marker = "EVALUATION-LIFECYCLE-PARENT"

let input_text = function
  | Openai.Responses.Item.Input_message { content; _ } ->
    List.filter_map content ~f:(function
      | Text { text; _ } -> Some text
      | _ -> None)
    |> String.concat ~sep:"\n"
  | Output_message { content; _ } ->
    List.map content ~f:(fun part -> part.Openai.Responses.Output_message.text)
    |> String.concat ~sep:"\n"
  | _ -> ""
;;

let execute ~env candidate =
  let pending = ref None in
  let parent_requests = ref 0 in
  let child_requests = ref 0 in
  let child_reads = ref 0 in
  let post_stream ~sw:_ ~inputs =
    match
      List.exists inputs ~f:(fun item -> String.equal (input_text item) parent_marker)
    with
    | true ->
      incr parent_requests;
      (match !pending with
       | None -> Stdlib.Seq.empty
       | Some call ->
         pending := None;
         H.call_events [ call ])
    | false ->
      incr child_requests;
      (match List.last inputs with
       | Some (Openai.Responses.Item.Function_call_output { output = Text text; _ }) ->
         H.require
           (String.is_substring text ~substring:"EVIDENCE-CONTENT")
           "child did not obtain evidence through the inherited reader";
         incr child_reads;
         answer
           ~id:(sprintf "child-answer-%d" !child_reads)
           (sprintf "review-%d:EVIDENCE-CONTENT" !child_reads)
       | _ ->
         (match !child_reads with
          | 0 -> ()
          | _ ->
            H.require
              (List.exists inputs ~f:(fun item ->
                 String.is_substring
                   (input_text item)
                   ~substring:"review-1:EVIDENCE-CONTENT"))
              "child follow-up lost its prior assistant output");
         H.call_events
           [ ( sprintf "child-read-%d" !child_requests
             , "read_file"
             , `Object [ "root", `String "ledgers"; "file", `String "evidence.txt" ] )
           ])
  in
  let sources =
    [ ( "agent.chatmd"
      , "<developer>"
        ^ parent_marker
        ^ "</developer>\n"
        ^ "<authoring_context policy=\"manual\"/>\n"
        ^ Execution_cases.read_declaration
        ^ "\n\
           <tool name=\"agent_create\"/><tool name=\"agent_send\"/><tool \
           name=\"agent_read\"/>"
        ^ "<tool name=\"agent_wait\"/><tool name=\"agent_stop\"/>" )
    ]
  in
  match
    H.with_session
      ~durable:true
      ~env
      ~sources
      ~workspace_files:[ "evidence.txt", "EVIDENCE-CONTENT" ]
      ~post_stream
      (fun ~workspace:_ embedded ->
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
           let serial = ref 0 in
           let invoke name args =
             incr serial;
             let id = sprintf "lifecycle-%d" !serial in
             let expected_requests = !parent_requests + 2 in
             pending := Some (id, name, args);
             ignore
               (H.request
                  embedded
                  (Session_send_message
                     { session_id = H.session_id embedded
                     ; attachment_id = (H.attachment embedded).id
                     ; content =
                         { kind = Plain_text
                         ; text = "Execute lifecycle step."
                         ; attachments = []
                         }
                     ; idempotency_key = P.Idempotency_key.of_string id |> H.get
                     })
                : P.Method_result.t);
             let rec wait () =
               let snapshot = H.snapshot embedded in
               Option.iter snapshot.failure ~f:(fun e -> raise (H.Protocol_error e));
               match
                 !parent_requests >= expected_requests
                 && Option.is_none snapshot.session.active_operation
               with
               | false ->
                 Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                 wait ()
               | true ->
                 H.require
                   (!parent_requests = expected_requests)
                   "extra parent model turn";
                 (match H.outcome snapshot id with
                  | Complete value -> value
                  | outcome ->
                    raise
                      (H.Scenario_failure
                         (Sexp.to_string_hum (P.Invocation.sexp_of_outcome outcome))))
             in
             wait ()
           in
           let created = invoke "agent_create" (creation candidate) in
           let session_id = field created "session_id" in
           let target = [ "session_id", session_id ] in
           let first_page = invoke "agent_read" (`Object target) in
           let cursor = ref (field first_page "next_cursor") in
           List.iter [ 1; 2 ] ~f:(fun index ->
             let replacements =
               [ "$session_id", session_id
               ; "$cursor", !cursor
               ; "$message", `String (sprintf "Review evidence, round %d." index)
               ; "$key", `String (sprintf "review-round-%d" index)
               ]
             in
             let sent =
               invoke "agent_send" (substitute replacements (field candidate "send"))
             in
             let receipt = field sent "receipt_id" in
             let waited =
               invoke
                 "agent_wait"
                 (`Object
                     (target @ [ "receipt_id", receipt; "timeout_ms", `Number "10000" ]))
             in
             H.require
               (Jsonaf.exactly_equal (field waited "reason") (`String "receipt_terminal"))
               "child receipt never completed";
             let page =
               invoke "agent_read" (substitute replacements (field candidate "read"))
             in
             let text = Jsonaf.to_string page in
             H.require
               (String.is_substring
                  text
                  ~substring:(sprintf "review-%d:EVIDENCE-CONTENT" index))
               "new child output missing";
             (match index with
              | 2 ->
                H.require
                  (not (String.is_substring text ~substring:"review-1:EVIDENCE-CONTENT"))
                  "read repeated old output instead of using its cursor"
              | _ -> ());
             cursor := field page "next_cursor");
           H.require (!child_reads = 2) "expected one native evidence read per child turn";
           ignore
             (invoke
                "agent_stop"
                (`Object
                    (target
                     @ [ "mode", `String "cancel"
                       ; "idempotency_key", `String "review-stop"
                       ]))
              : Jsonaf.t);
           let rec stopped () =
             match
               H.request
                 embedded
                 (Session_get
                    { session_id = P.Id.Session.of_json session_id |> H.get
                    ; history = None
                    })
             with
             | Session_get snapshot ->
               (match snapshot.session.desired_state, snapshot.session.observed_state with
                | Stopped, Stopped -> ()
                | _ ->
                  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                  stopped ())
             | _ -> failwith "unexpected child snapshot response"
           in
           stopped ();
           Passed))
  with
  | result -> result
  | exception H.Scenario_failure message -> Failed (Semantics, message)
  | exception Eio.Time.Timeout ->
    Failed (Infrastructure, "child evaluation exceeded its host deadline")
;;
