open Core
open Runner
module C = Chat_response.Tool_capability
module EC = Chat_response.Extension_compiler
module M = Chat_response.Moderator_manager
module P = Agent_protocol
module I = P.Invocation

let input_schema =
  Jsonaf.of_string
    {|{"type":"object","required":["text"],"properties":{"text":{"type":"string"}},"additionalProperties":false}|}
;;

(* A real, pure host registration for the evaluation's selected digest tool.
   No shell, filesystem, model or Process callback exists in this host. *)
let capabilities ~on_call =
  Mirage_crypto_rng_unix.use_default ();
  let module Definition = struct
    type input = string

    let name = "digest"
    let description = Some "Return the SHA-256 hex digest of the UTF-8 text field."
    let type_ = "function"
    let parameters = input_schema

    let input_of_string text =
      Jsonaf.of_string text |> Jsonaf.member_exn "text" |> Jsonaf.string_exn
    ;;
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:true
      (fun text ->
         on_call text;
         Openai.Responses.Tool_output.Output.Text
           (Chatmd_shell_spec.Source_ref.digest text))
  in
  C.create
    ~owner:"digest-evaluation"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "pure digest only")
    [ Chatmd_shell_spec.Source_ref.digest "digest-evaluation-sha256-v1", implementation ]
  |> Result.map_error ~f:(fun e -> e.C.message)
  |> Result.ok_or_failwith
;;

let validate ~env ~host ~capabilities candidate =
  match
    Moderator_cases.binding_validation ~id:"digest_owner" ~name:"hash" ~env candidate
  with
  | Invalid _ as failed -> failed
  | Valid ->
    Chat_response.Authoring_validation.validate
      ~env
      ~host
      ~capabilities
      (`Object
          [ "version", `Number "1"
          ; "target", `String "moderator"
          ; "source", Execution_cases.field candidate "source"
          ; "tools", `Array [ `String "digest" ]
          ])
    |> Reference_backend.classification
;;

let execute ~env candidate =
  let module H = Execution_host in
  match
    Moderator_cases.binding_validation ~id:"digest_owner" ~name:"hash" ~env candidate
  with
  | Invalid (kind, message) -> Failed (kind, message)
  | Valid ->
    let calls = ref [] in
    let caps = capabilities ~on_call:(fun text -> calls := text :: !calls) in
    let parsed =
      Chatmd_source_bundle.create
        ~root_file:"agent.chatmd"
        ~sources:(Moderator_cases.sources ~id:"digest_owner" candidate)
        ()
      |> Result.ok_or_failwith
      |> Prompt.Chat_markdown.parse_source_bundle ~dir:(Eio.Stdenv.cwd env)
    in
    (match
       EC.prepare_definition_in_domain
         ~delegated_moderator:true
         ~env
         ~capabilities:caps
         parsed.root
     with
     | Error diagnostics ->
       Failed
         ( Semantics
         , String.concat
             ~sep:"\n"
             (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string) )
     | Ok definition ->
       let _, artifact =
         M.Registry.of_definition M.Registry.empty definition |> Result.ok_or_failwith
       in
       let manager =
         M.create_entries
           ~env
           ~artifact:(Option.value_exn artifact)
           ~capabilities:Chat_response.Moderation.Capabilities.default
           ~allocator:
             (History_entry.Allocator.create
                ~namespace:"digest-evaluation"
                ~next_sequence:0
              |> Result.ok_or_failwith)
           ()
         |> Result.ok_or_failwith
       in
       let prepared = List.hd_exn (EC.prepared_tools definition) in
       let session_id = P.Id.Session.create () in
       let texts = [ ""; "abc"; "Unicode Ω and 😀" ] in
       let results =
         List.map texts ~f:(fun text ->
           let invocation =
             I.create
               { id = P.Id.Invocation.create ()
               ; session_id
               ; generation = 0
               ; origin = Model
               ; provider_call_id =
                   Some ("digest-" ^ Chatmd_shell_spec.Source_ref.digest text)
               ; call_entry_id = None
               ; parent_invocation = None
               ; parent_job = None
               ; tool_name = "hash"
               ; implementation_revision = EC.fingerprint prepared
               ; capability_fingerprint = C.fingerprint (EC.capabilities prepared)
               ; input = `Object [ "text", `String text ]
               ; created_at = P.Timestamp.now ()
               ; deadline = None
               }
             |> H.get
             |> I.dispatch
             |> H.get
           in
           let before = List.length !calls in
           let result =
             M.handle_invocation_entries
               manager
               ~invocation
               ~history:[]
               ~available_tools:[]
               ~session_meta:`Null
               ~now_ms:0
               ~validate_work:(fun _ -> Error "no background work in digest evaluation")
               ~on_tool_call:(fun ~name ~args ->
                 match C.find caps ~name with
                 | Error e -> Error e.message
                 | Ok binding ->
                   (match C.native_implementation binding with
                    | None -> Error "digest host has no managed dependencies"
                    | Some implementation ->
                      let schema =
                        Chatmd_shell_spec.Tool_schema.compile
                          (C.reference binding).input_schema
                        |> Result.map_error ~f:(fun errors ->
                          Sexp.to_string_hum
                            [%sexp
                              (errors : Chatmd_shell_spec.Tool_schema.diagnostic list)])
                        |> Result.ok_or_failwith
                      in
                      (match Chatmd_shell_spec.Tool_schema.validate schema args with
                       | Error _ -> Error "invalid digest input"
                       | Ok () ->
                         (match implementation.run (Jsonaf.to_string args) with
                          | Openai.Responses.Tool_output.Output.Text result ->
                            Ok (Tool_ok (`String result))
                          | _ -> Error "digest returned a non-text result"))))
               ~prepare_resolution:(fun ~resolved:_ ~outcome:_ ~snapshot:_ ->
                 Ok (M.memory_commit ignore))
           in
           match result with
           | Ok ({ status = Resolved (Complete (`String result)); _ }, _)
             when String.equal result (Chatmd_shell_spec.Source_ref.digest text)
                  && List.length !calls = before + 1
                  && Option.exists (List.hd !calls) ~f:(String.equal text) -> Passed
           | Ok _ ->
             Failed
               ( Semantics
               , "digest must be called once with the original text and its output \
                  preserved" )
           | Error message -> Failed (Semantics, message))
       in
       List.find results ~f:(function
         | Passed -> false
         | _ -> true)
       |> Option.value ~default:Passed)
;;
