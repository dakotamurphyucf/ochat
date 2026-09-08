open Core
module CM = Prompt.Chat_markdown
module Spec = Chatmd_shell_spec.Extension_spec
module D = Chatmd_shell_spec.Diagnostic
module C = Tool_capability

type t =
  { elements : CM.top_level_elements list
  ; capabilities : C.t
  ; authoring : Authoring_policy.t
  ; moderators : (Spec.script * Chatml_host_runtime.compiled_script) list
  ; source_fingerprint : string
  ; fingerprint : string
  }

let elements t = t.elements
let capabilities t = t.capabilities
let authoring t = t.authoring
let moderators t = t.moderators
let source_fingerprint t = t.source_fingerprint
let fingerprint t = t.fingerprint
let error ?source code message = Error [ D.error ?source ~code message ]

let plain_message (message : CM.msg) =
  let plain (item : CM.content_item) =
    match item with
    | CM.Agent _ -> false
    | Basic item ->
      String.equal item.type_ "text"
      && Option.is_none item.image_url
      && Option.is_none item.document_url
  in
  List.mem [ "user"; "assistant"; "developer"; "system" ] message.role ~equal:String.equal
  && Option.is_none message.function_call
  && Option.is_none message.tool_call
  && Option.is_none message.tool_call_id
  && Option.is_none message.ochat_history_id
  && Option.is_none message.id
  && Option.is_none message.status
  && Option.is_none message.phase
  && (match message.type_ with
      | None | Some "message" -> true
      | _ -> false)
  &&
  match message.content with
  | None | Some (Text _) -> true
  | Some (Items items) -> List.for_all items ~f:plain
;;

let inspect elements =
  let open Result.Let_syntax in
  List.fold
    elements
    ~init:(Ok ([], [], None, 0))
    ~f:(fun state element ->
      let%bind names, scripts, context, configs = state in
      match element with
      | CM.Tool (Inherited name) -> Ok (name :: names, scripts, context, configs)
      | Tool _ ->
        error
          "delegation.tool_reconfiguration"
          "generated tools must use type=inherited; new implementations and resource \
           configuration are not inherited references"
      | Extension_script script
        when Spec.equal_script_kind script.kind Spec.Moderator_script ->
        Ok (names, script :: scripts, context, configs)
      | Extension_script _
      | Script _
      | Shell_script _
      | Shell_runtime _
      | Moderator_runtime _ ->
        error
          "delegation.execution_configuration"
          "generated definitions may contain only extensibility-v1 lifecycle moderators"
      | Authoring_help _ ->
        error
          "delegation.metadata_reconfiguration"
          "inherited authoring metadata cannot be replaced by the child"
      | Authoring_context policy when Option.is_none context ->
        Ok (names, scripts, Some policy, configs)
      | Authoring_context _ ->
        error "authoring.duplicate_policy" "only one authoring policy is permitted"
      | Config config ->
        let valid_string = function
          | None -> true
          | Some s -> (not (String.is_empty (String.strip s))) && String.length s <= 256
        in
        if
          configs > 0
          || Option.is_some config.id
          || (not (valid_string config.model && valid_string config.reasoning_effort))
          || Option.value_map config.max_tokens ~default:false ~f:(fun n -> n <= 0)
          || Option.value_map config.temperature ~default:false ~f:(fun x ->
            (not (Float.is_finite x)) || Float.(x < 0. || x > 2.))
        then
          error
            "delegation.invalid_config"
            "invalid or duplicate generation configuration; session identity is \
             host-owned"
        else Ok (names, scripts, context, configs + 1)
      | Msg message
      | Developer message
      | System message
      | User message
      | Assistant message ->
        if plain_message message
        then Ok (names, scripts, context, configs)
        else
          error
            "delegation.message_admission"
            "generated initial messages must be plain text without resource loading or \
             persisted history identities"
      | Tool_call _ | Tool_response _ | Reasoning _ ->
        error
          "delegation.history_admission"
          "generated definitions cannot inject provider tool results or persisted \
           reasoning")
  |> Result.bind ~f:(fun (names, scripts, context, _) ->
    if List.length names > 4096 || List.length scripts > 1
    then
      error
        "delegation.definition_limit"
        "too many tool references or lifecycle moderators"
    else if Option.is_some (List.find_a_dup names ~compare:String.compare)
    then error "delegation.duplicate_tool" "inherited tool references must be unique"
    else Ok (List.rev names, List.rev scripts, context))
;;

let prepare
      ?(limits = Chatml_compilation.default_limits)
      ?catalog
      ~env
      ~dir
      ~ceiling
      ~requested_names
      bundle
  =
  let open Result.Let_syntax in
  let%bind requested =
    C.select ceiling ~names:requested_names
    |> Result.map_error ~f:(fun e -> [ D.error ~code:e.code e.message ])
  in
  let%bind parsed =
    try Ok (CM.parse_source_bundle ~dir bundle) with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      error "delegation.invalid_source" (String.prefix (Exn.to_string exn) (16 * 1024))
  in
  let%bind names, scripts, context = inspect parsed.root in
  let%bind () =
    if List.is_empty parsed.agents
    then Ok ()
    else
      error
        "delegation.implicit_agent"
        "invoke an inherited agent/session tool instead of an implicit agent definition"
  in
  let%bind authoring =
    Authoring_policy.resolve_context
      ?context
      ?catalog
      ~ceiling:requested
      ~selected_names:names
      ()
    |> Result.map_error ~f:(fun e -> [ D.error ~code:e.code e.message ])
  in
  let capabilities = Authoring_policy.capabilities authoring in
  let%bind () =
    List.fold scripts ~init:(Ok ()) ~f:(fun state script ->
      let%bind () = state in
      Extension_compiler.validate_script ~max_source_bytes:limits.max_source_bytes script)
  in
  let%bind moderators =
    if
      (not (Float.is_finite limits.wall_seconds))
      || Float.(limits.wall_seconds <= 0. || limits.wall_seconds > 30.)
      || limits.max_source_bytes <= 0
      || limits.max_source_bytes > 1024 * 1024
    then error "chatml.invalid_limits" "invalid generated definition compilation limits"
    else (
      try
        Eio.Time.Timeout.run_exn
          (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) limits.wall_seconds)
          (fun () ->
             List.fold scripts ~init:(Ok []) ~f:(fun state script ->
               let%bind compiled = state in
               let%map program =
                 Chatml_compilation.compile
                   ~limits
                   ~env
                   ~target:Delegated_moderator_v1
                   ~source:(Spec.script_text script)
                   ()
                 |> Result.map_error ~f:(fun e ->
                   [ D.error ~source:script.source_ref ~code:e.code e.message ])
               in
               (script, program) :: compiled)
             |> Result.map ~f:List.rev)
      with
      | Eio.Time.Timeout ->
        error
          "chatml.compile_timeout"
          "generated definition exceeded its aggregate compilation budget")
  in
  let source_fingerprint = Chatmd_source_bundle.fingerprint bundle in
  let fingerprint =
    [%sexp
      ("ochat.generated-admission.v1" : string)
    , (source_fingerprint : string)
    , (C.fingerprint requested : string)
    , (Authoring_policy.fingerprint authoring : string)
    , (Chatml_compilation.contract Delegated_moderator_v1 : Sexp.t)]
    |> Sexp.to_string
    |> Chatmd_shell_spec.Source_ref.digest
  in
  Ok
    { elements = parsed.root
    ; capabilities
    ; authoring
    ; moderators
    ; source_fingerprint
    ; fingerprint
    }
;;
