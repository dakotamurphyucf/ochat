open Core
module C = Tool_capability
module Compiler = Chatml_compilation
module D = Chatmd_shell_spec.Diagnostic
module Source_ref = Chatmd_shell_spec.Source_ref
module Schema = Chatmd_shell_spec.Tool_schema
module Metadata = Chatmd_shell_spec.Authoring_metadata

type target =
  | One_off_script
  | Standalone_tool
  | Moderator
[@@deriving sexp, equal]

type moderator_surface =
  | Ordinary
  | Delegated
[@@deriving sexp, equal]

type host =
  { runtime_identity : string
  ; targets : target list
  ; moderator_surface : moderator_surface
  ; compilation : Compiler.limits
  }

let create_host ~runtime_identity ~targets ~moderator_surface ~compilation =
  let open Result.Let_syntax in
  let%bind () =
    Compiler.validate_limits compilation
    |> Result.map_error ~f:(fun error -> error.Compiler.message)
  in
  match
    String.is_empty runtime_identity
    || String.length runtime_identity > 256
    || not (Stdlib.String.is_valid_utf_8 runtime_identity)
  with
  | true -> Error "runtime identity must contain between 1 and 256 bytes of valid UTF-8"
  | false -> Ok { runtime_identity; targets; moderator_surface; compilation }
;;

let target_id = function
  | One_off_script -> "one_off_script"
  | Standalone_tool -> "standalone_tool"
  | Moderator -> "moderator"
;;

let entrypoint_topic = function
  | One_off_script -> "runtime.invocations.one-off"
  | Standalone_tool -> "runtime.invocations.standalone"
  | Moderator -> "runtime.invocations.moderator"
;;

let topics =
  [ "chatml.syntax.calls", "guide/chatml-language-spec.md"
  ; "chatml.types", "guide/chatml-language-spec.md"
  ; "chatml.tasks", "guide/chatml-language-spec.md"
  ; "chatmd.declarations.schemas", "agent-server/extensibility-foundations.md"
  ; "runtime.invocations.one-off", "agent-server/extensibility-foundations.md"
  ; "runtime.invocations.standalone", "agent-server/extensibility-foundations.md"
  ; "runtime.invocations.moderator", "agent-server/extensibility-foundations.md"
  ; "runtime.invocations.validation", "agent-server/extensibility-foundations.md"
  ; "runtime.authority.tool-selection", "agent-server/extensibility-foundations.md"
  ]
;;

let help target =
  let task =
    match target with
    | One_off_script -> Metadata.One_off_script
    | Standalone_tool -> Metadata.Standalone_tool
    | Moderator -> Metadata.Moderator_tool
  in
  Metadata.
    { version = 1
    ; package = "ochat.authoring." ^ Metadata.task_id task ^ ".v1"
    ; tasks = [ task ]
    ; topics =
        ([ "chatml.syntax.calls"
         ; "chatml.types"
         ; "chatml.tasks"
         ; entrypoint_topic target
         ; "runtime.authority.tool-selection"
         ; "runtime.invocations.validation"
         ]
         @
         match target with
         | Standalone_tool -> [ "chatmd.declarations.schemas" ]
         | _ -> [])
    ; required_helpers = [ Reference; Validation ]
    }
;;

let helper_metadata = Metadata.{ authoring = None; helper = Some Validation }

type diagnostic =
  { diagnostic : D.t
  ; topic_ids : string list
  }
[@@deriving sexp]

type report =
  { target : target option
  ; source : Source_ref.t option
  ; validation_id : string option
  ; compiler_contract : string option
  ; capability_fingerprint : string option
  ; runtime_identity : string
  ; diagnostics : diagnostic list
  ; checked : string list
  ; deferred : string list
  }
[@@deriving sexp]

let valid report = List.is_empty report.diagnostics
let strings xs = `Array (List.map xs ~f:(fun x -> `String x))

let optional f = function
  | None -> `Null
  | Some x -> f x
;;

let to_json report =
  `Object
    [ "version", `Number "1"
    ; "scope", `String "inline_script"
    ; ("valid", if valid report then `True else `False)
    ; "target", optional (fun t -> `String (target_id t)) report.target
    ; "source", optional Source_ref.jsonaf_of_t report.source
    ; "validation_id", optional (fun x -> `String x) report.validation_id
    ; "compiler_contract", optional (fun x -> `String x) report.compiler_contract
    ; ( "capability_fingerprint"
      , optional (fun x -> `String x) report.capability_fingerprint )
    ; "runtime_identity", `String report.runtime_identity
    ; ( "diagnostics"
      , `Array
          (List.map report.diagnostics ~f:(fun d ->
             `Object
               [ "diagnostic", D.jsonaf_of_t d.diagnostic
               ; "topic_ids", strings d.topic_ids
               ])) )
    ; "checked", strings report.checked
    ; "deferred", strings report.deferred
    ]
;;

let parameters =
  `Object
    [ "type", `String "object"
    ; ( "properties"
      , `Object
          [ "version", `Object [ "const", `Number "1" ]
          ; "target", `Object [ "type", `String "string" ]
          ; "source", `Object [ "type", `String "string" ]
          ; ( "tools"
            , `Object
                [ "type", `String "array"
                ; "maxItems", `Number "128"
                ; ( "items"
                  , `Object
                      [ "type", `String "string"
                      ; "minLength", `Number "1"
                      ; "maxLength", `Number "256"
                      ] )
                ] )
          ; "input_schema", `True
          ; "output_schema", `True
          ] )
    ; "required", strings [ "version"; "target"; "source"; "tools" ]
    ; "additionalProperties", `False
    ]
;;

let request_schema =
  match Schema.compile parameters with
  | Ok schema -> schema
  | Error _ -> failwith "invalid authoring validation request schema"
;;

type request =
  { target : target
  ; source : string
  ; tools : string list
  ; input_schema : Jsonaf.t option
  ; output_schema : Jsonaf.t option
  }

let bounded_text max_bytes text =
  let decode_prefix text =
    Utf8_ingest.add (Utf8_ingest.create ()) (String.prefix text max_bytes)
  in
  (* Drop an incomplete trailing character. A second bounded pass also confines
     replacement expansion if a compiler/host diagnostic contains malformed bytes. *)
  decode_prefix (decode_prefix text)
;;

let issue ?source ?(path = []) ~topics code message =
  let path = List.take path 16 |> List.map ~f:(bounded_text 256) in
  { diagnostic = D.error ?source ~path ~code (bounded_text 4096 message)
  ; topic_ids = topics
  }
;;

let invalid ?(path = []) message =
  Error
    [ issue
        ~path
        ~topics:[ "runtime.invocations.validation" ]
        "authoring.invalid_request"
        message
    ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind () =
    Schema.validate request_schema json
    |> Result.map_error ~f:(fun errors ->
      List.take errors 16
      |> List.map ~f:(fun error ->
        issue
          ~path:error.Schema.path
          ~topics:[ "runtime.invocations.validation" ]
          "authoring.invalid_request"
          error.message))
  in
  let%bind target =
    match Jsonaf.member_exn "target" json |> Jsonaf.string_exn with
    | "one_off_script" -> Ok One_off_script
    | "standalone_tool" -> Ok Standalone_tool
    | "moderator" -> Ok Moderator
    | _ ->
      Error
        [ issue
            ~path:[ "target" ]
            ~topics:[ "runtime.invocations.validation" ]
            "authoring.unsupported_target"
            "unsupported inline-script validation target"
        ]
  in
  let tools =
    Jsonaf.member_exn "tools" json |> Jsonaf.list_exn |> List.map ~f:Jsonaf.string_exn
  in
  let%bind () =
    match List.find_a_dup tools ~compare:String.compare with
    | None -> Ok ()
    | Some _ -> invalid ~path:[ "tools" ] "selected tool names must be unique"
  in
  let input_schema = Jsonaf.member "input_schema" json
  and output_schema = Jsonaf.member "output_schema" json in
  let%map () =
    match target, input_schema, output_schema with
    | Standalone_tool, Some _, Some _ -> Ok ()
    | Standalone_tool, _, _ -> invalid "standalone validation requires both schemas"
    | (One_off_script | Moderator), None, None -> Ok ()
    | _ -> invalid "schemas are only accepted for standalone-tool validation"
  in
  { target
  ; source = Jsonaf.member_exn "source" json |> Jsonaf.string_exn
  ; tools
  ; input_schema
  ; output_schema
  }
;;

let compiler_target host = function
  | One_off_script -> Compiler.One_off_v1
  | Standalone_tool -> Compiler.Tool_v1
  | Moderator ->
    (match host.moderator_surface with
     | Ordinary -> Compiler.Moderator_v1
     | Delegated -> Compiler.Delegated_moderator_v1)
;;

let host_fingerprint (host : host) =
  [%sexp
    ("ochat.authoring.validation.host.v1" : string)
  , (host.runtime_identity : string)
  , (host.moderator_surface : moderator_surface)
  , (host.compilation.max_source_bytes : int)
  , (host.compilation.wall_seconds : float)
  , (List.map host.targets ~f:(fun target ->
       target, Compiler.contract (compiler_target host target))
     : (target * Sexp.t) list)]
  |> Sexp.to_string
  |> Source_ref.digest
;;

let source_ref source =
  let line = ref 1
  and column = ref 0 in
  String.iter source ~f:(function
    | '\n' ->
      incr line;
      column := 0
    | _ -> incr column);
  let source_sha256 = Source_ref.digest source in
  Source_ref.
    { file = "candidate-" ^ source_sha256 ^ ".chatml"
    ; source_dir = "."
    ; prompt_dir = "."
    ; namespace = None
    ; start_pos = { offset = 0; line = 1; column = 0 }
    ; end_pos = { offset = String.length source; line = !line; column = !column }
    ; source_sha256
    }
;;

let compile_diagnostic target source (error : Compiler.error) =
  let code, message, source, topic_ids =
    match error.diagnostic with
    | None -> error.code, error.message, source, [ "runtime.invocations.validation" ]
    | Some diagnostic ->
      let source =
        match diagnostic.span with
        | None -> source
        | Some span ->
          let position (p : Source.position) : Source_ref.position =
            { offset = p.offset; line = p.line; column = p.column }
          in
          { source with
            Source_ref.start_pos = position span.left
          ; end_pos = position span.right
          }
      in
      let code, topic =
        match diagnostic.stage with
        | Parse -> "chatml.parse_error", "chatml.syntax.calls"
        | Typecheck -> "chatml.type_error", "chatml.types"
      in
      ( code
      , diagnostic.message
      , source
      , (match diagnostic.stage with
         | Parse -> [ topic; entrypoint_topic target ]
         | Typecheck -> [ topic; "chatml.syntax.calls"; entrypoint_topic target ]) )
  in
  issue ~source ~path:[ "source" ] ~topics:topic_ids code message
;;

let validate ~env ~(host : host) ~capabilities json =
  let report =
    ref
      { target = None
      ; source = None
      ; validation_id = None
      ; compiler_contract = None
      ; capability_fingerprint = None
      ; runtime_identity = host.runtime_identity
      ; diagnostics = []
      ; checked = []
      ; deferred = []
      }
  in
  let check name = report := { !report with checked = !report.checked @ [ name ] } in
  let run () =
    let open Result.Let_syntax in
    let%bind request = decode json in
    check "request";
    report := { !report with target = Some request.target };
    let%bind () =
      match List.mem host.targets request.target ~equal:equal_target with
      | true -> Ok ()
      | false ->
        Error
          [ issue
              ~path:[ "target" ]
              ~topics:[ "runtime.invocations.validation" ]
              "authoring.unavailable_target"
              "target is unavailable on this host"
          ]
    in
    let%bind () =
      match String.length request.source > host.compilation.max_source_bytes with
      | false -> Ok ()
      | true ->
        Error
          [ issue
              ~path:[ "source" ]
              ~topics:[ "runtime.invocations.validation" ]
              "chatml.source_limit"
              "script exceeds the host compiler source limit"
          ]
    in
    let source = source_ref request.source in
    report := { !report with source = Some source };
    let%bind selected =
      C.select capabilities ~names:request.tools
      |> Result.map_error ~f:(fun error ->
        [ issue
            ~path:[ "tools" ]
            ~topics:[ "runtime.authority.tool-selection" ]
            error.C.code
            error.message
        ])
    in
    check "selected_capabilities";
    let%bind () =
      List.filter_map
        [ "input_schema", request.input_schema; "output_schema", request.output_schema ]
        ~f:(fun (name, schema) -> Option.map schema ~f:(fun schema -> name, schema))
      |> List.map ~f:(fun (name, schema) ->
        Schema.compile schema
        |> Result.map ~f:(fun _ -> check name)
        |> Result.map_error ~f:(fun errors ->
          List.take errors 16
          |> List.map ~f:(fun error ->
            issue
              ~path:(name :: error.Schema.path)
              ~topics:[ "chatmd.declarations.schemas" ]
              error.code
              error.message)))
      |> Result.all
      |> Result.map ~f:(fun _ -> ())
    in
    let target = compiler_target host request.target in
    let contract = Compiler.contract target |> Sexp.to_string |> Source_ref.digest in
    let fingerprint = C.fingerprint selected in
    let schema_identity = function
      | None -> "absent"
      | Some json -> Jsonaf.to_string json |> Source_ref.digest
    in
    let identity =
      [%sexp
        ("ochat.authoring.validation.v1" : string)
      , (source.source_sha256 : string)
      , (target_id request.target : string)
      , (contract : string)
      , (fingerprint : string)
      , (host.runtime_identity : string)
      , (host_fingerprint host : string)
      , (host.compilation.max_source_bytes : int)
      , (host.compilation.wall_seconds : float)
      , (host.targets : target list)
      , (schema_identity request.input_schema : string)
      , (schema_identity request.output_schema : string)]
      |> Sexp.to_string
      |> Source_ref.digest
    in
    report
    := { !report with
         validation_id = Some identity
       ; compiler_contract = Some contract
       ; capability_fingerprint = Some fingerprint
       };
    let%map _ =
      Compiler.compile ~limits:host.compilation ~env ~target ~source:request.source ()
      |> Result.map_error ~f:(fun error ->
        [ compile_diagnostic request.target source error ])
    in
    List.iter [ "syntax"; "types"; "entrypoints" ] ~f:check;
    report
    := { !report with
         deferred =
           ([ "initializer_evaluation"
            ; "runtime_tool_calls"
            ; "current_permissions"
            ; "external_effects"
            ; "runtime_input_output"
            ; "chatmd_declarations"
            ]
            @
            match request.target with
            | Moderator -> [ "state_serialization" ]
            | _ -> [])
       }
  in
  match run () with
  | Ok () -> !report
  | Error diagnostics -> { !report with diagnostics }
;;
