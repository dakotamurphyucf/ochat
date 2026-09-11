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
  | Generated_chatmd
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
  ; bundle_limits : Chatmd_source_bundle.limits
  ; catalog : Authoring_policy.catalog option
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
  | false ->
    Ok
      { runtime_identity
      ; targets
      ; moderator_surface
      ; compilation
      ; bundle_limits = Chatmd_source_bundle.default_limits
      ; catalog = None
      }
;;

let for_delegated host = { host with moderator_surface = Delegated }
let compilation_limits host = host.compilation
let bundle_limits host = host.bundle_limits
let catalog host = host.catalog

let configure_generated host ~limits ~catalog =
  let open Result.Let_syntax in
  let%map _ =
    Chatmd_source_bundle.create
      ~limits
      ~root_file:"root.chatmd"
      ~sources:[ "root.chatmd", "" ]
      ()
  in
  { host with bundle_limits = limits; catalog }
;;

let target_id = function
  | One_off_script -> "one_off_script"
  | Standalone_tool -> "standalone_tool"
  | Moderator -> "moderator"
  | Generated_chatmd -> "generated_chatmd"
;;

let entrypoint_topic = function
  | One_off_script -> "runtime.invocations.one-off"
  | Standalone_tool -> "runtime.invocations.standalone"
  | Moderator -> "runtime.invocations.moderator"
  | Generated_chatmd -> "runtime.delegation.generated"
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
  ; "runtime.delegation.generated", "agent-server/extensibility-foundations.md"
  ]
;;

let help target =
  let task =
    match target with
    | One_off_script -> Metadata.One_off_script
    | Standalone_tool -> Metadata.Standalone_tool
    | Moderator -> Metadata.Moderator_tool
    | Generated_chatmd -> Metadata.Child_agent
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
         | Standalone_tool | Generated_chatmd -> [ "chatmd.declarations.schemas" ]
         | _ -> [])
        (* These execution contracts do not call a helper. Auto/preload policy adds
       reference and validation tools; manual policy leaves their exposure to the
       author. Custom tools may still declare genuine required dependencies. *)
    ; required_helpers = []
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
    ; ( "scope"
      , `String
          (match report.target with
           | Some Generated_chatmd -> "generated_bundle"
           | _ -> "inline_script") )
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

let inline_parameters =
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
  match Schema.compile inline_parameters with
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
  | Generated_chatmd -> Compiler.Delegated_moderator_v1
;;

let host_fingerprint (host : host) =
  [%sexp
    ("ochat.authoring.validation.host.v1" : string)
  , (host.runtime_identity : string)
  , (host.moderator_surface : moderator_surface)
  , (host.compilation.max_source_bytes : int)
  , (host.compilation.wall_seconds : float)
  , (host.bundle_limits.max_source_bytes : int)
  , (host.bundle_limits.max_bundle_bytes : int)
  , (host.bundle_limits.max_files : int)
  , (Option.map host.catalog ~f:Authoring_policy.catalog_fingerprint : string option)
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

let validate_inline ~env ~(host : host) ~capabilities json =
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

let generated_parameters =
  `Object
    [ "type", `String "object"
    ; ( "properties"
      , `Object
          [ "version", `Object [ "const", `Number "1" ]
          ; "target", `Object [ "const", `String "generated_chatmd" ]
          ; "root_file", `Object [ "type", `String "string"; "maxLength", `Number "1024" ]
          ; ( "sources"
            , `Object
                [ "type", `String "array"
                ; "maxItems", `Number "256"
                ; ( "items"
                  , `Object
                      [ "type", `String "object"
                      ; ( "properties"
                        , `Object
                            [ ( "path"
                              , `Object
                                  [ "type", `String "string"
                                  ; "maxLength", `Number "1024"
                                  ] )
                            ; "text", `Object [ "type", `String "string" ]
                            ] )
                      ; "required", strings [ "path"; "text" ]
                      ; "additionalProperties", `False
                      ] )
                ] )
          ; ( "tools"
            , Jsonaf.member_exn "tools" (Jsonaf.member_exn "properties" inline_parameters)
            )
          ] )
    ; "required", strings [ "version"; "target"; "root_file"; "sources"; "tools" ]
    ; "additionalProperties", `False
    ]
;;

(* Keep an object-shaped tool schema; target-specific required/forbidden fields
   are validated again by the service, including duplicate keys. *)
let parameters =
  let fields schema = Jsonaf.member_exn "properties" schema |> Jsonaf.assoc_list_exn in
  let common = fields inline_parameters in
  let generated =
    List.filter (fields generated_parameters) ~f:(fun (key, _) ->
      not (List.Assoc.mem common key ~equal:String.equal))
  in
  `Object
    [ "type", `String "object"
    ; "properties", `Object (common @ generated)
    ; "required", strings [ "version"; "target"; "tools" ]
    ; "additionalProperties", `False
    ]
;;

let generated_schema =
  Schema.compile generated_parameters
  |> Result.map_error ~f:(fun _ -> "invalid generated validation schema")
  |> Result.ok_or_failwith
;;

let validate_generated ~env ~(host : host) ~capabilities json =
  let report =
    ref
      { target = Some Generated_chatmd
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
  let diagnostic ?source ?(path = []) code message =
    issue
      ?source
      ~path
      ~topics:[ "runtime.delegation.generated"; "runtime.authority.tool-selection" ]
      code
      message
  in
  let run () =
    let open Result.Let_syntax in
    let%bind () =
      Schema.validate generated_schema json
      |> Result.map_error ~f:(fun errors ->
        List.take errors 16
        |> List.map ~f:(fun error ->
          diagnostic ~path:error.Schema.path "authoring.invalid_request" error.message))
    in
    check "request";
    let%bind () =
      match List.mem host.targets Generated_chatmd ~equal:equal_target with
      | true -> Ok ()
      | false ->
        Error
          [ diagnostic
              "authoring.unavailable_target"
              "generated bundle validation is unavailable on this host"
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
    let root_file = Jsonaf.member_exn "root_file" json |> Jsonaf.string_exn in
    let sources =
      Jsonaf.member_exn "sources" json
      |> Jsonaf.list_exn
      |> List.map ~f:(fun item ->
        ( Jsonaf.member_exn "path" item |> Jsonaf.string_exn
        , Jsonaf.member_exn "text" item |> Jsonaf.string_exn ))
    in
    let%bind bundle =
      Chatmd_source_bundle.create ~limits:host.bundle_limits ~root_file ~sources ()
      |> Result.map_error ~f:(fun message ->
        [ diagnostic "delegation.invalid_source" message ])
    in
    check "bounded_source_bundle";
    let source =
      { (source_ref (List.Assoc.find_exn sources root_file ~equal:String.equal)) with
        Source_ref.file = root_file
      }
    in
    report := { !report with source = Some source };
    let%bind selected =
      C.select capabilities ~names:tools
      |> Result.map_error ~f:(fun e ->
        [ diagnostic ~path:[ "tools" ] e.C.code e.message ])
    in
    check "selected_capabilities";
    let contract =
      [%sexp
        ("ochat.generated-validation.v1" : string)
      , (Compiler.contract Compiler.Delegated_moderator_v1 : Sexp.t)]
      |> Sexp.to_string
      |> Source_ref.digest
    in
    let selection = C.fingerprint selected in
    let identity =
      [%sexp
        ("ochat.authoring.generated-validation.v1" : string)
      , (Chatmd_source_bundle.fingerprint bundle : string)
      , (selection : string)
      , (contract : string)
      , (host_fingerprint host : string)]
      |> Sexp.to_string
      |> Source_ref.digest
    in
    report
    := { !report with
         validation_id = Some identity
       ; compiler_contract = Some contract
       ; capability_fingerprint = Some selection
       };
    let%map admission =
      Generated_admission.prepare
        ~limits:host.compilation
        ?catalog:host.catalog
        ~env
        ~dir:(Eio.Stdenv.cwd env)
        ~ceiling:selected
        ~requested_names:tools
        bundle
      |> Result.map_error ~f:(fun errors ->
        List.take errors 16
        |> List.map ~f:(fun error ->
          issue
            ?source:error.D.source
            ~path:error.path
            ~topics:
              [ "runtime.delegation.generated"
              ; "chatmd.declarations.schemas"
              ; "chatml.syntax.calls"
              ; "chatml.types"
              ; "runtime.authority.tool-selection"
              ]
            error.code
            error.message))
    in
    List.iter
      [ "captured_source_closure"
      ; "chatmd_declarations"
      ; "inherited_bindings"
      ; "authoring_policy"
      ; "delegated_surface"
      ; "syntax"
      ; "types"
      ; "entrypoints"
      ]
      ~f:check;
    let fingerprint = C.fingerprint (Generated_admission.capabilities admission) in
    report
    := { !report with
         capability_fingerprint = Some fingerprint
       ; deferred =
           [ "initializer_evaluation"
           ; "state_serialization"
           ; "runtime_tool_calls"
           ; "current_permissions"
           ; "external_effects"
           ; "runtime_input_output"
           ; "session_creation"
           ; "parent_moderation"
           ; "lifetime_and_revocation"
           ]
       }
  in
  match run () with
  | Ok () -> !report
  | Error diagnostics -> { !report with diagnostics }
;;

let validate ~env ~host ~capabilities json =
  match Jsonaf.member "target" json with
  | Some (`String "generated_chatmd") -> validate_generated ~env ~host ~capabilities json
  | _ -> validate_inline ~env ~host ~capabilities json
;;
