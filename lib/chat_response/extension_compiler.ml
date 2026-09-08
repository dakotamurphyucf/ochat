open Core
module Spec = Chatmd_shell_spec.Extension_spec
module D = Chatmd_shell_spec.Diagnostic
module X = Chatml.Chatml_extension_surface
module S = Chatmd_shell_spec.Chatmd_script_spec
module Duration = Chatmd_shell_spec.Duration

type t =
  { declaration : Spec.tool
  ; program : Chatml_host_runtime.compiled_script
  ; capabilities : Tool_capability.t
  ; fingerprint : string
  ; input_schema : Chatmd_shell_spec.Tool_schema.t
  ; output_schema : Chatmd_shell_spec.Tool_schema.t
  ; completion_schema : Chatmd_shell_spec.Tool_schema.t option
  }

let declaration t = t.declaration
let program t = t.program
let capabilities t = t.capabilities
let fingerprint t = t.fingerprint
let input_schema t = t.input_schema
let output_schema t = t.output_schema
let completion_schema t = t.completion_schema

let validate_script ~max_source_bytes (script : Spec.script) =
  let fail code message = Error [ D.error ~source:script.source_ref ~code message ] in
  let source = Spec.script_text script in
  let limits = script.limits in
  let wall = Duration.to_seconds limits.wall_time in
  if script.version <> 1 || max_source_bytes <= 0 || max_source_bytes > 1024 * 1024
  then fail "chatml.invalid_contract" "unsupported script version or source limit"
  else if String.length source > max_source_bytes
  then fail "chatml.source_limit" "script source exceeds the configured byte limit"
  else if
    not (String.equal script.source_sha256 (Chatmd_shell_spec.Source_ref.digest source))
  then fail "chatml.source_mismatch" "script bytes do not match their retained digest"
  else if
    (not (Float.is_finite wall))
    || Float.(wall <= 0. || wall > 60.)
    || limits.fuel <= 0
    || limits.fuel > 10_000_000
    || limits.max_tasks <= 0
    || limits.max_tasks > 100_000
    || limits.max_depth <= 0
    || limits.max_depth > 128
    || limits.max_array_items <= 0
    || limits.max_array_items > 100_000
    || Int64.(
         Duration.bytes_to_int64 limits.max_value_bytes <= 0L
         || Duration.bytes_to_int64 limits.max_value_bytes > 8_388_608L)
    || Int64.(
         Duration.bytes_to_int64 limits.max_output_bytes <= 0L
         || Duration.bytes_to_int64 limits.max_output_bytes > 1_048_576L)
  then fail "chatml.invalid_limits" "script execution limits are outside supported bounds"
  else Ok ()
;;

let prepare_with
      ?(validate_schema = Spec.validate_schema)
      ~compile
      ~max_source_bytes
      ~scripts
      ~capabilities
      (tool : Spec.tool)
  =
  let open Result.Let_syntax in
  let fail code message = Error [ D.error ~source:tool.source_ref ~code message ] in
  let%bind () =
    if tool.version <> 1 || max_source_bytes <= 0 || max_source_bytes > 1024 * 1024
    then fail "chatml.invalid_contract" "unsupported extension version or source limit"
    else if
      Option.is_some
        (List.find_a_dup
           (List.map scripts ~f:(fun script -> script.Spec.id))
           ~compare:String.compare)
    then fail "chatml.duplicate_script" "script IDs must be unique"
    else Ok ()
  in
  let%bind id, kind, target, selected =
    match tool.implementation with
    | Moderator id when List.is_empty tool.uses ->
      Ok (id, Spec.Moderator_script, Chatml_compilation.Moderator_v1, capabilities)
    | Standalone { script; entrypoint = "run" } ->
      Tool_capability.select capabilities ~names:tool.uses
      |> Result.map_error ~f:(fun error ->
        [ D.error ~source:tool.source_ref ~code:error.code error.message ])
      |> Result.map ~f:(fun selected ->
        script, Spec.Tool_script, Chatml_compilation.Tool_v1, selected)
    | _ -> fail "chatml.invalid_binding" "incompatible tool binding or entrypoint"
  in
  let%bind script =
    match List.find scripts ~f:(fun script -> String.equal script.Spec.id id) with
    | Some script when script.version = 1 && Spec.equal_script_kind script.kind kind ->
      Ok script
    | _ ->
      fail "chatml.missing_handler" "required versioned handler script is unavailable"
  in
  let source = Spec.script_text script in
  let%bind () = validate_script ~max_source_bytes script in
  let%bind input_schema = validate_schema tool.input_schema in
  let%bind output_schema = validate_schema tool.output_schema in
  let%bind completion_schema =
    match tool.completion_schema with
    | None -> Ok None
    | Some schema -> validate_schema schema |> Result.map ~f:Option.some
  in
  let%bind program =
    compile ~target ~source
    |> Result.map_error ~f:(fun (error : Chatml_compilation.error) ->
      [ D.error ~source:script.source_ref ~code:error.code error.message ])
  in
  let fingerprint =
    [%sexp
      ("ochat.extension-compiler.v2" : string)
    , (Chatml_compilation.contract target : Sexp.t)
    , (Chatmd_shell_spec.Tool_schema.dialect : string)
    , (tool : Spec.tool)
    , (script : Spec.script)
    , (Tool_capability.fingerprint selected : string)]
    |> Sexp.to_string
    |> Chatmd_shell_spec.Source_ref.digest
  in
  Ok
    { declaration = tool
    ; program
    ; capabilities = selected
    ; fingerprint
    ; input_schema
    ; output_schema
    ; completion_schema
    }
;;

let prepare ?(max_source_bytes = 256 * 1024) ~scripts ~capabilities tool =
  let compile ~target ~source =
    let surface, required_bindings =
      match target with
      | Chatml_compilation.One_off_v1 -> X.one_off_v1, X.one_off_entrypoints
      | Tool_v1 -> X.tool_v1, X.tool_entrypoints
      | Moderator_v1 -> X.moderator_v1, X.moderator_entrypoints
      | Delegated_moderator_v1 -> X.delegated_moderator_v1, X.moderator_entrypoints
    in
    Chatml_host_runtime.compile_script ~surface ~required_bindings ~source ()
    |> Result.map_error ~f:(fun message ->
      Chatml_compilation.{ code = "chatml.invalid_handler"; message })
  in
  prepare_with ~compile ~max_source_bytes ~scripts ~capabilities tool
;;

let prepare_isolated
      ?(limits = Chatml_compilation.default_limits)
      ~env
      ~worker
      ~scripts
      ~capabilities
      tool
  =
  let compile ~target ~source =
    Chatml_compilation.compile ~limits ~env ~worker ~target ~source ()
  in
  prepare_with
    ~compile
    ~max_source_bytes:limits.max_source_bytes
    ~scripts
    ~capabilities
    tool
;;

type definition =
  { prepared_tools : t list
  ; compiled_scripts : (Spec.script * Chatml_host_runtime.compiled_script) list
  ; definition_fingerprint : string
  }

let prepared_tools definition = definition.prepared_tools
let compiled_scripts definition = definition.compiled_scripts
let definition_fingerprint definition = definition.definition_fingerprint

let prepare_definition_isolated
      ?(limits = Chatml_compilation.default_limits)
      ~env
      ~worker
      ~capabilities
      elements
  =
  let module CM = Prompt.Chat_markdown in
  let open Result.Let_syntax in
  let fail code message = Error [ D.error ~code message ] in
  let%bind () =
    if
      (not (Float.is_finite limits.wall_seconds))
      || Float.(limits.wall_seconds <= 0. || limits.wall_seconds > 30.)
      || limits.max_source_bytes <= 0
      || limits.max_source_bytes > 1024 * 1024
    then fail "chatml.invalid_limits" "invalid definition compilation limits"
    else if List.length elements > 16_384
    then fail "chatml.definition_limit" "too many definition elements"
    else Ok ()
  in
  let scripts =
    List.filter_map elements ~f:(function
      | CM.Extension_script script -> Some script
      | _ -> None)
  in
  let tools =
    List.filter_map elements ~f:(function
      | CM.Tool (Extension tool) -> Some tool
      | _ -> None)
  in
  let%bind () =
    if List.length scripts > 128 || List.length tools > 4096
    then fail "chatml.definition_limit" "too many extension scripts or tools"
    else Ok ()
  in
  let%bind _ =
    try Ok (CM.validate_declarations elements) with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      fail "chatml.invalid_definition" (String.prefix (Exn.to_string exn) (16 * 1024))
  in
  let source_bytes = ref 0 in
  let sources = Hash_set.create (module String) in
  let consume text =
    if Hash_set.mem sources text
    then Ok ()
    else if String.length text > (8 * 1024 * 1024) - !source_bytes
    then
      fail
        "chatml.definition_limit"
        "definition exceeds distinct source/schema byte budget"
    else (
      Hash_set.add sources text;
      source_bytes := !source_bytes + String.length text;
      Ok ())
  in
  let schema_cache = Hashtbl.create (module String) in
  let validate_schema (schema : Spec.schema) =
    let%bind () = consume schema.source_text in
    if
      not
        (String.equal
           schema.source_sha256
           (Chatmd_shell_spec.Source_ref.digest schema.source_text))
    then Spec.validate_schema schema
    else (
      match Hashtbl.find schema_cache schema.source_text with
      | Some compiled -> Ok compiled
      | None ->
        let%map compiled = Spec.validate_schema schema in
        Hashtbl.set schema_cache ~key:schema.source_text ~data:compiled;
        compiled)
  in
  let%bind () =
    List.fold scripts ~init:(Ok ()) ~f:(fun result script ->
      let%bind () = result in
      let%bind () = consume (Spec.script_text script) in
      validate_script ~max_source_bytes:limits.max_source_bytes script)
  in
  let%bind () =
    List.fold tools ~init:(Ok ()) ~f:(fun result tool ->
      let%bind () = result in
      List.fold
        (tool.Spec.input_schema
         :: tool.output_schema
         :: Option.to_list tool.completion_schema)
        ~init:(Ok ())
        ~f:(fun result schema ->
          let%bind () = result in
          Result.map (validate_schema schema) ~f:(fun _ -> ())))
  in
  let cache = Hashtbl.create (module String) in
  let compile ~target ~source =
    let key = Sexp.to_string (Chatml_compilation.sexp_of_target target) ^ ":" ^ source in
    match Hashtbl.find cache key with
    | Some compiled -> Ok compiled
    | None ->
      let%map compiled =
        Chatml_compilation.compile ~limits ~env ~worker ~target ~source ()
      in
      Hashtbl.set cache ~key ~data:compiled;
      compiled
  in
  try
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) limits.wall_seconds (fun () ->
      let%bind compiled_scripts =
        List.fold scripts ~init:(Ok []) ~f:(fun result script ->
          let%bind compiled = result in
          let target =
            match script.Spec.kind with
            | Moderator_script -> Chatml_compilation.Moderator_v1
            | Tool_script -> Tool_v1
          in
          compile ~target ~source:(Spec.script_text script)
          |> Result.map ~f:(fun program -> (script, program) :: compiled)
          |> Result.map_error ~f:(fun error ->
            [ D.error ~source:script.source_ref ~code:error.code error.message ]))
        |> Result.map ~f:List.rev
      in
      let%map prepared_tools =
        List.fold tools ~init:(Ok []) ~f:(fun result tool ->
          let%bind prepared = result in
          let id =
            match tool.Spec.implementation with
            | Moderator id -> id
            | Standalone { script; _ } -> script
          in
          (* Registry validation established the script identity and kind.
             Reuse that exact compiled program without hashing the source again
             for every tool bound to a shared handler. *)
          let program =
            List.find_map_exn compiled_scripts ~f:(fun (script, program) ->
              Option.some_if (String.equal script.Spec.id id) program)
          in
          let%map tool =
            prepare_with
              ~validate_schema
              ~compile:(fun ~target:_ ~source:_ -> Ok program)
              ~max_source_bytes:limits.max_source_bytes
              ~scripts
              ~capabilities
              tool
          in
          tool :: prepared)
        |> Result.map ~f:List.rev
      in
      let definition_fingerprint =
        [%sexp
          ("ochat.extension-definition.v1" : string)
        , (scripts : Spec.script list)
        , (List.map prepared_tools ~f:fingerprint : string list)
        , (Tool_capability.fingerprint capabilities : string)
        , (Chatml_compilation.contract Tool_v1 : Sexp.t)
        , (Chatml_compilation.contract Moderator_v1 : Sexp.t)]
        |> Sexp.to_string
        |> Chatmd_shell_spec.Source_ref.digest
      in
      { prepared_tools; compiled_scripts; definition_fingerprint })
  with
  | Eio.Time.Timeout ->
    fail "chatml.compile_timeout" "definition exceeded its aggregate compilation budget"
;;
