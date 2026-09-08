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

let prepare_with ~compile ~max_source_bytes ~scripts ~capabilities (tool : Spec.tool) =
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
  let limits = script.limits in
  let wall = Duration.to_seconds limits.wall_time in
  let%bind () =
    if String.length source > max_source_bytes
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
    then
      fail "chatml.invalid_limits" "script execution limits are outside supported bounds"
    else Ok ()
  in
  let%bind input_schema = Spec.validate_schema tool.input_schema in
  let%bind output_schema = Spec.validate_schema tool.output_schema in
  let%bind completion_schema =
    match tool.completion_schema with
    | None -> Ok None
    | Some schema -> Spec.validate_schema schema |> Result.map ~f:Option.some
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
