open Core
module Spec = Chatmd_shell_spec.Extension_spec
module S = Chatmd_shell_spec.Chatmd_script_spec
module D = Chatmd_shell_spec.Diagnostic
module R = Chatmd_shell_spec.Source_ref
module A = Chatmd_attributes
module Ast = Chatmd_ast

exception Invalid of D.t list

let get = function
  | Ok value -> value
  | Error error -> raise_notrace (Invalid [ error ])
;;

let many = function
  | Ok value -> value
  | Error errors -> raise_notrace (Invalid errors)
;;

let protect f =
  try Ok (f ()) with
  | Invalid errors -> Error errors
;;

let fail source path code message =
  raise_notrace (Invalid [ D.error ~source ~path ~code message ])
;;

let attrs source path allowed raw = A.create ~source ~path ~allowed raw |> get
let required attributes name = A.required attributes name |> get
let optional attributes name = A.optional attributes name |> get

let empty source path children =
  List.iter children ~f:(function
    | Ast.Text text when String.for_all text ~f:Char.is_whitespace -> ()
    | _ ->
      fail
        source
        path
        "chatmd.extension_unexpected_content"
        "declaration requires empty content")
;;

let load loader source_node source path reference =
  let resolved =
    match Source_loader.resolve_within_root loader ~base:source_node ~reference with
    | Ok value -> value
    | Error message -> fail source path "chatmd.extension_source_unavailable" message
  in
  let text =
    match Source_loader.read_bounded ~max_bytes:(1024 * 1024) loader resolved with
    | Ok text -> text
    | Error _ ->
      fail
        source
        path
        "chatmd.extension_source_unavailable"
        "extension source unavailable or exceeds byte limit"
  in
  resolved, text
;;

let schema loader source_node source name reference =
  let resolved, text = load loader source_node source [ "tool"; name ] reference in
  let position : R.position = { offset = 0; line = 1; column = 1 } in
  let schema_source =
    R.create
      ~file:(Source_loader.relative_path resolved)
      ~source_dir:(Eio.Path.native_exn (Source_loader.materialized_dir resolved))
      ~prompt_dir:source.R.prompt_dir
      ~namespace:source.namespace
      ~start_pos:position
      ~end_pos:{ position with offset = String.length text }
      ~source:text
  in
  let value : Spec.schema =
    { path = reference
    ; source_text = text
    ; source_sha256 = R.digest text
    ; source_ref = schema_source
    }
  in
  ignore (many (Spec.validate_schema value) : Chatmd_shell_spec.Tool_schema.t);
  value
;;

let valid_tool_name name =
  String.length name > 0
  && String.length name <= 64
  && String.for_all name ~f:(function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false)
;;

let tool ~loader ~source_node ~source node =
  protect (fun () ->
    match node with
    | Ast.Element (Ast.Tool, raw, children) ->
      let attributes =
        attrs
          source
          [ "tool" ]
          [ "type"
          ; "name"
          ; "description"
          ; "moderator"
          ; "script"
          ; "entrypoint"
          ; "input_schema"
          ; "output_schema"
          ; "completion_schema"
          ]
          raw
      in
      let name = required attributes "name" in
      if not (valid_tool_name name)
      then
        fail
          source
          [ "tool"; "name" ]
          "chatmd.extension_invalid_name"
          "tool name requires 1..64 ASCII letters, digits, underscores or hyphens";
      let implementation =
        match
          ( required attributes "type"
          , optional attributes "moderator"
          , optional attributes "script"
          , optional attributes "entrypoint" )
        with
        | "moderator", Some id, None, None when not (String.is_empty (String.strip id)) ->
          Spec.Moderator (R.qualify source id)
        | "chatml", None, Some id, Some "run" when not (String.is_empty (String.strip id))
          -> Standalone { script = R.qualify source id; entrypoint = "run" }
        | _ ->
          fail
            source
            [ "tool" ]
            "chatmd.extension_conflicting_binding"
            "moderator tools require moderator only; chatml tools require script and \
             entrypoint=run"
      in
      let uses =
        List.filter_map children ~f:(function
          | Ast.Text text when String.for_all text ~f:Char.is_whitespace -> None
          | Ast.Element (Ast.Uses, raw, children) ->
            let attributes = attrs source [ "tool"; "uses" ] [ "tool" ] raw in
            empty source [ "tool"; "uses" ] children;
            let tool = required attributes "tool" in
            if not (valid_tool_name tool)
            then
              fail
                source
                [ "tool"; "uses" ]
                "chatmd.extension_invalid_capability"
                "uses must name an exact registered tool";
            Some tool
          | _ ->
            fail
              source
              [ "tool" ]
              "chatmd.extension_unexpected_content"
              "only uses declarations are permitted")
      in
      if Option.is_some (List.find_a_dup uses ~compare:String.compare)
      then
        fail
          source
          [ "tool"; "uses" ]
          "chatmd.extension_duplicate_capability"
          "duplicate selected tool";
      (match implementation with
       | Moderator _ when not (List.is_empty uses) ->
         fail
           source
           [ "tool"; "uses" ]
           "chatmd.extension_invalid_capability"
           "moderator tools use their owning moderator's configured capabilities"
       | _ -> ());
      (* Check every attribute before reading dependencies. *)
      let input = required attributes "input_schema"
      and output = required attributes "output_schema" in
      let completion = optional attributes "completion_schema"
      and description = optional attributes "description" in
      { Spec.version = 1
      ; name
      ; description
      ; implementation
      ; uses
      ; source_ref = source
      ; input_schema = schema loader source_node source "input_schema" input
      ; output_schema = schema loader source_node source "output_schema" output
      ; completion_schema =
          Option.map completion ~f:(schema loader source_node source "completion_schema")
      }
    | _ ->
      fail source [ "tool" ] "chatmd.extension_expected_tool" "expected tool declaration")
;;

let script ~dir ~loader ~source_node ~source ~attributes:raw ~inline_source =
  protect (fun () ->
    let allowed =
      [ "id"
      ; "language"
      ; "kind"
      ; "api"
      ; "src"
      ; "wall_time"
      ; "fuel"
      ; "max_tasks"
      ; "max_value"
      ; "max_output"
      ; "max_array_items"
      ; "max_depth"
      ]
    in
    let attributes = attrs source [ "script" ] allowed raw in
    List.iter allowed ~f:(fun name -> ignore (optional attributes name : string option));
    if not (String.equal (required attributes "language") "chatml")
    then
      fail
        source
        [ "script"; "language" ]
        "chatmd.script_invalid_language"
        "only ChatML is supported";
    let kind =
      match required attributes "kind", optional attributes "api" with
      | "moderator", Some "extensibility-v1" -> Spec.Moderator_script
      | "tool", (None | Some "extensibility-v1") -> Tool_script
      | _ ->
        fail
          source
          [ "script"; "api" ]
          "chatmd.extension_invalid_surface"
          "extension moderator requires api=extensibility-v1; tool scripts use the v1 \
           tool surface"
    in
    if Spec.equal_script_kind kind Tool_script
    then ignore (required attributes "id" : string);
    let path = optional attributes "src" in
    if Option.is_some path && not (String.is_empty inline_source)
    then
      fail
        source
        [ "script" ]
        "chatmd.script_conflicting_source"
        "cannot combine inline source and src";
    let text =
      match path with
      | None -> inline_source
      | Some path -> snd (load loader source_node source [ "script"; "src" ] path)
    in
    if String.length text > 1024 * 1024
    then
      fail
        source
        [ "script" ]
        "chatmd.script_source_limit"
        "script source exceeds byte limit";
    let defaults =
      [ "wall_time", "10s"
      ; "fuel", "1000000"
      ; "max_tasks", "10000"
      ; "max_value", "1MiB"
      ; "max_output", "256KiB"
      ; "max_array_items", "10000"
      ; "max_depth", "64"
      ]
    in
    let normalized =
      List.filter raw ~f:(fun (name, _) ->
        not (List.mem [ "api"; "src"; "kind" ] name ~equal:String.equal))
      @ [ "kind", Some "moderator" ]
    in
    let normalized =
      normalized
      @ List.filter_map defaults ~f:(fun (name, value) ->
        if Option.is_none (optional attributes name)
        then Some (name, Some value)
        else None)
    in
    let parsed =
      Chatmd_script_declaration.parse
        ~dir
        ~loader
        ~source_node
        ~source
        ~attributes:normalized
        ~inline_source:text
      |> many
    in
    let duration = Chatmd_shell_spec.Duration.to_seconds parsed.limits.wall_time in
    if
      Float.(duration > 60.)
      || parsed.limits.fuel > 10_000_000
      || parsed.limits.max_tasks > 100_000
      || parsed.limits.max_depth > 128
      || parsed.limits.max_array_items > 100_000
      || Int64.(
           Chatmd_shell_spec.Duration.bytes_to_int64 parsed.limits.max_value_bytes
           > 8_388_608L)
      || Int64.(
           Chatmd_shell_spec.Duration.bytes_to_int64 parsed.limits.max_output_bytes
           > 1_048_576L)
    then
      fail
        source
        [ "script" ]
        "chatmd.script_invalid_limit"
        "extension script limit exceeds host ceiling";
    { Spec.version = 1
    ; id = parsed.id
    ; kind
    ; source =
        (match path with
         | None -> S.Inline text
         | Some path -> Src { path; source_text = text })
    ; source_sha256 = parsed.source_sha256
    ; source_ref = source
    ; limits = parsed.limits
    })
;;

let authoring_context ~source node =
  protect (fun () ->
    match node with
    | Ast.Element (Ast.Authoring_context, raw, children) ->
      let attributes = attrs source [ "authoring_context" ] [ "policy"; "topics" ] raw in
      empty source [ "authoring_context" ] children;
      let policy =
        match required attributes "policy", optional attributes "topics" with
        | "auto", None -> Spec.Auto
        | "manual", None -> Manual
        | "preload", Some topics ->
          let topics =
            String.split_on_chars topics ~on:[ ' '; '\t'; '\n'; '\r' ]
            |> List.filter ~f:(Fn.non String.is_empty)
          in
          if
            List.is_empty topics
            || Option.is_some (List.find_a_dup topics ~compare:String.compare)
            || not
                 (List.for_all topics ~f:(fun topic ->
                    String.length topic <= 128
                    && String.for_all topic ~f:(function
                      | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | '/' -> true
                      | _ -> false)))
          then
            fail
              source
              [ "authoring_context"; "topics" ]
              "chatmd.authoring_invalid_topics"
              "preload requires unique nonempty topic identifiers";
          Preload topics
        | _ ->
          fail
            source
            [ "authoring_context" ]
            "chatmd.authoring_invalid_policy"
            "use auto/manual without topics, or preload with explicit topics"
      in
      { Spec.version = 1; policy; source_ref = source }
    | _ ->
      fail
        source
        [ "authoring_context" ]
        "chatmd.authoring_invalid_declaration"
        "expected authoring_context")
;;

let authoring_help ~source node =
  protect (fun () ->
    let module M = Chatmd_shell_spec.Authoring_metadata in
    let path = [ "authoring_help" ] in
    match node with
    | Ast.Element (Ast.Authoring_help, raw, children) ->
      let attributes =
        attrs source path [ "tool"; "package"; "tasks"; "topics"; "required_helpers" ] raw
      in
      empty source path children;
      let tool = required attributes "tool" in
      if
        String.is_empty tool
        || String.length tool > 256
        || not
             (String.for_all tool ~f:(function
                | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | ':' | '.' -> true
                | _ -> false))
      then
        fail
          source
          path
          "chatmd.authoring_invalid_tool"
          "use an exact registered tool name";
      let words value =
        String.split_on_chars value ~on:[ ' '; '\t'; '\r'; '\n' ]
        |> List.filter ~f:(Fn.non String.is_empty)
      in
      let task name =
        match
          List.find
            [ M.One_off_script
            ; Standalone_tool
            ; Moderator_tool
            ; Child_agent
            ; Background_workflow
            ]
            ~f:(fun t -> String.equal (M.task_id t) name)
        with
        | Some task -> task
        | None ->
          fail
            source
            path
            "chatmd.authoring_invalid_task"
            ("unknown authoring task: " ^ String.prefix name 128)
      in
      let helper = function
        | "ochat_authoring_context" -> M.Reference
        | "ochat_validate" -> M.Validation
        | _ ->
          fail source path "chatmd.authoring_invalid_helper" "unknown authoring helper"
      in
      let help : M.help =
        { version = 1
        ; package = required attributes "package"
        ; tasks = List.map (words (required attributes "tasks")) ~f:task
        ; topics = words (required attributes "topics")
        ; required_helpers =
            Option.value_map
              (optional attributes "required_helpers")
              ~default:[]
              ~f:(fun value -> List.map (words value) ~f:helper)
        }
      in
      (match M.validate_help help with
       | Ok () -> ()
       | Error message -> fail source path "chatmd.authoring_invalid_help" message);
      { Spec.tool; help; source_ref = source }
    | _ ->
      fail source path "chatmd.authoring_invalid_declaration" "expected authoring_help")
;;

let escape value =
  value
  |> String.substr_replace_all ~pattern:"&" ~with_:"&amp;"
  |> String.substr_replace_all ~pattern:"\"" ~with_:"&quot;"
  |> String.substr_replace_all ~pattern:"<" ~with_:"&lt;"
  |> String.substr_replace_all ~pattern:">" ~with_:"&gt;"
;;

let attributes values =
  String.concat
    ~sep:" "
    (List.map values ~f:(fun (name, value) -> sprintf "%s=\"%s\"" name (escape value)))
;;

let serialize_tool (tool : Spec.tool) =
  let binding =
    match tool.implementation with
    | Moderator id -> [ "type", "moderator"; "moderator", id ]
    | Standalone { script; entrypoint } ->
      [ "type", "chatml"; "script", script; "entrypoint", entrypoint ]
  in
  let fields =
    [ "name", tool.name ]
    @ binding
    @ [ "input_schema", tool.input_schema.path; "output_schema", tool.output_schema.path ]
    @ Option.to_list
        (Option.map tool.completion_schema ~f:(fun value ->
           "completion_schema", value.path))
    @ Option.to_list (Option.map tool.description ~f:(fun value -> "description", value))
  in
  if List.is_empty tool.uses
  then sprintf "<tool %s />" (attributes fields)
  else
    sprintf
      "<tool %s>%s</tool>"
      (attributes fields)
      (String.concat
         (List.map tool.uses ~f:(fun name ->
            sprintf "<uses %s/>" (attributes [ "tool", name ]))))
;;

let serialize_script (script : Spec.script) =
  let encoded =
    Chatmd_script_declaration.serialize
      { S.id = script.id
      ; language = "chatml"
      ; kind = Moderator
      ; source = script.source
      ; source_ref = script.source_ref
      ; source_sha256 = script.source_sha256
      ; limits = script.limits
      }
  in
  String.substr_replace_first
    encoded
    ~pattern:"kind=\"moderator\""
    ~with_:
      (match script.kind with
       | Moderator_script -> "kind=\"moderator\" api=\"extensibility-v1\""
       | Tool_script -> "kind=\"tool\"")
;;

let serialize_authoring (config : Spec.authoring_context) =
  let values =
    match config.policy with
    | Auto -> [ "policy", "auto" ]
    | Manual -> [ "policy", "manual" ]
    | Preload topics -> [ "policy", "preload"; "topics", String.concat ~sep:" " topics ]
  in
  sprintf "<authoring_context %s/>" (attributes values)
;;

let serialize_help (declaration : Spec.authoring_help) =
  let module M = Chatmd_shell_spec.Authoring_metadata in
  let help = declaration.help in
  let values =
    [ "tool", declaration.tool
    ; "package", help.package
    ; "tasks", String.concat ~sep:" " (List.map help.tasks ~f:M.task_id)
    ; "topics", String.concat ~sep:" " help.topics
    ]
    @
    if List.is_empty help.required_helpers
    then []
    else
      [ ( "required_helpers"
        , String.concat ~sep:" " (List.map help.required_helpers ~f:M.helper_name) )
      ]
  in
  sprintf "<authoring_help %s/>" (attributes values)
;;
