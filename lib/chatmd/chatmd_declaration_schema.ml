open! Core
module E = Chatmd_shell_spec.Shell_element
module Shell_context = Chatmd_shell_schema.Context

module Family = struct
  type t =
    | Plain_tag of Chatmd_ast.tag
    | Script_legacy
    | Script_extension
    | Tool_legacy
    | Tool_extension
    | Tool_shell
    | Tool_read_file
    | Tool_inherited
    | Shell_element of Shell_context.t
  [@@deriving sexp_of]

  let equal a b =
    match a, b with
    | Plain_tag a, Plain_tag b -> Chatmd_ast.tag_equal a b
    | Script_legacy, Script_legacy
    | Script_extension, Script_extension
    | Tool_legacy, Tool_legacy
    | Tool_extension, Tool_extension
    | Tool_shell, Tool_shell
    | Tool_read_file, Tool_read_file
    | Tool_inherited, Tool_inherited -> true
    | Shell_element a, Shell_element b ->
      Chatmd_shell_schema.Root.equal (Shell_context.root a) (Shell_context.root b)
      && List.equal E.equal (Shell_context.ancestors a) (Shell_context.ancestors b)
      && E.equal (Shell_context.element a) (Shell_context.element b)
    | _ -> false
  ;;
end

module Attribute_names = struct
  type t =
    | Exhaustive of string list
    | Helpful of string list
  [@@deriving sexp_of]

  let names = function
    | Exhaustive names | Helpful names -> names
  ;;
end

module Choice = Chatmd_shell_schema.Value_choices

module Script_surface = struct
  type t =
    | Moderator
    | Tool
  [@@deriving equal, sexp_of]

  let moderator_kind = "moderator"
  let tool_kind = "tool"
  let api_v1 = "extensibility-v1"
  let kind_names = [ moderator_kind; tool_kind ]
  let api_names = [ api_v1 ]

  let of_values ~kind ~api =
    if
      String.equal kind moderator_kind
      && Option.value_map api ~default:false ~f:(String.equal api_v1)
    then Some Moderator
    else if
      String.equal kind tool_kind
      && Option.value_map api ~default:true ~f:(String.equal api_v1)
    then Some Tool
    else None
  ;;
end

module Extension_tool_binding = struct
  type t =
    | Moderator of string
    | Standalone of
        { script : string
        ; entrypoint : string
        }
  [@@deriving sexp_of]

  let moderator_type = "moderator"
  let chatml_type = "chatml"
  let run_entrypoint = "run"
  let type_names = [ moderator_type; chatml_type ]
  let entrypoint_names = [ run_entrypoint ]

  let of_values ~type_name ~moderator ~script ~entrypoint =
    match moderator, script, entrypoint with
    | Some id, None, None
      when String.equal type_name moderator_type
           && not (String.is_empty (String.strip id)) -> Some (Moderator id)
    | None, Some id, Some entrypoint
      when String.equal type_name chatml_type
           && String.equal entrypoint run_entrypoint
           && not (String.is_empty (String.strip id)) ->
      Some (Standalone { script = id; entrypoint })
    | _ -> None
  ;;
end

let script_legacy_attributes =
  [ "id"
  ; "language"
  ; "kind"
  ; "src"
  ; "project"
  ; "environment"
  ; "wall_time"
  ; "fuel"
  ; "max_tasks"
  ; "max_value"
  ; "max_output"
  ; "max_array_items"
  ; "max_depth"
  ]
;;

let script_extension_attributes =
  [ "id"
  ; "language"
  ; "kind"
  ; "api"
  ; "src"
  ; "project"
  ; "environment"
  ; "wall_time"
  ; "fuel"
  ; "max_tasks"
  ; "max_value"
  ; "max_output"
  ; "max_array_items"
  ; "max_depth"
  ]
;;

let tool_extension_attributes =
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
;;

let tool_inherited_attributes = [ "name"; "type" ]

let tool_legacy_helpful_attributes =
  [ "name"
  ; "command"
  ; "agent"
  ; "mcp_server"
  ; "description"
  ; "local"
  ; "persistence"
  ; "source_mode"
  ; "strict"
  ; "client_id_env"
  ; "client_secret_env"
  ; "include"
  ; "includes"
  ]
;;

let message_helpful_attributes =
  [ "role"
  ; "type"
  ; "name"
  ; "id"
  ; "status"
  ; "phase"
  ; "ochat-history-id"
  ; "function_call"
  ; "function_name"
  ; "tool_call"
  ; "tool_call_id"
  ]
;;

let last_attribute attributes name =
  List.fold attributes ~init:None ~f:(fun last (candidate, value) ->
    if String.equal candidate name then Some value else last)
;;

let has_name attributes name =
  List.exists attributes ~f:(fun (candidate, _) -> String.equal candidate name)
;;

let any_value attributes name ~f =
  List.exists attributes ~f:(fun (candidate, value) ->
    String.equal candidate name && Option.value_map value ~default:false ~f)
;;

let script_extension_route attributes =
  has_name attributes "api"
  || any_value attributes "kind" ~f:(String.equal Script_surface.tool_kind)
;;

let tool_extension_route attributes =
  any_value attributes "type" ~f:(fun value ->
    List.mem Extension_tool_binding.type_names value ~equal:String.equal)
;;

let tool_inherited_route attributes =
  any_value attributes "type" ~f:(String.equal "inherited")
;;

let tool_shell_route attributes =
  has_name attributes "runtime"
  || Option.value_map
       (last_attribute attributes "type")
       ~default:false
       ~f:(Option.value_map ~default:false ~f:(String.equal "shell"))
;;

let tool_read_file_route attributes =
  List.for_all [ "command"; "agent"; "mcp_server" ] ~f:(fun name ->
    not (has_name attributes name))
  && Option.value_map
       (last_attribute attributes "name")
       ~default:false
       ~f:
         (Option.value_map ~default:false ~f:(fun value ->
            let value = String.strip value in
            String.equal value "read_file" || String.equal value "get_contents"))
;;

let family_of_complete ~tag ~attributes =
  match tag with
  | Chatmd_ast.Script ->
    if script_extension_route attributes then Family.Script_extension else Script_legacy
  | Tool ->
    if tool_inherited_route attributes
    then Tool_inherited
    else if tool_extension_route attributes
    then Tool_extension
    else if tool_shell_route attributes
    then Tool_shell
    else if tool_read_file_route attributes
    then Tool_read_file
    else Tool_legacy
  | Msg
  | User
  | Agent
  | Assistant
  | System
  | Developer
  | Doc
  | Img
  | Import
  | Config
  | Reasoning
  | Summary
  | Tool_call
  | Tool_response
  | Shell_access
  | Moderator_runtime
  | Uses
  | Authoring_context
  | Authoring_help
  | Shell_element _ -> Plain_tag tag
;;

module Prefix = struct
  type issue =
    | Duplicate_attribute of string
    | Invalid_complete_discriminator of string
  [@@deriving sexp_of]

  type t =
    | Viable of Family.t list
    | Uncertain of Family.t list
    | Malformed of issue
    | Limit_exceeded
  [@@deriving sexp_of]
end

(* This is an authoring-query limit, not a new declaration grammar rule. The
   batch parser still receives the original bytes. Preflight at most one item
   beyond the cap before sorting, hashing or comparing attacker-controlled
   attribute names. *)
let admitted_prefix_attributes attributes ~checkpoint =
  let rec loop count bytes = function
    | [] -> true
    | _ when count >= 128 -> false
    | (name, value) :: rest ->
      checkpoint ();
      let name_bytes = String.length name in
      let value_bytes = Option.value_map value ~default:0 ~f:String.length in
      if
        name_bytes > 4_096
        || value_bytes > 4_096
        || bytes > 65_536 - name_bytes - value_bytes
      then false
      else loop (count + 1) (bytes + name_bytes + value_bytes) rest
  in
  loop 0 0 attributes
;;

let attributes = function
  | Family.Script_legacy -> Attribute_names.Exhaustive script_legacy_attributes
  | Script_extension -> Exhaustive script_extension_attributes
  | Tool_extension -> Exhaustive tool_extension_attributes
  | Tool_shell ->
    Exhaustive
      (Chatmd_shell_schema.allowed_for_parser_path_exn ~root:Tool_shell ~path:[ "tool" ])
  | Tool_read_file ->
    Exhaustive
      (Chatmd_shell_schema.allowed_for_parser_path_exn
         ~root:Tool_read_file
         ~path:[ "tool" ])
  | Tool_inherited -> Exhaustive tool_inherited_attributes
  | Tool_legacy -> Helpful tool_legacy_helpful_attributes
  | Shell_element context ->
    (match Chatmd_shell_schema.allowed context with
     | Some names -> Exhaustive names
     | None -> Helpful [])
  | Plain_tag tag ->
    (match tag with
     | Chatmd_ast.Moderator_runtime -> Exhaustive [ "shell_runtime" ]
     | Shell_access ->
       Exhaustive
         (Chatmd_shell_schema.allowed_for_parser_path_exn
            ~root:Shell_access
            ~path:[ "shell_access" ])
     | Uses -> Exhaustive [ "tool" ]
     | Authoring_context -> Exhaustive [ "policy"; "topics" ]
     | Authoring_help ->
       Exhaustive [ "tool"; "package"; "tasks"; "topics"; "required_helpers" ]
     | Msg -> Helpful message_helpful_attributes
     | Doc -> Helpful [ "src"; "local"; "strip"; "markdown"; "capture" ]
     | Img -> Helpful [ "src"; "local" ]
     | Import -> Exhaustive [ "src"; "namespace" ]
     | Config ->
       Helpful
         [ "max_tokens"
         ; "model"
         ; "reasoning_effort"
         ; "temperature"
         ; "show_tool_call"
         ; "id"
         ]
     | Agent -> Helpful [ "src"; "local"; "source_mode" ]
     | Summary -> Helpful [ "type" ]
     | Reasoning -> Helpful [ "id"; "status"; "ochat-history-id" ]
     | User | Assistant | System | Developer | Tool_call | Tool_response ->
       Helpful message_helpful_attributes
     | Script | Tool | Shell_element _ -> Helpful [])
;;

let families_of_prefix
      ~scope
      ~tag
      ~completed_attributes
      ~uncertain_discriminator
      ~checkpoint
  =
  if not (admitted_prefix_attributes completed_attributes ~checkpoint)
  then Prefix.Limit_exceeded
  else (
    let duplicates =
      List.map completed_attributes ~f:fst |> List.find_a_dup ~compare:String.compare
    in
    let script_discriminator_error =
      if Chatmd_ast.tag_equal tag Script && not uncertain_discriminator
      then (
        match last_attribute completed_attributes "api" with
        | Some None -> Some "api"
        | Some (Some value)
          when not (List.mem Script_surface.api_names value ~equal:String.equal) ->
          Some "api"
        | None | Some (Some _) ->
          (match last_attribute completed_attributes "kind" with
           | Some None -> Some "kind"
           | Some (Some value)
             when not
                    (String.equal value Script_surface.tool_kind
                     || List.mem
                          Chatmd_shell_spec.Chatmd_script_spec.kind_names
                          value
                          ~equal:String.equal) -> Some "kind"
           | None | Some (Some _) -> None))
      else None
    in
    match duplicates with
    | Some name ->
      (match tag with
       | Script
       | Shell_access
       | Moderator_runtime
       | Uses
       | Authoring_context
       | Authoring_help
       | Import
       | Shell_element _ -> Prefix.Malformed (Duplicate_attribute name)
       | Tool ->
         Prefix.Uncertain
           [ Tool_legacy; Tool_extension; Tool_shell; Tool_read_file; Tool_inherited ]
       | _ -> Prefix.Uncertain [ Plain_tag tag ])
    | None ->
      (match script_discriminator_error with
       | Some name -> Prefix.Malformed (Invalid_complete_discriminator name)
       | None ->
         let shell_family element =
           match Chatmd_shell_schema.Context.of_scope scope ~element ~checkpoint with
           | Some context -> [ Family.Shell_element context ]
           | None -> []
         in
         let families =
           match tag with
           | Chatmd_ast.Script ->
             if uncertain_discriminator
             then [ Family.Script_legacy; Script_extension ]
             else if script_extension_route completed_attributes
             then [ Script_extension ]
             else [ Script_legacy; Script_extension ]
           | Tool ->
             if uncertain_discriminator
             then
               [ Tool_legacy; Tool_extension; Tool_shell; Tool_read_file; Tool_inherited ]
             else if tool_inherited_route completed_attributes
             then [ Tool_inherited ]
             else if tool_extension_route completed_attributes
             then [ Tool_extension; Tool_inherited ]
             else if tool_shell_route completed_attributes
             then
               [ Tool_shell; Tool_extension; Tool_inherited; Tool_legacy; Tool_read_file ]
             else if tool_read_file_route completed_attributes
             then
               [ Tool_read_file; Tool_extension; Tool_shell; Tool_inherited; Tool_legacy ]
             else
               [ Tool_legacy; Tool_extension; Tool_shell; Tool_read_file; Tool_inherited ]
           | Shell_element element -> shell_family element
           | Msg
           | User
           | Agent
           | Assistant
           | System
           | Developer
           | Doc
           | Img
           | Import
           | Config
           | Reasoning
           | Summary
           | Tool_call
           | Tool_response
           | Shell_access
           | Moderator_runtime
           | Uses
           | Authoring_context
           | Authoring_help -> [ Plain_tag tag ]
         in
         if uncertain_discriminator
         then Prefix.Uncertain families
         else Prefix.Viable families))
;;

module Attribute_candidate = struct
  type applicability =
    | All_viable_families
    | Only of Family.t list
  [@@deriving sexp_of]

  type t =
    { name : string
    ; applicability : applicability
    }

  let name t = t.name
  let applicability t = t.applicability
end

let attributes_for_prefix prefix ~completed_attributes ~checkpoint =
  if not (admitted_prefix_attributes completed_attributes ~checkpoint)
  then []
  else (
    let families =
      match prefix with
      | Prefix.Viable families | Uncertain families -> families
      | Malformed _ | Limit_exceeded -> []
    in
    let present = String.Set.of_list (List.map completed_attributes ~f:fst) in
    let names =
      List.concat_map families ~f:(fun family ->
        Attribute_names.names (attributes family))
      |> List.dedup_and_sort ~compare:String.compare
    in
    List.filter_map names ~f:(fun name ->
      checkpoint ();
      if Set.mem present name
      then None
      else (
        let applicable =
          List.filter families ~f:(fun family ->
            checkpoint ();
            List.mem (Attribute_names.names (attributes family)) name ~equal:String.equal)
        in
        let applicability =
          if Int.equal (List.length applicable) (List.length families)
          then Attribute_candidate.All_viable_families
          else Only applicable
        in
        Some { Attribute_candidate.name; applicability })))
;;

let choices family ~attribute =
  match family, attribute with
  | (Family.Script_legacy | Script_extension), "language" ->
    Some (Choice.Closed [ "chatml" ])
  | Script_legacy, "kind" -> Some (Closed Chatmd_shell_spec.Chatmd_script_spec.kind_names)
  | Script_extension, "kind" -> Some (Closed Script_surface.kind_names)
  | Script_extension, "api" -> Some (Closed Script_surface.api_names)
  | Tool_extension, "type" -> Some (Closed Extension_tool_binding.type_names)
  | Tool_extension, "entrypoint" -> Some (Closed Extension_tool_binding.entrypoint_names)
  | Tool_shell, "type" -> Some (Closed [ "shell" ])
  | Tool_shell, "mode" ->
    Some (Closed Chatmd_shell_spec.Shell_tool_spec.Source_mode.names)
  | Tool_shell, "stdin" ->
    Some (Closed (List.map Chatmd_shell_spec.Shell_tool_spec.Enum.stdin ~f:fst))
  | Tool_shell, "rationale" ->
    Some (Closed (List.map Chatmd_shell_spec.Shell_tool_spec.Enum.rationale ~f:fst))
  | Tool_shell, "result" ->
    Some (Closed (List.map Chatmd_shell_spec.Shell_tool_spec.Enum.result ~f:fst))
  | Tool_shell, "stream" ->
    Some (Closed (List.map Chatmd_shell_spec.Shell_tool_spec.Enum.stream ~f:fst))
  | Tool_shell, "nonzero" ->
    Some (Closed (List.map Chatmd_shell_spec.Shell_tool_spec.Enum.nonzero ~f:fst))
  | Tool_inherited, "type" -> Some (Closed [ "inherited" ])
  | Plain_tag Shell_access, "pipefail" ->
    Some (Closed (List.map Chatmd_shell_schema.Enum.bool ~f:fst))
  | Shell_element context, attribute -> Chatmd_shell_schema.choices context ~attribute
  | _ -> None
;;

let tag_names scope ~checkpoint =
  let names = List.map Chatmd_ast.official_names ~f:fst @ E.all_names in
  List.filter names ~f:(fun name ->
    checkpoint ();
    Option.is_some (Chatmd_tag_scope.recognize scope ~name))
;;
