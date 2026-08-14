open! Core
module D = Chatmd_shell_spec.Diagnostic
module S = Chatmd_shell_spec.Chatmd_script_spec

exception Parse_error of D.t

let fail diagnostic = raise_notrace (Parse_error diagnostic)

let result f =
  try Ok (f ()) with
  | Parse_error diagnostic -> Error [ diagnostic ]
;;

let attribute = function
  | Ok value -> value
  | Error diagnostic -> fail diagnostic
;;

let load_source ~dir ~source path =
  try Io.load_doc ~dir path with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | _ ->
    fail
      (D.error
         ~source
         ~path:[ "script"; "src" ]
         ~code:"chatmd.script_source_unavailable"
         (sprintf "failed to load ChatML script %S relative to the prompt directory" path))
;;

let source ~dir ~source attributes inline_source =
  match attribute (Chatmd_attributes.optional attributes "src") with
  | Some path when not (String.is_empty inline_source) ->
    fail
      (D.error
         ~source
         ~path:[ "script" ]
         ~code:"chatmd.script_conflicting_source"
         "script cannot combine src with inline source")
  | Some path when String.is_empty (String.strip path) ->
    fail
      (D.error
         ~source
         ~path:[ "script"; "src" ]
         ~code:"chatmd.script_empty_source"
         "script src cannot be empty")
  | Some path -> S.Src { path; source_text = load_source ~dir ~source path }
  | None when String.is_empty inline_source ->
    fail
      (D.error
         ~source
         ~path:[ "script" ]
         ~code:"chatmd.script_empty_source"
         "script requires inline source or src")
  | None -> S.Inline inline_source
;;

let script_id source attributes kind =
  let id = attribute (Chatmd_attributes.optional attributes "id") in
  match id, kind with
  | None, S.Moderator -> "main"
  | None, _ ->
    fail
      (D.error
         ~source
         ~path:[ "script"; "id" ]
         ~code:"chatmd.script_id_required"
         "shell ChatML scripts require an explicit id")
  | Some id, _ when String.is_empty (String.strip id) ->
    fail
      (D.error
         ~source
         ~path:[ "script"; "id" ]
         ~code:"chatmd.script_invalid_id"
         "script id cannot be empty")
  | Some id, _ -> id
;;

let limit_error source name message =
  D.error
    ~source
    ~path:[ "script"; name ]
    ~code:"chatmd.script_invalid_limit"
    message
;;

let positive_int source attributes name default =
  match attribute (Chatmd_attributes.optional attributes name) with
  | None -> default
  | Some value ->
    (match Int.of_string_opt value with
     | Some value when value > 0 -> value
     | _ -> fail (limit_error source name "limit must be a positive integer"))
;;

let duration source attributes name default =
  match attribute (Chatmd_attributes.optional attributes name) with
  | None -> default
  | Some value ->
    (match Chatmd_shell_spec.Duration.parse value with
     | Ok value -> value
     | Error message -> fail (limit_error source name message))
;;

let bytes source attributes name default =
  match attribute (Chatmd_attributes.optional attributes name) with
  | None -> default
  | Some value ->
    (match Chatmd_shell_spec.Duration.parse_bytes value with
     | Ok value when Int64.(Chatmd_shell_spec.Duration.bytes_to_int64 value > 0L) -> value
     | Ok _ -> fail (limit_error source name "limit must be greater than zero")
     | Error message -> fail (limit_error source name message))
;;

let limits source attributes =
  let default = S.default_limits in
  { S.wall_time = duration source attributes "wall_time" default.wall_time
  ; fuel = positive_int source attributes "fuel" default.fuel
  ; max_tasks = positive_int source attributes "max_tasks" default.max_tasks
  ; max_value_bytes = bytes source attributes "max_value" default.max_value_bytes
  ; max_output_bytes = bytes source attributes "max_output" default.max_output_bytes
  ; max_array_items =
      positive_int source attributes "max_array_items" default.max_array_items
  ; max_depth = positive_int source attributes "max_depth" default.max_depth
  }
;;

let parse ~dir ~source:source_ref ~attributes ~inline_source =
  result (fun () ->
    let attributes =
      Chatmd_attributes.create
        ~source:source_ref
        ~path:[ "script" ]
        ~allowed:
          [ "id"
          ; "language"
          ; "kind"
          ; "src"
          ; "wall_time"
          ; "fuel"
          ; "max_tasks"
          ; "max_value"
          ; "max_output"
          ; "max_array_items"
          ; "max_depth"
          ]
        attributes
      |> attribute
    in
    let language = attribute (Chatmd_attributes.required attributes "language") in
    if not (String.equal language "chatml")
    then
      fail
        (D.error
           ~source:source_ref
           ~path:[ "script"; "language" ]
           ~code:"chatmd.script_invalid_language"
           "only language=chatml is supported");
    let kind_name = attribute (Chatmd_attributes.required attributes "kind") in
    let kind = S.kind_of_string source_ref kind_name |> attribute in
    let source = source ~dir ~source:source_ref attributes inline_source in
    let source_text =
      match source with
      | S.Inline value | S.Src { source_text = value; _ } -> value
    in
    { S.id = script_id source_ref attributes kind
    ; language
    ; kind
    ; source
    ; source_ref
    ; source_sha256 = Chatmd_shell_spec.Source_ref.digest source_text
    ; limits = limits source_ref attributes
    }
    |> S.qualify)
;;

let duplicate script =
  D.error
    ~source:script.S.source_ref
    ~path:[ "script"; script.S.id ]
    ~code:"chatmd.duplicate_script"
    ("duplicate script id: " ^ script.S.id)
;;

let validate_registry scripts =
  let _, duplicates =
    List.fold scripts ~init:(String.Set.empty, []) ~f:(fun (seen, errors) script ->
      if Set.mem seen script.S.id
      then seen, duplicate script :: errors
      else Set.add seen script.S.id, errors)
  in
  match List.rev duplicates with
  | [] -> Ok scripts
  | errors -> Error errors
;;

let validate_prompt_registry ~moderator_ids scripts =
  let moderator_errors =
    if List.length moderator_ids <= 1
    then []
    else
      [ D.error
          ~path:[ "script" ]
          ~code:"chatmd.multiple_moderators"
          "at most one moderator script may be selected"
      ]
  in
  let duplicate_ids =
    moderator_ids @ List.map scripts ~f:(fun script -> script.S.id)
    |> List.find_a_dup ~compare:String.compare
    |> Option.to_list
    |> List.map ~f:(fun id ->
      D.error
        ~path:[ "script"; id ]
        ~code:"chatmd.duplicate_script"
        ("duplicate script id: " ^ id))
  in
  let registry_errors =
    match validate_registry scripts with
    | Ok _ -> []
    | Error diagnostics -> diagnostics
  in
  match moderator_errors @ duplicate_ids @ registry_errors with
  | [] -> Ok ()
  | diagnostics -> Error diagnostics
;;

let escape value =
  value
  |> String.substr_replace_all ~pattern:"&" ~with_:"&amp;"
  |> String.substr_replace_all ~pattern:"\"" ~with_:"&quot;"
  |> String.substr_replace_all ~pattern:"<" ~with_:"&lt;"
  |> String.substr_replace_all ~pattern:">" ~with_:"&gt;"
;;

let serialize script =
  let limits = script.S.limits in
  let attributes =
    sprintf
      "id=\"%s\" language=\"chatml\" kind=\"%s\" wall_time=\"%s\" fuel=\"%d\" \
       max_tasks=\"%d\" max_value=\"%s\" max_output=\"%s\" max_array_items=\"%d\" \
       max_depth=\"%d\""
      (escape script.S.id)
      (S.kind_to_string script.kind)
      (Chatmd_shell_spec.Duration.to_string limits.wall_time)
      limits.fuel
      limits.max_tasks
      (Chatmd_shell_spec.Duration.bytes_to_string limits.max_value_bytes)
      (Chatmd_shell_spec.Duration.bytes_to_string limits.max_output_bytes)
      limits.max_array_items
      limits.max_depth
  in
  match script.source with
  | S.Inline source -> sprintf "<script %s>%s</script>" attributes source
  | S.Src { path; _ } -> sprintf "<script %s src=\"%s\" />" attributes (escape path)
;;
