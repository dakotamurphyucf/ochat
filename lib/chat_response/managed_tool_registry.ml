open Core
module C = Tool_capability
module EC = Extension_compiler
module Spec = Chatmd_shell_spec.Extension_spec
module D = Chatmd_shell_spec.Diagnostic
module CM = Prompt.Chat_markdown

type t =
  { capabilities : C.t
  ; definition : EC.definition
  ; tools : (Agent_protocol.Id.Capability.t * EC.t) list
  ; authority_fingerprint : string
  }

let capabilities t = t.capabilities
let definition t = t.definition
let authority_fingerprint t = t.authority_fingerprint

let prepare
      ?(limits = Chatml_compilation.default_limits)
      ~env
      ~owner
      ~capabilities
      elements
  =
  let open Result.Let_syntax in
  let fail code message = Error [ D.error ~code message ] in
  let%bind () =
    if List.length elements > 16_384
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
  let helps =
    List.filter_map elements ~f:(function
      | CM.Authoring_help help -> Some help
      | _ -> None)
  in
  let%bind () =
    if List.length scripts > 128 || List.length tools > 4096 || List.length helps > 4096
    then fail "chatml.definition_limit" "too many captured extension declarations"
    else Ok ()
  in
  let%bind () =
    try
      ignore (CM.validate_declarations elements);
      Ok ()
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      fail "chatml.invalid_definition" (String.prefix (Exn.to_string exn) (16 * 1024))
  in
  let remaining = ref (8 * 1024 * 1024) in
  let seen = Hash_set.create (module String) in
  let consume text =
    if String.length text > 8 * 1024 * 1024
    then fail "chatml.definition_limit" "captured source/schema budget exceeded"
    else if Hash_set.mem seen text
    then Ok ()
    else if String.length text > !remaining
    then fail "chatml.definition_limit" "captured source/schema budget exceeded"
    else (
      Hash_set.add seen text;
      remaining := !remaining - String.length text;
      Ok ())
  in
  let%bind () =
    List.fold_result scripts ~init:() ~f:(fun () script ->
      let%bind () = consume (Spec.script_text script) in
      EC.validate_script ~max_source_bytes:limits.max_source_bytes script)
  in
  let schema_json = Hashtbl.create (module String) in
  let validate_schema (schema : Spec.schema) =
    let%bind () = consume schema.source_text in
    match Hashtbl.find schema_json schema.source_text with
    | Some (sha256, _) when String.equal sha256 schema.source_sha256 -> Ok ()
    | None | Some _ ->
      let%map _ = Spec.validate_schema schema in
      Hashtbl.set
        schema_json
        ~key:schema.source_text
        ~data:(schema.source_sha256, Jsonaf.of_string schema.source_text)
  in
  let%bind () =
    List.fold_result tools ~init:() ~f:(fun () tool ->
      let%bind () =
        match C.find capabilities ~name:tool.Spec.name with
        | Ok _ ->
          fail
            "capability.duplicate_name"
            "extension conflicts with an existing capability"
        | Error _ -> Ok ()
      in
      List.fold_result
        (tool.input_schema :: tool.output_schema :: Option.to_list tool.completion_schema)
        ~init:()
        ~f:(fun () schema -> validate_schema schema))
  in
  let names =
    String.Set.of_list
      (List.map tools ~f:(fun tool -> tool.Spec.name)
       @ List.map (C.references capabilities) ~f:(fun reference -> reference.C.name))
  in
  let%bind () =
    List.fold_result tools ~init:() ~f:(fun () tool ->
      match List.find tool.Spec.uses ~f:(fun name -> not (Set.mem names name)) with
      | None -> Ok ()
      | Some _ ->
        Error
          [ D.error
              ~source:tool.source_ref
              ~code:"capability.not_selected"
              "extension requires an unavailable tool"
          ])
  in
  let%bind () =
    List.fold_result helps ~init:() ~f:(fun () help ->
      let%bind () =
        if not (Set.mem names help.Spec.tool)
        then fail "authoring.unknown_tool" "help references an unavailable tool"
        else
          Chatmd_shell_spec.Authoring_metadata.validate_help help.help
          |> Result.map_error ~f:(fun message ->
            [ D.error ~source:help.source_ref ~code:"authoring.invalid_metadata" message ])
      in
      let%bind () =
        match C.find capabilities ~name:help.tool with
        | Error _ -> Ok ()
        | Ok binding ->
          (match (C.metadata binding).authoring with
           | Some admitted
             when Chatmd_shell_spec.Authoring_metadata.equal_help admitted help.help ->
             Ok ()
           | None | Some _ ->
             fail
               "authoring.metadata_override"
               "existing capability help must match its admitted metadata")
      in
      consume (Sexp.to_string (Spec.sexp_of_authoring_help help)))
  in
  let base_permissions =
    List.map (C.references capabilities) ~f:(fun reference ->
      let binding =
        C.find capabilities ~name:reference.name |> Result.ok |> Option.value_exn
      in
      reference.name, C.permission_fingerprint binding)
  in
  let compact_schema (schema : Spec.schema) = { schema with source_text = "" } in
  let compact_tool (tool : Spec.tool) =
    { tool with
      input_schema = compact_schema tool.input_schema
    ; output_schema = compact_schema tool.output_schema
    ; completion_schema = Option.map tool.completion_schema ~f:compact_schema
    }
  in
  let compact_script (script : Spec.script) =
    { script with
      source =
        (match script.source with
         | Inline _ -> Inline ""
         | Src source -> Src { source with source_text = "" })
    }
  in
  (* Content digests were validated above. Do not repeat shared source/schema
     bodies in a potentially large aggregate fingerprint or descriptor tree.
     Bind the full captured definition and its actual base authority. Including
     all declarations is conservative and avoids recursively hashing live managed
     references. Runtime recursion is distinct from parser-checked uses cycles. *)
  let authority_fingerprint =
    [%sexp
      ("ochat.managed-authority.v1" : string)
    , (List.map scripts ~f:compact_script : Spec.script list)
    , (List.map tools ~f:compact_tool : Spec.tool list)
    , (helps : Spec.authoring_help list)
    , (base_permissions : (string * string) list)
    , (Chatml_compilation.contract Tool_v1 : Sexp.t)
    , (Chatml_compilation.contract Moderator_v1 : Sexp.t)
    , (Chatmd_shell_spec.Tool_schema.dialect : string)]
    |> Sexp.to_string
    |> Chatmd_shell_spec.Source_ref.digest
  in
  let registrations =
    List.map tools ~f:(fun tool ->
      let help_metadata =
        match
          List.find helps ~f:(fun help -> String.equal help.Spec.tool tool.Spec.name)
        with
        | None -> Chatmd_shell_spec.Authoring_metadata.empty
        | Some help ->
          { Chatmd_shell_spec.Authoring_metadata.authoring = Some help.help
          ; helper = None
          }
      in
      let info : Openai.Completions.tool =
        { type_ = "function"
        ; function_ =
            { name = tool.name
            ; description = tool.description
            ; parameters =
                Hashtbl.find_exn schema_json tool.input_schema.source_text |> snd
            ; strict = false
            }
        }
      in
      C.
        { descriptor = info
        ; target = tool.implementation
        ; metadata = help_metadata
        ; implementation_revision =
            [%sexp
              ("ochat.managed-declaration.v1" : string)
            , (compact_tool tool : Spec.tool)
            , (authority_fingerprint : string)]
            |> Sexp.to_string
            |> Chatmd_shell_spec.Source_ref.digest
        })
  in
  let%bind capabilities =
    C.extend_managed
      capabilities
      ~owner
      ~resource_fingerprint:authority_fingerprint
      registrations
    |> Result.map_error ~f:(fun error -> [ D.error ~code:error.C.code error.message ])
  in
  let%map definition =
    EC.prepare_definition_in_domain ~limits ~env ~capabilities elements
  in
  let tools =
    List.map (EC.prepared_tools definition) ~f:(fun prepared ->
      let binding =
        C.find capabilities ~name:(EC.declaration prepared).name
        |> Result.ok
        |> Option.value_exn
      in
      (C.reference binding).id, prepared)
  in
  { capabilities; definition; tools; authority_fingerprint }
;;

let resolve t binding =
  let open Result.Let_syntax in
  let reference = C.reference binding in
  let%bind _ =
    C.resolve t.capabilities ~id:reference.id ~fingerprint:reference.fingerprint
  in
  match C.implementation binding with
  | Native _ ->
    Error
      C.
        { code = "capability.not_managed"
        ; message = "native target has no prepared extension"
        }
  | Managed target ->
    (match
       List.Assoc.find t.tools reference.id ~equal:Agent_protocol.Id.Capability.equal
     with
     | Some prepared
       when Spec.equal_implementation (EC.declaration prepared).implementation target ->
       Ok prepared
     | _ ->
       Error
         C.
           { code = "capability.stale_reference"
           ; message = "managed target does not match its compiled binding"
           })
;;

let revalidate t ~current =
  let open Result.Let_syntax in
  let%bind selected =
    C.select
      current
      ~names:
        (List.map (C.references t.capabilities) ~f:(fun reference -> reference.C.name))
  in
  match String.equal (C.fingerprint selected) (C.fingerprint t.capabilities) with
  | true -> Ok ()
  | false ->
    Error
      C.
        { code = "capability.stale_reference"
        ; message = "managed definition authority changed"
        }
;;

type execution =
  { prepared : EC.t
  ; binding : C.binding
  ; invocation : Agent_protocol.Invocation.t
  }

let prepared execution = execution.prepared
let binding execution = execution.binding
let invocation execution = execution.invocation

let admit t ~current ~selected ~(reference : C.reference) ~invocation =
  let module I = Agent_protocol.Invocation in
  let open Result.Let_syntax in
  let%bind () = revalidate t ~current in
  let%bind () =
    List.fold_result (C.references selected) ~init:() ~f:(fun () reference ->
      C.resolve t.capabilities ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map ~f:ignore)
  in
  let%bind binding =
    C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
  in
  let%bind prepared = resolve t binding in
  let context = invocation.I.context in
  match invocation.status with
  | Dispatching
    when String.equal context.tool_name reference.name
         && String.equal context.implementation_revision reference.implementation_revision
         && String.equal context.capability_fingerprint (C.fingerprint selected)
         && Result.is_ok (I.validate invocation) -> Ok { prepared; binding; invocation }
  | _ ->
    Error
      C.
        { code = "capability.stale_context"
        ; message = "invocation does not match the selected managed capability"
        }
;;
