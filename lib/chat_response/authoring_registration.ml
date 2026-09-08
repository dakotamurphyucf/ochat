open Core
module Spec = Chatmd_shell_spec.Extension_spec
module Metadata = Chatmd_shell_spec.Authoring_metadata
module C = Tool_capability

type t =
  { capabilities : C.t
  ; sources : (string * Chatmd_shell_spec.Source_ref.t) list
  }

let capabilities t = t.capabilities
let sources t = t.sources
let error code message = Error C.{ code; message }

let create ?(host_metadata = []) ~declarations ~owner ~resource_fingerprint registrations =
  let open Result.Let_syntax in
  let%bind () =
    if
      List.exists registrations ~f:(fun (revision, _) ->
        String.length revision <> 64
        || not
             (String.for_all revision ~f:(function
                | '0' .. '9' | 'a' .. 'f' -> true
                | _ -> false)))
    then error "capability.invalid_registration" "invalid original implementation digest"
    else if List.length declarations > 4096
    then error "authoring.resource_limit" "too many authoring help declarations"
    else if
      Option.is_some
        (List.find_a_dup declarations ~compare:(fun a b ->
           String.compare a.Spec.tool b.Spec.tool))
    then error "authoring.duplicate_metadata" "duplicate help declarations for one tool"
    else Ok ()
  in
  let names =
    List.map registrations ~f:(fun (_, tool) -> tool.Ochat_function.info.function_.name)
  in
  let%bind metadata =
    List.fold declarations ~init:(Ok host_metadata) ~f:(fun result declaration ->
      let%bind metadata = result in
      let name = declaration.Spec.tool in
      if
        String.is_empty name
        || String.length name > 256
        || not
             (String.for_all name ~f:(function
                | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | ':' | '.' -> true
                | _ -> false))
      then error "authoring.invalid_metadata" "invalid exact callable name"
      else if not (List.mem names name ~equal:String.equal)
      then error "authoring.unknown_tool" ("help references an unavailable tool: " ^ name)
      else if List.Assoc.mem host_metadata name ~equal:String.equal
      then
        error "authoring.metadata_override" "authored help cannot replace host metadata"
      else if
        String.equal name (Metadata.helper_name Reference)
        || String.equal name (Metadata.helper_name Validation)
      then
        error
          "authoring.helper_metadata"
          "reserved helper tools cannot request authoring help"
      else (
        let%map () =
          Metadata.validate_help declaration.help
          |> Result.map_error ~f:(fun message ->
            C.{ code = "authoring.invalid_metadata"; message })
        in
        (name, Metadata.{ authoring = Some declaration.help; helper = None }) :: metadata))
  in
  let registrations =
    List.map registrations ~f:(fun (revision, implementation) ->
      match
        List.find declarations ~f:(fun declaration ->
          String.equal
            declaration.Spec.tool
            implementation.Ochat_function.info.function_.name)
      with
      | None -> revision, implementation
      | Some declaration ->
        let revision =
          [%sexp
            ("ochat.authored-help.v1" : string)
          , (revision : string)
          , (declaration : Spec.authoring_help)]
          |> Sexp.to_string
          |> Chatmd_shell_spec.Source_ref.digest
        in
        revision, implementation)
  in
  let%map capabilities = C.create ~metadata ~owner ~resource_fingerprint registrations in
  { capabilities
  ; sources =
      List.map declarations ~f:(fun declaration ->
        declaration.Spec.tool, declaration.source_ref)
  }
;;

let resolve
      ?host_metadata
      ?context
      ?catalog
      ~declarations
      ~owner
      ~resource_fingerprint
      ~registrations
      ~selected_names
      ()
  =
  let open Result.Let_syntax in
  let%bind registration =
    create ?host_metadata ~declarations ~owner ~resource_fingerprint registrations
  in
  let%map policy =
    Authoring_policy.resolve_context
      ?context
      ?catalog
      ~ceiling:registration.capabilities
      ~selected_names
      ()
    |> Result.map_error ~f:(fun error -> C.{ code = error.code; message = error.message })
  in
  registration, policy
;;
