open! Core
module P = Agent_protocol
module Policy = Agent_session.Permission_policy
module Catalogs = Session_factory.Developer_catalog
module Capability = Chat_response.Tool_capability

module Mode = struct
  type authorizer = P.Host_invocation.t -> Capability.binding -> (unit, P.Error.t) result

  type t =
    | Explicit_callback of authorizer
    | Selected_prompt_profile
end

type t =
  { profile_id : string
  ; revision_digest : string
  ; profile : Policy.t
  }

let denied message = P.Error.create Permission_denied ~message ~retryable:false ()

let pin ~catalogs ~profile_id =
  match Catalogs.find_profile catalogs profile_id with
  | None -> Error (denied "selected developer permission profile is unavailable")
  | Some profile -> Ok { profile_id; revision_digest = profile.revision_digest; profile }
;;

let check_current t ~catalogs =
  Catalogs.with_admission catalogs ~f:(fun () ->
    match Catalogs.find_profile catalogs t.profile_id with
    | Some profile when String.equal profile.revision_digest t.revision_digest -> Ok ()
    | Some _ | None -> Error (denied "selected developer permission profile changed"))
  |> Result.map_error ~f:(function
    | Catalogs.Admission.Error.Busy -> denied "developer permission catalog is busy"
    | Publication_uncertain -> denied "developer permission catalog is uncertain"
    | Foreign_domain -> denied "developer permission catalog belongs to another domain")
  |> Result.join
;;

let check_runtime_binding runtime invocation binding =
  let open Result.Let_syntax in
  let reference = Capability.reference binding in
  let context = P.Host_invocation.context invocation in
  let%bind capabilities =
    Lazy.force runtime.Chat_response.Agent_runtime.capabilities
    |> Result.map_error ~f:(fun _ -> denied "selected native runtime is unavailable")
  in
  match
    Capability.resolve capabilities ~id:reference.id ~fingerprint:reference.fingerprint
  with
  | Error _ -> Error (denied "native binding is not in the selected runtime")
  | Ok selected ->
    let same_implementation =
      match Capability.implementation binding, Capability.implementation selected with
      | Capability.Native implementation, Capability.Native selected_implementation ->
        phys_equal implementation selected_implementation
      | Capability.Managed target, Capability.Managed selected_target ->
        Chatmd_shell_spec.Extension_spec.equal_implementation target selected_target
      | Capability.Native _, Capability.Managed _
      | Capability.Managed _, Capability.Native _ -> false
    in
    if
      same_implementation
      && String.equal
           (Capability.permission_fingerprint binding)
           (Capability.permission_fingerprint selected)
      && String.equal context.tool_name reference.name
      && String.equal context.implementation_revision reference.implementation_revision
    then Ok (Capability.native_implementation selected |> Option.is_some)
    else Error (denied "native binding does not match the selected implementation")
;;

let authorize t ~runtime ~check_current invocation binding =
  let open Result.Let_syntax in
  let%bind () = check_current () in
  let%bind native_binding = check_runtime_binding runtime invocation binding in
  let context = P.Host_invocation.context invocation in
  if native_binding && Set.mem runtime.shell_tool_names context.tool_name
  then check_current ()
  else (
    let identity_digest =
      String.concat
        ~sep:"\000"
        [ "ochat.native-permission.v2"
        ; t.revision_digest
        ; context.tool_name
        ; context.implementation_revision
        ; Capability.permission_fingerprint binding
        ; Jsonaf.to_string context.input
        ]
      |> Chatmd_shell_spec.Source_ref.digest
    in
    let request : Policy.invocation =
      { tool_name = context.tool_name
      ; identity_digest
      ; invocation_display = context.tool_name ^ "(<redacted>)"
      ; effects = [ "tool_invocation" ]
      }
    in
    let decision = Policy.decide t.profile ~responder_available:false request in
    let%bind () = check_current () in
    match decision with
    | Allow_now -> Ok ()
    | Deny_now _ | Request_permission ->
      Error (denied "native invocation denied by developer permission profile")
    | Request_review ->
      let result = Policy.review t.profile request in
      let%bind () = check_current () in
      (match result with
       | Ok Allow -> Ok ()
       | Ok (Deny _) | Error _ ->
         Error (denied "native invocation denied by developer permission reviewer")))
;;
