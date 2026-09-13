open Core
module P = Agent_protocol
module C = Chat_response.Tool_capability
module Source = Authored_agent_source

type t =
  { source : Source.t
  ; reference : C.reference
  ; capabilities : C.t
  }

let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let capability result =
  Result.map_error result ~f:(fun (error : C.error) ->
    P.Error.create Permission_denied ~message:error.message ~retryable:false ())
;;

let implementation_revision ~source ~capabilities =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result (C.references capabilities) ~init:() ~f:(fun () reference ->
      let%bind binding =
        C.resolve capabilities ~id:reference.id ~fingerprint:reference.fingerprint
        |> capability
      in
      C.check_delegation binding |> capability)
  in
  let%map pins = Chat_response.Background_request.capability_pins capabilities in
  [%sexp
    ("ochat.authored-agent-binding.v1" : string)
  , (Source.fingerprint source : string)
  , (pins : (string * string) list)]
  |> Sexp.to_string_mach
  |> Chatmd_shell_spec.Source_ref.digest
;;

let wrapper ~source ~public ~(reference : C.reference) ~capabilities =
  let open Result.Let_syntax in
  let%bind revision = implementation_revision ~source ~capabilities in
  let%bind binding =
    C.resolve public ~id:reference.id ~fingerprint:reference.fingerprint |> capability
  in
  match C.implementation binding with
  | Native _
    when String.equal reference.name (Source.identity source).tool_name
         && String.equal reference.implementation_revision revision -> Ok ()
  | Native _ | Managed _ ->
    denied
      "delegation.authored_binding_changed: wrapper differs from admitted \
       source/resources"
;;

let bind ~source ~public ~reference ~capabilities =
  let open Result.Let_syntax in
  let%map () = wrapper ~source ~public ~reference ~capabilities in
  { source; reference; capabilities }
;;

let resolve t ~(record : Agent_store.Delegation_store.record) ~public ~current =
  let open Result.Let_syntax in
  let%bind () =
    match record.admission.authored_tool with
    | Some origin
      when String.equal origin.name (Source.identity t.source).tool_name
           && String.equal origin.source_sha256 (Source.fingerprint t.source) -> Ok ()
    | None | Some _ ->
      denied
        "delegation.authored_source_changed: record differs from authored declaration"
  in
  let%bind () =
    wrapper ~source:t.source ~public ~reference:t.reference ~capabilities:current
  in
  match String.equal (C.fingerprint current) (C.fingerprint t.capabilities) with
  | true -> Ok current
  | false ->
    denied "delegation.authored_resources_changed: private resources need fresh admission"
;;
