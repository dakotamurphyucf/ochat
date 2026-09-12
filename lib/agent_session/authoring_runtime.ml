open Core
module Q = Chat_response.Authoring_context
module V = Chat_response.Authoring_validation
module P = Chat_response.Authoring_policy
module C = Chat_response.Tool_capability
module M = Chat_response.Authoring_materialization
module Metadata = Chatmd_shell_spec.Authoring_metadata
module Spec = Chatmd_shell_spec.Extension_spec
module CM = Prompt.Chat_markdown

type t =
  { context : Q.t
  ; host : V.host
  ; policy : P.t
  ; capabilities : C.t
  }

let context () =
  Q.create
    ~secret:
      (Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string)
    ()
;;

let configure_host ?host ~(policy : Chat_response.One_off_request.policy) () =
  let open Result.Let_syntax in
  let%bind host =
    match host with
    | Some host -> Ok host
    | None ->
      V.create_host
        ~runtime_identity:"ochat.extensibility.v1"
        ~targets:[ One_off_script; Standalone_tool; Moderator; Generated_chatmd ]
        ~moderator_surface:Ordinary
        ~compilation:policy.compilation
  in
  match V.catalog host with
  | Some _ -> Ok host
  | None ->
    let%bind context = context () in
    let%bind catalog = M.catalog context ~host in
    V.configure_generated host ~limits:(V.bundle_limits host) ~catalog:(Some catalog)
;;

let policy_spec elements =
  match
    List.filter_map elements ~f:(function
      | CM.Authoring_context spec -> Some spec
      | _ -> None)
  with
  | [] -> Ok None
  | [ spec ] -> Ok (Some spec)
  | _ -> Error "authoring.invalid_policy: multiple authoring-context declarations"
;;

let augment ~registrations elements =
  let open Result.Let_syntax in
  let%map context = policy_spec elements in
  let manual =
    Option.exists context ~f:(fun context -> Spec.equal_policy context.policy Manual)
  in
  let declared name =
    List.exists elements ~f:(function
      | CM.Tool (Builtin selected) | Tool (Persistent_agent ({ name = selected; _ }, _))
        -> String.equal name selected
      | _ -> false)
  in
  let active =
    List.exists elements ~f:(function
      | CM.Authoring_help _ -> true
      | _ -> false)
    || List.exists registrations ~f:(fun registration ->
      declared registration.Chat_response.Agent_runtime.implementation.info.function_.name
      && Option.exists registration.authoring_metadata ~f:(fun metadata ->
        Option.is_some metadata.Metadata.authoring))
  in
  match active && not manual with
  | false -> elements
  | true ->
    elements
    @ List.filter_map [ Metadata.Reference; Validation ] ~f:(fun helper ->
      let name = Metadata.helper_name helper in
      Option.some_if (not (declared name)) (CM.Tool (Builtin name)))
;;

let prepare ?admitted ~host ~elements ~capabilities () =
  let open Result.Let_syntax in
  let%bind context = context () in
  let%bind configured = policy_spec elements in
  let%bind policy =
    match admitted with
    | Some policy -> Ok policy
    | None ->
      P.resolve_context
        ?context:configured
        ?catalog:(V.catalog host)
        ~ceiling:capabilities
        ~selected_names:
          (List.filter_map (C.references capabilities) ~f:(fun reference ->
             let declared =
               List.exists elements ~f:(function
                 | CM.Tool (Builtin name) -> String.equal name reference.name
                 | _ -> false)
             in
             match C.find capabilities ~name:reference.name with
             | Ok binding when Option.is_some (C.metadata binding).helper && not declared
               -> None
             | _ -> Some reference.name))
        ()
      |> Result.map_error ~f:(fun error -> error.P.code ^ ": " ^ error.message)
  in
  let%bind _ =
    M.create ~context ~host ~policy ~capabilities ~scope:"host-preparation" ()
  in
  match P.authoring_tools policy with
  | [] -> Ok None
  | _ -> Ok (Some { context; host; policy; capabilities })
;;

let materialize t ~input =
  M.create
    ~context:t.context
    ~host:t.host
    ~policy:t.policy
    ~capabilities:t.capabilities
    ~scope:
      (M.session_scope
         ~session_id:input.Operation_worker.Input.session_id
         ~generation:input.session_generation)
    ()
  |> Result.map_error ~f:(fun message -> Agent_protocol.Error.invalid_request message)
;;
