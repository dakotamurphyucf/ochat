open Core
module P = Agent_protocol
module B = Agent_session.Runtime_builder
module Source = Agent_session.Authored_agent_source
module Prepared_source = Agent_session.Authored_source_preparation
module Binding = Agent_session.Authored_agent_binding
module C = Chat_response.Tool_capability

type entry =
  { prepared : B.authored_resources
  ; reference : C.reference
  ; binding : Binding.t
  }

type t = entry String.Table.t

let create () = String.Table.create ()
let key reference = P.Id.Capability.to_string reference.C.id
let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let capability result =
  Result.map_error result ~f:(fun error ->
    P.Error.create Permission_denied ~message:error.C.message ~retryable:false ())
;;

let capabilities prepared =
  Lazy.force prepared.B.resources.native.capabilities |> capability
;;

let install t ~sw ~public prepared =
  let open Result.Let_syntax in
  let%bind entries =
    List.map prepared ~f:(fun (prepared : B.authored_resources) ->
      let%bind wrapper =
        C.find public ~name:(Source.identity prepared.source).tool_name |> capability
      in
      let reference = C.reference wrapper in
      let%bind capabilities = capabilities prepared in
      let%map binding =
        Binding.bind ~source:prepared.source ~public ~reference ~capabilities
      in
      { prepared; reference; binding })
    |> Result.all
  in
  let keys = List.map entries ~f:(fun entry -> key entry.reference) in
  match
    List.contains_dup keys ~compare:String.compare || List.exists keys ~f:(Hashtbl.mem t)
  with
  | true ->
    denied "delegation.authored_resources: wrapper is already owned by a resource scope"
  | false ->
    (* No suspension between validation, publication and cleanup registration.
       Factory resource ownership and lookup run on its actor domain. *)
    Eio.Cancel.protect (fun () ->
      Eio.Switch.check sw;
      List.iter entries ~f:(fun entry ->
        Hashtbl.add_exn t ~key:(key entry.reference) ~data:entry);
      Eio.Switch.on_release sw (fun () -> List.iter keys ~f:(Hashtbl.remove t));
      Ok ())
;;

let entry t ~public ~name =
  let open Result.Let_syntax in
  let%bind wrapper = C.find public ~name |> capability in
  let%bind entry =
    Hashtbl.find t (key (C.reference wrapper))
    |> Result.of_option
         ~error:
           (P.Error.create
              Permission_denied
              ~message:
                "delegation.authored_resources: owning resource scope is unavailable"
              ~retryable:false
              ())
  in
  let%map _ =
    C.resolve public ~id:entry.reference.id ~fingerprint:entry.reference.fingerprint
    |> capability
  in
  entry
;;

let realized_capabilities prepared =
  if Lazy.is_val prepared.B.resources.native.capabilities
  then capabilities prepared
  else denied "delegation.authored_resources: private capabilities are not realized"
;;

let find t ~public ~name =
  let open Result.Let_syntax in
  let%bind entry = entry t ~public ~name in
  let%bind current = realized_capabilities entry.prepared in
  let%map () = Binding.check_current entry.binding ~public ~current in
  entry.prepared
;;

let resolve t (record : Agent_store.Delegation_store.record) ~public =
  let open Result.Let_syntax in
  let%bind source =
    Agent_store.Delegation_store.Admission.authored_source record.admission
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  match source with
  | None ->
    denied "delegation.authored_resources: generated origin has no authored wrapper"
  | Some origin ->
    let%bind entry = entry t ~public ~name:origin.name in
    let%bind current = realized_capabilities entry.prepared in
    Binding.resolve entry.binding ~record ~public ~current
;;

module Source_plan = struct
  module Node = struct
    type t =
      { source : Prepared_source.t
      ; children : t list
      }
  end

  type t = Node.t list

  let capture ~max_depth ~revision ~capture =
    let module Revision = Agent_session.Prompt_revision in
    let open Result.Let_syntax in
    (* Validate the entire captured graph before native preparation can connect an
     MCP server or request shell admission. Paths are relative to one immutable
     source closure, so a back edge cannot evade cycle detection by re-rooting. *)
    let rec plan ancestors revision =
      Revision.elements revision
      |> List.filter_map ~f:(function
        | Prompt.Chat_markdown.Tool (Persistent_agent (agent, _)) -> Some agent.name
        | _ -> None)
      |> List.map ~f:(fun tool_name ->
        let%bind source = capture ~parent_revision:revision ~tool_name in
        let path = (Source.identity (Prepared_source.source source)).root_relative_path in
        match
          List.length ancestors > max_depth || List.mem ancestors path ~equal:String.equal
        with
        | true -> denied "delegation.authored_graph: cyclic or excessive private ancestry"
        | false ->
          let%map children = plan (path :: ancestors) (Prepared_source.revision source) in
          ({ source; children } : Node.t))
      |> Result.all
    in
    plan [ Revision.root_relative_path revision ] revision
  ;;

  let equivalent a b =
    let a_source = Prepared_source.source a in
    let b_source = Prepared_source.source b in
    let a_revision = Prepared_source.revision a in
    let b_revision = Prepared_source.revision b in
    Source.Identity.equal (Source.identity a_source) (Source.identity b_source)
    && String.equal (Source.fingerprint a_source) (Source.fingerprint b_source)
    && P.Id.Prompt_revision.equal
         (Agent_session.Prompt_revision.id a_revision)
         (Agent_session.Prompt_revision.id b_revision)
    && String.equal
         (Agent_session.Prompt_revision.artifact a_revision).manifest_sha256
         (Agent_session.Prompt_revision.artifact b_revision).manifest_sha256
  ;;

  let select t ~(selector : Agent_store.Captured_source_kind.Authored_tool.t) ~descend =
    let open Result.Let_syntax in
    let rec search (selected : Node.t option) nodes =
      List.fold_result nodes ~init:selected ~f:(fun selected (node : Node.t) ->
        let source = Prepared_source.source node.source in
        let%bind selected =
          if
            String.equal selector.name (Source.identity source).tool_name
            && String.equal selector.source_sha256 (Source.fingerprint source)
          then (
            match selected with
            | None -> Ok (Some node)
            | Some prior ->
              if equivalent prior.source node.source
              then Ok selected
              else denied "delegation.authored_graph: source selector is ambiguous")
          else Ok selected
        in
        if descend then search selected node.children else Ok selected)
    in
    let%bind selected = search None t in
    Result.of_option
      selected
      ~error:
        (P.Error.create
           Permission_denied
           ~message:"delegation.authored_graph: source selector is unavailable"
           ~retryable:false
           ())
  ;;

  let find t ~selector =
    select t ~selector ~descend:true |> Result.map ~f:(fun node -> node.Node.source)
  ;;

  let find_direct t ~selector =
    select t ~selector ~descend:false
    |> Result.map ~f:(fun node -> node.Node.source, node.children)
  ;;
end

let prepare t ~sw ~max_depth ~revision ~capture ~build ~services ~prepare_source =
  let open Result.Let_syntax in
  let%bind plans = Source_plan.capture ~max_depth ~revision ~capture in
  let rec prepare (plans : Source_plan.t) =
    let%bind prepared =
      List.map plans ~f:(fun (plan : Source_plan.Node.t) ->
        let%bind children, native_registrations = prepare plan.children in
        let%bind prepared = build ~source:plan.source ~native_registrations in
        let%bind public = capabilities prepared in
        let%map () = install t ~sw ~public children in
        prepared)
      |> Result.all
    in
    let%map registrations =
      List.map prepared ~f:(fun prepared ->
        let%bind capabilities = capabilities prepared in
        let%map registration =
          Agent_session.Authored_agent_call.registration
            ~source:prepared.source
            ~capabilities
            ~services:(services prepared)
            ()
        in
        let source =
          Chat_response.Native_source_registration.create
            ~implementation:registration.implementation
            ~prepare:(fun ~launch ~request ~checkpoint ->
              match Chat_response.Source_launch.conversation_owner launch with
              | None ->
                Error
                  (Agent_protocol.Error.invalid_request
                     "persistent agent source requires a conversation owner")
              | Some owner ->
                prepare_source
                  prepared
                  ~implementation:registration.implementation
                  ~owner
                  ~request
                  ~checkpoint)
        in
        { registration with source_requirement = Source (Some source) })
      |> Result.all
    in
    prepared, registrations
  in
  prepare plans
;;
