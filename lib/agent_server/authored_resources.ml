open Core
module P = Agent_protocol
module B = Agent_session.Runtime_builder
module Source = Agent_session.Authored_agent_source
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

let find t ~public ~name =
  Result.map (entry t ~public ~name) ~f:(fun entry -> entry.prepared)
;;

let resolve t (record : Agent_store.Delegation_store.record) ~public =
  let open Result.Let_syntax in
  match record.admission.authored_tool with
  | None ->
    denied "delegation.authored_resources: generated origin has no authored wrapper"
  | Some origin ->
    let%bind entry = entry t ~public ~name:origin.name in
    let%bind current = capabilities entry.prepared in
    Binding.resolve entry.binding ~record ~public ~current
;;

type plan =
  { parent_revision : Agent_session.Prompt_revision.t
  ; tool_name : string
  ; children : plan list
  }

let prepare t ~sw ~max_depth ~revision ~build ~services =
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
      let%bind source = Source.capture ~parent:revision ~tool_name in
      let path = (Source.identity source).root_relative_path in
      match
        List.length ancestors > max_depth || List.mem ancestors path ~equal:String.equal
      with
      | true -> denied "delegation.authored_graph: cyclic or excessive private ancestry"
      | false ->
        let%bind child_revision = Source.resource_revision ~parent:revision source in
        let%map children = plan (path :: ancestors) child_revision in
        { parent_revision = revision; tool_name; children })
    |> Result.all
  in
  let%bind plans = plan [ Revision.root_relative_path revision ] revision in
  let rec prepare plans =
    let%bind prepared =
      List.map plans ~f:(fun plan ->
        let%bind children, native_registrations = prepare plan.children in
        let%bind prepared =
          build
            ~parent_revision:plan.parent_revision
            ~tool_name:plan.tool_name
            ~native_registrations
        in
        let%bind public = capabilities prepared in
        let%map () = install t ~sw ~public children in
        prepared)
      |> Result.all
    in
    let%map registrations =
      List.map prepared ~f:(fun prepared ->
        let%bind capabilities = capabilities prepared in
        Agent_session.Authored_agent_call.registration
          ~source:prepared.source
          ~capabilities
          ~services:(services prepared)
          ())
      |> Result.all
    in
    prepared, registrations
  in
  prepare plans
;;
