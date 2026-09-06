open! Core

type update =
  | Projection of Agent_projection.t
  | Connection_changed of Connection_status.t
  | Connection_failed of Agent_protocol.Error.t

type shared =
  { mutex : Eio.Mutex.t
  ; updates : update Eio.Stream.t
  ; mutable projection : Agent_projection.t option
  ; mutable status : Connection_status.t
  }

type t =
  { handle : Agent_client.Reconnect.t
  ; shared : shared
  }

type create_options =
  { prompt : string
  ; workspace : string
  ; liveness : Agent_protocol.Session.liveness
  ; permission_profile : string option
  ; display_name : string option
  ; labels : (string * string) list
  ; mode : Agent_protocol.Session.attachment_mode
  }

let publish shared update =
  ignore (Eio.Stream.take_nonblocking shared.updates : update option);
  Eio.Stream.add shared.updates update
;;

let install_failure shared failure =
  Eio.Mutex.use_rw ~protect:true shared.mutex (fun () ->
    shared.status <- Connection_status.failed failure);
  publish shared (Connection_failed failure)
;;

let connection_status = function
  | Agent_client.Reconnect.Connected -> Connection_status.connected ()
  | Reconnecting { attempt } -> Connection_status.reconnecting ~attempt
  | Disconnected -> Connection_status.disconnected ()
  | Failed failure -> Connection_status.failed failure
;;

let install_status shared status =
  let status = connection_status status in
  Eio.Mutex.use_rw ~protect:true shared.mutex (fun () -> shared.status <- status);
  publish shared (Connection_changed status)
;;

let install_projection shared projection =
  match Agent_projection.of_client_projection projection with
  | Error failure -> install_failure shared failure
  | Ok projection ->
    Eio.Mutex.use_rw ~protect:true shared.mutex (fun () ->
      shared.projection <- Some projection;
      shared.status <- Connection_status.connected ());
    publish shared (Projection projection)
;;

let attach ~sw ~clock ~connection ?(reconnect = None) ~session_id ~mode () =
  let open Result.Let_syntax in
  let shared =
    { mutex = Eio.Mutex.create ()
    ; updates = Eio.Stream.create 1
    ; projection = None
    ; status = Connection_status.connected ()
    }
  in
  let%bind handle =
    Agent_client.Reconnect.attach
      ~sw
      ~clock
      ~connection
      ~reconnect
      ~session_id
      ~mode
      ~on_update:(install_projection shared)
      ~on_status:(install_status shared)
      ~on_error:(install_failure shared)
      ()
  in
  let%map projection =
    Agent_projection.of_client_projection (Agent_client.Reconnect.projection handle)
  in
  shared.projection <- Some projection;
  publish shared (Projection projection);
  { handle; shared }
;;

let create_spec connection options =
  let open Result.Let_syntax in
  let%bind prompt = Agent_client.Catalog.resolve_prompt connection ~name:options.prompt in
  let%bind workspace =
    Agent_client.Catalog.resolve_workspace connection ~name:options.workspace
  in
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt.id)
    ~workspace:(Configured workspace.id)
    ~liveness:options.liveness
    ~persistence:Durable
    ?permission_profile:options.permission_profile
    ~start_immediately:true
    ?display_name:options.display_name
    ~labels:options.labels
    ()
;;

let create ~sw ~clock ~connection ?(reconnect = None) options =
  let open Result.Let_syntax in
  let%bind _ =
    Agent_client.Session_handle.initialize
      connection
      ~implementation_name:"chat-tui"
      ~implementation_version:"dev"
  in
  let%bind spec = create_spec connection options in
  let shared =
    { mutex = Eio.Mutex.create ()
    ; updates = Eio.Stream.create 1
    ; projection = None
    ; status = Connection_status.connected ()
    }
  in
  let%bind handle =
    Agent_client.Reconnect.create
      ~sw
      ~clock
      ~connection
      ~reconnect
      ~spec
      ~mode:options.mode
      ~on_update:(install_projection shared)
      ~on_status:(install_status shared)
      ~on_error:(install_failure shared)
      ()
  in
  let%map projection =
    Agent_projection.of_client_projection (Agent_client.Reconnect.projection handle)
  in
  shared.projection <- Some projection;
  publish shared (Projection projection);
  { handle; shared }
;;

let projection t =
  Eio.Mutex.use_ro t.shared.mutex (fun () -> Option.value_exn t.shared.projection)
;;

let status t = Eio.Mutex.use_ro t.shared.mutex (fun () -> t.shared.status)
let next_update t = Eio.Stream.take t.shared.updates
let take_update_nonblocking t = Eio.Stream.take_nonblocking t.shared.updates
let send_content t content = Agent_client.Reconnect.send_message t.handle content

let send_text t text =
  send_content
    t
    Agent_protocol.Session.Message_content.{ kind = Plain_text; text; attachments = [] }
;;

let compact t =
  let snapshot = Agent_projection.snapshot (projection t) in
  Agent_client.Reconnect.compact t.handle ~expected_revision:(Some snapshot.revision)
;;

let delete_history t history_id =
  let snapshot = Agent_projection.snapshot (projection t) in
  Agent_client.Reconnect.delete_history
    t.handle
    ~expected_revision:snapshot.revision
    history_id
;;

let cancel_active_operation t =
  let session = (Agent_projection.snapshot (projection t)).session in
  match session.active_operation with
  | None ->
    Error
      (Agent_protocol.Error.create
         Invalid_state
         ~message:"session has no active operation"
         ~retryable:false
         ())
  | Some operation -> Agent_client.Reconnect.cancel_operation t.handle operation.id
;;

let start t ~queue_if_limited = Agent_client.Reconnect.start t.handle ~queue_if_limited
let stop t ~mode = Agent_client.Reconnect.stop t.handle ~mode
let respond_permission t = Agent_client.Reconnect.respond_permission t.handle
let revoke_grant t = Agent_client.Reconnect.revoke_grant t.handle
let read_audit t = Agent_client.Reconnect.read_audit t.handle
let detach t = Agent_client.Reconnect.detach t.handle
let close t = Agent_client.Reconnect.close t.handle
let attachment t = Agent_client.Reconnect.attachment t.handle
