open! Core
module P = Agent_protocol
module R = P.Developer_repl_wire
module C = P.Developer_common

type retained =
  { request : R.Request.t
  ; initialization : P.Initialize.Response.t
  ; canonical_params : string
  ; byte_count : int
  }

module Pending_create = struct
  type identity =
    { run_id : P.Id.Developer_run.t
    ; key : P.Idempotency_key.t
    }

  type t =
    { retained : retained
    ; identity : identity
    }

  let identity t = t.identity
end

module Pending_submit = struct
  type identity =
    { instance : R.Wire_id.Instance.t
    ; epoch : R.Wire_id.Epoch.t
    ; expected_revision : R.Wire_id.Revision.t
    ; key : P.Idempotency_key.t
    }

  type t =
    { retained : retained
    ; identity : identity
    }

  let identity t = t.identity
end

module Pending_task = struct
  type identity =
    { run_id : P.Id.Developer_run.t
    ; key : P.Idempotency_key.t
    ; fresh_epoch : (R.Wire_id.Instance.t * R.Wire_id.Epoch.t) option
    }

  type t =
    { retained : retained
    ; identity : identity
    }

  let identity t = t.identity
end

module Pending_reset = struct
  type identity =
    { instance : R.Wire_id.Instance.t
    ; epoch : R.Wire_id.Epoch.t
    ; key : P.Idempotency_key.t
    }

  type t =
    { retained : retained
    ; identity : identity
    }

  let identity t = t.identity
end

module Pending_close = struct
  type identity =
    { instance : R.Wire_id.Instance.t
    ; key : P.Idempotency_key.t
    }

  type t =
    { retained : retained
    ; identity : identity
    }

  let identity t = t.identity
end

type pending =
  { creates : Pending_create.t list
  ; submits : Pending_submit.t list
  ; tasks : Pending_task.t list
  ; resets : Pending_reset.t list
  ; closes : Pending_close.t list
  }

type t =
  { connection : Connection.t
  ; initialization : P.Initialize.Response.t
  ; capabilities : R.Capabilities.t
  ; mutex : Eio.Mutex.t
  ; mutable pending : pending
  }

let empty_pending = { creates = []; submits = []; tasks = []; resets = []; closes = [] }
let maximum_pending = 64
let maximum_pending_bytes = 16 * 1024 * 1024

let bounded_snapshot_count pending =
  let rec count_list count = function
    | [] -> Some count
    | _ :: rest -> if count >= maximum_pending then None else count_list (count + 1) rest
  in
  let open Option.Let_syntax in
  let%bind count = count_list 0 pending.creates in
  let%bind count = count_list count pending.submits in
  let%bind count = count_list count pending.tasks in
  let%bind count = count_list count pending.resets in
  count_list count pending.closes
;;

let incompatible message =
  P.Error.create Incompatible_protocol ~message ~retryable:false ()
;;

let interrupted message = P.Error.create Interrupted ~message ~retryable:false ()
let resource_limit message = P.Error.create Resource_limit ~message ~retryable:false ()
let conflict message = P.Error.create Idempotency_conflict ~message ~retryable:false ()

let with_lock t f =
  match
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      match f () with
      | value -> Ok value
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  with
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let current t =
  match Connection.initialization t.connection with
  | Some initialization when phys_equal initialization t.initialization -> Ok ()
  | Some _ | None -> Error (incompatible "REPL initialization changed")
;;

let feature = function
  | R.Request.Watch _ | Unwatch _ -> R.Features.events
  | Capabilities
  | Create _
  | Submit _
  | Observe _
  | Cancel_submission _
  | Inspect_type _
  | Task_references_page _
  | Task_submit _
  | Task_observe _
  | Task_await_terminal _
  | Task_cancel_and_wait _
  | Reset_and_wait _
  | Close_and_wait _ -> R.Features.repl
;;

let preflight t request =
  let open Result.Let_syntax in
  let%bind () = current t in
  if
    not (List.mem t.initialization.enabled_features (feature request) ~equal:String.equal)
  then Error (incompatible "REPL feature was not selected")
  else if not (R.Capabilities.supports t.capabilities (R.Request.method_name request))
  then Error (incompatible "REPL method is not advertised")
  else
    R.Request.of_method_and_params
      ~limits:(R.Capabilities.limits t.capabilities)
      ~method_:(R.Request.method_name request)
      ~params:(R.Request.params request)
;;

let create connection =
  let open Result.Let_syntax in
  let%bind initialization =
    Connection.initialization connection
    |> Result.of_option ~error:(incompatible "REPL requires initialization")
  in
  let%bind capabilities = Connection.repl_capabilities connection in
  let t =
    { connection
    ; initialization
    ; capabilities
    ; mutex = Eio.Mutex.create ()
    ; pending = empty_pending
    }
  in
  let%map _ = preflight t R.Request.Capabilities in
  t
;;

let capabilities t = t.capabilities
let pending t = with_lock t (fun () -> t.pending)

let all_retained pending =
  List.concat
    [ List.map pending.creates ~f:(fun item -> item.Pending_create.retained)
    ; List.map pending.submits ~f:(fun item -> item.Pending_submit.retained)
    ; List.map pending.tasks ~f:(fun item -> item.Pending_task.retained)
    ; List.map pending.resets ~f:(fun item -> item.Pending_reset.retained)
    ; List.map pending.closes ~f:(fun item -> item.Pending_close.retained)
    ]
;;

let has_capacity t byte_count =
  let retained = all_retained t.pending in
  let consumed = List.sum (module Int) retained ~f:(fun item -> item.byte_count) in
  List.length retained < maximum_pending
  && byte_count <= maximum_pending_bytes
  && consumed <= maximum_pending_bytes - byte_count
;;

let same_source (original : P.Initialize.Response.t) (current : P.Initialize.Response.t) =
  P.Id.Server.equal original.P.Initialize.Response.server_id current.server_id
  && P.Id.Principal.equal original.principal.id current.principal.id
  && String.equal
       original.principal.authentication_kind
       current.principal.authentication_kind
;;

let same_create a b =
  let a = Pending_create.identity a
  and b = Pending_create.identity b in
  P.Id.Developer_run.equal a.run_id b.run_id && P.Idempotency_key.equal a.key b.key
;;

let same_submit a b =
  let a = Pending_submit.identity a
  and b = Pending_submit.identity b in
  R.Wire_id.Instance.equal a.instance b.instance && P.Idempotency_key.equal a.key b.key
;;

let same_task a b =
  let a = Pending_task.identity a
  and b = Pending_task.identity b in
  P.Id.Developer_run.equal a.run_id b.run_id && P.Idempotency_key.equal a.key b.key
;;

let same_reset a b =
  let a = Pending_reset.identity a
  and b = Pending_reset.identity b in
  R.Wire_id.Instance.equal a.instance b.instance && P.Idempotency_key.equal a.key b.key
;;

let same_close a b =
  let a = Pending_close.identity a
  and b = Pending_close.identity b in
  R.Wire_id.Instance.equal a.instance b.instance && P.Idempotency_key.equal a.key b.key
;;

let merge_items ~same existing incoming =
  List.fold incoming ~init:(Ok existing) ~f:(fun result item ->
    Result.bind result ~f:(fun items ->
      match List.find items ~f:(same item) with
      | None -> Ok (item :: items)
      | Some old when phys_equal old item -> Ok items
      | Some _ -> Error (conflict "REPL pending key has conflicting custody")))
;;

let adopt_pending t incoming =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind () = current t in
    if Option.is_none (bounded_snapshot_count incoming)
    then Error (resource_limit "REPL pending adoption exceeds item capacity")
    else if
      not
        (List.for_all (all_retained incoming) ~f:(fun item ->
           same_source item.initialization t.initialization))
    then Error (incompatible "REPL pending item belongs to another identity")
    else (
      let%bind creates =
        merge_items ~same:same_create t.pending.creates incoming.creates
      in
      let%bind submits =
        merge_items ~same:same_submit t.pending.submits incoming.submits
      in
      let%bind tasks = merge_items ~same:same_task t.pending.tasks incoming.tasks in
      let%bind resets = merge_items ~same:same_reset t.pending.resets incoming.resets in
      let%bind closes = merge_items ~same:same_close t.pending.closes incoming.closes in
      let combined = { creates; submits; tasks; resets; closes } in
      let retained = all_retained combined in
      let within_bytes =
        List.fold retained ~init:(Some 0) ~f:(fun total item ->
          Option.bind total ~f:(fun total ->
            if item.byte_count < 0 || item.byte_count > maximum_pending_bytes - total
            then None
            else Some (total + item.byte_count)))
        |> Option.is_some
      in
      if List.length retained > maximum_pending || not within_bytes
      then Error (resource_limit "REPL pending adoption exceeds capacity")
      else (
        t.pending <- combined;
        Ok ())))
;;

let retain t request =
  let open Result.Let_syntax in
  let%bind canonical_params = P.Json_codec.canonical_string (R.Request.params request) in
  let byte_count = String.length canonical_params in
  if has_capacity t byte_count
  then Ok { request; initialization = t.initialization; canonical_params; byte_count }
  else Error (resource_limit "REPL pending capacity exhausted")
;;

let send t request =
  Connection.request_repl
    t.connection
    ~limits:(R.Capabilities.limits t.capabilities)
    request
;;

let perform t ~request ~get ~set ~same ~make ~decode =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind request = preflight t request in
    let%bind retained = retain t request in
    let item = make retained in
    if List.exists (get t.pending) ~f:(same item)
    then Error (interrupted "REPL key remains unresolved")
    else (
      t.pending <- set t.pending (item :: get t.pending);
      let%bind response = send t request in
      let%map value = decode t item response in
      t.pending
      <- set t.pending (List.filter (get t.pending) ~f:(fun x -> not (phys_equal x item)));
      value))
;;

let retry t item ~get ~set ~decode ~retained =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    if not (List.exists (get t.pending) ~f:(phys_equal item))
    then Error (incompatible "REPL pending item is foreign")
    else (
      let original = retained item in
      let%bind _ = preflight t original.request in
      let%bind response = send t original.request in
      let%map value = decode t item response in
      t.pending
      <- set t.pending (List.filter (get t.pending) ~f:(fun x -> not (phys_equal x item)));
      value))
;;

let abandon t item ~get ~set =
  with_lock t (fun () ->
    if not (List.exists (get t.pending) ~f:(phys_equal item))
    then Error (incompatible "REPL pending item is foreign")
    else (
      t.pending
      <- set t.pending (List.filter (get t.pending) ~f:(fun x -> not (phys_equal x item)));
      Ok ()))
;;

let get_creates p = p.creates
let get_submits p = p.submits
let get_tasks p = p.tasks
let get_resets p = p.resets
let get_closes p = p.closes
let set_creates p creates = { p with creates }
let set_submits p submits = { p with submits }
let set_tasks p tasks = { p with tasks }
let set_resets p resets = { p with resets }
let set_closes p closes = { p with closes }

let decode_create _ item = function
  | R.Response.Create view
    when P.Id.Developer_run.equal (R.View.run_id view) item.Pending_create.identity.run_id
    -> Ok view
  | _ -> Error (incompatible "REPL Create response changed identity")
;;

let decode_submit _ item = function
  | R.Response.Submit submission
    when R.Wire_id.Epoch.equal
           (R.Submission.epoch submission)
           item.Pending_submit.identity.epoch -> Ok submission
  | _ -> Error (incompatible "REPL Submit response changed identity")
;;

let task_view_matches t item view =
  let owner = R.Task_view.owner view in
  P.Id.Server.equal owner.host_id t.initialization.server_id
  && P.Id.Principal.equal owner.principal_id t.initialization.principal.id
  && P.Id.Developer_run.equal owner.run_id item.Pending_task.identity.run_id
  && P.Idempotency_key.equal (R.Task_view.key view) item.Pending_task.identity.key
;;

let decode_task t item = function
  | R.Response.Task_submit view when task_view_matches t item view -> Ok view
  | _ -> Error (incompatible "REPL Task response changed identity")
;;

let decode_reset _ item = function
  | R.Response.Reset_and_wait view
    when R.Wire_id.Instance.equal
           (R.View.instance view)
           item.Pending_reset.identity.instance -> Ok view
  | _ -> Error (incompatible "REPL Reset response changed identity")
;;

let decode_close _ _ = function
  | R.Response.Close_and_wait -> Ok ()
  | _ -> Error (incompatible "REPL Close response changed method")
;;

let create_instance t request =
  let pending_identity =
    Pending_create.{ run_id = R.Create.run_id request; key = R.Create.key request }
  in
  perform
    t
    ~request:(R.Request.Create request)
    ~get:get_creates
    ~set:set_creates
    ~same:same_create
    ~make:(fun retained -> Pending_create.{ retained; identity = pending_identity })
    ~decode:decode_create
;;

let submit t request =
  let pending_identity =
    Pending_submit.
      { instance = R.Submit.instance request
      ; epoch = R.Submit.epoch request
      ; expected_revision = R.Submit.expected_revision request
      ; key = R.Submit.key request
      }
  in
  perform
    t
    ~request:(R.Request.Submit request)
    ~get:get_submits
    ~set:set_submits
    ~same:same_submit
    ~make:(fun retained -> Pending_submit.{ retained; identity = pending_identity })
    ~decode:decode_submit
;;

let task_submit t ~view request =
  match R.Task_submit.fresh_epoch request with
  | None -> Error (incompatible "new REPL Task requires a current epoch")
  | Some (instance, epoch)
    when not
           (R.Wire_id.Instance.equal instance (R.View.instance view)
            && R.Wire_id.Epoch.equal epoch (R.View.epoch view)) ->
    Error (incompatible "new REPL Task epoch differs from its Create/Reset view")
  | Some _ ->
    let pending_identity =
      Pending_task.
        { run_id = R.View.run_id view
        ; key = R.Task_submit.key request
        ; fresh_epoch = R.Task_submit.fresh_epoch request
        }
    in
    perform
      t
      ~request:(R.Request.Task_submit request)
      ~get:get_tasks
      ~set:set_tasks
      ~same:same_task
      ~make:(fun retained -> Pending_task.{ retained; identity = pending_identity })
      ~decode:decode_task
;;

let reset_and_wait t request =
  let pending_identity =
    Pending_reset.
      { instance = R.Current_epoch.instance request
      ; epoch = R.Current_epoch.epoch request
      ; key = R.Current_epoch.key request
      }
  in
  perform
    t
    ~request:(R.Request.Reset_and_wait request)
    ~get:get_resets
    ~set:set_resets
    ~same:same_reset
    ~make:(fun retained -> Pending_reset.{ retained; identity = pending_identity })
    ~decode:decode_reset
;;

let close_and_wait t request =
  let pending_identity =
    Pending_close.{ instance = R.Lookup.instance request; key = R.Lookup.key request }
  in
  perform
    t
    ~request:(R.Request.Close_and_wait request)
    ~get:get_closes
    ~set:set_closes
    ~same:same_close
    ~make:(fun retained -> Pending_close.{ retained; identity = pending_identity })
    ~decode:decode_close
;;

let retry_create t item =
  retry
    t
    item
    ~get:get_creates
    ~set:set_creates
    ~decode:decode_create
    ~retained:(fun item -> item.Pending_create.retained)
;;

let retry_submit t item =
  retry
    t
    item
    ~get:get_submits
    ~set:set_submits
    ~decode:decode_submit
    ~retained:(fun item -> item.Pending_submit.retained)
;;

let retry_task t item =
  retry t item ~get:get_tasks ~set:set_tasks ~decode:decode_task ~retained:(fun item ->
    item.Pending_task.retained)
;;

let retry_reset t item =
  retry t item ~get:get_resets ~set:set_resets ~decode:decode_reset ~retained:(fun item ->
    item.Pending_reset.retained)
;;

let retry_close t item =
  retry t item ~get:get_closes ~set:set_closes ~decode:decode_close ~retained:(fun item ->
    item.Pending_close.retained)
;;

let abandon_create t item = abandon t item ~get:get_creates ~set:set_creates
let abandon_submit t item = abandon t item ~get:get_submits ~set:set_submits
let abandon_task t item = abandon t item ~get:get_tasks ~set:set_tasks
let abandon_reset t item = abandon t item ~get:get_resets ~set:set_resets
let abandon_close t item = abandon t item ~get:get_closes ~set:set_closes

let read_locked t request ~decode =
  let open Result.Let_syntax in
  let%bind request = preflight t request in
  let%bind response = send t request in
  decode response
;;

let read t request ~decode = with_lock t (fun () -> read_locked t request ~decode)

let observe t lookup =
  read t (R.Request.Observe lookup) ~decode:(function
    | R.Response.Observe value -> Ok value
    | _ -> Error (incompatible "REPL Observe response changed method"))
;;

let cancel_submission t lookup =
  Connection.request_repl_control
    t.connection
    ~initialized_as:t.initialization
    ~limits:(R.Capabilities.limits t.capabilities)
    (Connection.Repl_control.Cancel_submission lookup)
  |> Result.bind ~f:(function
    | R.Response.Cancel_submission value -> Ok value
    | _ -> Error (incompatible "REPL Cancel response changed method"))
;;

let inspect_type t selector =
  read t (R.Request.Inspect_type selector) ~decode:(function
    | R.Response.Inspect_type value -> Ok value
    | _ -> Error (incompatible "REPL Inspect response changed method"))
;;

let task_references_page t request =
  read t (R.Request.Task_references_page request) ~decode:(function
    | R.Response.Task_references_page value -> Ok value
    | _ -> Error (incompatible "REPL reference page response changed method"))
;;

let task_observe t lookup =
  read t (R.Request.Task_observe lookup) ~decode:(function
    | R.Response.Task_observe value -> Ok value
    | _ -> Error (incompatible "REPL Task Observe response changed method"))
;;

let task_await_terminal t lookup =
  read t (R.Request.Task_await_terminal lookup) ~decode:(function
    | R.Response.Task_await_terminal value -> Ok value
    | _ -> Error (incompatible "REPL Task Await response changed method"))
;;

let task_cancel_and_wait t lookup =
  Connection.request_repl_control
    t.connection
    ~initialized_as:t.initialization
    ~limits:(R.Capabilities.limits t.capabilities)
    (Connection.Repl_control.Task_cancel_and_wait lookup)
  |> Result.bind ~f:(function
    | R.Response.Task_cancel_and_wait value -> Ok value
    | _ -> Error (incompatible "REPL Task Cancel response changed method"))
;;

let observe_pending_submit t item =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    if not (List.exists t.pending.submits ~f:(phys_equal item))
    then Error (incompatible "REPL pending Submit is foreign")
    else (
      let identity = Pending_submit.identity item in
      let lookup = R.Lookup.create ~instance:identity.instance ~key:identity.key in
      let%bind result =
        read_locked t (R.Request.Observe lookup) ~decode:(function
          | R.Response.Observe value -> Ok value
          | _ -> Error (incompatible "REPL Observe response changed method"))
      in
      match result with
      | Some value
        when not (R.Wire_id.Epoch.equal (R.Submission.epoch value) identity.epoch) ->
        Error (incompatible "REPL pending Observe changed epoch")
      | Some _ | None -> Ok result))
;;

let observe_pending_task t item =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    if not (List.exists t.pending.tasks ~f:(phys_equal item))
    then Error (incompatible "REPL pending Task is foreign")
    else (
      let identity = Pending_task.identity item in
      let lookup = C.Lookup.create ~run_id:identity.run_id ~key:identity.key in
      let%bind result =
        read_locked t (R.Request.Task_observe lookup) ~decode:(function
          | R.Response.Task_observe value -> Ok value
          | _ -> Error (incompatible "REPL Task Observe response changed method"))
      in
      match result with
      | Some value when not (task_view_matches t item value) ->
        Error (incompatible "REPL pending Task Observe changed identity")
      | Some _ | None -> Ok result))
;;

let watch_and_observe_repl t lookup =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind () =
      read_locked t (R.Request.Watch (R.Watch_key.Repl lookup)) ~decode:(function
        | R.Response.Watch -> Ok ()
        | _ -> Error (incompatible "REPL Watch response changed method"))
    in
    read_locked t (R.Request.Observe lookup) ~decode:(function
      | R.Response.Observe value -> Ok value
      | _ -> Error (incompatible "REPL Observe response changed method")))
;;

let watch_and_observe_task t lookup =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind () =
      read_locked t (R.Request.Watch (R.Watch_key.Task lookup)) ~decode:(function
        | R.Response.Watch -> Ok ()
        | _ -> Error (incompatible "REPL Watch response changed method"))
    in
    read_locked t (R.Request.Task_observe lookup) ~decode:(function
      | R.Response.Task_observe value -> Ok value
      | _ -> Error (incompatible "REPL Task Observe response changed method")))
;;

let unwatch t key =
  read t (R.Request.Unwatch key) ~decode:(function
    | R.Response.Unwatch -> Ok ()
    | _ -> Error (incompatible "REPL Unwatch response changed method"))
;;

let change_of_notification t envelope =
  let open Result.Let_syntax in
  let%bind () = current t in
  match envelope with
  | P.Envelope.Notification notification
    when String.equal notification.method_ R.Change.method_name ->
    if
      (not
         (List.mem
            t.initialization.enabled_features
            R.Features.events
            ~equal:String.equal))
      || not (R.Capabilities.supports t.capabilities "developer.repl.watch")
    then Error (incompatible "REPL events were not selected")
    else
      R.Change.of_json ~limits:(R.Capabilities.limits t.capabilities) notification.params
      |> Result.map ~f:Option.some
  | Notification _ | Request _ | Response _ -> Ok None
;;
