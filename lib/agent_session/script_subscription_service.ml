open Core
module P = Agent_protocol
module S = P.Subscription
module EC = Chat_response.Extension_compiler
module Ops = Chat_response.Subscription_operations
module Managed = Chat_response.Managed_tool_registry

type origin =
  | Direct of EC.t * P.Invocation.t
  | Managed of Managed.execution

let origin_values = function
  | Direct (prepared, invocation) -> prepared, invocation
  | Managed execution -> Managed.prepared execution, Managed.invocation execution
;;

type host =
  { stage :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> previous:S.t option
      -> next:S.t
      -> (int, P.Error.t) result
  ; get :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> P.Id.Subscription.t
      -> (S.t, P.Error.t) result
  ; select :
      P.Job.launch_owner -> P.Invocation.observer -> int list -> (unit, P.Error.t) result
  ; abort : P.Job.launch_owner -> int -> unit
  }

type t =
  { now : unit -> P.Timestamp.t
  ; limits : Staged_subscriptions.limits
  ; host : host
  }

type commit_state =
  | Open
  | Prepared
  | Committed

type scope =
  { service : t
  ; owner : P.Job.launch_owner
  ; source : P.Invocation.observer
  ; originating : origin option
  ; active : bool Atomic.t
  ; mutable issued : int list
  ; mutable created : (int * P.Id.Subscription.t) list
  ; mutable commit_state : commit_state
  }

let message result = Result.map_error result ~f:(fun error -> error.P.Error.message)

let create ~now ~limits ~host =
  Staged_subscriptions.validate_limits limits |> message |> Result.ok_or_failwith;
  { now; limits; host }
;;

let check scope =
  match Atomic.get scope.active, scope.commit_state with
  | true, Open -> Ok ()
  | _ -> Error "subscription scope has ended"
;;

let abort scope receipt =
  Eio.Cancel.protect (fun () -> scope.service.host.abort scope.owner receipt);
  scope.issued <- List.filter scope.issued ~f:(fun other -> not (Int.equal receipt other));
  scope.created
  <- List.filter scope.created ~f:(fun (other, _) -> not (Int.equal receipt other))
;;

let abort_all scope = List.iter scope.issued ~f:(abort scope)

let validate_origin scope =
  match scope.originating, scope.owner with
  | None, _ -> Ok ()
  | Some origin, P.Job.Invocation id ->
    let prepared, invocation = origin_values origin in
    let script = EC.script prepared in
    (match (EC.declaration prepared).implementation, invocation.status with
     | Moderator _, Dispatching
       when P.Id.Invocation.equal invocation.context.id id
            && String.equal script.id scope.source.script_id
            && String.equal script.source_sha256 scope.source.source_sha256
            && String.equal (EC.declaration prepared).name invocation.context.tool_name
            &&
            match origin with
            | Direct _ ->
              String.equal
                (EC.fingerprint prepared)
                invocation.context.implementation_revision
            | Managed _ -> true -> Ok ()
     | _ -> Error "subscription origin is not its compiled moderator invocation")
  | Some _, Moderator_event _ ->
    Error "event cannot own a new subscription acknowledgement"
;;

let with_scope service ~owner ~source ~originating ~error f =
  let scope =
    { service
    ; owner
    ; source
    ; originating
    ; active = Atomic.make true
    ; issued = []
    ; created = []
    ; commit_state = Open
    }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set scope.active false)
    ~f:(fun () ->
      let result =
        try
          let open Result.Let_syntax in
          let%bind () = validate_origin scope |> Result.map_error ~f:error in
          let%bind value = f scope in
          match scope.commit_state with
          | Committed -> Ok value
          | Open | Prepared ->
            Error (error "subscription transaction was not acknowledged")
        with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          abort_all scope;
          Stdlib.Printexc.raise_with_backtrace exn backtrace
      in
      match result with
      | Ok _ -> result
      | Error _ ->
        abort_all scope;
        result)
;;

let stage scope ~previous ~next =
  let open Result.Let_syntax in
  let%bind () = check scope in
  Eio.Cancel.protect (fun () ->
    let%map receipt =
      scope.service.host.stage scope.owner scope.source ~previous ~next |> message
    in
    scope.issued <- receipt :: scope.issued;
    (match previous with
     | None -> scope.created <- (receipt, next.S.context.id) :: scope.created
     | Some _ -> ());
    receipt)
;;

let get scope id =
  let open Result.Let_syntax in
  let%bind () = check scope in
  scope.service.host.get scope.owner scope.source id |> message
;;

let handlers scope : Ops.handlers =
  let open Result.Let_syntax in
  { create =
      (fun ~kind ~lifetime_ms ~wake ->
        let%bind () = check scope in
        let%bind origin =
          Result.of_option
            scope.originating
            ~error:"subscription creation requires an originating moderator tool"
        in
        let prepared, invocation = origin_values origin in
        let lifetime_ms =
          Option.value lifetime_ms ~default:scope.service.limits.default_lifetime_ms
        in
        let%bind () =
          match
            lifetime_ms > 0 && lifetime_ms <= scope.service.limits.max_lifetime_ms
          with
          | true -> Ok ()
          | false -> Error "subscription lifetime exceeds host policy"
        in
        let created_at = scope.service.now () in
        let deadline =
          Time_ns.add
            (P.Timestamp.to_time_ns created_at)
            (Time_ns.Span.of_int_ms lifetime_ms)
          |> P.Timestamp.of_time_ns
        in
        let%bind subscription =
          S.create
            { id = P.Id.Subscription.create ()
            ; session_id = invocation.context.session_id
            ; generation = invocation.context.generation
            ; invocation_id = invocation.context.id
            ; source = Some scope.source
            ; kind
            ; created_at
            ; deadline
            ; completion_schema =
                Option.map
                  (EC.completion_schema prepared)
                  ~f:Chatmd_shell_spec.Tool_schema.to_json
            ; wake
            ; ingress_capability = None
            }
          |> message
        in
        let%map receipt = stage scope ~previous:None ~next:subscription in
        receipt, subscription.context.id)
  ; get = get scope
  ; finish =
      (fun ~id ~expected_epoch completion ->
        let%bind previous = get scope id in
        let now = scope.service.now () in
        let completion =
          match previous.result with
          | None when P.Timestamp.compare now previous.context.deadline >= 0 ->
            P.Completion.Expired
          | _ -> completion
        in
        let%bind next, _ = S.finish previous ~expected_epoch ~now completion |> message in
        let%map receipt = stage scope ~previous:(Some previous) ~next in
        receipt, next)
  ; rollback = abort scope
  }
;;

let moderator_transaction scope : Ops.transaction =
  { handlers = handlers scope
  ; prepare =
      (fun receipts ->
        let open Result.Let_syntax in
        let%bind () = check scope in
        let%bind () =
          scope.service.host.select scope.owner scope.source receipts |> message
        in
        let%map () = check scope in
        scope.commit_state <- Prepared;
        fun () ->
          scope.commit_state <- Committed;
          scope.issued <- [];
          scope.created <- [])
  }
;;

let validate_work scope id =
  let open Result.Let_syntax in
  let%bind () = check scope in
  let%bind () =
    match
      List.exists scope.created ~f:(fun (_, created) ->
        P.Id.Subscription.equal created id)
    with
    | true -> Ok ()
    | false -> Error "pending subscription was not created by this invocation"
  in
  let%bind subscription = get scope id in
  match scope.owner with
  | P.Job.Invocation owner
    when P.Id.Invocation.equal owner subscription.context.invocation_id -> Ok ()
  | _ -> Error "pending subscription has a different originating invocation"
;;
