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
  { create :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> kind:string
      -> lifetime_ms:int
      -> wake:P.Completion.wake
      -> completion_schema:Jsonaf.t option
      -> (int * S.t, P.Error.t) result
  ; stage :
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
  ; get_job : P.Job.launch_owner -> P.Id.Job.t -> (P.Job.t, P.Error.t) result
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
  ; schedules : Script_schedule_service.scope option
  ; active : bool Atomic.t
  ; mutable issued : int list
  ; mutable created : (int * P.Id.Subscription.t) list
  ; mutable schedule_dependencies : (int * int list) list
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
  Option.iter scope.schedules ~f:(fun schedules ->
    List.Assoc.find scope.schedule_dependencies receipt ~equal:Int.equal
    |> Option.iter ~f:(List.iter ~f:(Script_schedule_service.rollback schedules)));
  scope.schedule_dependencies
  <- List.Assoc.remove scope.schedule_dependencies receipt ~equal:Int.equal;
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

let with_scope ?schedules service ~owner ~source ~originating ~error f =
  let scope =
    { service
    ; owner
    ; source
    ; originating
    ; schedules
    ; active = Atomic.make true
    ; issued = []
    ; created = []
    ; schedule_dependencies = []
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

let with_schedule_mutations scope f =
  let open Result.Let_syntax in
  let%bind schedules =
    Result.of_option
      scope.schedules
      ~error:"subscription timer transactions are not installed"
  in
  let receipts = ref [] in
  let record receipt = receipts := receipt :: !receipts in
  let rollback () = List.iter !receipts ~f:(Script_schedule_service.rollback schedules) in
  Eio.Cancel.protect (fun () ->
    match f schedules record with
    | Ok (receipt, value) ->
      scope.schedule_dependencies <- (receipt, !receipts) :: scope.schedule_dependencies;
      Ok (receipt, value)
    | Error _ as result ->
      rollback ();
      result
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      rollback ();
      Stdlib.Printexc.raise_with_backtrace exn backtrace)
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
        let prepared, _invocation = origin_values origin in
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
        let completion_schema =
          Option.map
            (EC.completion_schema prepared)
            ~f:Chatmd_shell_spec.Tool_schema.to_json
        in
        Eio.Cancel.protect (fun () ->
          let%map receipt, subscription =
            scope.service.host.create
              scope.owner
              scope.source
              ~kind
              ~lifetime_ms
              ~wake
              ~completion_schema
            |> message
          in
          scope.issued <- receipt :: scope.issued;
          scope.created <- (receipt, subscription.context.id) :: scope.created;
          receipt, subscription.context.id))
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
        let save () =
          let%map receipt = stage scope ~previous:(Some previous) ~next in
          receipt, next
        in
        match previous.timer_id with
        | None -> save ()
        | Some timer ->
          with_schedule_mutations scope (fun schedules record ->
            let%bind receipt = Script_schedule_service.cancel schedules timer in
            record receipt;
            save ()))
  ; arm =
      (fun ~id ~expected_epoch ~timer_id ~job_id ->
        let%bind previous = get scope id in
        let%bind () =
          match job_id with
          | None -> Ok ()
          | Some id ->
            scope.service.host.get_job scope.owner id |> message |> Result.map ~f:ignore
        in
        let%bind next = S.arm previous ~expected_epoch ~timer_id ~job_id |> message in
        let save () =
          let%map receipt = stage scope ~previous:(Some previous) ~next in
          receipt, next
        in
        match previous.timer_id, timer_id with
        | None, None -> save ()
        | old_timer, new_timer ->
          with_schedule_mutations scope (fun schedules record ->
            let%bind () =
              match old_timer with
              | None -> Ok ()
              | Some id ->
                let%map receipt = Script_schedule_service.cancel schedules id in
                record receipt
            in
            let%bind () =
              match new_timer with
              | None -> Ok ()
              | Some id ->
                let%bind timer = Script_schedule_service.get schedules id in
                let%bind ownership =
                  match timer.ownership, timer.status with
                  | Some ({ subscription = None; _ } as ownership), P.Schedule.Scheduled
                    -> Ok ownership
                  | _ ->
                    Error "subscription timer must be scheduled and not already bound"
                in
                let bound =
                  { timer with
                    ownership =
                      Some
                        { ownership with
                          subscription = Some (next.context.id, next.epoch)
                        }
                  }
                in
                let%map receipt =
                  Script_schedule_service.stage
                    schedules
                    ~previous:(Some timer)
                    ~next:bound
                in
                record receipt
            in
            save ()))
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
        let%bind () =
          match scope.schedules with
          | None -> Ok ()
          | Some schedules ->
            let dependencies =
              List.concat_map receipts ~f:(fun receipt ->
                Option.value
                  (List.Assoc.find scope.schedule_dependencies receipt ~equal:Int.equal)
                  ~default:[])
            in
            Script_schedule_service.retain_dependencies schedules dependencies
        in
        let%map () = check scope in
        scope.commit_state <- Prepared;
        fun () ->
          scope.commit_state <- Committed;
          scope.issued <- [];
          scope.created <- [];
          scope.schedule_dependencies <- [])
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
