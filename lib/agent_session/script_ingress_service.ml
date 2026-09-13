open Core
module P = Agent_protocol
module I = External_ingress
module Ops = Chat_response.Ingress_operations

type host =
  { register :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> subscription_id:P.Id.Subscription.t
      -> expected_epoch:int
      -> namespace:string
      -> schema:Jsonaf.t
      -> (int * I.t, P.Error.t) result
  ; get :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> P.Id.Capability.t
      -> (I.t, P.Error.t) result
  ; revoke :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> P.Id.Capability.t
      -> reason:string
      -> (int * I.t, P.Error.t) result
  ; select :
      P.Job.launch_owner -> P.Invocation.observer -> int list -> (unit, P.Error.t) result
  ; abort : P.Job.launch_owner -> int -> unit
  }

type t = { host : host }

type commit_state =
  | Open
  | Prepared
  | Committed

type scope =
  { service : t
  ; owner : P.Job.launch_owner
  ; source : P.Invocation.observer
  ; active : bool Atomic.t
  ; mutable issued : int list
  ; mutable commit_state : commit_state
  }

let create ~host = { host }
let message result = Result.map_error result ~f:(fun error -> error.P.Error.message)

let check scope =
  match Atomic.get scope.active, scope.commit_state with
  | true, Open -> Ok ()
  | _ -> Error "ingress scope has ended"
;;

let rollback scope receipt =
  Eio.Cancel.protect (fun () -> scope.service.host.abort scope.owner receipt);
  scope.issued <- List.filter scope.issued ~f:(fun other -> not (Int.equal receipt other))
;;

let abort_all scope = List.iter scope.issued ~f:(rollback scope)

let with_scope service ~owner ~source ~error f =
  let scope =
    { service
    ; owner
    ; source
    ; active = Atomic.make true
    ; issued = []
    ; commit_state = Open
    }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set scope.active false)
    ~f:(fun () ->
      let result =
        try
          let open Result.Let_syntax in
          let%bind value = f scope in
          match scope.commit_state with
          | Committed -> Ok value
          | Open | Prepared -> Error (error "ingress transaction was not acknowledged")
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

(* Status is data, never a credential or an enqueue capability. Preserve exact
   epoch spelling across the JSON bridge; event payloads stay in their receipts. *)
let view (value : I.t) =
  let c = value.context in
  let number n = `Number (Int.to_string n) in
  `Object
    [ "version", `Number "1"
    ; "registration_id", P.Id.Capability.to_json c.id
    ; "subscription_id", P.Id.Subscription.to_json c.subscription_id
    ; "epoch", `String (Int.to_string c.epoch)
    ; "namespace", `String c.namespace
    ; "created_at", P.Timestamp.to_json c.created_at
    ; "expires_at", P.Timestamp.to_json c.expires_at
    ; ( "revoked"
      , Option.value_map value.revoked ~default:`Null ~f:(fun reason -> `String reason) )
    ; ( "limits"
      , `Object
          [ "max_payload_bytes", number c.limits.max_payload_bytes
          ; "max_payload_depth", number c.limits.max_payload_depth
          ; "max_receipts", number c.limits.max_receipts
          ; "rate_count", number c.limits.rate_count
          ; "rate_window_ms", number c.limits.rate_window_ms
          ] )
    ; ( "receipts"
      , `Array
          (List.map value.receipts ~f:(fun receipt ->
             `Object
               [ "event_id", P.Id.Ingress_event.to_json receipt.id
               ; "key", P.Idempotency_key.to_json receipt.key
               ; "payload_sha256", `String receipt.payload_sha256
               ; "accepted_at", P.Timestamp.to_json receipt.accepted_at
               ])) )
    ]
;;

let reserve scope f =
  let open Result.Let_syntax in
  let%bind () = check scope in
  Eio.Cancel.protect (fun () ->
    let%map receipt, value = f () |> message in
    scope.issued <- receipt :: scope.issued;
    receipt, value)
;;

let moderator_transaction scope : Ops.transaction =
  { handlers =
      { register =
          (fun ~subscription_id ~expected_epoch ~namespace ~schema ->
            reserve scope (fun () ->
              scope.service.host.register
                scope.owner
                scope.source
                ~subscription_id
                ~expected_epoch
                ~namespace
                ~schema)
            |> Result.map ~f:(fun (receipt, value) -> receipt, value.I.context.id))
      ; get =
          (fun id ->
            let open Result.Let_syntax in
            let%bind () = check scope in
            scope.service.host.get scope.owner scope.source id
            |> message
            |> Result.map ~f:view)
      ; revoke =
          (fun id ~reason ->
            reserve scope (fun () ->
              scope.service.host.revoke scope.owner scope.source id ~reason)
            |> Result.map ~f:(fun (receipt, value) -> receipt, view value))
      ; rollback = rollback scope
      }
  ; prepare =
      (fun receipts ->
        let open Result.Let_syntax in
        let%bind () = check scope in
        let seen = Hash_set.create (module Int) in
        let%bind () =
          List.fold_result receipts ~init:() ~f:(fun () receipt ->
            match
              Hash_set.mem seen receipt, List.mem scope.issued receipt ~equal:Int.equal
            with
            | false, true ->
              Hash_set.add seen receipt;
              Ok ()
            | _ -> Error "ingress receipts are duplicated or foreign")
        in
        let%bind () =
          match
            List.equal
              Int.equal
              receipts
              (List.filter (List.rev scope.issued) ~f:(Hash_set.mem seen))
          with
          | true -> Ok ()
          | false -> Error "ingress receipts are out of execution order"
        in
        let%bind () =
          scope.service.host.select scope.owner scope.source receipts |> message
        in
        let%map () = check scope in
        scope.commit_state <- Prepared;
        fun () ->
          scope.commit_state <- Committed;
          scope.issued <- [])
  }
;;
