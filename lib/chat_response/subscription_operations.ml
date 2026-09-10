open Core
module P = Agent_protocol
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module R = Chatml_host_runtime
module Id = P.Id.Subscription

type handlers =
  { create :
      kind:string
      -> lifetime_ms:int option
      -> wake:P.Completion.wake
      -> (int * Id.t, string) result
  ; get : Id.t -> (P.Subscription.t, string) result
  ; finish :
      id:Id.t
      -> expected_epoch:int
      -> P.Completion.t
      -> (int * P.Subscription.t, string) result
  ; arm :
      id:Id.t
      -> expected_epoch:int
      -> timer_id:P.Id.Schedule.t option
      -> job_id:P.Id.Job.t option
      -> (int * P.Subscription.t, string) result
  ; rollback : int -> unit
  }

type transaction =
  { handlers : handlers
  ; prepare : int list -> (unit -> unit, string) result
  }

let dynamic_handlers current =
  let active f =
    match current () with
    | None -> Error "subscription transaction is not installed"
    | Some transaction -> f transaction.handlers
  in
  { create =
      (fun ~kind ~lifetime_ms ~wake ->
        active (fun h -> h.create ~kind ~lifetime_ms ~wake))
  ; get = (fun id -> active (fun h -> h.get id))
  ; finish =
      (fun ~id ~expected_epoch result ->
        active (fun h -> h.finish ~id ~expected_epoch result))
  ; arm =
      (fun ~id ~expected_epoch ~timer_id ~job_id ->
        active (fun h -> h.arm ~id ~expected_epoch ~timer_id ~job_id))
  ; rollback =
      (fun receipt ->
        match current () with
        | Some transaction -> transaction.handlers.rollback receipt
        | None -> failwith "recorded subscription mutation lost its active transaction")
  }
;;

let protocol result = Result.map_error result ~f:(fun error -> error.P.Error.message)

let id = function
  | L.VString value -> protocol (Id.of_string value)
  | _ -> Error "expected subscription identity"
;;

let receipt (operation : L.eff) =
  let extract = function
    | L.VVariant ("Subscription_receipt", [ L.VInt ticket; _ ]) :: _ when ticket >= 0 ->
      Ok (Some ticket)
    | _ -> Error "invalid recorded subscription mutation"
  in
  match operation.op, List.length operation.args with
  | "Subscription.create", 4
  | ("Subscription.complete" | "Subscription.fail" | "Subscription.cancel"), 4 ->
    extract operation.args
  | "Subscription.arm", 5 -> extract operation.args
  | ( ( "Subscription.create"
      | "Subscription.complete"
      | "Subscription.fail"
      | "Subscription.cancel" )
    , _ ) -> Error "invalid recorded subscription mutation"
  | "Subscription.arm", _ -> Error "invalid recorded subscription mutation"
  | _ -> Ok None
;;

let split_mutations effects =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module Int) in
  let%map receipts, ordinary =
    List.fold_result effects ~init:([], []) ~f:(fun (receipts, ordinary) operation ->
      let%bind found = receipt operation in
      match found with
      | None -> Ok (receipts, operation :: ordinary)
      | Some ticket when Hash_set.mem seen ticket ->
        Error "duplicate recorded subscription mutation"
      | Some ticket ->
        Hash_set.add seen ticket;
        Ok (ticket :: receipts, ordinary))
  in
  List.rev receipts, List.rev ordinary
;;

let install ?control ~handlers (config : R.runtime_config) =
  let open Result.Let_syntax in
  let mutation name perform : R.op_def =
    { name
    ; kind =
        Local_transactional_with_result
          { rollback =
              (fun args ->
                match receipt L.{ op = name; args } with
                | Ok (Some ticket) -> handlers.rollback ticket
                | _ -> failwith "host recorded an invalid subscription receipt")
          }
    ; phase_check = R.allow_all_phases
    ; perform =
        (fun _ args ->
          let%bind ticket, value = perform args in
          match ticket < 0 with
          | true -> Error "host returned an invalid subscription receipt"
          | false -> Ok (L.VVariant ("Subscription_receipt", [ L.VInt ticket; value ])))
    }
  in
  let finish name decode =
    mutation name (function
      | [ subscription; L.VInt expected_epoch; value ] when expected_epoch >= 0 ->
        let%bind id = id subscription in
        let%bind completion = decode value in
        let%bind () = protocol (P.Completion.validate completion) in
        let%map ticket, result = handlers.finish ~id ~expected_epoch completion in
        ticket, V.import_json ?control (P.Subscription.to_json result)
      | _ ->
        Error (name ^ ": expected subscription identity, nonnegative epoch and result"))
  in
  let operations =
    [ mutation "Subscription.create" (function
        | [ L.VString kind; lifetime; wake ] ->
          let%bind lifetime_ms =
            match lifetime with
            | L.VVariant ("None", []) -> Ok None
            | L.VVariant ("Some", [ L.VInt ms ]) when ms > 0 -> Ok (Some ms)
            | _ -> Error "Subscription.create: expected optional positive lifetime"
          in
          let%bind wake =
            match wake with
            | L.VVariant ("Request_turn", []) -> Ok P.Completion.Request_turn
            | L.VVariant ("Next_turn", []) -> Ok P.Completion.Next_turn
            | L.VVariant ("No_wake", []) -> Ok P.Completion.No_wake
            | _ -> Error "Subscription.create: invalid wake policy"
          in
          let%map ticket, id = handlers.create ~kind ~lifetime_ms ~wake in
          ticket, L.VString (Id.to_string id)
        | _ ->
          Error "Subscription.create: expected kind, optional lifetime and wake policy")
    ; { name = "Subscription.get"
      ; kind = External_sync
      ; phase_check = R.allow_all_phases
      ; perform =
          (fun _ -> function
             | [ value ] ->
               let%bind id = id value in
               let%map result = handlers.get id in
               V.import_json ?control (P.Subscription.to_json result)
             | _ -> Error "Subscription.get: expected subscription identity")
      }
    ; finish "Subscription.complete" (fun value ->
        let%map payload = V.export_json ?control value in
        P.Completion.Succeeded payload)
    ; finish "Subscription.fail" (function
        | L.VRecord fields ->
          (match
             ( Map.find fields "code"
             , Map.find fields "message"
             , Map.find fields "retryable"
             , Map.find fields "details" )
           with
           | ( Some (L.VString code)
             , Some (L.VString message)
             , Some (L.VBool retryable)
             , Some details )
             when Map.length fields = 4 ->
             let%map details = V.export_json ?control details in
             P.Completion.Failed { code; message; retryable; details }
           | _ -> Error "Subscription.fail: invalid tool error")
        | _ -> Error "Subscription.fail: expected tool error")
    ; finish "Subscription.cancel" (function
        | L.VString reason -> Ok (P.Completion.Cancelled reason)
        | _ -> Error "Subscription.cancel: expected cancellation reason")
    ; mutation "Subscription.arm" (function
        | [ subscription; L.VInt expected_epoch; timer; job ] when expected_epoch >= 0 ->
          let%bind id = id subscription in
          let optional_id decode = function
            | L.VVariant ("None", []) -> Ok None
            | L.VVariant ("Some", [ L.VString value ]) ->
              protocol (decode (`String value)) |> Result.map ~f:Option.some
            | _ -> Error "Subscription.arm: expected optional work identity"
          in
          let%bind timer_id = optional_id P.Id.Schedule.of_json timer in
          let%bind job_id = optional_id P.Id.Job.of_json job in
          let%map ticket, result = handlers.arm ~id ~expected_epoch ~timer_id ~job_id in
          ticket, V.import_json ?control (P.Subscription.to_json result)
        | _ ->
          Error
            "Subscription.arm: expected subscription, epoch and optional timer/job \
             identities")
    ]
  in
  { config with
    operations =
      operations
      @ List.filter config.operations ~f:(fun existing ->
        not
          (List.exists operations ~f:(fun operation ->
             String.equal operation.name existing.name)))
  }
;;
