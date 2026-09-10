open Core
module P = Agent_protocol
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module R = Chatml_host_runtime

type handlers =
  { create :
      delay_ms:int
      -> payload:Jsonaf.t
      -> misfire:P.Schedule.misfire
      -> (int * P.Schedule.t, string) result
  ; get : P.Id.Schedule.t -> (P.Schedule.t, string) result
  ; cancel : P.Id.Schedule.t -> (int, string) result
  ; rollback : int -> unit
  }

type transaction =
  { handlers : handlers
  ; prepare : int list -> (unit -> unit, string) result
  }

let dynamic_handlers current =
  let active f =
    match current () with
    | None -> Error "schedule transaction is not installed"
    | Some transaction -> f transaction.handlers
  in
  { create =
      (fun ~delay_ms ~payload ~misfire ->
        active (fun h -> h.create ~delay_ms ~payload ~misfire))
  ; get = (fun id -> active (fun h -> h.get id))
  ; cancel = (fun id -> active (fun h -> h.cancel id))
  ; rollback =
      (fun receipt ->
        match current () with
        | Some transaction -> transaction.handlers.rollback receipt
        | None -> failwith "recorded schedule mutation lost its active transaction")
  }
;;

let receipt (operation : L.eff) =
  let expected =
    match operation.op with
    | "Schedule.after_ms_json" -> Some 3
    | "Schedule.after_ms_with_policy" -> Some 4
    | "Schedule.cancel" -> Some 2
    | _ -> None
  in
  match expected, operation.args with
  | None, _ -> Ok None
  | Some count, L.VVariant ("Schedule_receipt", [ L.VInt ticket; _ ]) :: _
    when ticket >= 0 && Int.equal count (List.length operation.args) -> Ok (Some ticket)
  | _ -> Error "invalid recorded schedule mutation"
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
        Error "duplicate recorded schedule mutation"
      | Some ticket ->
        Hash_set.add seen ticket;
        Ok (ticket :: receipts, ordinary))
  in
  List.rev receipts, List.rev ordinary
;;

let id = function
  | L.VString value ->
    P.Id.Schedule.of_string value
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
  | _ -> Error "expected schedule identity"
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
                | _ -> failwith "host recorded an invalid schedule receipt")
          }
    ; phase_check = R.allow_all_phases
    ; perform =
        (fun _ args ->
          let%bind ticket, value = perform args in
          match ticket >= 0 with
          | true -> Ok (L.VVariant ("Schedule_receipt", [ VInt ticket; value ]))
          | false -> Error "host returned an invalid schedule receipt")
    }
  in
  let create delay payload misfire =
    match delay with
    | L.VInt delay_ms when delay_ms >= 0 ->
      let%bind payload = V.export_json ?control payload in
      let%map receipt, schedule = handlers.create ~delay_ms ~payload ~misfire in
      receipt, L.VString (P.Id.Schedule.to_string schedule.id)
    | _ -> Error "schedule delay must be nonnegative"
  in
  let operations =
    [ mutation "Schedule.after_ms_json" (function
        | [ delay; payload ] -> create delay payload Deliver_once_immediately
        | _ -> Error "Schedule.after_ms: expected delay and JSON payload")
    ; mutation "Schedule.after_ms_with_policy" (function
        | [ delay; payload; misfire ] ->
          let%bind misfire =
            match misfire with
            | L.VVariant ("Deliver_once_immediately", []) ->
              Ok P.Schedule.Deliver_once_immediately
            | L.VVariant ("Skip_if_expired", []) -> Ok P.Schedule.Skip_if_expired
            | L.VVariant ("Fail", []) -> Ok P.Schedule.Fail
            | _ -> Error "invalid schedule misfire policy"
          in
          create delay payload misfire
        | _ ->
          Error
            "Schedule.after_ms_with_policy: expected delay, JSON payload and misfire \
             policy")
    ; mutation "Schedule.cancel" (function
        | [ value ] ->
          let%bind id = id value in
          let%map ticket = handlers.cancel id in
          ticket, L.VUnit
        | _ -> Error "Schedule.cancel: expected schedule identity")
    ; { name = "Schedule.get"
      ; kind = External_sync
      ; phase_check = R.allow_all_phases
      ; perform =
          (fun _ -> function
             | [ value ] ->
               let%bind id = id value in
               let%map schedule = handlers.get id in
               V.import_json ?control (P.Schedule.to_json schedule)
             | _ -> Error "Schedule.get: expected schedule identity")
      }
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
