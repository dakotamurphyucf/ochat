open Core
module P = Agent_protocol
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module R = Chatml_host_runtime

type correlation =
  { key : string
  ; invocation_id : P.Id.Invocation.t option
  ; work : P.Invocation.work option
  }

type handlers =
  { publish :
      correlation:correlation
      -> completion:P.Completion.t
      -> wake:P.Completion.wake
      -> (int * P.Delivery.t, string) result
  ; get : P.Id.Delivery.t -> (P.Delivery.t, string) result
  ; rollback : int -> unit
  }

type transaction =
  { handlers : handlers
  ; prepare : int list -> (unit -> unit, string) result
  }

let dynamic_handlers current =
  let active f =
    match current () with
    | None -> Error "notification transaction is not installed"
    | Some transaction -> f transaction.handlers
  in
  { publish =
      (fun ~correlation ~completion ~wake ->
        active (fun h -> h.publish ~correlation ~completion ~wake))
  ; get = (fun id -> active (fun h -> h.get id))
  ; rollback =
      (fun receipt ->
        match current () with
        | Some transaction -> transaction.handlers.rollback receipt
        | None -> failwith "recorded notification lost its active transaction")
  }
;;

let receipt (operation : L.eff) =
  match operation.op, operation.args with
  | ( "Notification.publish"
    , [ L.VVariant ("Notification_receipt", [ VInt ticket; _ ]); _; _; _ ] )
    when ticket >= 0 -> Ok (Some ticket)
  | "Notification.publish", _ -> Error "invalid recorded notification mutation"
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
        Error "duplicate recorded notification mutation"
      | Some ticket ->
        Hash_set.add seen ticket;
        Ok (ticket :: receipts, ordinary))
  in
  List.rev receipts, List.rev ordinary
;;

let protocol result = Result.map_error result ~f:(fun error -> error.P.Error.message)

let optional decode = function
  | L.VVariant ("None", []) -> Ok None
  | VVariant ("Some", [ value ]) -> Result.map (decode value) ~f:Option.some
  | _ -> Error "expected optional notification reference"
;;

let identifier decode = function
  | L.VString value -> decode value |> protocol
  | _ -> Error "expected notification reference identity"
;;

let correlation = function
  | L.VRecord fields when Map.length fields = 3 ->
    let open Result.Let_syntax in
    let%bind key =
      V.expect_record_field "correlation" fields "key"
      |> Result.bind ~f:(V.expect_string "key")
    in
    let%bind invocation_id =
      V.expect_record_field "correlation" fields "invocation_id"
      |> Result.bind ~f:(optional (identifier P.Id.Invocation.of_string))
    in
    let%map work =
      V.expect_record_field "correlation" fields "work"
      |> Result.bind
           ~f:
             (optional (function
                | L.VVariant ("Job", [ value ]) ->
                  identifier P.Id.Job.of_string value
                  |> Result.map ~f:(fun id -> P.Invocation.Job id)
                | L.VVariant ("Subscription", [ value ]) ->
                  identifier P.Id.Subscription.of_string value
                  |> Result.map ~f:(fun id -> P.Invocation.Subscription id)
                | _ -> Error "expected Job or Subscription notification reference"))
    in
    { key; invocation_id; work }
  | _ -> Error "expected notification correlation record"
;;

let completion ?control value =
  let open Result.Let_syntax in
  let%bind result =
    match value with
    | L.VVariant ("Succeeded", [ payload ]) ->
      V.export_json ?control payload
      |> Result.map ~f:(fun value -> P.Completion.Succeeded value)
    | VVariant ("Cancelled", [ VString reason ]) -> Ok (P.Completion.Cancelled reason)
    | VVariant ("Expired", []) -> Ok P.Completion.Expired
    | VVariant ("Failed", [ VRecord fields ]) when Map.length fields = 4 ->
      let%bind code =
        V.expect_record_field "notification error" fields "code"
        |> Result.bind ~f:(V.expect_string "code")
      in
      let%bind message =
        V.expect_record_field "notification error" fields "message"
        |> Result.bind ~f:(V.expect_string "message")
      in
      let%bind retryable =
        match Map.find fields "retryable" with
        | Some (L.VBool value) -> Ok value
        | _ -> Error "invalid notification retryable flag"
      in
      let%map details =
        V.expect_record_field "notification error" fields "details"
        |> Result.bind ~f:(V.export_json ?control)
      in
      P.Completion.Failed { code; message; retryable; details }
    | _ -> Error "expected notification completion"
  in
  let%map () = P.Completion.validate result |> protocol in
  result
;;

let install ?control ~handlers (config : R.runtime_config) =
  let open Result.Let_syntax in
  let operations : R.op_def list =
    [ { name = "Notification.publish"
      ; kind =
          Local_transactional_with_result
            { rollback =
                (fun args ->
                  match receipt L.{ op = "Notification.publish"; args } with
                  | Ok (Some ticket) -> handlers.rollback ticket
                  | _ -> failwith "host recorded an invalid notification receipt")
            }
      ; phase_check = R.allow_all_phases
      ; perform =
          (fun _ -> function
             | [ reference; result; wake ] ->
               let%bind correlation = correlation reference in
               let%bind completion = completion ?control result in
               let%bind wake =
                 match wake with
                 | L.VVariant ("Request_turn", []) -> Ok P.Completion.Request_turn
                 | VVariant ("Next_turn", []) -> Ok P.Completion.Next_turn
                 | VVariant ("No_wake", []) -> Ok P.Completion.No_wake
                 | _ -> Error "invalid notification wake policy"
               in
               let%bind ticket, delivery =
                 handlers.publish ~correlation ~completion ~wake
               in
               (match ticket >= 0 with
                | true ->
                  Ok
                    (L.VVariant
                       ( "Notification_receipt"
                       , [ VInt ticket
                         ; VString (P.Id.Delivery.to_string delivery.context.id)
                         ] ))
                | false -> Error "host returned an invalid notification receipt")
             | _ ->
               Error "Notification.publish: expected correlation, completion and wake")
      }
    ; { name = "Notification.get"
      ; kind = External_sync
      ; phase_check = R.allow_all_phases
      ; perform =
          (fun _ -> function
             | [ value ] ->
               let%bind id = identifier P.Id.Delivery.of_string value in
               let%map delivery = handlers.get id in
               V.import_json ?control (P.Delivery.to_json delivery)
             | _ -> Error "Notification.get: expected delivery identity")
      }
    ]
  in
  { config with operations = operations @ config.operations }
;;
