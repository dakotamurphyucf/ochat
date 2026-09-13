open Core
module P = Agent_protocol
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module R = Chatml_host_runtime

type handlers =
  { register :
      subscription_id:P.Id.Subscription.t
      -> expected_epoch:int
      -> namespace:string
      -> schema:Jsonaf.t
      -> (int * P.Id.Capability.t, string) result
  ; get : P.Id.Capability.t -> (Jsonaf.t, string) result
  ; revoke : P.Id.Capability.t -> reason:string -> (int * Jsonaf.t, string) result
  ; rollback : int -> unit
  }

type transaction =
  { handlers : handlers
  ; prepare : int list -> (unit -> unit, string) result
  }

let dynamic_handlers current =
  let active f =
    match current () with
    | None -> Error "ingress transaction is not installed"
    | Some transaction -> f transaction.handlers
  in
  { register =
      (fun ~subscription_id ~expected_epoch ~namespace ~schema ->
        active (fun h -> h.register ~subscription_id ~expected_epoch ~namespace ~schema))
  ; get = (fun id -> active (fun h -> h.get id))
  ; revoke = (fun id ~reason -> active (fun h -> h.revoke id ~reason))
  ; rollback =
      (fun receipt ->
        match current () with
        | Some transaction -> transaction.handlers.rollback receipt
        | None -> failwith "recorded ingress mutation lost its active transaction")
  }
;;

let receipt (operation : L.eff) =
  let expected =
    match operation.op with
    | "Ingress.register" -> Some 5
    | "Ingress.revoke" -> Some 3
    | _ -> None
  in
  match expected, operation.args with
  | None, _ -> Ok None
  | Some count, L.VVariant ("Ingress_receipt", [ VInt ticket; _ ]) :: _
    when ticket >= 0 && Int.equal count (List.length operation.args) -> Ok (Some ticket)
  | _ -> Error "invalid recorded ingress mutation"
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
        Error "duplicate recorded ingress mutation"
      | Some ticket ->
        Hash_set.add seen ticket;
        Ok (ticket :: receipts, ordinary))
  in
  List.rev receipts, List.rev ordinary
;;

let identifier decode = function
  | L.VString value ->
    decode value |> Result.map_error ~f:(fun error -> error.P.Error.message)
  | _ -> Error "expected ingress identity"
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
                | _ -> failwith "host recorded an invalid ingress receipt")
          }
    ; phase_check = R.allow_all_phases
    ; perform =
        (fun _ args ->
          let%bind ticket, value = perform args in
          match ticket >= 0 with
          | true -> Ok (L.VVariant ("Ingress_receipt", [ VInt ticket; value ]))
          | false -> Error "host returned an invalid ingress receipt")
    }
  in
  let operations =
    [ mutation "Ingress.register" (function
        | [ subscription; L.VInt expected_epoch; VString namespace; schema ]
          when expected_epoch >= 0 ->
          let%bind subscription_id =
            identifier P.Id.Subscription.of_string subscription
          in
          let%bind schema = V.export_json ?control schema in
          let%map ticket, id =
            handlers.register ~subscription_id ~expected_epoch ~namespace ~schema
          in
          ticket, L.VString (P.Id.Capability.to_string id)
        | _ ->
          Error
            "Ingress.register: expected subscription, nonnegative epoch, namespace and \
             schema")
    ; mutation "Ingress.revoke" (function
        | [ value; L.VString reason ] ->
          let%bind id = identifier P.Id.Capability.of_string value in
          let%bind ticket, json = handlers.revoke id ~reason in
          (match V.import_json ?control json with
           | value -> Ok (ticket, value)
           | exception exn ->
             let backtrace = Stdlib.Printexc.get_raw_backtrace () in
             (* Projection may hit an execution limit before the interpreter
                records the private receipt for ordinary catch rollback. *)
             Eio.Cancel.protect (fun () -> handlers.rollback ticket);
             Stdlib.Printexc.raise_with_backtrace exn backtrace)
        | _ -> Error "Ingress.revoke: expected registration identity and reason")
    ; { name = "Ingress.get"
      ; kind = External_sync
      ; phase_check = R.allow_all_phases
      ; perform =
          (fun _ -> function
             | [ value ] ->
               let%bind id = identifier P.Id.Capability.of_string value in
               let%map json = handlers.get id in
               V.import_json ?control json
             | _ -> Error "Ingress.get: expected registration identity")
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
