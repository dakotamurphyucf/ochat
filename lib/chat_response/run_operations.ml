open! Core
module P = Agent_protocol
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module R = Chatml_host_runtime

type handlers =
  { stage : P.Run_action.t -> (int, string) result
  ; rollback : int -> unit
  }

type transaction =
  { handlers : handlers
  ; prepare : int list -> (P.Run_action.t option, string) result
  }

let dynamic_handlers current =
  { stage =
      (fun action ->
        match current () with
        | None -> Error "run callback scope is not installed"
        | Some transaction -> transaction.handlers.stage action)
  ; rollback =
      (fun receipt ->
        match current () with
        | None -> failwith "recorded run action lost its callback scope"
        | Some transaction -> transaction.handlers.rollback receipt)
  }
;;

let receipt (operation : L.eff) =
  match operation.op with
  | "Run.continue" | "Run.wait" | "Run.finish" ->
    let expected_arity = if String.equal operation.op "Run.continue" then 1 else 2 in
    if List.length operation.args <> expected_arity
    then Error "invalid recorded run action arity"
    else (
      match operation.args with
      | L.VVariant ("Run_receipt", [ L.VInt ticket; L.VUnit ]) :: _ when ticket >= 0 ->
        Ok (Some ticket)
      | _ -> Error "invalid recorded run action receipt")
  | _ -> Ok None
;;

let split_actions effects =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module Int) in
  let%map selected, ordinary =
    List.fold_result effects ~init:([], []) ~f:(fun (selected, ordinary) operation ->
      let%bind ticket = receipt operation in
      match ticket with
      | None -> Ok (selected, operation :: ordinary)
      | Some ticket ->
        if Hash_set.mem seen ticket
        then Error "duplicate recorded run action receipt"
        else (
          Hash_set.add seen ticket;
          Ok (ticket :: selected, ordinary)))
  in
  List.rev selected, List.rev ordinary
;;

let install ?control ~handlers (config : R.runtime_config) =
  let open Result.Let_syntax in
  let decode decoder value =
    let%bind json = V.export_json ?control value in
    Result.map_error (decoder json) ~f:(fun error -> error.P.Error.message)
  in
  let operation name action : R.op_def =
    { name
    ; kind =
        Local_transactional_with_result
          { rollback =
              (fun args ->
                match receipt L.{ op = name; args } with
                | Ok (Some ticket) -> handlers.rollback ticket
                | Ok None | Error _ ->
                  failwith "host recorded an invalid run action receipt")
          }
    ; phase_check = R.allow_all_phases
    ; perform =
        (fun _ args ->
          let%bind action = action args in
          let%bind ticket = handlers.stage action in
          if ticket < 0
          then Error "host returned an invalid run action receipt"
          else Ok (L.VVariant ("Run_receipt", [ L.VInt ticket; L.VUnit ])))
    }
  in
  let operations =
    [ operation "Run.continue" (function
        | [] -> Ok P.Run_action.Continue
        | _ -> Error "Run.continue: expected no arguments")
    ; operation "Run.wait" (function
        | [ value ] ->
          Result.map (decode P.Run_wake.of_json value) ~f:(fun wake ->
            P.Run_action.Wait wake)
        | _ -> Error "Run.wait: expected an exact authorized wake")
    ; operation "Run.finish" (function
        | [ value ] ->
          let%bind action = decode P.Run_action.of_json value in
          (match action with
           | Finish _ -> Ok action
           | Continue | Wait _ -> Error "Run.finish: expected a finish decision")
        | _ -> Error "Run.finish: expected a finish decision")
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
