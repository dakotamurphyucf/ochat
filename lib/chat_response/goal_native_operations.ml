open! Core
module L = Chatml.Chatml_lang
module R = Chatml_host_runtime

let name = "Goal._native_receipt"

let receipt (operation : L.eff) =
  if not (String.equal operation.op name)
  then Ok None
  else (
    match operation.args with
    | L.VVariant ("Goal_native_receipt", [ L.VInt ticket; L.VUnit ])
      :: [ L.VInt argument ]
      when ticket >= 0 && Int.equal ticket argument -> Ok (Some ticket)
    | _ -> Error "invalid recorded native Goal receipt")
;;

let install ~current (config : R.runtime_config) =
  let scope () =
    Result.of_option (current ()) ~error:"Goal callback transaction is absent"
  in
  let operation : R.op_def =
    { name
    ; phase_check = R.allow_all_phases
    ; kind =
        Local_transactional_with_result
          { rollback =
              (fun args ->
                match receipt L.{ op = name; args }, current () with
                | Ok (Some ticket), Some scope ->
                  (Goal_native_scope.handlers scope).rollback ticket
                | _ ->
                  failwith "recorded native Goal receipt lost its real callback owner")
          }
    ; perform =
        (fun _ args ->
          let open Result.Let_syntax in
          match args with
          | [ L.VInt ticket ] when ticket >= 0 ->
            let%bind scope = scope () in
            let%map () = (Goal_native_scope.handlers scope).accept ticket in
            L.VVariant ("Goal_native_receipt", [ L.VInt ticket; L.VUnit ])
          | _ -> Error "internal Goal receipt requires one issued ticket")
    }
  in
  { config with
    operations =
      operation
      :: List.filter config.operations ~f:(fun op -> not (String.equal op.name name))
  }
;;

let prepare scope effects =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module Int) in
  let%bind tickets, ordinary =
    List.fold_result effects ~init:([], []) ~f:(fun (tickets, ordinary) operation ->
      let%bind ticket = receipt operation in
      match ticket with
      | None -> Ok (tickets, operation :: ordinary)
      | Some ticket ->
        if Hash_set.mem seen ticket
        then Error "duplicate native Goal receipt"
        else (
          Hash_set.add seen ticket;
          Ok (ticket :: tickets, ordinary)))
  in
  let%map () =
    match scope, List.rev tickets with
    | None, [] -> Ok ()
    | None, _ :: _ -> Error "Goal receipt has no real callback owner"
    | Some scope, tickets -> (Goal_native_scope.handlers scope).prepare tickets
  in
  List.rev ordinary
;;

let deliver scope ~receipts ~perform =
  let open Result.Let_syntax in
  match scope, receipts with
  | None, [] -> Ok ()
  | None, _ :: _ -> Error "Goal native receipt route was lost"
  | Some scope, receipts ->
    List.fold_result receipts ~init:() ~f:(fun () ticket ->
      let%bind deliverable = (Goal_native_scope.handlers scope).deliverable ticket in
      if not deliverable then Ok () else perform L.{ op = name; args = [ VInt ticket ] })
;;
