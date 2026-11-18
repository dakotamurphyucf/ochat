open! Core
module Diagnostic = Agent_session.Cleanup_diagnostic

(* These reads are factual only. A refused/escaped read owns one bounded
   diagnostic at this actual selection; it never replaces the physical closer. *)
let observe_result f =
  match f () with
  | Ok value -> Ok value
  | Error error -> Error (Agent_session.Callback_cleanup.Failure.Protocol error)
  | exception exn ->
    Error
      (Agent_session.Callback_cleanup.Failure.Raised
         (exn, Stdlib.Printexc.get_raw_backtrace ()))
;;

(* Finite factual reads for an actual physical selection. These retain no
   executable cleanup and never enroll newer Product operations on refresh. *)
module Product_selection = struct
  module P = Agent_session.Runtime_builder.Product_callbacks

  type operation =
    { original : P.Operation_snapshot.t
    ; mutable latest : P.Operation_snapshot.t
    ; mutable read_failure : Diagnostic.t option
    }

  type t =
    | Selected of operation list
    | Read_failed of Diagnostic.t

  let capture observer =
    match observe_result (fun () -> P.snapshot observer) with
    | Ok snapshot ->
      Selected
        (List.map (P.Snapshot.operations snapshot) ~f:(fun original ->
           { original; latest = original; read_failure = None }))
    | Error failure -> Read_failed (Diagnostic.create [ failure ])
  ;;

  let latest operation = operation.latest
  let read_failure operation = operation.read_failure

  let refresh operation =
    match observe_result (fun () -> P.Operation_snapshot.refresh operation.original) with
    | Ok snapshot ->
      operation.latest <- snapshot;
      Ok snapshot
    | Error failure ->
      let diagnostic =
        match operation.read_failure with
        | Some diagnostic -> diagnostic
        | None ->
          let diagnostic = Diagnostic.create [ failure ] in
          operation.read_failure <- Some diagnostic;
          diagnostic
      in
      Error diagnostic
  ;;

  let custody_joined = function
    | Read_failed _ -> false
    | Selected operations ->
      List.for_all operations ~f:(fun operation ->
        match refresh operation with
        | Error _ -> false
        | Ok snapshot ->
          P.Operation_snapshot.registration_closed snapshot
          && Option.is_some (P.Operation_snapshot.terminal snapshot)
          && Option.for_all
               (P.Operation_snapshot.callbacks snapshot)
               ~f:Callback_disposal.parent_settled)
  ;;

  let diagnostics = function
    | Read_failed diagnostic -> [ diagnostic ]
    | Selected operations ->
      List.concat_map operations ~f:(fun operation ->
        ignore (refresh operation : (_, _) result);
        P.Operation_snapshot.diagnostics operation.latest
        @ Option.to_list operation.read_failure)
  ;;
end

(* One actual per-invocation claim and its write-once failure slots. A new idle
   invocation gets another lease and another receipt; no first-call sink survives. *)
module Lease_callback = struct
  module C = Agent_session.Callback_cleanup
  module Slot = C.Completion_snapshot.Failure_slot

  type t =
    { completion : C.Completion.t
    ; mutable slots : (Slot.kind * Diagnostic.t) list
    ; mutable read_failure : Diagnostic.t option
    }

  let completion t = t.completion
  let read_failure t = t.read_failure

  let diagnostic t ~slot ~failure =
    match List.find t.slots ~f:(fun (prior, _) -> Slot.equal_kind prior slot) with
    | Some (_, diagnostic) -> diagnostic
    | None ->
      let diagnostic = Diagnostic.create [ failure ] in
      t.slots <- t.slots @ [ slot, diagnostic ];
      diagnostic
  ;;

  let retain t observation =
    List.iter
      (C.Completion_snapshot.failure_slots
         (C.Completion_observation.snapshot observation))
      ~f:(fun selected ->
        ignore
          (diagnostic t ~slot:(Slot.kind selected) ~failure:(Slot.failure selected)
           : Diagnostic.t))
  ;;

  let create observation =
    let completion = C.Completion_observation.completion observation in
    let t = { completion; slots = []; read_failure = None } in
    retain t observation;
    t
  ;;

  let observe t =
    match observe_result (fun () -> C.Completion_observation.observe t.completion) with
    | Ok observation ->
      retain t observation;
      Ok observation
    | Error failure ->
      let diagnostic =
        match t.read_failure with
        | Some diagnostic -> diagnostic
        | None ->
          let diagnostic = Diagnostic.create [ failure ] in
          t.read_failure <- Some diagnostic;
          diagnostic
      in
      Error diagnostic
  ;;

  let diagnostics t =
    (* Current Completion projection order, independent of when a write-once
       slot first became observable. Previously issued lists remain immutable. *)
    List.filter_map
      [ Slot.Release; Lexical_handler; Reply_boundary; Capture ]
      ~f:(fun kind ->
        List.find_map t.slots ~f:(fun (actual, diagnostic) ->
          Option.some_if (Slot.equal_kind actual kind) diagnostic))
    @ Option.to_list t.read_failure
  ;;
end
