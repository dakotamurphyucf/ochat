open! Core
module P = Agent_protocol
module A = Agent_session
module Ledger = A.Recovery_window_ledger
module Window = Ledger.Window
module Member = Ledger.Member

type phase =
  | Reserved
  | Armed of P.History.entry
  | Adopted of P.Recovery_window.Citation.t
  | Resolved

type reservation =
  { handle : Agent_store.Session_store.Handle.t
  ; authorize : A.Session_state.t -> (unit, P.Error.t) result
  ; result : P.Recovery_window.Citation.t option Eio.Promise.t
  ; resolve : P.Recovery_window.Citation.t option Eio.Promise.u
  ; mutable phase : phase
  }

type t = { mutable reservations : reservation list }

let create () = { reservations = [] }

let reserve t ~handle ~authorize =
  if List.length t.reservations >= 8
  then None
  else (
    let result, resolve = Eio.Promise.create () in
    let reservation = { handle; authorize; result; resolve; phase = Reserved } in
    t.reservations <- reservation :: t.reservations;
    Some reservation)
;;

let row_limits =
  Document_schema.Limits.create
    ~max_bytes:(4 * 1024 * 1024)
    ~max_fields:65_536
    ~max_nodes:131_072
    ~max_depth:64
  |> Result.ok
  |> Option.value_exn
;;

let arm t (row : P.History.entry) =
  match t.phase with
  | Reserved
    when Result.is_ok (Document_schema.Json.validate ~limits:row_limits row.payload) ->
    t.phase <- Armed row;
    true
  | Reserved | Armed _ | Adopted _ | Resolved -> false
;;

let resolve t citation =
  match t.phase with
  | Resolved -> ()
  | Reserved | Armed _ | Adopted _ ->
    t.phase <- Resolved;
    Eio.Promise.resolve t.resolve citation
;;

let release t reservation =
  resolve reservation None;
  t.reservations
  <- List.filter t.reservations ~f:(fun candidate ->
       not (phys_equal candidate reservation))
;;

let await t = Eio.Promise.await t.result

let appended_member (state : A.Session_state.t) row =
  Option.bind state.recovery_windows ~f:(fun ledger ->
    let active = Ledger.active ledger in
    Window.fold_members active ~init:None ~f:(fun found member ->
      match found, Member.source member with
      | None, Live basis
        when Int64.equal basis.revision state.counters.revision
             && Member.matches member row
             && (not (Member.is_withdrawn member))
             && Option.exists
                  (List.nth
                     state.conversation.canonical_history
                     (Member.source_ordinal member))
                  ~f:(P.History.equal_entry row) -> Ok (Some (Member.citation member))
      | _ -> Ok found)
    |> Result.ok
    |> Option.join)
;;

let idle (state : A.Session_state.t) =
  match state.lifecycle.observed, state.active_operation with
  | Idle, None -> true
  | _ -> false
;;

let committed t ~handle state events =
  let appended =
    List.exists events ~f:(fun (event : P.Event.Durable.t) ->
      match event.kind with
      | History_appended -> true
      | _ -> false)
  in
  let settle reservation citation =
    if idle state
    then resolve reservation (Some citation)
    else reservation.phase <- Adopted citation
  in
  List.iter t.reservations ~f:(fun reservation ->
    if phys_equal handle reservation.handle
    then (
      match reservation.phase with
      | Armed row ->
        (match reservation.authorize state with
         | Error _ -> resolve reservation None
         | Ok () when appended ->
           Option.iter (appended_member state row) ~f:(settle reservation)
         | Ok () -> ())
      | Adopted citation ->
        (match reservation.authorize state with
         | Error _ -> resolve reservation None
         | Ok () -> settle reservation citation)
      | Reserved | Resolved -> ()))
;;
