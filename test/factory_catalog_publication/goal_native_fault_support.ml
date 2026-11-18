open! Core
open Agent_server_test_support
open Retry_support
module Intent = P.Goal_native_resume_intent

(* Fault selection inspects actual target-writer transactions. These records are
   evidence DATA only and cannot issue a callback, loan, preparation or receipt. *)
type cut =
  | Native_complete
  | Goal_command_unknown
  | After_goal_ack of (unit -> unit)

type selection =
  { session : P.Id.Session.t
  ; request : A.Goal_resume_request.t
  }

type t =
  { cut : cut
  ; mutable selected : selection option
  ; mutable candidate : Agent_store.Transaction.t option
  ; mutable hits : int
  ; reached : unit Eio.Promise.t
  ; resolver : unit Eio.Promise.u
  }

let create cut =
  let reached, resolver = Eio.Promise.create () in
  { cut; selected = None; candidate = None; hits = 0; reached; resolver }
;;

let request_equal selection intent =
  A.Goal_resume_request.equal selection.request (Intent.request intent)
  && P.Id.Principal.equal principal.id (Intent.principal_id intent)
;;

let matches t selection delta =
  Goal_admission_fault_tests.atoms delta
  |> List.exists ~f:(function
    | A.Session_delta.Invocation_changed invocation ->
      (match t.cut, invocation.P.Invocation.status, invocation.native_goal_action with
       | ( Native_complete
         , Resolved (Complete _)
         , Some (P.Goal_native_action_intent.Resume intent) ) ->
         request_equal selection intent
         &&
           (match Intent.phase intent with
           | Staged -> true
           | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _ -> false)
       | _ -> false)
    | Goal_catalog_changed catalog ->
      (match t.cut with
       | Native_complete -> false
       | Goal_command_unknown | After_goal_ack _ ->
         A.Goal_catalog.receipt
           catalog
           ~principal_id:principal.id
           ~key:(A.Goal_resume_request.key selection.request)
           ~request_sha256:(A.Goal_resume_request.digest selection.request |> protocol_ok)
         |> protocol_ok
         |> Option.is_some)
    | _ -> false)
;;

let reached t env path =
  match t.selected with
  | None -> ()
  | Some selection ->
    let scan =
      Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
      |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
      |> Existing.store_ok
    in
    (match List.last scan.entries with
     | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
       let transaction =
         Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
         |> Existing.store_ok
       in
       if P.Id.Session.equal transaction.session_id selection.session
       then (
         let delta =
           A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
           |> document_ok
           |> A.Session_delta_document.value
         in
         if matches t selection delta
         then (
           t.selected <- None;
           t.candidate <- Some transaction;
           t.hits <- t.hits + 1;
           Eio.Promise.resolve t.resolver ();
           match t.cut with
           | After_goal_ack revoke -> revoke ()
           | Native_complete | Goal_command_unknown ->
             raise (Core_unix.Unix_error (EIO, "native Goal selected writer sync", path))))
     | Some _ | None -> ())
;;

let wrap t env =
  Crash_fault_io.wrap
    env
    ~boundary:
      (match t.cut with
       | After_goal_ack _ -> After_sync
       | Native_complete | Goal_command_unknown -> Before_sync)
    ~matches:(fun path ->
      String.is_substring path ~substring:"/journal/"
      && String.is_suffix path ~suffix:".log")
    ~reached:(reached t env)
;;

let arm t entry request =
  t.selected <- Some { session = (state entry).identity.session_id; request }
;;

let await t =
  Eio.Promise.await t.reached;
  [%test_eq: int] 1 t.hits;
  Option.value_exn t.candidate
;;
