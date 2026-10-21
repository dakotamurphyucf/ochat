open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Recovery = Store.Recovery

let error code message ~retryable = P.Error.create code ~message ~retryable ()

let store_error = function
  | Store.Error.Store failure -> Agent_store.Store_error.to_protocol_error failure
  | Conflict | Prewrite_conflict ->
    error P.Error.Conflict "developer recovery must be reconciled" ~retryable:true
  | Foreign_or_stale ->
    error P.Error.Lease_stale "developer recovery owner changed" ~retryable:false
  | Limit_exceeded ->
    error P.Error.Resource_limit "developer recovery limit reached" ~retryable:false
  | Closed ->
    error P.Error.Invalid_state "developer recovery owner is closed" ~retryable:false
;;

let store result = Result.map_error result ~f:store_error

let diverged () =
  error P.Error.Persistence_error "developer recovery diverged" ~retryable:false
;;

let settle_inference window pending =
  let open Result.Let_syntax in
  let%map _ = store (Recovery.commit_inference_interruption window pending) in
  ()
;;

let settle_terminal window pending =
  let open Result.Let_syntax in
  let%map _ = store (Recovery.commit_terminal window pending ~admit:(fun () -> true)) in
  ()
;;

let resume_inference window pending =
  let open Result.Let_syntax in
  match Recovery.reconcile_inference_interruption window pending with
  | Ok (Store.Installed _) -> Ok ()
  | Ok Store.Absent | Error Store.Error.Prewrite_conflict ->
    let%bind refreshed = store (Recovery.reprepare_inference_same window pending) in
    settle_inference window refreshed
  | Ok Store.Diverged -> Error (diverged ())
  | Error error -> Error (store_error error)
;;

let resume_terminal window pending =
  let open Result.Let_syntax in
  match Recovery.reconcile_terminal window pending with
  | Ok (Store.Installed _) -> Ok ()
  | Ok Store.Absent | Error Store.Error.Prewrite_conflict ->
    let%bind refreshed = store (Recovery.reprepare_same window pending) in
    settle_terminal window refreshed
  | Ok Store.Diverged -> Error (diverged ())
  | Error error -> Error (store_error error)
;;

let interrupted now job =
  let state = P.Developer_job.state job in
  { state with
    status = P.Job.Interrupted "recovered after daemon restart"
  ; completed_at = Some (now ())
  ; result = None
  ; retry_failure = None
  }
;;

let recover window ~now =
  let open Result.Let_syntax in
  let rec drain () =
    match Recovery.inference_pending window, Recovery.pending window with
    | Some pending, None ->
      let%bind () = resume_inference window pending in
      drain ()
    | None, Some pending ->
      let%bind () = resume_terminal window pending in
      drain ()
    | Some _, Some _ -> Error (diverged ())
    | None, None ->
      let%bind view = store (Recovery.observe window) in
      (match Recovery.View.inference_candidates view with
       | candidate :: _ ->
         let%bind pending =
           store (Recovery.prepare_inference_interruption window ~view ~candidate)
         in
         let%bind () = settle_inference window pending in
         drain ()
       | [] ->
         (match Recovery.View.candidates view with
          | candidate :: _ ->
            let job = Recovery.Candidate.job candidate in
            let%bind pending =
              store
                (Recovery.prepare_terminal
                   window
                   ~view
                   ~candidate
                   ~after:(interrupted now job))
            in
            let%bind () = settle_terminal window pending in
            drain ()
          | [] -> Ok ()))
  in
  drain ()
;;
