open! Core
module P = Agent_protocol

type 'borrow attempt =
  { previous : Session_state.t
  ; candidate : Session_transition.t
  ; receipt : P.Moderator_execution.t
  ; borrow : 'borrow
  ; original : Execution_custody.Admission.t option
  ; writer : Session_commit_attempt.t
  }

module Phase = struct
  type 'borrow t =
    | Waiting
    | Prepared of 'borrow attempt
    | Activated of 'borrow attempt
    | Callback_acknowledged of 'borrow attempt
    | Released
end

module Extent = struct
  type t =
    | Not_started
    | Not_entered
    | Entered
    | Joined_without_entry
    | Joined_after_entry
end

type 'borrow t =
  { issuer : unit ref
  ; domain : int
  ; mutable phase : 'borrow Phase.t
  ; mutable extent : Extent.t
  ; mutable cancellation_requested : bool
  }

let error message = P.Error.create Conflict ~message ~retryable:false ()
let on_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)

let check_domain t =
  if on_domain t
  then Ok ()
  else Error (error "delegated policy claim belongs to another Actor domain")
;;

let read t f =
  if not (on_domain t) then invalid_arg "delegated policy claim: foreign domain";
  f t
;;

let create ~issuer =
  { issuer
  ; domain = (Stdlib.Domain.self () :> int)
  ; phase = Waiting
  ; extent = Not_started
  ; cancellation_requested = false
  }
;;

let check_owner t ~issuer =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  if phys_equal t.issuer issuer
  then Ok ()
  else Error (error "delegated policy claim belongs to another actual Actor")
;;

let phase t = read t (fun t -> t.phase)
let extent t = read t (fun t -> t.extent)
let cancellation_requested t = read t (fun t -> t.cancellation_requested)

let cancel t =
  if on_domain t
  then (
    t.cancellation_requested <- true;
    match t.phase with
    | Waiting -> t.phase <- Released
    | Prepared _ | Activated _ | Callback_acknowledged _ | Released -> ())
;;

let enter t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase, t.extent, t.cancellation_requested with
  | Activated _, Not_entered, false ->
    t.extent <- Entered;
    Ok ()
  | (Waiting | Prepared _ | Activated _ | Callback_acknowledged _ | Released), _, _ ->
    Error (error "delegated policy claim has no unentered current callback")
;;

let with_extent t f =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.extent with
  | Not_started ->
    t.extent <- Not_entered;
    Exn.protect
      ~finally:(fun () ->
        t.extent
        <- (match t.extent with
            | Not_entered -> Joined_without_entry
            | Entered -> Joined_after_entry
            | Not_started | Joined_without_entry | Joined_after_entry ->
              failwith "delegated policy owning extent was displaced"))
      ~f
  | Not_entered | Entered | Joined_without_entry | Joined_after_entry ->
    Error (error "delegated policy wrapper already started or joined")
;;

module Attempt = struct
  let create ~previous ~candidate ~receipt ~borrow ~original =
    { previous
    ; candidate
    ; receipt
    ; borrow
    ; original
    ; writer =
        Session_commit_attempt.create ~context:Delegated_policy_claim ~previous ~candidate
    }
  ;;

  let previous t = t.previous
  let candidate t = t.candidate
  let receipt t = t.receipt
  let borrow t = t.borrow
  let original t = t.original
  let writer t = t.writer
  let discard t = Option.iter t.original ~f:Execution_custody.Admission.retire
end

let prepare t attempt =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase, t.extent, t.cancellation_requested with
  | Waiting, Not_entered, false ->
    t.phase <- Prepared attempt;
    Ok ()
  | (Waiting | Prepared _ | Activated _ | Callback_acknowledged _ | Released), _, _ ->
    Error (error "delegated policy claim already selected or cancelled")
;;

let activate t attempt =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase with
  | Prepared actual when phys_equal actual attempt ->
    t.phase <- Activated actual;
    Ok ()
  | Waiting | Prepared _ | Activated _ | Callback_acknowledged _ | Released ->
    Error (error "delegated policy claim lost its exact prepared attempt")
;;

let joined t =
  match t.extent with
  | Joined_without_entry | Joined_after_entry -> true
  | Not_started | Not_entered | Entered -> false
;;

let release_no_writer t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase with
  | Waiting when joined t ->
    t.phase <- Released;
    Ok ()
  | Released when joined t -> Ok ()
  | Prepared attempt when joined t && Session_commit_attempt.no_writer attempt.writer ->
    Attempt.discard attempt;
    t.phase <- Released;
    Ok ()
  | Prepared attempt -> Session_commit_attempt.unresolved attempt.writer
  | Waiting | Activated _ | Callback_acknowledged _ | Released ->
    Error
      (error "delegated policy claim has not reached joined exact no-writer retirement")
;;

module Callback_acknowledgement = struct
  type 'borrow capture = 'borrow t

  type 'borrow t =
    { capture : 'borrow capture
    ; previous : 'borrow Phase.t
    ; incoming : 'borrow Phase.t
    }

  let prepare capture ~borrow =
    let open Result.Let_syntax in
    let%bind () = check_domain capture in
    match capture.phase with
    | Activated attempt when phys_equal attempt.borrow borrow ->
      Ok
        { capture
        ; previous = capture.phase
        ; incoming = Phase.Callback_acknowledged attempt
        }
    | Waiting | Prepared _ | Activated _ | Callback_acknowledged _ | Released ->
      Error (error "delegated policy callback ACK differs from its actual claim")
  ;;

  let check t =
    let open Result.Let_syntax in
    let%bind () = check_domain t.capture in
    if phys_equal t.capture.phase t.previous
    then Ok ()
    else Error (error "delegated policy callback ACK was displaced or already installed")
  ;;

  (* Trusted actual Actor suffix, after check and SAME terminal ACK/release.
     No yield or fallible action is allowed between check and this assignment. *)
  let install t = t.capture.phase <- t.incoming
end

let release_activated t ~borrow =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase with
  | Callback_acknowledged attempt when phys_equal borrow attempt.borrow && joined t ->
    t.phase <- Released;
    Ok ()
  | Waiting | Prepared _ | Activated _ | Callback_acknowledged _ | Released ->
    Error
      (error "delegated policy claim retirement differs from its joined actual borrow")
;;
