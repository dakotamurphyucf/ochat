open! Core
module P = Agent_protocol

module Transaction = struct
  type t =
    { session_id : P.Id.Session.t
    ; generation : int
    ; sequence : int64
    ; revision : int64
    ; previous_hash : string option
    ; payload : string
    ; hash : string
    ; max_payload_length : int
    }
end

module Returned = struct
  type t =
    { sequence : int64
    ; hash : string
    }
end

module Acknowledgement = struct
  type t =
    { transaction : Transaction.t
    ; returned : Returned.t
    }

  let transaction t = t.transaction
  let returned t = t.returned
end

module Progress = struct
  type t =
    | Unobserved
    | Before_writer
    | Writer_entered of Transaction.t
    | Journal_acknowledged of Acknowledgement.t
end

module Outcome = struct
  type t =
    | Not_called
    | In_progress
    | Returned of (unit, P.Error.t) result
    | Raised of exn * Stdlib.Printexc.raw_backtrace
end

type t =
  { domain : int
  ; previous : Session_state.t
  ; candidate : Session_transition.t
  ; mutable progress : Progress.t
  ; mutable outcome : Outcome.t
  ; mutable writer_returned : Returned.t option
  }

let error message = P.Error.create Conflict ~message ~retryable:false ()
let on_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)

let check_domain t =
  if on_domain t
  then Ok ()
  else Error (error "commit observation belongs to another domain")
;;

let read t f =
  if not (on_domain t) then invalid_arg "Session_commit_observation: foreign domain";
  f t
;;

module Owner = struct
  type observation = t
  type nonrec t = t

  let create ~previous ~candidate =
    { domain = (Stdlib.Domain.self () :> int)
    ; previous
    ; candidate
    ; progress = Unobserved
    ; outcome = Not_called
    ; writer_returned = None
    }
  ;;

  let commit t ~commit =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    let%bind () =
      match t.outcome with
      | Not_called -> Ok ()
      | In_progress | Returned _ | Raised _ ->
        Error (error "commit observation was already invoked")
    in
    t.outcome <- In_progress;
    match commit ~observation:t with
    | result ->
      let result =
        match result, t.progress with
        | Ok (), (Before_writer | Writer_entered _) ->
          Error
            (error "observed persistence returned without its journal acknowledgement")
        | (Ok () | Error _), (Unobserved | Journal_acknowledged _)
        | Error _, (Before_writer | Writer_entered _) -> result
      in
      t.outcome <- Returned result;
      result
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      t.outcome <- Raised (exn, backtrace);
      Exn.raise_with_original_backtrace exn backtrace
  ;;
end

module Host = struct
  type observation = t
  type nonrec t = t

  let observe_entry t ~previous ~candidate =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    match t.outcome, t.progress with
    | In_progress, Unobserved
      when phys_equal t.previous previous && phys_equal t.candidate candidate ->
      t.progress <- Before_writer;
      Ok t
    | ( (Not_called | In_progress | Returned _ | Raised _)
      , (Unobserved | Before_writer | Writer_entered _ | Journal_acknowledged _) ) ->
      Error (error "commit observation differs from its exact persistence candidate")
  ;;

  let transaction_matches t (transaction : Transaction.t) =
    let state = t.candidate.state in
    P.Id.Session.equal transaction.session_id t.previous.identity.session_id
    && P.Id.Session.equal transaction.session_id state.identity.session_id
    && Int.equal transaction.generation state.identity.generation
    && Int64.equal transaction.sequence state.counters.transaction_sequence
    && Int64.equal transaction.revision state.counters.revision
    && transaction.max_payload_length > 0
    && String.length transaction.payload <= transaction.max_payload_length
    && String.equal
         transaction.hash
         Digestif.SHA256.(to_hex (digest_string transaction.payload))
  ;;

  let with_writer t ~transaction ~commit =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    let%bind () =
      match t.outcome, t.progress with
      | In_progress, Before_writer when transaction_matches t transaction -> Ok ()
      | ( (Not_called | In_progress | Returned _ | Raised _)
        , (Unobserved | Before_writer | Writer_entered _ | Journal_acknowledged _) ) ->
        Error (error "writer observation differs from its actual transaction")
    in
    (* This mark is immediately before the real canonical writer invocation.
       Even cancellation during bounded queue admission remains conservative. *)
    t.progress <- Writer_entered transaction;
    let%bind returned = commit () in
    t.writer_returned <- Some returned;
    if
      Int64.equal transaction.sequence returned.Returned.sequence
      && String.equal transaction.hash returned.hash
    then (
      t.progress <- Journal_acknowledged { transaction; returned };
      Ok returned)
    else Error (error "canonical writer reply differs from its exact transaction")
  ;;
end

let progress t = read t (fun t -> t.progress)
let outcome t = read t (fun t -> t.outcome)
