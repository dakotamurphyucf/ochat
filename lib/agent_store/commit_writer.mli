(** Single-fiber persist-before-publish writer for one loaded session. *)

type t

type committed =
  { transaction_sequence : int64
  ; transaction_hash : string
  ; journal_position : Journal.append_result
  }

(** [create] starts a bounded writer fiber owned by [sw]. *)
val create
  :  sw:Eio.Switch.t
  -> journal:Journal.t
  -> session_id:Agent_protocol.Id.Session.t
  -> next_transaction_sequence:int64
  -> previous_transaction_hash:string option
  -> queue_capacity:int
  -> (t, Store_error.t) result

(** [commit] blocks until the requested durability level is reached. An
    exceptional journal failure is re-raised to its initiating caller with the
    original backtrace. Before that reply, the writer becomes unavailable:
    queued and later requests fail deterministically without using uncertain
    journal counters. Recovery requires a freshly reopened journal/writer.
    Synthetic operation Timeout/cancellation does not kill the worker; genuine
    cancellation of its owning context still terminates it. Callers need not
    share that context: worker shutdown releases queued reply waits and cancels
    blocked bounded-queue admission, returning a typed closed failure. An already
    resolved normal or exceptional reply takes precedence over shutdown. *)
val commit
  :  t
  -> durability:Journal_segment.durability
  -> Transaction.t
  -> (committed, Store_error.t) result

(** [close] terminates the writer fiber after preceding accepted requests.
    It remains usable after an exceptional operation has made the writer
    unavailable. Concurrent callers or worker cancellation cannot leave close
    blocked on a dead queue; requests ordered after close may fail closed. *)
val close : t -> unit
