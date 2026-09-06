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

(** [commit] blocks until the requested durability level is reached. *)
val commit
  :  t
  -> durability:Journal_segment.durability
  -> Transaction.t
  -> (committed, Store_error.t) result

(** [close] drains no new work and terminates the writer fiber. *)
val close : t -> unit
