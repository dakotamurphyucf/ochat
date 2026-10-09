(** Resource binding for root requests across operations in one runtime graph.
    Does not own selection or authorization. Single Eio domain; detached children
    and admitted jobs never borrow this receiver. *)
type t

val create : Inference_runtime.Session.t -> t

(** [resolved] must come from current host resolution for this capture. Reuses
    qualified same-identity/policy preparation through derive_in_session; otherwise
    opens a fresh adapter resource and retires only the previous root binding.
    Original moderator binding is untouched. Concurrent root borrowing rejects.
    Candidate preparation/retirement may yield outside the actor mailbox.
    Callback exits always release ownership, preserving original exceptions. *)
val with_context
  :  t
  -> resolved:Inference_runtime.Context.t
  -> f:(Inference_runtime.Context.t -> 'a)
  -> ('a, Inference_runtime.Preparation_error.t) result

(** Closes admission immediately. During a borrow, resource retirement waits for
    callback exit; admitted request evidence is never replaced. Graph closure
    still closes transport using existing Session cancellation ownership. *)
val close : t -> unit

(** Decorate the actor capture port at runtime graph construction. Resolution and
    actor admission remain with [source]; receiver owns transport resources only. *)
val wrap : t -> Root_context.t -> Root_context.t
