(** Shared pure FIFO eligibility; no actor, persistence or I/O capability. *)
module Boundary : sig
  type t =
    | Worker of Agent_protocol.Id.Operation.t
    | Idle_start
  [@@deriving equal, sexp]
end

type t

(** Validate the actual current root, generation and host runtime admission.
    A closed gate/stopped desired state produces an empty prefix. An unrelated
    worker ID is a typed conflict, never an idle/terminal inference. *)
val create
  :  Session_state.t
  -> boundary:Boundary.t
  -> runtime_admission_open:bool
  -> (t, Agent_protocol.Error.t) result

(** Maximal contiguous eligible prefix. An ineligible head blocks every later
    entry, irrespective of its own timing. A released after-root entry cannot
    join that same live root even if a restored carrier already contains terminal
    evidence; an actual later root or idle boundary is required. Does not mutate
    the queue. *)
val eligible_prefix
  :  t
  -> Pending_input_document.t list
  -> (Pending_input_document.t list, Agent_protocol.Error.t) result
