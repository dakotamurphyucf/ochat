(** Pure aggregate settlement shared by actual Operation and Job completion.
    This is DATA construction, with no authorization, callbacks or I/O. *)

(** [previous] is the exact retained live Run in [index]. [owned_work] and
    [terminal_work] include the actual newly completed occurrence and any already
    selected retry successor. Produces exactly one next Run revision. Waiting
    remains waiting for its real callback; quiescent Single_turn completes, while
    successful Workflow requires a retained pending Finish. Existing outcome
    precedence and pending-intent consumption are shared by both producers.

    A terminal winner receives the canonical terminal receipt in this same index,
    using [now] for the receipt and Run. [operation_id] is the actual completing
    Operation when present; Job completion consumes a Finish without inventing an
    Operation. The caller persists this index with the actual work terminal fact
    in one transaction and supplies the matching live completion clock sample. *)
val prepare
  :  Run_state.t
  -> state:Session_state.t
  -> previous:Agent_protocol.Run.t
  -> owned_work:Agent_protocol.Run_work.t list
  -> terminal_work:Agent_protocol.Run_work.Terminal.t list
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> now:Agent_protocol.Timestamp.t
  -> (Run_state.t, Agent_protocol.Error.t) result
