open! Core

(** A bounded, read-only projection of compiler types for declarations in one
    acknowledged submission. Indices are zero-based statement/binding ordinals;
    these declarations do not carry individual runtime value previews. *)
type entry =
  { statement_index : int
  ; binding_index : int
  ; inspection : Agent_protocol.Developer_repl_wire.Type_inspection.t
  }

type t

val empty : t
val entries : t -> entry list
val limited : t -> bool
val error : t -> Agent_protocol.Error.t option

(** At most 16 sequential inspections and 4096 aggregate name/type bytes.
    Reads use the captured exact submission key and epoch. Each returned
    revision is checked; the first failure stops this pass without retrying or
    changing the acknowledged submission. Cancellation is never swallowed. *)
val read
  :  Agent_client.Developer_repl_connection.t
  -> current:Agent_protocol.Developer_repl_wire.Current_epoch.t
  -> submission:Agent_protocol.Developer_repl_wire.Submission.t
  -> t
