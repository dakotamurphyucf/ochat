(** Display-only metadata from an already principal-projected session snapshot.
    No tool arguments, outputs, progress text, error messages or credentials are
    retained here. Acknowledgements and background results remain distinct. *)
type row =
  { key : string
  ; label : string
  ; status : string
  ; active : bool
  }
[@@deriving equal]

type t =
  { session_id : string
  ; generation : int
  ; active_jobs : int
  ; pending_completions : int
  ; rows : row array
  }
[@@deriving equal]

val of_snapshot : Agent_protocol.Snapshot.t -> t
