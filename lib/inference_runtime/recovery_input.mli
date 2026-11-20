(** Body-free evidence from one exact neutral model preparation. An omitted
    position contains no occurrence or payload evidence. *)
type t

type row = private
  { ordinal : int
  ; occurrence : History_entry.Id.t
  ; digest : Chatml_source.Digest.t
  }

val preparation_id : t -> string
val configuration_digest : t -> Chatml_source.Digest.t
val input_count : t -> int
val rows : t -> row list
val max_input_count : int
val max_projection_bytes : int

(** Digest the supported neutral semantic projection without encoding the raw
    provider envelope. Callers must have already excluded protected material. *)
val projected_digest
  :  History_entry.t
  -> remaining_bytes:int
  -> Chatml_source.Digest.t option * int

(** Private constructor for the inference runtime's accepted plan. *)
val capture
  :  Inference.Request.t
  -> configuration:Inference.Observation.Configuration.t
  -> omit:(History_entry.Id.t -> bool)
  -> t option
