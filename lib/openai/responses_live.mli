open! Core

(** Pure translation of legacy DTO observations. Owns scoped aliases, never a
    host allocator, canonical admission, guessed wire capture or commit. *)
type t

val create : scope:Transcript.Scope.t -> limits:Document_schema.Limits.t -> t
val start : t -> (t * Transcript.Stream.t list, string) Result.t

val observe_legacy
  :  t
  -> entry_id:History_entry.Id.t option
  -> Responses.Response_stream.t
  -> (t * Transcript.Stream.t list, string) Result.t

(** Only the owner calls this after successful admission, with the exact committed
    entry. Provider done/completed observations cannot produce finalization. *)
val finalized : t -> History_entry.t -> (t * Transcript.Stream.t list, string) Result.t

(** Observed provider outcome only; the owner delays finishing until local outputs
    have committed. None means no terminal provider observation was received. *)
val completion : t -> Transcript.Stream.completion option

val finish
  :  t
  -> completion:Transcript.Stream.completion
  -> (t * Transcript.Stream.t list, string) Result.t
