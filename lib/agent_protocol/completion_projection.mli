(** Evidence retained by a host-managed standalone completion adapter. The hashes
    bind the original immutable invocation contract and terminal job storage; a
    rejected projection never replaces the job's business result. This DTO alone
    does not authorize publication. Admission must validate the materialized
    completion against both the stored result and the original contract. *)
type t =
  { job_attempt : int
  ; contract_sha256 : string
  ; result_sha256 : string
  ; rejected : bool
  }
[@@deriving equal, sexp]

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Hash the exact canonical protocol representation, including an artifact's
    owner, outcome and content digest when the result is stored externally. *)
val contract_digest : Completion_contract.t -> string

val result_digest : Stored_completion.t -> string

(** Fixed, bounded public error, without rejected business data. *)
val rejection : Invocation.tool_error
