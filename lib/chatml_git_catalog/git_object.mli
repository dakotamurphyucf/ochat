open Core
module Id = Chatml_git_host.Object_id

(** Private exact cat-file --batch decoder/hash verifier; no repository/I/O. *)
module Kind : sig
  type t =
    | Commit
    | Tree
    | Blob
  [@@deriving equal, sexp_of]
end

module Error : sig
  type t =
    | Missing
    | Malformed_protocol
    | Hash_mismatch
    | Wrong_kind
    | Body_limit
  [@@deriving equal, sexp_of]
end

(** Verified exact body and format-qualified ID. Header/type/size/trailer agree;
    hash is Git's hash(kind ^ " " ^ decimal byte length ^ NUL ^ raw body).
    SHA1/SHA256 Git IDs remain distinct from captured-source SHA256. *)
type t

(** bounded body copying/hashing polls, including actual chunk work. max_bytes
    bounds body; the caller201 query allowance also includes bounded header/LF.
    Missing requires exact requested-ID missing record. Reject mismatched header
    ID/type/size, decimal overflow, unexpected/trailing/multiple records or truncation.
    The checkpoint transports invocation-local bounded work/deadline failure only
    through a private scoped escape; unrelated user exceptions remain unchanged. *)
val verify
  :  Id.t
  -> wire:string
  -> expected:Kind.t
  -> max_bytes:int
  -> admit_bytes:(int -> unit)
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val id : t -> Id.t
val kind : t -> Kind.t
val body : t -> string
