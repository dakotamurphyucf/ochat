(** Host-owned private publication for one physically retained session. A
    descriptor can be returned only when its exact saved selection and adopted
    bytes are durable. This store does not establish source membership, actor
    authorization, or original converter provenance. Its Agent_session
    adapter supplies those checks at the actual occurrence/part. *)
open! Core

module P = Agent_protocol
module Reference = Recovery_asset_reference

type t

type error =
  | Denied
  | Unavailable
  | Missing
  | Corrupt
  | Unsupported
  | Limit
  | Conflict
[@@deriving equal, sexp_of]

val create
  :  Blob_store.t
  -> sessions:Session_store.t
  -> handle:Session_store.Handle.t
  -> (t, error) result

(** Trusted synchronous and nonyielding guard on the current source. It runs
    before lookup, at durable commit and again after the last yielding read or
    publication action. It must never await the owning session actor. *)
type check = unit -> bool

(** Persist a private intent before stage I/O. Only an exact retry of the
    originally selected kind, MIME, digest and length can reuse a pending or
    committed coordinate. A different value fails without writing any bytes.
    The original timestamp and principal remain part of the selected metadata. *)
val publish
  :  t
  -> sw:Eio.Switch.t
  -> check:check
  -> P.Recovery_window.Citation.t
  -> part:int
  -> kind:P.Blob.kind
  -> media_type:string
  -> principal:P.Id.Principal.t
  -> now:P.Timestamp.t
  -> max_bytes:int
  -> string
  -> (Reference.t, error) result

(** Reconcile an exact pending selection using only complete verified local
    bytes. Missing/short stages stay private; the original producer can retry
    with the exact bytes or an irreversible source fence can retire the stage.
    Never calls a converter or searches other stages. *)
val resume
  :  t
  -> sw:Eio.Switch.t
  -> check:check
  -> accepts:(Reference.t -> string -> bool)
  -> P.Recovery_window.Citation.t
  -> part:int
  -> max_bytes:int
  -> (Reference.t, error) result

(** Exact committed lookup and verified whole-byte read. A fresh read
    stabilizes an earlier uncertain directory sync before using the commit.
    A pending selection alone remains unavailable. Return protected bytes to
    the same trusted host scope, with another check at any later delivery. *)
val read
  :  t
  -> sw:Eio.Switch.t
  -> check:check
  -> P.Recovery_window.Citation.t
  -> part:int
  -> max_bytes:int
  -> (Reference.t * string, error) result

(** Cheap bounded maintenance discovery. A positive result only schedules the
    verified collection attempt and supplies no authority to reclaim bytes. *)
val has_entries : Retention_reader.t -> (bool, Store_error.t) result

(** One bounded quiescent collection under the physical session lease and
    blob retention coordinator. [retired] proves an irreversible fence for
    this exact citation. Uncertainty, reversible withdrawals, unknowns and an
    active read retain custody. No persisted filename grants authority. *)
val collect
  :  t
  -> reader:Retention_reader.t
  -> retired:(P.Recovery_window.Citation.t -> bool)
  -> (int option, Store_error.t) result
