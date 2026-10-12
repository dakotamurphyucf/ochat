open! Core

(** Canonical, inspectable shell runtime manifest. *)

type extension_script =
  { id : string
  ; kind : Chatmd_script_spec.kind
  ; source_sha256 : string
  ; surface_version : string
  ; entrypoint : string
  ; limits : Chatmd_script_spec.limits
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type edge_kind =
  | Extends
  | Tool_runtime
  | Moderator_runtime
  | Chatml_extension
  | Executable_extension
  | Worker_runtime
  | External_backend
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type dependency =
  { from_id : string
  ; to_id : string
  ; kind : edge_kind
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type payload =
  { encoding_version : string
  ; platform : Shell_spec.platform
  ; runtimes : Shell_spec.t list
  ; tools : Shell_tool_spec.t list
  ; moderator_runtime : string option
  ; extension_scripts : extension_script list
  ; dependencies : dependency list
  ; required_features : Feature.t list
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

module Prepared_hook : sig
  (** Immutable execution fact for one actually compiled prepared hook entrypoint.
      Its complete program fingerprint includes source bytes, interfaces, scopes,
      compiler contract and selected entrypoint. This value alone grants no
      authority and does not prove that compilation occurred. Registry derives it
      from the authentic compiled program, never from authored digest claims. *)
  type t [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

  (** Nonempty NUL-free ID/entrypoint and lowercase64-hex program fingerprint.
      Existing declaration admission and the aggregate manifest cap bound names.
      Admission walks poll [checkpoint]. Decoders preserve these same invariants. *)
  val create
    :  id:string
    -> kind:Chatmd_script_spec.kind
    -> entrypoint:string
    -> program_fingerprint:string
    -> checkpoint:(unit -> unit)
    -> t Or_error.t

  val id : t -> string
  val kind : t -> Chatmd_script_spec.kind
  val entrypoint : t -> string
  val program_fingerprint : t -> string
end

module Authorization : sig
  (** Validated immutable v2 projection, ordered by script ID then entrypoint.
      Construction is owned by [with_prepared_hooks]; no native capability,
      initializer, callback or prepared source inventory is retained. *)
  type t [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

  val prepared_hooks : t -> Prepared_hook.t list
  val resource_sha256 : t -> string
end

type t =
  { payload : payload
  ; canonical_json : string
  ; sha256 : string
  ; authorization : Authorization.t option
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** [create payload] creates the resource/declaration v1 manifest. It sorts JSON
    object keys, serializes [payload], and hashes the exact versioned encoding.
    Lists retain their supplied order. Its canonical bytes remain unchanged. *)
val create : payload -> t

(** Add the authentic prepared projection to a raw manifest. Reject already
    projected manifests, duplicate (ID,entrypoint) facts, facts not matching a raw
    script ID/kind, and a projected script missing its declared default entrypoint.
    Empty projection returns the original manifest unchanged. Nonempty projection
    uses canonical [ochat.shell.execution.v2] JSON containing the unchanged raw
    resource payload and sorted prepared facts; [sha256] binds the whole encoding.
    Checked byte-length admission precedes output allocation. Comparison, encoding,
    copying and hashing poll bounded chunks. [max_bytes] is the complete canonical
    output cap; the startup owner separately admits those exact metadata bytes.
    Checkpoint exceptions and their original backtraces propagate unchanged. *)
val with_prepared_hooks
  :  t
  -> Prepared_hook.t list
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

(** Original declaration/material digest, independently of prepared code. Does
    not itself authorize execution of a prepared hook. *)
val resource_sha256 : t -> string

(** Actual canonical authorization encoding: v1 for empty projection, v2 otherwise.
    Signature payloads use this accessor rather than raw [payload.encoding_version]. *)
val encoding_version : t -> string
