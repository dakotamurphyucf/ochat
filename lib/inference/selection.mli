open! Core

(** Private immutable storage evidence for an inference selection. An unresolved
    historical value carries no target, defaults or execution authority. Resolving
    host policy and persisting a capture before dispatch belong to the session
    owner; this module performs no I/O or credential lookup. *)
module Error = Request.Error

type view =
  | Unresolved
  | Captured of Request.Target.t

type t

(** Authored unresolved storage value. The complete wrapper is admitted under
    [limits], including its encoded size. *)
val unresolved : limits:Document_schema.Limits.t -> (t, Error.t) Result.t

(** Authored capture of an already resolved target. Both the target's native
    invariants and the complete wrapper must satisfy [limits]. This records
    selection evidence, not current eligibility or authorization to dispatch. *)
val captured
  :  Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (t, Error.t) Result.t

val view : t -> view

(** Compare the admitted selection view using [Request.Target.equal] for captured
    targets. This is not exact stored JSON identity and must not replace byte,
    digest, unknown-field or authority checks. *)
val equal : t -> t -> bool

(** Capture an unresolved value without retiring its unknown wrapper members or
    changing their order. Validate the original value under [limits] before the
    edit, then admit the complete result. An already captured value may repeat
    only a complete [Request.Target.equal] target: unknown values, numeric lexemes
    and presence participate, while object member order does not. Repetition
    returns the existing value unchanged; a different target conflicts. This is
    not a target-change or override API. *)
val capture
  :  t
  -> target:Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (t, Error.t) Result.t

(** Replace a selection only after the host approves the target-change policy.
    This is distinct from immutable [capture]. Both original and replacement
    satisfy [limits]; wrapper unknown members and order remain unchanged. For
    model/settings changes, callers use [Request.Target.with_model] or
    [Request.Target.with_setting] to retain the target's own unknown fields.
    This operation grants no authority and performs no dispatch. *)
val change
  :  t
  -> target:Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (t, Error.t) Result.t

(** Exact private storage JSON. The wrapper has [state] equal to ["unresolved"]
    with no [target] member, or ["captured"] with a required target decoded by
    [Request.Target.of_json]. Other state tags, a target on an unresolved value,
    explicit null targets and malformed native values reject. The original
    complete JSON is validated before conversion and retained, including unknown
    wrapper/target members, order and lexical JSON numbers. No public disclosure
    projection is implied. Enclosing storage codecs may own this complete subtree
    as a value because this module owns its preservation and validation. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, Error.t) Result.t
val validate : t -> limits:Document_schema.Limits.t -> (unit, Error.t) Result.t
