(** Versioned, nonsecret intent for an externally admitted developer Model_call.
    The value is pure comparison/storage data: it grants no actor, provider,
    parent, job-claim, result-read or execution authority. *)
open! Core

type t

(** Encode exactly one supported [agent_prompt_v1] request and the complete
    selected Target. [request] preserves its original JSON including unknown
    unique-key fields. Developer admission rejects duplicate keys through the
    shared whole-tree validator, then applies the real shared recipe grammar;
    only syntax-only conversation [Model_spawn_request.decode] retains its
    historic first-duplicate-field lookup behavior. [target] is revalidated
    including nonsecret binding/revision fields and unknown target members.
    [limits] cap the COMPLETE encoded envelope (request, Target and wrapper)
    as one tree, rather than giving each subtree a full independent budget.
    The physical host separately proves that its selected provider issued
    this Target for the original actor. *)
val create
  :  request:Jsonaf.t
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (t, Protocol_error.t) result

val request : t -> Jsonaf.t
val target : t -> Inference.Request.Target.t

(** Canonical v1 envelope with exactly [schema_version], [recipe], [request]
    and [target] members. The envelope has no secret, native Context, callback
    or Session ID; unknown nested request/Target members remain lossless. *)
val to_json : t -> Jsonaf.t

(** Apply the same aggregate envelope, recipe, full-tree bounds and Target
    validation as [create] to every persisted job on load/recovery. Unsupported
    version, unknown envelope member, duplicate key, malformed recipe or
    invalid Target rejects. *)
val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, Protocol_error.t) result
