(** Installed, readonly documentation queries. Construction loads the checked
    in-memory corpus; queries never invoke tools, providers or filesystem reads. *)
type t

(** [secret] is a host-generated unpredictable cursor signing key, never model
    input. The immutable service can be shared across callers. Cursors also bind
    each caller's scope, target host, capability selection and corpus revision.
    Default responses target 12,000 estimated tokens with a configurable 32,000
    ceiling. Counts use ceil(UTF-8 response bytes / 3), explicitly labelled as an
    estimate, not an exact tokenizer, upper bound or provider billing count. *)
val create
  :  ?default_tokens:int
  -> ?max_tokens:int
  -> secret:string
  -> unit
  -> (t, string) result

val fingerprint : t -> string
val parameters : Jsonaf.t

(** The host supplies the actual calling session/generation scope and narrowed
    capabilities. Task selection cannot enable an unavailable host target.
    Search returns ranked topic metadata/excerpts. Topic/prepare assemble stable
    prerequisite closures; continue resumes signed query positions. Whole reference
    sections, including code fences, remain atomic; insufficient budgets explicitly
    report the next minimum. The topic structure remains flat.

    Prepare starts with a flat orientation describing each feature's purpose,
    useful scenarios and direct reference roots. It distinguishes selected tools
    and enabled authoring targets from reference compatibility and runtime grants.

    [complete] concerns pagination of this query, not full feature coverage.
    Prepared packages remain labelled foundation-only until A01's full coverage
    audit and signature/tool-schema package integration are completed. *)
val query
  :  t
  -> host:Authoring_validation.host
  -> capabilities:Tool_capability.t
  -> scope:string
  -> Jsonaf.t
  -> Jsonaf.t
