(** An owner-neutral invocation record for the shared admission path. The
    conversation arm retains the entire original [Invocation.t], including
    provider/history/routing/observation/handler/publication metadata. Existing
    conversation codecs and actor transitions remain authoritative. Developer
    records have no synthetic Session ID, model call, or provider history. *)

type context =
  { id : Id.Invocation.t
  ; owner : Execution_owner.t
  ; origin : Invocation.origin
  ; parent_invocation : Id.Invocation.t option
  ; parent_job : Id.Job.t option
  ; tool_name : string
  ; implementation_revision : string
  ; capability_fingerprint : string
  ; input : Jsonaf.t
  ; created_at : Timestamp.t
  ; deadline : Timestamp.t option
  }
[@@deriving equal, sexp_of]

type t

(** Lossless adapter. Projection returns the exact original conversation record,
    not a reconstruction from [context]. *)
val of_conversation : Invocation.t -> (t, Error.t) result

val to_conversation : t -> Invocation.t option

(** Pure data construction, not admission. Only a trusted developer actor may
    persist/dispatch it after checking the exact owner and selected capture.
    Model/Moderator origins and provider-history fields have no developer form. *)
val create_developer : context -> (t, Error.t) result

val context : t -> context
val status : t -> Invocation.status
val outcome : t -> Invocation.outcome option
val validate : t -> (unit, Error.t) result
val dispatch : t -> (t, Error.t) result
val resolve : t -> Invocation.outcome -> (t, Error.t) result
val cancel : t -> reason:string -> (t, Error.t) result
