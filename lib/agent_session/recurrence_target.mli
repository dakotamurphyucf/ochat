open! Core

(** Immutable, bounded target selection data. Neither target kind grants access,
    source acquisition, child publication, Run admission or execution authority.
    The complete target is its OWN universal carrier; enclosing codecs must own
    this entire document as a value, preserving its original compatible unknowns.
    Complete target storage is bounded to 24 MiB including envelope/unknowns. *)
type t

module Parent : sig
  (** Captured inheritance selectors, not current authorization. Complete parent
      metadata is bounded to 256 KiB; inference selection retains its own exact
      unknowns and numeric lexemes. No credentials, callbacks or native IDs. *)
  type t

  val create
    :  workspace:Workspace_instance.t
    -> permission_profile:string
    -> permission_profile_digest:string
    -> runtime_policy:string option
    -> configuration_revision:int64
    -> inference_selection:Inference.Selection.t
    -> capability_pins:(string * string) list
    -> (t, Agent_protocol.Error.t) result

  val workspace : t -> Workspace_instance.t
  val permission_profile : t -> string
  val permission_profile_digest : t -> string
  val runtime_policy : t -> string option
  val configuration_revision : t -> int64
  val inference_selection : t -> Inference.Selection.t
  val capability_pins : t -> (string * string) list
end

type view =
  | Existing_session of
      { session : Agent_protocol.Session_ref.t
      ; source : Agent_protocol.Run_source.t
      ; configuration_revision : int64
      ; input : Agent_protocol.Session.Message_content.t
      }
  | Fresh_child of
      { artifact : Agent_store.Captured_prompt_artifact.t
      ; definition : Prompt_definition.t
      ; parent : Parent.t
      }

(** The frozen input is ordinary user content, never Authored_start. Its complete
    JSON is bounded to 256 KiB. The target's configuration frontier pins its
    existing inference selection; no second Selection copy is stored here. *)
val create_existing
  :  session:Agent_protocol.Session_ref.t
  -> source:Agent_protocol.Run_source.t
  -> configuration_revision:int64
  -> input:Agent_protocol.Session.Message_content.t
  -> (t, Agent_protocol.Error.t) result

(** Preserve complete captured artifact custody plus the actual original validated
    prompt definition (including root invocation/tool ABI and policy selectors).
    Match artifact definition/source identities where present and captured parent
    selectors. Complete definition metadata is bounded to 256 KiB before any
    constructor/recursive codec. Stored root_file is provenance only: captured
    replay must not reread it. The runtime admission owner rechecks current host
    and native policies before installation or initialization; this value never
    authorizes them. *)
val create_fresh
  :  artifact:Agent_store.Captured_prompt_artifact.t
  -> definition:Prompt_definition.t
  -> parent:Parent.t
  -> (t, Agent_protocol.Error.t) result

val view : t -> view

(** Exact retained definition slice for a fresh-child target, including its
    compatible unknown fields. Existing-session targets reject. This is immutable
    data custody, never current catalog membership or execution permission. *)
val fresh_definition_json : t -> (Jsonaf.t, Agent_protocol.Error.t) result

val digest : t -> (string, Agent_protocol.Error.t) result
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result
val limits : Document_schema.Limits.t
