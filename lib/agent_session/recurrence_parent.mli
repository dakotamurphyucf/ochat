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

(** Explicit bounded projection; compatible unknowns belong to the enclosing
    target carrier. Inference.Selection retains its own complete subtree. *)
val to_jsonaf : t -> (Jsonaf.t, Agent_protocol.Error.t) result

val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
