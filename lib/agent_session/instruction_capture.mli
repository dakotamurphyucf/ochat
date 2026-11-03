open! Core

(** Pure, immutable consistency evidence. These values neither prove an
    authorized read nor grant permission to activate or use their contents. *)
type t

module Kind : sig
  type t =
    | Project
    | Skills
  [@@deriving equal, sexp_of]
end

(** Project members must be AGENTS.md files in strict root-to-target order,
    from this root and applicable to this scope. Completeness is a host check. *)
val project
  :  root:Instruction_content.Root_id.t
  -> scope:Instruction_content.Scope.t
  -> Instruction_content.Snapshot.t
  -> (t, Agent_protocol.Error.t) result

(** Nonempty, distinct qualified skill members, retaining their explicit order. *)
val skills : Instruction_content.Snapshot.t -> (t, Agent_protocol.Error.t) result

val id : t -> string
val kind : t -> Kind.t
val root : t -> Instruction_content.Root_id.t option
val scope : t -> Instruction_content.Scope.t
val loaded : t -> Instruction_content.Loaded.t list
val file_key : Instruction_content.Id.t -> string
val payloads : t -> Agent_store.Prompt_program_bundle.Payload.t list
val to_json : t -> Jsonaf.t

(** Restore only from the supplied exact immutable pool; never reads files.
    The caller bounds aggregate work through [checkpoint]. *)
val of_json
  :  Jsonaf.t
  -> pool:Agent_store.Prompt_program_bundle.Payload.t String.Map.t
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

val shape : Document_schema.Shape.t
