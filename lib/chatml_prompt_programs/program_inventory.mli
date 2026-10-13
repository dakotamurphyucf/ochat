open Core
module Captured = Chatml_source
module Revision = Program_source_revision
module Binding = Chatml_prompt_binding
module Storage = Agent_store.Prompt_program_bundle

module Input : sig
  type t =
    | Provided of Chatml_prepared_program.t
    | Selected of Chatml_project_build.t
end

module Limits : sig
  type t

  val create
    :  max_programs:int
    -> max_parser_inputs:int
    -> max_logical_sources:int
    -> max_total_input_bytes:int
    -> project_work_steps:int
    -> document:Document_schema.Limits.t
    -> t Or_error.t

  val policy_identity : t -> Sexp.t
  val max_programs : t -> int
  val max_parser_inputs : t -> int
  val max_logical_sources : t -> int
  val max_total_input_bytes : t -> int
  val project_work_steps : t -> int
  val document : t -> Document_schema.Limits.t
end

module Error : sig
  type t =
    | Document of Document_schema.Error.t
    | Storage of Agent_store.Store_error.t
    | Provided of Chatml_program_document.Error.t
    | Selected of Chatml_project_build.Error.t
    | Invalid_binding of string
    | Duplicate_program of Binding.t * Captured.Digest.t
    | Conflicting_source of Captured.Id.t
    | Missing_contract of Binding.t
    | Unexpected_contract of Binding.t
    | Contract_mismatch of Binding.t
    | Script_mismatch of Binding.t
    | Invalid_inventory of string
    | Limit_exceeded
end

module Program : sig
  type kind =
    | Provided of Chatml_program_document.t
    | Selected of Revision.t

  type t =
    { key : string
    ; binding : Binding.t
    ; compiler_contract : Captured.Digest.t
    ; fingerprint : Captured.Digest.t
    ; input_bytes : int
    ; sources : Revision.t list
    ; kind : kind
    }

  val key : Binding.t -> Captured.Digest.t -> string
end

module Selection : sig
  type t =
    { program : string
    ; binding : Binding.t
    }
end

(** Bounded immutable inventory. Source bytes are retained once per ID/revision,
    parser inputs once per role/path revision, and physical bodies once per hash. *)
type t =
  { programs : Program.t list
  ; inputs : Chatmd_parser_input.t list
  ; sources : Captured.t list
  ; selections : Selection.t list
  ; root : Chatmd_parser_input.t
  }

(** Count and byte preflight before projection/maps. Source equality is byte exact,
    not merely equal hash. Authoring performs no source loading or preparation. *)
val capture
  :  Chatmd_source_bundle.t
  -> parsed_contexts:Prompt.Chat_markdown.Parsed.t list
  -> root:Chatmd_parser_input.t
  -> programs:(Binding.t * Input.t) list
  -> active_programs:(Binding.t * Captured.Digest.t) list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Validate all metadata references, source/payload/parser membership, duplicate
    identities and known descriptor assertions. No inactive graph is executable.
    Inventory walks and body comparisons poll; identity comparison operates on
    labels preadmitted by the bounded metadata schema. Failure publishes no partial inventory. *)
val validate
  :  t
  -> authored:Chatmd_source_bundle.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

val payloads
  :  t
  -> authored:Chatmd_source_bundle.t
  -> checkpoint:(unit -> unit)
  -> (Storage.Payload.t list, Error.t) Result.t

val find_program : t -> string -> checkpoint:(unit -> unit) -> Program.t option
val find_source : t -> Revision.t -> checkpoint:(unit -> unit) -> Captured.t option
val bytes_equal : string -> string -> checkpoint:(unit -> unit) -> bool

val admit_sources
  :  Captured.t list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Captured.t list, Error.t) Result.t

val admit_inputs
  :  Chatmd_parser_input.t list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Chatmd_parser_input.t list, Error.t) Result.t

val admit_authored
  :  Chatmd_source_bundle.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

val count : 'a list -> int -> checkpoint:(unit -> unit) -> (unit, Error.t) Result.t

(** Individual map/set identity comparisons are bounded by the admitted262144-byte
    document ceiling; portable paths are at most1024 bytes and digests64 bytes. *)

val iter_result
  :  'a list
  -> f:('a -> (unit, Error.t) Result.t)
  -> (unit, Error.t) Result.t

val same_source : Captured.t -> Captured.t -> checkpoint:(unit -> unit) -> bool

val input_equal_exact
  :  Chatmd_parser_input.t
  -> Chatmd_parser_input.t
  -> checkpoint:(unit -> unit)
  -> bool
