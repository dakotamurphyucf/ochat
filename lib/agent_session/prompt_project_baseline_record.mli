open Core
module Custody = Chatml_prompt_programs
module Plan = Chat_response.Script_contract_plan
module Slot = Custody.Binding.Slot
module Role = Plan.Program.Role

module Limits : sig
  type t

  (** Positive complete metadata/work/identity byte bounds; nonnegative retained
      definition/reference counts. No default or eviction. Counts are checked
      before projections/maps; bytes include unknown fields and JSON envelope.
      Work is one cooperative allowance per operation, including nested custody
      projection, comparisons and encoding polls. It is not CPU accounting. *)
  val max_definitions : t -> int

  val max_references : t -> int
  val max_identity_bytes : t -> int
  val max_work_steps : t -> int
  val document : t -> Document_schema.Limits.t

  val create
    :  max_definitions:int
    -> max_references:int
    -> max_identity_bytes:int
    -> max_work_steps:int
    -> document:Document_schema.Limits.t
    -> t Or_error.t
end

module Error : sig
  type t =
    | Limit_exceeded
    | Invalid_association of string
    | Ambiguous_association
    | Foreign_artifact
    | Foreign_operator_intent
    | Missing_program_bundle
    | Document of Document_schema.Error.t
    | Custody of Custody.Error.t
    | Retained of Custody.Retained.Error.t
end

module Work : sig
  type t

  val poll : t -> unit
  val remaining : t -> int
  val string_equal : t -> string -> string -> bool

  val run
    :  Limits.t
    -> checkpoint:(unit -> unit)
    -> f:(t -> ('a, Error.t) Result.t)
    -> ('a, Error.t) Result.t
end

module Prepared : sig
  (** Transient authentic role/program association from the SAME actual captured
      parser contexts, plans and returned immutable custody. No native owner,
      source loader, callback or compiler/runtime state. *)
  type t

  (** Bound supplied counts before traversal/copy; verify each Plan owns an actual
      root/queued parser context and each exact binding+contract resolves in the
      admitted retained inventory. Deduplicate repeated identical requests;
      reject a stable slot/role mapped to different exact keys. Preserve all
      genuinely requested roles, including inactive queued contexts. Old unrelated
      custody rows are not assigned fabricated roles. *)
  val create
    :  Custody.t
    -> capture:Prompt_revision_builder.Capture.t
    -> plans:Plan.t list
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val custody : t -> Custody.t
end

module Slot_assertion : sig
  type t

  val of_slot : Slot.t -> t

  val create
    :  registration_path:Source.Portable_path.t
    -> declaration_role:Custody.Parser_input.Role.t
    -> declaration_path:Source.Portable_path.t
    -> script_id:string
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val registration_path : t -> Source.Portable_path.t
  val declaration_role : t -> Custody.Parser_input.Role.t
  val declaration_path : t -> Source.Portable_path.t
  val script_id : t -> string
end

module Reference : sig
  type t

  val create
    :  slot:Slot_assertion.t
    -> role:Role.t
    -> program_key:string
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val association_key : t -> string
  val slot : t -> Slot_assertion.t
  val role : t -> Role.t
  val program_key : t -> string
end

module Entry : sig
  (** Durable reference only: exact definition, original Source_receipt,
      last-successful revision/manifest and stable role -> exact program key.
      It contains neither lock bodies nor live/native identities. Decoded entries
      are structural references until reconciled with their exact artifact. *)
  type t

  val definition_id : t -> Agent_protocol.Id.Prompt_definition.t
  val source_receipt : t -> Agent_store.Prompt_source_receipt.t
  val revision_id : t -> Agent_protocol.Id.Prompt_revision.t
  val manifest_sha256 : t -> string
  val references : t -> Reference.t list

  (** Private wire constructor validates complete claims but proves neither
      artifact presence nor successful preparation. Public success needs finalize. *)
  val of_fields
    :  definition_id:Agent_protocol.Id.Prompt_definition.t
    -> source_receipt:Agent_store.Prompt_source_receipt.t
    -> revision_id:Agent_protocol.Id.Prompt_revision.t
    -> manifest_sha256:string
    -> references:Reference.t list
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  module Prior : sig
    type t

    type selection =
      | New_slot
      | Provided
      | Selected of Chatml_source.t

    (** A missing slot/role in an otherwise reconciled authoritative baseline is
        New_slot, not unknown history or permission to use Initial. Provided is
        a known prior record, not a missing lock. The host's existing pure build
        gate decides Local-only no-prior versus explicit Git update. *)
    val find
      :  t
      -> slot:Slot.t
      -> role:Role.t
      -> limits:Limits.t
      -> checkpoint:(unit -> unit)
      -> (selection, Error.t) Result.t
  end

  (** Caller supplies the exact artifact loaded by Prompt_artifact_store.load.
      Independently require its revision, original manifest admission digest,
      definition and source receipt to match this entry. Admit the present program
      bundle structurally against its exact authored inventory and resolve every
      role reference to its exact original program key/slot. The persisted index
      is the historical role-to-key authority, constructed from authentic plans
      on success; reconciliation does not infer that role from an old compiler
      contract. A recomputed coherent role association rewrites that authority,
      not an independently authenticated role proof. No graph restore,
      current compiler contract, initializer or source IO. A corrupt/missing
      artifact is an error, never New_slot. Existing old graphs need not compile. *)
  val reconcile
    :  t
    -> artifact:Agent_store.Prompt_artifact_store.Artifact.t
    -> custody_limits:Custody.Limits.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (Prior.t, Error.t) Result.t
end

module Success : sig
  (** Only a finalized actual successful revision can create a replacement token;
      structurally decoded historical Entry values cannot claim new success.
      This is semantic finalization, not artifact durability; the host installs
      the exact artifact before publishing its baseline and protects that artifact
      from pruning. *)
  type t

  val entry : t -> Entry.t

  (** Require definition and original receipt, exact same complete custody storage
      manifest/payloads, authentic Root/raw inventory and prepared references.
      Zero-program parser custody is valid. No timestamp-based selection. Retain
      immutable references, not Prepared/Parsed objects or live authority. *)
  val finalize
    :  Prepared.t
    -> revision:Prompt_revision.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end
