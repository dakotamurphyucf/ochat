open! Core
module P = Agent_protocol
module W = P.Authoring.Wire
module Ledger = Chatml_authoring_context
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Sources = Authoring_source_host
module Upload = Authoring_upload_host
module Captured = Chatml_source
module Build = Chatml_project_build
module Activity = Authoring_context_activity
module Selection_rows = Authoring_document_selection_rows

module Limits : sig
  type t =
    { max_connections : int
    ; max_active_requests_per_connection : int
    ; max_roots_per_connection : int
    ; max_response_bytes : int
    ; max_provisional_results : int
    }

  val create
    :  max_connections:int
    -> max_active_requests_per_connection:int
    -> max_roots_per_connection:int
    -> max_response_bytes:int
    -> max_provisional_results:int
    -> t Or_error.t
end

module Selection_policy : sig
  type t =
    { project_contract :
        Operator_authorization.t
        -> root:Sources.Root.t
        -> project:Source.Portable_path.t
        -> selection:P.Authoring.Context.Selection.t
        -> Chatml.Chatml_compiler_contract.t option
    ; document_static :
        Operator_authorization.t -> requested_environment:string option -> Build.t option
    }

  val create
    :  project_contract:
         (Operator_authorization.t
          -> root:Sources.Root.t
          -> project:Source.Portable_path.t
          -> selection:P.Authoring.Context.Selection.t
          -> Chatml.Chatml_compiler_contract.t option)
    -> document_static:
         (Operator_authorization.t
          -> requested_environment:string option
          -> Build.t option)
    -> t
end

module Supplied_document_policy : sig
  module Identity : sig
    type t

    (** Physical policy incarnation, never derived from a context label or
        caller-supplied document. It is comparison data, not a read grant. *)
    val equal : t -> t -> bool
  end

  type t

  val create : authorize:(Operator_authorization.t -> bool) -> t
  val identity : t -> Identity.t
  val read_admitted : t -> Operator_authorization.t -> bool
end

module Selected_feature : sig
  type t =
    | Core_authoring
    | Bundle_upload
    | Semantic
    | Lexical
    | Assistance
end

module Error : sig
  type t =
    | Uninstalled
    | Unselected_feature
    | Unauthorized
    | Expired_actor
    | Foreign_connection
    | Stale_negotiation
    | Stale_context
    | Stale_source
    | Source_unavailable
    | Selection_unavailable
    | Busy
    | Closed
    | Resource_limit
    | Invalid_position of P.Authoring.Failure.invalid_position
    | Invalid_request
    | Analysis_unavailable
    | Checked_template_failed
    | Storage_unavailable
    | Upload_failure of Upload.Error.t

  val to_protocol : t -> P.Error.t
end

type phase = Activity.phase =
  | Active
  | Revoking
  | Closed

type context_owner = Activity.context_owner =
  | Host_project of Sources.Context.t
  | Host_document of Sources.Document_context.t
  | Uploaded_project of Upload.Context_capture.t
  | Supplied_document of Operator_authorization.t

type context_entry = Activity.context_entry
type connection_entry = Activity.connection_entry

(** One latest genuine parsed import closure retained in this same owner.
    Admission checks source identity/digest/length and bounded file/byte work
    outside the gate. The capture owns no parser loader, source owner or grant.
    It also retains immutable imported bytes for a later in-memory Prefix
    reparse after a root edit, never an old Parsed value as current facts.
    One bounded immutable set of per-declaration selection rows belongs to
    this same head/generation; no second mutable selection registry exists. *)
module Retained_document_capture : sig
  type t

  val prepare
    :  context:context_entry
    -> inputs:Ledger.Document.Inputs.t
    -> capture:Sources.Document_context.Parsed_capture.t
    -> rows:Selection_rows.t
    -> max_sources:int
    -> max_bytes:int
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) result

  val matches_current
    :  t
    -> context:context_entry
    -> inputs:Ledger.Document.Inputs.t
    -> bool

  val matches_import_owner
    :  t
    -> context:context_entry
    -> inputs:Ledger.Document.Inputs.t
    -> bool

  val capture : t -> Sources.Document_context.Parsed_capture.t

  (** Complete bounded per-declaration evidence for this exact parser receipt.
      The selected target, metadata and Build in each row remain comparison
      data; serving them still requires current host policy and script checks. *)
  val selection_rows : t -> Selection_rows.t

  (** Physical publication-head equality, including [None]. Equal bytes in a
      later admission do not make an earlier borrowed head current again. *)
  val same_head : t option -> t option -> bool

  (** Rebuild only the root member from current unsaved bytes, preserving the
      first captured import bytes. No native read, parser fact or source grant
      is generated. Newly required imports remain absent and parser rejection
      becomes typed Unavailable at the service boundary. *)
  val recompose_bundle
    :  t
    -> inputs:Ledger.Document.Inputs.t
    -> checkpoint:(unit -> unit)
    -> (Chatmd_source_bundle.t, Error.t) result
end

(** This private record is the sole mutable connection/context/cache owner.
    Methods borrow it; only the final pure gate transition changes published
    ledger and caches. Host callbacks and source/Store work occur outside it. *)
type t =
  { sw : Eio.Switch.t
  ; domain : int
  ; clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; host : P.Id.Server.t
  ; sources : Sources.t option
  ; upload : Upload.t option
  ; supplied_document : Supplied_document_policy.t option
  ; selection_policy : Selection_policy.t
  ; analysis_policy : Project.Policy.t
  ; protocol_limits : P.Authoring.Limits.t
  ; limits : Limits.t
  ; gate : Eio.Mutex.t
  ; mutable connections : connection_entry list
  ; mutable project_cache : Project.Project_cache.t
  ; mutable document_cache : Document.Document_cache.t
  ; mutable document_capture : Retained_document_capture.t option
  ; mutable analysis_index : Authoring_analysis_index.t
  ; mutable provisional_index : Authoring_provisional_index.t
  ; mutable checked_templates : Authoring_checked_template_cache.t
  ; mutable closed : bool
  ; mutable release_hook : Eio.Switch.hook
  }

val with_gate : t -> (unit -> 'a) -> 'a
val check_domain : t -> (unit, Error.t) result

(** Nonyielding try-lock. Callback is pure gate-owned CAS only; rejection
    leaves the request registered so its candidate can be closed before join. *)
val try_with_gate : t -> (unit -> ('a, Error.t) result) -> ('a, Error.t) result

val find_connection : t -> Connection_context.t -> connection_entry option
val find_context : connection_entry -> P.Id.Authoring_context.t -> context_entry option
val check_entry : t -> connection_entry -> (unit, Error.t) result

val check_actor
  :  Connection_context.t
  -> Operator_authorization.t
  -> (unit, Error.t) result

val check_actor_context
  :  Operator_authorization.t
  -> Operator_authorization.t
  -> (unit, Error.t) result

(* [feature] selects one of the two installed authoring feature names; the
   default keeps existing core-method callers unchanged. The same physical
   negotiation lease and joined activity owner apply to both choices. *)

(** Own one registered request through all yielding work and rollback. After
    [final_check], the no-yield gate callback repeats phase/lease and executes
    [commit], then unregisters atomically. [abort] runs under cancellation
    protection before a rejected candidate is unregistered. [final_check] may
    invoke bounded, nonyielding trusted current-policy callbacks. [commit] is
    pure gate-owned CAS: no policy callback, I/O, or Eio wait is permitted.
    [after_commit], when provided, joins committed cleanup while this request
    remains active; a failed cleanup retains the retired owner for close retry. *)
val with_request
  :  ?feature:Selected_feature.t
  -> ?final_check:(unit -> (unit, Error.t) result)
  -> ?commit:('a -> ('a, Error.t) result)
  -> ?after_commit:(unit -> unit)
  -> ?abort:(unit -> unit)
  -> t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> checkpoint:(unit -> unit)
  -> (connection_entry -> checkpoint:(unit -> unit) -> ('a, Error.t) result)
  -> ('a, Error.t) result

val transition : t -> Ledger.t -> unit

module Current : sig
  type basis =
    | Project
    | Document

  type t =
    { owner : Ledger.t
    ; domain : int
    ; context : context_entry
    ; expected : P.Authoring.Context.Expected.t
    ; live : bool ref
    ; retained_document_capture : Retained_document_capture.t option
    ; borrowed_project_cache : Project.Project_cache.t
    ; borrowed_document_cache : Document.Document_cache.t
    ; borrowed_analysis_index : Authoring_analysis_index.t
    ; borrowed_provisional_index : Authoring_provisional_index.t
    ; borrowed_checked_templates : Authoring_checked_template_cache.t
    }

  val basis : t -> basis
  val expected : t -> P.Authoring.Context.Expected.t
  val analysis_index : t -> (Authoring_analysis_index.t, Error.t) result
  val provisional_index : t -> (Authoring_provisional_index.t, Error.t) result
  val checked_templates : t -> (Authoring_checked_template_cache.t, Error.t) result

  (** Exactly the immutable heads captured with [owner] for this live request.
      Candidate work uses this coherent tuple; final Owner CAS rejects any
      intervening replacement before the pure publication callback. *)
  val analysis_heads
    :  t
    -> ( Project.Project_cache.t * Document.Document_cache.t * Authoring_analysis_index.t
         , Error.t )
         result

  val project_inputs : t -> checkpoint:(unit -> unit) -> (Ledger.Inputs.t, Error.t) result

  val document_inputs
    :  t
    -> checkpoint:(unit -> unit)
    -> (Ledger.Document.Inputs.t, Error.t) result

  (** The genuine capture borrowed at this request's admission for the exact
      Document identity, generation and root source ID/digest/length. [None]
      means none was admitted. It never triggers an import read or parser run.
      A concurrent request may evict this head while work runs; the final pure
      publication must compare the borrowed head with the CURRENT Core head
      under the same gate as both analysis cache heads and context generation,
      rejecting a changed head. An edit hides old Parsed facts; refresh, close
      or owner replacement also retire its import bytes. Imported source IDs
      come only from genuine [input_sources] pairs. The token must be live. *)
  val document_capture
    :  t
    -> checkpoint:(unit -> unit)
    -> (Sources.Document_context.Parsed_capture.t option, Error.t) result

  (** Borrow only the row for this genuine declaration from the matching
      current Parsed head. Missing/evicted evidence requires explicit Analyze;
      this lookup performs no import/project read or Build composition. The
      Current token must still be live before and after its checkpoint. *)
  val selection_row
    :  t
    -> declaration:Prompt.Chat_markdown.Declaration.t
    -> checkpoint:(unit -> unit)
    -> (Selection_rows.Row.t option, Error.t) result

  (** A bundle recomposed solely from current unsaved root bytes and the last
      immutable imports of this SAME context/source owner borrowed at request
      admission. It never returns an old Parsed value or reads changed
      dependency files. A new/missing import is unavailable until explicit
      Analyze/rebuild. Owner replacement, refresh, close or eviction at
      admission returns [None]; a later eviction is rejected by final CAS. *)
  val frozen_document_bundle
    :  t
    -> checkpoint:(unit -> unit)
    -> (Chatmd_source_bundle.t option, Error.t) result
end

val close_owner : context_owner -> unit
val close_connection_and_wait : t -> Connection_context.t -> (unit, Error.t) result
val close_and_wait : t -> (unit, Error.t) result

val create
  :  sw:Eio.Switch.t
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> host:P.Id.Server.t
  -> sources:Sources.t option
  -> upload:Upload.t option
  -> supplied_document:Supplied_document_policy.t option
  -> selection_policy:Selection_policy.t
  -> context_limits:Ledger.Limits.t
  -> analysis_policy:Project.Policy.t
  -> protocol_limits:P.Authoring.Limits.t
  -> limits:Limits.t
  -> t

val publish_initialize_and_wait
  :  t
  -> connection:Connection_context.t
  -> response:P.Initialize.Response.t
  -> (unit, Error.t) result
