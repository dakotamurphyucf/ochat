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

module Limits : sig
  type t =
    { max_connections : int
    ; max_active_requests_per_connection : int
    ; max_roots_per_connection : int
    ; max_response_bytes : int
    }

  val create
    :  max_connections:int
    -> max_active_requests_per_connection:int
    -> max_roots_per_connection:int
    -> max_response_bytes:int
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
  type t = { authorize : Operator_authorization.t -> bool }

  val create : authorize:(Operator_authorization.t -> bool) -> t
end

module Selected_feature : sig
  type t =
    | Core_authoring
    | Bundle_upload
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
  ; protocol_limits : P.Authoring.Limits.t
  ; limits : Limits.t
  ; gate : Eio.Mutex.t
  ; mutable connections : connection_entry list
  ; mutable project_cache : Project.Project_cache.t
  ; mutable document_cache : Document.Document_cache.t
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
    }

  val basis : t -> basis
  val expected : t -> P.Authoring.Context.Expected.t
  val project_inputs : t -> checkpoint:(unit -> unit) -> (Ledger.Inputs.t, Error.t) result

  val document_inputs
    :  t
    -> checkpoint:(unit -> unit)
    -> (Ledger.Document.Inputs.t, Error.t) result
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
