open! Core
module P = Agent_protocol

(** Canonical, bounded original developer command before source capture.
    A matching historical key can be compared without a live source root,
    editor connection, provider, Root or current captured code. This value
    is data, not source-read or execution authority. *)
type t

module Route : sig
  type t =
    | Local_project of
        { root_index : int
        ; dependency_root_indices : int list
        ; project : Source.Portable_path.t
        ; selection : Chatml_project.Selection.request
        }
    | Local_source of
        { root_index : int
        ; member : Source.Portable_path.t
        }
    | Authoring_project of { expected : P.Authoring.Context.Expected.t }
    | Document_script of
        { expected : P.Authoring.Context.Expected.t
        ; selector : P.Authoring.Analysis.Target.selector
        }

  (** Indices and expected editor revisions are stable requested selectors.
      None is a physical source grant. Original construction validates their
      finite representation; fresh capture separately checks actual issuance. *)
end

module Fresh_source : sig
  type t =
    | Local_project of
        { root : Authoring_source_host.Root.t
        ; dependency_roots : Authoring_source_host.Root.t list
        }
    | Local_source of { root : Authoring_source_host.Root.t }
    | Authoring_project of { connection : Connection_context.t }
    | Document_script of { connection : Connection_context.t }

  (** Physical current source/connection only for an authoritative key miss.
      Its kind, configured root indices, expected context and issuer must
      match [Route.t] before the source owner reads/captures anything. The host may
      omit it entirely for a historical same-request retry. *)
end

(** One versioned canonical digest over command kind and explicit compiler ABI
    target, stable route/selector,
    explicit requested environment/editor revision, entrypoint, bounded JSON
    input/fixtures and requested budget/output options. A Live command requires
    explicit configured Prompt and Workspace definition IDs, both included in
    this digest; pure Check/Fixture require both absent. The IDs are requested
    coordinates, not grants. Fresh Live admission must select a current Ready
    Prompt revision, authorize its configured Workspace pair and actor, and
    resolve that actual Workspace_instance. Historical retry comparison needs
    neither catalog nor resolution. Validation rejects
    duplicate JSON keys, invalid/nonfinite limits and out-of-range selectors
    before hashing; absent option fields have distinct canonical tags.
    Resolved source bytes, capture fingerprint, actor token and mutable
    host-selected policy are excluded. No caller-supplied digest is accepted.
    The same immutable value supplies fresh source preparation inputs. *)
val create
  :  route:Route.t
  -> kind:Agent_store.Developer_command_receipt.Kind.t
  -> target:Chatml.Chatml_compiler_contract.Target.t
  -> prompt:P.Id.Prompt_definition.t option
  -> workspace:P.Id.Workspace_definition.t option
  -> entrypoint:string
  -> input:Jsonaf.t
  -> fixtures:Chatml_developer_capture.Fixture.t option
  -> requested_execution:Chatml_execution.limits option
  -> requested_max_output_bytes:int option
  -> bounds:Chatml_developer_capture.Json_bounds.t
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) result

val kind : t -> Agent_store.Developer_command_receipt.Kind.t
val target : t -> Chatml.Chatml_compiler_contract.Target.t
val route : t -> Route.t
val prompt : t -> P.Id.Prompt_definition.t option
val workspace : t -> P.Id.Workspace_definition.t option
val requested_execution : t -> Chatml_execution.limits option
val requested_max_output_bytes : t -> int option

(** Comparison data for the Store's existing request_fingerprint field;
    this is neither an authority token nor the resolved capture digest. *)
val fingerprint : t -> Chatml_source.Digest.t

(** Host-private projection of the exact immutable input/fixtures from which
    [fingerprint] was made. Consumers may not replace these with new payload. *)
val capture_request
  :  t
  -> (Authoring_context_service.Developer.Request.t, P.Error.t) result

module Identity : sig
  (** Shared bounded canonical representation for the same stable source route
      and requested execution limits. REPL create uses a distinct action/version
      envelope around these values; it never creates a dummy command kind. *)
  val valid_route : Route.t -> bool

  val route_json : Route.t -> Jsonaf.t
  val execution_json : Chatml_execution.limits -> Jsonaf.t
  val target_name : Chatml.Chatml_compiler_contract.Target.t -> string
  val digest_json : Jsonaf.t -> checkpoint:(unit -> unit) -> Chatml_source.Digest.t
end
