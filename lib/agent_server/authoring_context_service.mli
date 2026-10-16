(* Joined owner for source/context and bundle authoring handlers. *)

open! Core
module P = Agent_protocol
module W = P.Authoring.Wire
module Ledger = Chatml_authoring_context
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Sources = Authoring_source_host
module Upload = Authoring_upload_host

module Limits : sig
  type t

  (** Positive bounded host ceilings. A live connection entry is charged even
      if it has no open source context; close/reinitialize releases it after
      joined work. [max_active_requests] bounds all core and later borrowed
      semantic/assistance work per connection, while source-host/project build
      and protocol per-request caps continue to apply. [max_response_bytes]
      bounds an individual constructed wire response, not caller retention
      after it is returned. [max_provisional_results] separately caps the
      latest-per-context provisional prefix snapshots; checked caches remain
      under the immutable analysis policy's distinct [max_results] cap. Each
      provisional snapshot uses the same compiler input/fact/edge ceilings,
      and no client label controls retention. [max_roots_per_connection] bounds the map of
      connection-local root labels. These limits cannot enlarge Ledger,
      Source_host, Upload_host or Initialize limits. *)
  val create
    :  max_connections:int
    -> max_active_requests_per_connection:int
    -> max_roots_per_connection:int
    -> max_response_bytes:int
    -> max_provisional_results:int
    -> t Or_error.t
end

module Selection_policy : sig
  type t

  (** Trusted current host selection, not a client or on-disk resolver. The
      callback must be bounded/non-yielding and read already admitted host
      catalog/contract state. It returns the actual compiler contract for the
      exact requested project target/environment. The source host separately
      checks root grants and real project declaration/Build selection. On
      later requests the service repeats selection and compares the admitted
      contract identity; a retained context or digest is not authority. *)
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
          -> Chatml_project_build.t option)
    -> t

  (** Reuse an existing policy's document-static callback while replacing only
      its Project contract lookup with the immutable configured host binding.
      The returned callback remains bounded/nonyielding; neither this method
      nor a returned contract supplies source authority. *)
  val with_project_contract
    :  t
    -> project_contract:
         (Operator_authorization.t
          -> root:Sources.Root.t
          -> project:Source.Portable_path.t
          -> selection:P.Authoring.Context.Selection.t
          -> Chatml.Chatml_compiler_contract.t option)
    -> t
end

module Supplied_document_policy : sig
  type t

  (** Independent host/operator grant for caller-supplied exact bytes. It
      conveys no native source root or project authority. The callback is a
      bounded non-yielding read of current host policy and is checked for the
      original and request actors on EVERY operation. The service uses the
      existing exact Chatml_source constructor and Ledger limits; it never
      reparses, normalizes UTF-8 or writes a host file. [None] at create means
      this origin is uninstalled even if an upload owner exists. *)
  val create : authorize:(Operator_authorization.t -> bool) -> t
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

  (** Finite/path-free translation to the existing protocol code + authoring
      Failure reason where one exists. No source, native path, environment
      details, host policy or exception text enter the wire error. *)
  val to_protocol : t -> P.Error.t
end

type t
type owner = t

(** Construct one domain-confined service owner from EXISTING host owners.
    Operations on a different OCaml domain fail closed before touching the
    owner gate. Borrowed Current data accessors also reject use on another domain.
    [sources] and [upload] are borrowed and must outlive this service; this
    service closes only its own contexts/activity, never opens an extra root,
    durable store or source resolver. Availability is per origin:
    Host_project/Host_document/Roots/Host_list/Host_read need [sources];
    Uploaded_project needs [upload] and its independent current bundle-read
    grant; Supplied_document needs [supplied_document], not [sources]. Once
    admitted, Context/Source/Document operations work for any basis even when
    [sources=None]. Unsupported origins return typed Uninstalled or
    Selection_unavailable without pretending that a client ID grants custody.
    The caller supplies the real daemon server ID,
    one immutable analysis Policy instance, and same finite protocol limits
    advertised to clients. *)
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

(** The only mutable context ledger is [Project_cache.contexts]. The owner
    atomically applies every authorized Ledger transition to Project_cache
    and Document_cache under the SAME short host gate. Failed/cancelled
    preparation publishes neither a new context nor a new cache. Context_close
    commits revocation before joining resource cleanup; a later failure does
    not restore the closed context, and unfinished cleanup remains owned by
    the connection. No Eio wait, source read,
    parser, checker, callback or response encoding occurs under that gate. *)

module Current : sig
  type t

  type basis =
    | Project
    | Document

  val basis : t -> basis
  val expected : t -> P.Authoring.Context.Expected.t

  (** Borrowed request-scope exact inputs. Project and Document accessors
      reject the other basis. They check the live token and charge the one
      request work/byte policy before returning data. The returned immutable
      bytes/Build are comparison data, never a continuing source grant. *)
  val project_inputs : t -> checkpoint:(unit -> unit) -> (Ledger.Inputs.t, Error.t) result

  val document_inputs
    :  t
    -> checkpoint:(unit -> unit)
    -> (Ledger.Document.Inputs.t, Error.t) result
end

(** The selected semantic sibling borrows this same context owner and its
    joined request lifetime. Its Analyze result is published through the sole
    cache/capture/index gate; clients still select core to create/edit the
    context and semantic for this method. *)
module Semantic : sig
  module Script_selection : sig
    module Limits : sig
      type t

      (** Bounded canonical contract identity admission. The byte cap covers
          encoded identity allocation and the work cap covers conversion/hash
          checkpoints. Neither cap enlarges the actual host Build limits. *)
      val create : max_identity_bytes:int -> max_work_steps:int -> t Or_error.t
    end

    module Error : sig
      type t =
        | Identity_limit
        | Work_limit
        | Contract_mismatch

      val sexp_of_t : t -> Sexp.t
    end

    module Selected : sig
      type t

      (** Admit the actual host-selected Build and contract before the final
          nonyielding policy check. This validates Prepared's required and
          selected contract identities under both byte and work limits;
          equal digests alone never authorize a project route or source read.
          Checkpoint exceptions and cancellation preserve their identity. *)
      val admit
        :  build:Chatml_project_build.t
        -> contract:Chatml.Chatml_compiler_contract.t
        -> limits:Limits.t
        -> checkpoint:(unit -> unit)
        -> (t, Error.t) result

      val prepared_fingerprint : t -> Chatml_source.Digest.t
    end

    module Policy : sig
      type t

      (** One bounded, nonyielding lookup of already admitted host state for
          this exact current Parsed declaration. The callback must recheck
          BOTH original and request actors and the current selection. Optional
          project metadata is comparison evidence, never a source/root grant;
          an independently authorized static/upload selection remains valid
          without native metadata. It must not read, parse, compose or wait. *)
      val create
        :  select_script:
             (original_actor:Operator_authorization.t
              -> actor:Operator_authorization.t
              -> current:Ledger.Document.Inputs.t
              -> parsed:Prompt.Chat_markdown.Parsed.t
              -> declaration:Prompt.Chat_markdown.Declaration.t
              -> metadata:Sources.Document_context.Project_capture.t option
              -> Selected.t option)
        -> t
    end
  end

  module Project_metadata : sig
    module Policy : sig
      type t

      (** Bounded, nonyielding current host route check for genuine captured
          project metadata. Recheck original/request actors; equal source
          digest or path does not grant native custody. The returned Boolean
          admits comparison data only, not a Build or read authority. *)
      val create
        :  authorize_now:
             (original_actor:Operator_authorization.t
              -> actor:Operator_authorization.t
              -> current:Ledger.Document.Inputs.t
              -> parsed:Prompt.Chat_markdown.Parsed.t
              -> declaration:Prompt.Chat_markdown.Declaration.t
              -> capture:Sources.Document_context.Project_capture.t
              -> bool)
        -> t
    end
  end

  module Limits : sig
    type t

    (** Bundle/parser and response limits remain independent. Selection limits
        admit one complete finite set of genuine declarations on explicit
        Analyze/rebuild: row count includes metadata-only/unavailable rows;
        source and canonical-input bytes measure retained representation, not
        exact heap or failed native reads. Each lower Project attempt retains
        its own configured source/file/work caps, and collection shares one
        checkpoint allowance across all rows. Prefix/Query/Assistance never
        acquire new project/import bytes. *)
    val create
      :  max_document_bundle_files:int
      -> max_document_bundle_bytes:int
      -> max_parser_work_steps:int
      -> max_diagnostic_work_steps:int
      -> max_selection_rows:int
      -> max_retained_selection_source_bytes:int
      -> max_retained_selection_input_bytes:int
      -> max_selection_work_steps:int
      -> t Or_error.t
  end

  module Prefix_limits : sig
    type t

    (** Bounded frozen-bundle parse and host-request work. The joined prefix
        checker uses the existing immutable Core analysis policy's separate
        compiler/conversion/semantic budget and selected contract. *)
    val create
      :  max_document_bundle_files:int
      -> max_document_bundle_bytes:int
      -> max_parser_work_steps:int
      -> max_host_work_steps:int
      -> t Or_error.t
  end

  module Query_limits : sig
    type t

    (** Host-owned source/selection admission work plus the existing lower
        bounded query producer. The request cannot enlarge either allowance. *)
    val create
      :  max_host_work_steps:int
      -> projection:Chatml_authoring_query.Limits.t
      -> t Or_error.t
  end

  type t

  val create
    :  owner:owner
    -> env:Eio_unix.Stdenv.base
    -> document_dir:Eio.Fs.dir_ty Eio.Path.t
    -> script_selection:Script_selection.Policy.t option
    -> project_metadata:Project_metadata.Policy.t option
    -> project_build_limits:Chatml_project_build.Limits.t
    -> limits:Limits.t
    -> prefix_limits:Prefix_limits.t
    -> query_limits:Query_limits.t
    -> t

  val analyze
    :  t
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> P.Authoring.Analysis.Analyze.Request.t
    -> (P.Authoring.Analysis.Analyze.t, Error.t) result

  val prefix
    :  t
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> P.Authoring.Analysis.Prefix.Request.t
    -> (P.Authoring.Analysis.Prefix.t, Error.t) result

  (** Current certified semantic facts, same-run failed syntax, or the exact
      published provisional prefix. Context lifecycle still requires core;
      this method checks its own negotiated semantic feature. *)
  val query
    :  t
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> P.Authoring.Analysis.Query.Request.t
    -> (P.Authoring.Analysis.Query.t, Error.t) result
end

(** Lexical and early assistance borrow the same current context and joined
    request lifetime. Creating or editing that context still requires the core
    feature; each method separately requires its own negotiated feature.
    Parsed imports and project metadata become available only after an explicit
    Analyze capture for the current document generation. *)
module Assistance : sig
  type t

  module Limits : sig
    type t

    (** Finite per-request scan, projection, capture and response ceilings.
        The protocol limits must equal the owner's initialized limits. *)
    val create
      :  protocol_limits:P.Authoring.Limits.t
      -> max_scan_work_steps:int
      -> max_assistance_work_steps:int
      -> max_contract_identity_bytes:int
      -> max_contract_work_steps:int
      -> max_projection_work_steps:int
      -> max_parser_input_bytes:int
      -> max_imported_inputs:int
      -> max_selected_tools:int
      -> max_tool_metadata_bytes:int
      -> t Or_error.t
  end

  module Selected_tool_policy : sig
    module Registry : sig
      type t

      (** Admit only the host's actual selected immutable registry under the
          same finite assistance limits. This conveys comparison data, not
          permission to run tools. *)
      val admit
        :  Chat_response.Tool_capability.t
        -> limits:Limits.t
        -> checkpoint:(unit -> unit)
        -> (t, Error.t) result

      val selected : t -> Chat_response.Tool_capability.t
      val equal_identity : t -> t -> bool
    end

    type t

    (** This bounded nonyielding host callback rechecks both actors against
        the current selected registry. The context is a comparison view, not
        a grant. *)
    val create
      :  current:
           (original_actor:Operator_authorization.t
            -> actor:Operator_authorization.t
            -> context:P.Authoring.Context.t
            -> Registry.t option)
      -> t
  end

  val create
    :  owner:owner
    -> script_selection:Semantic.Script_selection.Policy.t option
    -> project_metadata:Semantic.Project_metadata.Policy.t option
    -> selected_tools:Selected_tool_policy.t option
    -> project_build_limits:Chatml_project_build.Limits.t
    -> limits:Limits.t
    -> (t, Error.t) result

  (** Exact authored or effective highlighting uses only currently captured
      bytes. The request selects lexical independently of semantic. *)
  val handle_lexical
    :  t
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> P.Authoring_assistance.Lexical.Request.t
    -> (W.Response.t, Error.t) result

  (** Structural help remains available for malformed authored input.
      Selected catalog, imported script, tool and path suggestions require
      their genuine current host evidence; missing evidence is reported as
      unavailable. The request selects assistance independently of semantic. *)
  val handle_assistance
    :  t
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> P.Authoring_assistance.Assistance.Request.t
    -> (W.Response.t, Error.t) result
end

(** Comparison-only developer capture through the SAME source and context
    owners. Preparing a Candidate captures immutable code under an explicit
    host-selected Standard One_off_v1 contract; it does not execute or create a
    Root. The hosted runner installs the existing Root.Registry admission after the
    Candidate's full/final checks. No transport method is advertised here. *)
module Developer : sig
  type t

  module Limits : sig
    type t

    val create
      :  max_prepared_input_bytes:int
      -> max_document_origin_bytes:int
      -> max_editor_versions:int
      -> build:Chatml_project_build.Limits.t
      -> t Or_error.t
  end

  module Selected_contract : sig
    type t

    (** Only the existing bounded Standard-contract cache can supply this
        immutable One_off_v1 or explicit Tool_v1 comparison identity. Host selection/currentness
        remains independent. *)
    val of_standard_entry : Authoring_standard_contracts.Entry.t -> (t, P.Error.t) result

    val of_standard_tool_entry
      :  Authoring_standard_contracts.Entry.t
      -> tool_name:string
      -> input_schema:Chatmd_shell_spec.Tool_schema.t
      -> output_schema:Chatmd_shell_spec.Tool_schema.t
      -> (t, P.Error.t) result

    module Tool_fixture : sig
      type t

      val name : t -> string
      val input_schema : t -> Chatmd_shell_spec.Tool_schema.t
      val output_schema : t -> Chatmd_shell_spec.Tool_schema.t
    end

    val contract : t -> Chatml.Chatml_compiler_contract.t
    val tool_fixture : t -> Tool_fixture.t option
    val same_identity : t -> t -> bool
  end

  module Request : sig
    type t

    (** Bounded, inert input/fixture admission; no compilation or effects. *)
    val create
      :  entrypoint:string
      -> target:Chatml.Chatml_compiler_contract.Target.t
      -> input:Jsonaf.t
      -> fixtures:Chatml_developer_capture.Fixture.t option
      -> json_bounds:Chatml_developer_capture.Json_bounds.t
      -> checkpoint:(unit -> unit)
      -> (t, P.Error.t) result
  end

  module Target_policy : sig
    type t

    (** Trusted bounded NONYIELDING selectors from already admitted host
        policy. Their arguments are comparison data, not source grants. The
        candidate reselects both original and current actors after waits and
        at Root's final admission; changed/missing identities reject. *)
    val create
      :  local_project:
           (Operator_authorization.t
            -> root:Sources.Root.t
            -> project:Source.Portable_path.t
            -> selection:Chatml_project.Selection.request
            -> Selected_contract.t option)
      -> local_source:
           (Operator_authorization.t
            -> root:Sources.Root.t
            -> member:Source.Portable_path.t
            -> Selected_contract.t option)
      -> authoring_project:
           (Operator_authorization.t
            -> context:P.Authoring.Context.t
            -> current:Ledger.Inputs.t
            -> Selected_contract.t option)
      -> document_project:
           (Operator_authorization.t
            -> context:P.Authoring.Context.t
            -> current:Ledger.Document.Inputs.t
            -> parsed:Prompt.Chat_markdown.Parsed.t
            -> declaration:Prompt.Chat_markdown.Declaration.t
            -> build:Chatml_project_build.t
            -> metadata:Sources.Document_context.Project_capture.t option
            -> Selected_contract.t option)
      -> document_source:
           (Operator_authorization.t
            -> context:P.Authoring.Context.t
            -> current:Ledger.Document.Inputs.t
            -> parsed:Prompt.Chat_markdown.Parsed.t
            -> declaration:Prompt.Chat_markdown.Declaration.t
            -> script:Chatml_source.t
            -> Selected_contract.t option)
      -> t
  end

  module Local_project : sig
    type t

    val create
      :  root:Sources.Root.t
      -> dependency_roots:Sources.Root.t list
      -> project:Source.Portable_path.t
      -> selection:Chatml_project.Selection.request
      -> t
  end

  module Local_source : sig
    type t

    val create : root:Sources.Root.t -> member:Source.Portable_path.t -> t
  end

  module Document_script : sig
    type t

    val create : selector:P.Authoring.Analysis.Target.selector -> t
  end

  module Source_selection : sig
    (** Exact source/contract selection shared by command capture and one
        process-local REPL epoch. Minted only from this installed Developer
        service; it does not create a command or grant interpreter effects. *)
    type t

    type checked

    val prepared : t -> Chatml_prepared_program.t
    val entry_source : t -> Chatml_source.t
    val selected_contract : t -> Selected_contract.t

    val check_current
      :  t
      -> actor:Operator_authorization.t
      -> checkpoint:(unit -> unit)
      -> (checked, P.Error.t) result

    val check_now
      :  t
      -> checked:checked
      -> actor:Operator_authorization.t
      -> (unit, P.Error.t) result

    module Shared : sig
      type checked

      (** Concurrent authenticated reads do not invalidate a running REPL
          statement's admission proof. This proof cannot enter a Candidate's
          exclusive command admission. *)
      val check_current
        :  t
        -> actor:Operator_authorization.t
        -> checkpoint:(unit -> unit)
        -> (checked, P.Error.t) result

      val check_now
        :  t
        -> checked:checked
        -> actor:Operator_authorization.t
        -> (unit, P.Error.t) result
    end
  end

  module Candidate : sig
    type t

    val capture : t -> Chatml_developer_capture.t
    val selected_contract : t -> Selected_contract.t

    (** The same closure survives a provisional Root recovery, but each
        attempt's yielding full check mints a fresh final comparison. Root
        supplies its physical original actor/capture and a fresh checkpoint. *)
    val final_admission
      :  t
      -> actor:Operator_authorization.t
      -> capture:Chatml_developer_capture.t
      -> checkpoint:(unit -> unit)
      -> (unit -> (unit, P.Error.t) result, P.Error.t) result
  end

  val prepare_local_project_selection
    :  t
    -> original_actor:Operator_authorization.t
    -> actor:Operator_authorization.t
    -> local:Local_project.t
    -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
    -> checkpoint:(unit -> unit)
    -> (Source_selection.t, P.Error.t) result

  val prepare_local_source_selection
    :  t
    -> original_actor:Operator_authorization.t
    -> actor:Operator_authorization.t
    -> source:Local_source.t
    -> checkpoint:(unit -> unit)
    -> (Source_selection.t, P.Error.t) result

  val prepare_authoring_project_selection
    :  t
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> expected:P.Authoring.Context.Expected.t
    -> checkpoint:(unit -> unit)
    -> (Source_selection.t, P.Error.t) result

  val prepare_document_script_selection
    :  t
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> expected:P.Authoring.Context.Expected.t
    -> script:Document_script.t
    -> checkpoint:(unit -> unit)
    -> (Source_selection.t, P.Error.t) result

  val capture_selection
    :  Source_selection.t
    -> request:Request.t
    -> checkpoint:(unit -> unit)
    -> (Candidate.t, P.Error.t) result

  val create : owner:owner -> policy:Target_policy.t -> limits:Limits.t -> t

  val prepare_local_project
    :  t
    -> original_actor:Operator_authorization.t
    -> actor:Operator_authorization.t
    -> local:Local_project.t
    -> request:Request.t
    -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
    -> checkpoint:(unit -> unit)
    -> (Candidate.t, P.Error.t) result

  val prepare_local_source
    :  t
    -> original_actor:Operator_authorization.t
    -> actor:Operator_authorization.t
    -> source:Local_source.t
    -> request:Request.t
    -> checkpoint:(unit -> unit)
    -> (Candidate.t, P.Error.t) result

  val prepare_authoring_project
    :  t
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> expected:P.Authoring.Context.Expected.t
    -> request:Request.t
    -> checkpoint:(unit -> unit)
    -> (Candidate.t, P.Error.t) result

  val prepare_document_script
    :  t
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> expected:P.Authoring.Context.Expected.t
    -> script:Document_script.t
    -> request:Request.t
    -> checkpoint:(unit -> unit)
    -> (Candidate.t, P.Error.t) result
end

(** Deliberately narrow test-only view of the same owner. Its opaque receipts
    cannot grant source access or escape a live publication probe. *)
module For_testing : sig
  type error =
    | Stale_context
    | Unselected_feature
    | Other

  (** Explicit test boundary for the authoring owner's otherwise private cache and
    capture invariants. These receipts convey no protocol/source authority and
    have no transport path. Production handlers never call this module. *)
  type feature =
    | Semantic
    | Lexical
    | Assistance
    | Bundle_upload

  type head =
    | Project_cache
    | Document_cache
    | Analysis_index
    | Provisional_index
    | Parsed_capture
    | Checked_template

  type probe
  type expired_borrow

  val with_publication_probe
    :  owner
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> expected:P.Authoring.Context.Expected.t
    -> feature:feature
    -> checkpoint:(unit -> unit)
    -> after_borrow:(probe -> unit)
    -> final_check:(unit -> (unit, error) result)
    -> publish:(unit -> unit)
    -> (expired_borrow, error) result

  (** Only the live probe can replace a physical head, under the original gate.
    Each replacement changes exactly the named head and no checked facts. *)
  val replace_head : probe -> head -> (unit, error) result

  val expired_borrow_rejects_inputs_index_and_capture : expired_borrow -> bool

  type capture_state =
    { parsed : bool
      (** Exact association check for the root-only supplied receipt used by the
      grouped fixture; imported receipts have distinct genuine source IDs. *)
    ; source_matches : bool
    ; project_metadata : bool
    ; selection_rows : int
    ; complete_rows : int
    }

  val capture_state
    :  owner
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> expected:P.Authoring.Context.Expected.t
    -> checkpoint:(unit -> unit)
    -> (capture_state, error) result

  val provisional_count
    :  owner
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> expected:P.Authoring.Context.Expected.t
    -> checkpoint:(unit -> unit)
    -> (int, error) result

  type index_view

  val index_view
    :  owner
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> expected:P.Authoring.Context.Expected.t
    -> checkpoint:(unit -> unit)
    -> (index_view, error) result

  val row_matches
    :  index_view
    -> capture:P.Authoring.Analysis.Capture.t
    -> snapshot:P.Authoring.Analysis.Snapshot_id.t
    -> bool

  val transition_index_to_current
    :  owner
    -> index_view
    -> actor:Operator_authorization.t
    -> connection:Connection_context.t
    -> expected:P.Authoring.Context.Expected.t
    -> checkpoint:(unit -> unit)
    -> (index_view, error) result

  (** Read-only preview through the SAME overlay adapter on a genuine captured
    host context and matching admitted Ledger/view. No source/root grant is
    manufactured from a wire label. *)
  val preview_project_overlay
    :  context:Sources.Context.t
    -> original_actor:Operator_authorization.t
    -> ledger:Ledger.t
    -> identity:Ledger.Identity.t
    -> view:P.Authoring.Context.t
    -> analysis_policy:Chatml_authoring_analysis.Policy.t
    -> limits:Chatml_project_build.Limits.t
    -> checkpoint:(unit -> unit)
    -> (Chatml_project_build.t * Chatml.Chatml_compiler_contract.t, error) result

  module Retained_document : sig
    type t

    val prepare
      :  context:Sources.Document_context.t
      -> original_actor:Operator_authorization.t
      -> ledger:Ledger.t
      -> identity:Ledger.Identity.t
      -> view:P.Authoring.Context.t
      -> capture:Sources.Document_context.Parsed_capture.t
      -> max_sources:int
      -> max_bytes:int
      -> checkpoint:(unit -> unit)
      -> (t, error) result

    val same_head : t -> t -> bool

    val matches_inputs
      :  t
      -> ledger:Ledger.t
      -> identity:Ledger.Identity.t
      -> view:P.Authoring.Context.t
      -> checkpoint:(unit -> unit)
      -> (bool, error) result

    val recompose_current
      :  t
      -> ledger:Ledger.t
      -> identity:Ledger.Identity.t
      -> view:P.Authoring.Context.t
      -> checkpoint:(unit -> unit)
      -> (Chatmd_source_bundle.t option, error) result
  end

  val classify_compilation_error
    :  Chatml_compilation.Analysis_error.t
    -> [ `Resource | `Selection | `Invalid | `Internal | `Unexpected ]

  val syntax_prefix_for_testing
    :  max_sites:int
    -> int list list
    -> checkpoint:(unit -> unit)
    -> int list * bool
end

(** Internal sibling-service seam. Admit actor, selected core feature,
    connection incarnation and expected CAS; pin one current owner under a
    bounded joined activity. The trusted callback may yield. Recheck exact
    original/current actor, host source or upload-read grant or independent
    supplied-document policy, selected catalog,
    exact expected context generation/basis and connection negotiation after
    it returns and before returning the response. A Current token cannot be
    used after callback return; an escaped token fails closed. The short
    ledger/cache gate is not held across [f].
    Semantic/assistance children add actual parser/per-script selection checks
    before their own publication; this scope does not claim to prove them.
    Semantic and assistance publication use the private owner gate; callers of
    this public method cannot supply a publication callback. *)
val with_current
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> checkpoint:(unit -> unit)
  -> f:(Current.t -> checkpoint:(unit -> unit) -> ('a, Error.t) result)
  -> ('a, Error.t) result

(** Source/context request subset: Roots, Host_list/read, Context_open/
    refresh/close, Source_list/read, Document_version/replace/close. This
    function rejects other Request constructors as Uninstalled. Root IDs and
    context IDs are fresh connection-local labels, not grants; Roots repeats
    host permission on every use. Host_read is a fresh native observation;
    Source_read is pinned ledger bytes. Uploaded_project initially invokes
    Upload.capture_for_context and accepts only a Project basis; every later
    request invokes Upload.check_context_current on its opaque owner-affine pin
    without restoring the compiler graph again. The
    response uses existing validated Wire constructors and exact bytes.
    Capabilities is the remaining nominal core method but must be constructed
    by the final installer from ALL actual installed handlers, including a
    bundle-only host; this owner does not advertise its own subset as the whole
    service. *)
val handle_core
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> W.Request.t
  -> (W.Response.t, Error.t) result

(** The installer supplies a pure, nonyielding builder over the exact
    negotiation held by the admitted connection entry. Construct the selected
    inventory only after the request is registered, so a reinitialize waiting
    on admission cannot return an earlier epoch's methods. Core or bundle must
    be selected. The installer never infers installation from wire codecs;
    this owner repeats actor/epoch/feature checks before response. *)
val handle_capabilities
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> make:(Connection_context.Authoring_negotiation.t -> P.Authoring.Capabilities.t)
  -> (W.Response.t, Error.t) result

(** Route the four bundle methods through the independently authorized upload
    host. Parentless requests can use a bundle-only connection. A request with
    a parent must additionally borrow the actual same-connection Core context
    and, for an independently selected Document prior, the installed script
    policy. That policy rechecks genuine current Parsed/declaration/Build
    evidence; a wire Expected cannot grant parent authority. This handler does
    not advertise itself; installation and transport routing belong to the
    protocol installer. *)
val handle_bundle
  :  ?script_selection:Semantic.Script_selection.Policy.t
  -> t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> W.Request.t
  -> (W.Response.t, Error.t) result

(** Single serialized per-connection reinitialization transition. The
    installer first computes a SUCCESSFUL Initialize.Response, then calls this
    function instead of mark_initialized directly. The service marks the old
    epoch Revoking, rejects new old-epoch requests, cancels/joins active work,
    closes source contexts and atomically evicts ledger/cache entries. ONLY
    AFTER that wait does it call Connection_context.mark_initialized with the
    exact response version/features and admit the new epoch. Concurrent
    initialize transitions serialize behind a distinct lifecycle gate; no
    request is admitted in the revocation/publication gap. A failed initialize
    computation never calls this function and leaves the old epoch untouched.
    Reentrant initialization from an active authoring request returns Busy
    before waiting; cancellation/unexpected exceptions preserve identity and
    leave old admission closed rather than publishing a partial new epoch. *)
val publish_initialize_and_wait
  :  t
  -> connection:Connection_context.t
  -> response:P.Initialize.Response.t
  -> (unit, Error.t) result

(** Idempotent transport close and daemon close cancel/join work before
    releasing borrowed source/upload owners. Close from one's own active
    request rejects Busy instead of self-joining. No old context ID or root ID
    can address a new Connection_context even if transport connection_id is
    reused. Cancellation/unexpected callback exceptions retain identity and
    backtrace; expected errors are returned as Error.t without poisoning gates. *)
val close_connection_and_wait : t -> Connection_context.t -> (unit, Error.t) result

val close_and_wait : t -> (unit, Error.t) result
