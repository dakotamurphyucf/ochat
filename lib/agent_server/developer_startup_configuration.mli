open! Core

(** Restart-bound, declarative opt-in for the installed Developer service. This
    value selects only comparison policy and finite capacity. It contains no
    callbacks, source handles, model credentials, protocol IDs as grants, or
    alternate native permission language. Absence from Server disables the
    service. Directly constructed values must pass [validate] before effects. *)

module Target : sig
  type t =
    | One_off_v1
    | Developer_tool_v1
    | Tool_v1 of
        { tool_name : string
        ; input_schema_json : string
        ; output_schema_json : string
        }
  [@@deriving compare, equal, sexp]
end

module Source : sig
  type document_origin =
    | Supplied_document
    | Host_document of
        { root : string
        ; member : string
        }
  [@@deriving compare, equal, sexp]

  type authoring_project =
    | Host_project of
        { root : string
        ; project : string
        ; request : Authoring_project_access_configuration.Selection.request
        }
    | Uploaded_project of { environment : string }
  [@@deriving compare, equal, sexp]

  type t =
    | Local_project of
        { root : string
        ; project : string
        ; request : Authoring_project_access_configuration.Selection.request
        }
    | Local_source of
        { root : string
        ; member : string
        }
    | Authoring_project of authoring_project
    | Document_project of
        { origin : document_origin
        ; script_id : string
        ; authored_project : Chatmd_shell_spec.Script_project.t
        }
    | Document_source of
        { origin : document_origin
        ; script_id : string
        }
  [@@deriving compare, equal, sexp]
end

module Principal_selector : sig
  (** There is no existing pure principal-ID/authentication-kind selection
      type: configured project grants also carry a root, upload grants carry an
      operation, and protocol Principals carry scopes/attributes. This pair is
      validated with the existing Principal authentication-kind grammar and
      remains comparison data, not an authenticated actor or permission. *)
  type t =
    { principal_id : Agent_protocol.Id.Principal.t
    ; authentication_kind : string
    }
  [@@deriving compare, equal, sexp]
end

module Selection : sig
  type t =
    { source : Source.t
    ; target : Target.t
    ; principals : Principal_selector.t list
    }
  [@@deriving compare, equal, sexp]
end

module Capacity : sig
  type t =
    { max_runs : int
    ; max_active_commands : int
    ; max_active_agent_calls : int
    ; max_agent_sources : int
    ; max_native_bindings : int
    ; max_retained_tasks : int
    ; max_watches_per_connection : int
    ; max_pending_approvals : int
    ; max_repl_instances : int
    ; max_repl_keys_per_instance : int
    ; max_repl_submissions_per_epoch : int
    ; max_repl_source_bytes : int
    }
  [@@deriving compare, equal, sexp]

  (** An ordinary finite starting capacity, not a hard product ceiling.
      [max_runs] bounds cumulative durable Developer run documents, separately
      from concurrent commands; a run can contain multiple command rows.
      Exhausting it denies a new run under the existing Store admission. Each
      scalar can be configured and is admitted by its existing lower owner.
      REPL historical keys survive Reset; exhaustion requires a new instance.
      These are fields of the explicit [Capacity.t] record, not parser defaults
      for omitted S-expression fields. *)
  val standard_v1 : t

  (** Repeat scalar admission before a lower-owner preset derives arithmetic
      bounds from a directly constructed Capacity. *)
  val validate : t -> (unit, Agent_protocol.Error.t) result
end

type model =
  | Disabled
  | Provider_default
[@@deriving compare, equal, sexp]

type approvals =
  | Disabled
  | Once
[@@deriving compare, equal, sexp]

type repl =
  | Disabled
  | Standard_v1
[@@deriving compare, equal, sexp]

(** Explicit recording opt-in for declarative Developer startup. Absence from a
    previously saved record remains disabled. Summary and Full select only the
    existing bounded recorder mode; they add no source-read grant or portable
    method by themselves. A portable page method also requires [portable]. *)
type traces =
  | No_traces
  | Trace_summary
  | Trace_full
[@@deriving compare, equal, sexp]

type t =
  { selections : Selection.t list
  ; capacity : Capacity.t
  ; model : model
  ; approvals : approvals
  ; repl : repl
  ; portable : bool
  ; traces : traces [@sexp.default No_traces]
  }
[@@deriving compare, equal, sexp]

(** Bound at most 64 rows, 64 KiB aggregate selector/name text, and 8 MiB
    aggregate Tool schema text before conversion. Tool schemas also obey the
    existing per-schema 1 MiB parser ceiling. Require unique
    (source,principal ID,authentication kind) choices and valid portable paths,
    project environments, script IDs, tool names, and principal identities.
    Reject missing principal lists and incompatible capacity. This pure check
    neither opens a source nor grants a selected target. The startup binder
    repeats it for direct records and checks physical roots/owners separately. *)
val validate : t -> (unit, Agent_protocol.Error.t) result

(** Validate with bounded traversal before enumerating distinct targets or
    constructing the one shared Standard cache. Return distinct target variants in first-seen order.
    Tool_v1 rows share one Standard Tool_v1 contract but retain distinct
    tool-name/schema identities at selected capture. No conversion occurs. *)
val standard_targets
  :  t
  -> (Chatml.Chatml_compiler_contract.Target.t list, Agent_protocol.Error.t) result
