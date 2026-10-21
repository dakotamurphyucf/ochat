(** Private declaration representation and immutable scope selection. Only the
    project decoder constructs declaration records after schema/reference checks.
    Public Chatml_project hides these records; no host authority is represented. *)
open Core

(** Pure versioned project declarations and selected scope intent. No directory is
    scanned, repository fetched, compiler invoked or initializer executed. Exact
    captured declaration bytes own every source location. Native authority and
    executable bindings remain with host admission. *)

module Declared_at : sig
  type t

  (** Whole exact declaration range plus decoded JSON field path. The JSON codec
      does not supply token positions; this never claims a field-byte range. *)
  val create : Chatml_source.t -> field_path:string list -> checkpoint:(unit -> unit) -> t

  val source : t -> Chatml_source.t
  val with_field_path : t -> field_path:string list -> t
  val location : t -> Chatml_source.Location.t
  val field_path : t -> string list
end

module Error : sig
  (** Existing schema/domain validation error plus its exact captured declaration.
      No renderer, excerpt store or second diagnostic system. Host presentation
      adapts details/field paths to the shared bounded ChatML diagnostic. Retains
      the supplied immutable capture even if decode rejects its size; capture
      admission belongs to the caller. No parsed tree or checkpoint is retained. *)
  type t

  (** Records supplied immutable ownership and schema details without scanning the
      source, validating capture size, or inventing a metadata-derived location. *)
  val create
    :  Chatml_source.t
    -> details:Document_schema.Error.t
    -> field_path:string list
    -> t

  val source : t -> Chatml_source.t
  val details : t -> Document_schema.Error.t
  val field_path : t -> string list
end

module Git_pin : sig
  type object_format =
    | Sha1
    | Sha256
  [@@deriving equal, sexp_of]

  (** Exact full lowercase hexadecimal commit identity, 40/64 bytes for the
      explicitly selected object format. No ref resolution or format inference. *)
  type t [@@deriving equal, sexp_of]

  val create : object_format:object_format -> commit:string -> t Or_error.t
  val object_format : t -> object_format
  val commit : t -> string
end

module Dependency : sig
  type source =
    | Local of { directory : string }
    | Git of
        { repository : string
        ; pin : Git_pin.t
        }

  (** Local directory can be relative to the declaring project or absolute on the
      selected runtime host, under explicit host authority. It is not a portable
      source member ID or granted root. Repository locator is untrusted intent;
      the Git host adapter validates supported transport/authentication. Neither
      locator is resolved here. Namespaces never derive from locator/name/hash. *)
  type t =
    { name : string
    ; source : source
    ; project : Source.Portable_path.t
    ; environment : string option
    ; namespace_override : string option
    ; declared_at : Declared_at.t
    }

  val is_valid_name : string -> checkpoint:(unit -> unit) -> bool
  val is_valid_locator : string -> checkpoint:(unit -> unit) -> bool
  val name : t -> string
  val source : t -> source
  val project : t -> Source.Portable_path.t
  val environment : t -> string option
  val namespace_override : t -> string option
  val declared_at : t -> Declared_at.t
end

module Module_ref : sig
  type catalog =
    | Self
    | Dependency of string
  [@@deriving equal, sexp_of]

  type t =
    { catalog : catalog
    ; name : string
    }
  [@@deriving equal, sexp_of]

  val is_valid_name : string -> checkpoint:(unit -> unit) -> bool
  val catalog : t -> catalog
  val name : t -> string
end

module Source_dir : sig
  type directory =
    | Root
    | Relative of Source.Portable_path.t

  (** Explicit nonrecursive source directory. Root is authored as '.', without
      weakening Source.Portable_path's file/member invariants. *)
  type t =
    { directory : directory
    ; declared_at : Declared_at.t
    }

  val directory : t -> directory
  val declared_at : t -> Declared_at.t
end

module Module_override : sig
  type interface_selection =
    | Companion
    | Explicit of Source.Portable_path.t

  (** Omitted interface selects an optional conventional companion for the chosen
      implementation during host preparation. Explicit interface must exist.
      Config null is invalid; no invented interface-only module or disable mode. *)
  type t =
    { name : string
    ; implementation : Source.Portable_path.t
    ; interface_selection : interface_selection
    ; declared_at : Declared_at.t
    }

  val name : t -> string
  val implementation : t -> Source.Portable_path.t
  val interface_selection : t -> interface_selection
  val declared_at : t -> Declared_at.t
end

module Compiler_requirement : sig
  (** Compiler-owned family/target names, checked by the existing target adapter.
      No independent compiler version registry or native-surface union. Optional
      exact digest is an advanced constraint, not routine author bookkeeping. *)
  type t =
    { compiler_family : string option
    ; allowed_targets : string list option
    ; exact_contract : Chatml_source.Digest.t option
    ; declared_at : Declared_at.t option
    }

  val unconstrained : t
  val compiler_family : t -> string option
  val allowed_targets : t -> string list option
  val exact_contract : t -> Chatml_source.Digest.t option
  val declared_at : t -> Declared_at.t option
end

module Scope_spec : sig
  type binding =
    { name : string
    ; module_ref : Module_ref.t
    ; declared_at : Declared_at.t
    }

  (** None means the selected package's own complete catalog. Some bindings is
      the entire explicit selected set, including a deliberately empty set.
      Dependency catalogs never inherit root/importer-local names. *)
  type t =
    { name : string
    ; bindings : binding list option
    ; compiler_requirement : Compiler_requirement.t
    ; declared_at : Declared_at.t option
    }

  val implicit_default : t
  val name : t -> string
  val bindings : t -> binding list option
  val compiler_requirement : t -> Compiler_requirement.t
  val declared_at : t -> Declared_at.t option
end

module Entry : sig
  type t =
    { name : string
    ; source : Source.Portable_path.t
    ; target_name : string
    ; declared_at : Declared_at.t
    }

  val name : t -> string
  val source : t -> Source.Portable_path.t
  val target_name : t -> string
  val declared_at : t -> Declared_at.t
end

module Selection : sig
  type request =
    | Entry of
        { name : string
        ; environment : string option
        }
    | Library of { environment : string option }

  type t =
    { entry : Entry.t option
    ; environment : Scope_spec.t
    ; file_environments : Scope_spec.t String.Map.t
    }

  val entry : t -> Entry.t option
  val environment : t -> Scope_spec.t

  (** Exact-file overrides select another whole environment of the SAME package.
      Implementation and interface paths select independently. All referenced
      environment names are validated before publication, so lookup is total. *)
  val scope_for : t -> source:Source.Portable_path.t -> Scope_spec.t
end

type t =
  { source : Chatml_source.t
  ; declared_at : Declared_at.t
  ; namespace : string
  ; source_dirs : Source_dir.t list
  ; exclusions : (Source.Portable_path.t * Declared_at.t) list
  ; module_overrides : Module_override.t list
  ; dependencies : Dependency.t list
  ; required_tools : (string * Declared_at.t) list
  ; environments : Scope_spec.t String.Map.t
  ; default_environment : string option
  ; file_environments : Scope_spec.t String.Map.t
  ; entries : (Entry.t * string option) String.Map.t
  }

val source : t -> Chatml_source.t

module Environment_names : sig
  module Error : sig
    type t =
      | Invalid_limit
      | Too_many
    [@@deriving equal, sexp_of]
  end

  (** Enumerate only this captured, decoded declaration's environment names.
      Charge each retained map entry before adding it to the result; reject a
      nonpositive ceiling or a map larger than [max_names] without first
      allocating [Map.keys]. This does not select or resolve an environment.
      The existing [source] accessor owns the declaration provenance.
      Callback exceptions propagate unchanged. *)
  val list
    :  t
    -> max_names:int
    -> checkpoint:(unit -> unit)
    -> (string list, Error.t) Result.t
end

module Declared_paths : sig
  type role =
    | Source_directory
    | Module_implementation
    | Module_interface
    | Entry_source
    | Local_dependency_project
  [@@deriving equal, sexp_of]

  module Error : sig
    type t =
      | Invalid_limit
      | Too_many
    [@@deriving equal, sexp_of]
  end

  (** Compare one path and role against ACTUAL retained decoded fields. The
      bounded walk includes every relevant field before answering; local
      dependency project members stay distinct from Git repository URLs.
      This is declaration evidence, not existence, routing or read authority. *)
  val contains
    :  t
    -> role:role
    -> member:Source.Portable_path.t
    -> max_entries:int
    -> checkpoint:(unit -> unit)
    -> (bool, Error.t) Result.t
end

val declared_at : t -> Declared_at.t
val namespace : t -> string
val source_dirs : t -> Source_dir.t list
val exclusions : t -> (Source.Portable_path.t * Declared_at.t) list
val module_overrides : t -> Module_override.t list
val dependencies : t -> Dependency.t list

(** Exact host tool names requested against the existing effective registry:
    Tool_capability.select already performs selection without grants/invocation.
    No synthetic capability IDs, live references, callbacks or implementations.
    Host adaptation/revalidation remains required; fixed target builtins unchanged.
    These requirements do not define per-file native permission profiles. *)
val required_tools : t -> (string * Declared_at.t) list

(** Explicit selector, entry's declared selector or package-owned default; never
    propagation of another package's environment name. Missing explicit/default
    selections fail precisely, never selecting the first map item. *)
val select
  :  t
  -> request:Selection.request
  -> checkpoint:(unit -> unit)
  -> (Selection.t, Error.t) Result.t
