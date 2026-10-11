open Core
module Captured = Chatml_source
module Project = Chatml_project
module Catalog = Chatml_catalog
module Prepared = Chatml_prepared_program

module Limits : sig
  type t

  val create
    :  max_work_steps:int
    -> max_input_bytes:int option
    -> document:Document_schema.Limits.t
    -> t Or_error.t

  val max_work_steps : t -> int
  val max_input_bytes : t -> int option
  val document : t -> Document_schema.Limits.t
end

module Error : sig
  type problem =
    | Missing_catalog of Captured.Id.t
    | Unexpected_catalog of Captured.Id.t
    | Namespace_conflict of string
    | Invalid_repository_member of Captured.Id.t
    | Invalid_acquisition_key
    | Invalid_dependency of string
    | Missing_module of Project.Module_ref.t
    | Compiler_requirement
    | Invalid_entry_selection
    | Invalid_lock_identity
    | Lock_mismatch
    | Missing_capture of Captured.Id.t
    | Duplicate_capture of Captured.Id.t
    | Needs_update
    | Limit_exceeded
    | Accounting_overflow
    | Declaration of Project.Error.t
    | Catalog of Catalog.Error.t
    | Prepared of Prepared.Error.t

  type t

  (** Missing captures may lack source ownership; never fabricate a location from
      a receipt ID. Nested errors preserve their original exact capture context. *)
  val create : ?declared_at:Project.Declared_at.t -> problem -> t

  val problem : t -> problem
  val declared_at : t -> Project.Declared_at.t option
end

module Catalog_input : sig
  (** Unadmitted acquisition facts. Some acquisition_key is derived by the host from its
      canonical admitted root/revision/selection identity. It is not author intent
      or proof of native provenance; composition uses it only within this attempt
      and never retains it in the completed result or lock. None is used only by
      capture-only reconstruction, which supplies one input per catalog and cannot
      establish equality of repeated acquisition inputs. Dependency associations
      are owner-local names selecting exact captured declaration IDs. *)
  type t =
    { catalog : Catalog.t
    ; origin : Project.Lock.Catalog.origin
    ; acquisition_key : string option
    ; dependencies : Project.Lock.Dependency_selection.t list
    }

  val declaration : t -> Captured.Id.t
  val declared_at : t -> Project.Declared_at.t
end

module Entry : sig
  type t =
    | Declared
    | Captured of Captured.t

  (** Declared consumes the root catalog's actual selected project entry. Captured
      consumes a root library selection and its exact selected environment, never
      an invented filename-based override. Repeated source IDs must agree in bytes
      and scope before final Prepared admission. *)
end

module Operation : sig
  type t =
    | Initial_prepare
    | Build_without_previous
    | Build of { previous_lock : Captured.t }
    | Update of { previous_lock : Captured.t option }
end

module Accounting : sig
  type t

  val create : work_steps:int -> t
  val work_steps : t -> int
end
