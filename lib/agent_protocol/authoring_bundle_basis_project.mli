(** Private typed basis constructors; canonical syntax is not source custody. *)
module A = Authoring_common

module Context = Authoring_context.Context
module Captured = Chatml_source
module Project = Chatml_project
module Path = Source.Portable_path

module Route : sig
  type t

  val create
    :  declaration:Captured.Id.t
    -> origin:Project.Lock.Catalog.origin
    -> dependencies:Project.Lock.Dependency_selection.t list
    -> (t, Error.t) result

  val declaration : t -> Captured.Id.t
  val origin : t -> Project.Lock.Catalog.origin
  val dependencies : t -> Project.Lock.Dependency_selection.t list
  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Project_basis : sig
  module Captured_entry : sig
    type delivery =
      | Uploaded_member
      | Derived_script
    [@@deriving equal, sexp_of]

    type t

    val create : source:A.Source.t -> delivery:delivery -> (t, Error.t) result
    val source : t -> A.Source.t
    val delivery : t -> delivery
    val equal : t -> t -> bool
  end

  type entry =
    | Declared
    | Captured of Captured_entry.t

  type mode =
    | Exact_lock of Captured.Id.t
    | Compose of
        { selected_previous_lock : A.Source.t option
        ; generated_lock : Captured.Id.t
        }

  type t

  val create
    :  root_source:Captured.Id.t
    -> selection:Context.Selection.t
    -> entry:entry
    -> source_members:Captured.Id.t list
    -> routes:Route.t list
    -> mode:mode
    -> (t, Error.t) result

  val root_source : t -> Captured.Id.t
  val selection : t -> Context.Selection.t
  val entry : t -> entry
  val source_members : t -> Captured.Id.t list
  val routes : t -> Route.t list
  val mode : t -> mode
  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
