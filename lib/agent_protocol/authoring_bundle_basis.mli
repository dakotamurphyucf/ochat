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

module Document_member : sig
  type t

  val create : source:Captured.Id.t -> member:Path.t -> (t, Error.t) result
  val source : t -> Captured.Id.t
  val member : t -> Path.t
  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Script : sig
  module Selector : sig
    type t

    val create
      :  registration_source:Captured.Id.t
      -> declaration_source:Captured.Id.t
      -> start_byte:int
      -> end_byte:int
      -> script_id:string
      -> body_digest:Captured.Digest.t
      -> body_byte_length:int
      -> (t, Error.t) result

    val registration_source : t -> Captured.Id.t
    val declaration_source : t -> Captured.Id.t
    val start_byte : t -> int
    val end_byte : t -> int
    val script_id : t -> string
    val body_digest : t -> Captured.Digest.t
    val body_byte_length : t -> int
    val key_compare : t -> t -> int
    val equal : t -> t -> bool
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  type choice =
    | Source_only
    | Selected_project of Project_basis.t

  type t

  val create : selector:Selector.t -> choice:choice -> (t, Error.t) result
  val selector : t -> Selector.t
  val choice : t -> choice
  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Chatmd_document : sig
  type t

  val create
    :  root_source:Captured.Id.t
    -> document_members:Document_member.t list
    -> scripts:Script.t list
    -> (t, Error.t) result

  val root_source : t -> Captured.Id.t
  val document_members : t -> Document_member.t list
  val scripts : t -> Script.t list
  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t =
  | Project of Project_basis.t
  | Chatmd_document of Chatmd_document.t

val equal : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** One whole-manifest preflight; [member_sources] is already bounded by the
    enclosing wire count/bytes. Reject missing or conflicting references,
    including dependency targets outside their Project subset or without a
    selected owner route, and conflicting
    derived/uploaded identities, before canonical JSON.
    No source-body or host authority validation happens here. *)
val validate_inventory : t -> member_sources:A.Source.t list -> (unit, Error.t) result
