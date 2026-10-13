(** Private typed basis constructors; canonical syntax is not source custody. *)
module A = Authoring_common

module Context = Authoring_context.Context
module Captured = Chatml_source
module Project = Chatml_project
module Path = Source.Portable_path
module Project_basis = Authoring_bundle_basis_project.Project_basis

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
