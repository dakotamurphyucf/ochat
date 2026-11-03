module Limits = Developer_repl_wire_common.Limits
module Wire_id = Developer_repl_wire_common.Wire_id

module Request : sig
  type t

  (** Read only, against one exact live revision. Path components are module
      identifiers (at most 16); prefix may be empty. Each identifier/prefix is
      ASCII and at most 128 bytes. At most 16 names may be requested. *)
  val create
    :  limits:Limits.t
    -> instance:Wire_id.Instance.t
    -> epoch:Wire_id.Epoch.t
    -> expected_revision:Wire_id.Revision.t
    -> path:string list
    -> prefix:string
    -> limit:int
    -> (t, Error.t) result

  val instance : t -> Wire_id.Instance.t
  val epoch : t -> Wire_id.Epoch.t
  val expected_revision : t -> Wire_id.Revision.t
  val path : t -> string list
  val prefix : t -> string
  val limit : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Page : sig
  type status =
    | Complete
    | Limited
    | Unavailable
  [@@deriving equal, sexp_of]

  type t

  (** Names are sorted, unique, valid ASCII identifiers. They are authoritative
      current-scope names, not draft typechecking results. [Unavailable] means
      an unselected configured module has no committed static member snapshot;
      it must have no names. No completion request initializes or checks it. *)
  val create
    :  limits:Limits.t
    -> revision:Wire_id.Revision.t
    -> names:string list
    -> status:status
    -> (t, Error.t) result

  val revision : t -> Wire_id.Revision.t
  val names : t -> string list
  val status : t -> status
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end
