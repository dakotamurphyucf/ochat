open Core

(** Private wire owner. Bounded JSON validation precedes one whole-source location
    capture; field origins share it. Conversion owns its invocation checkpoint and
    returns immutable declarations without retaining JSON or callbacks. *)
val decode
  :  Chatml_source.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_project_declaration.t, Chatml_project_declaration.Error.t) Result.t

(** Existing invocation-local declaration reader, shared by the actual lock codec.
    Remains private to this library; values returned by either codec retain no t. *)
module Reader : sig
  type t =
    { origin : Chatml_project_declaration.Declared_at.t
    ; checkpoint : unit -> unit
    }

  val origin : t -> string list -> Chatml_project_declaration.Declared_at.t

  val fail
    :  t
    -> string list
    -> string
    -> ('a, Chatml_project_declaration.Error.t) Result.t

  val object_fields
    :  t
    -> string list
    -> allowed:string list
    -> Jsonaf.t
    -> ((string * Jsonaf.t) list, Chatml_project_declaration.Error.t) Result.t

  val required
    :  t
    -> string list
    -> (string * Jsonaf.t) list
    -> string
    -> decode:
         (t
          -> string list
          -> Jsonaf.t
          -> ('a, Chatml_project_declaration.Error.t) Result.t)
    -> ('a, Chatml_project_declaration.Error.t) Result.t

  val optional
    :  t
    -> string list
    -> (string * Jsonaf.t) list
    -> string
    -> decode:
         (t
          -> string list
          -> Jsonaf.t
          -> ('a, Chatml_project_declaration.Error.t) Result.t)
    -> ('a option, Chatml_project_declaration.Error.t) Result.t

  val nonempty_string
    :  t
    -> string list
    -> Jsonaf.t
    -> (string, Chatml_project_declaration.Error.t) Result.t

  val module_name
    :  t
    -> string list
    -> Jsonaf.t
    -> (string, Chatml_project_declaration.Error.t) Result.t

  val array
    :  t
    -> string list
    -> decode:
         (t
          -> string list
          -> Jsonaf.t
          -> ('a, Chatml_project_declaration.Error.t) Result.t)
    -> Jsonaf.t
    -> ('a list, Chatml_project_declaration.Error.t) Result.t
end

module Decode : sig
  val dependency_source
    :  Reader.t
    -> string list
    -> Jsonaf.t
    -> ( Chatml_project_declaration.Dependency.source
         , Chatml_project_declaration.Error.t )
         Result.t
end
