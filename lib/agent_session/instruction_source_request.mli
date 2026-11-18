open! Core
module Content = Instruction_content

(** Bounded source references supplied by a caller. No bytes, permission or
    activation authority can be constructed here. *)
module Selection : sig
  type t =
    | Project of
        { root : Content.Root_id.t
        ; scope : Content.Scope.t
        }
    | Skills of Content.Root_id.t list

  val validate : t -> (unit, Agent_protocol.Error.t) result
  val roots : t -> Content.Root_id.t list
end

module Member : sig
  type t = private
    { root : Content.Root_id.t
    ; member : Source.Portable_path.t
    ; digest : Chatml_source.Digest.t
    }

  val create
    :  root:Content.Root_id.t
    -> member:Source.Portable_path.t
    -> digest:Chatml_source.Digest.t
    -> t

  val of_observed : Content.Observed.t -> t
  val equal : t -> t -> bool
end

type t

(** Project members are the complete expected root-to-target chain, possibly
    empty. Skills are a nonempty distinct ordered subset of the explicit roots.
    Only a real source facade may establish completeness/current contents. *)
val create
  :  selection:Selection.t
  -> members:Member.t list
  -> (t, Agent_protocol.Error.t) result

val selection : t -> Selection.t
val members : t -> Member.t list
val matches : t -> Instruction_capture.t -> bool

module Discovery : sig
  type t

  val create
    :  roots:Content.Root_id.t list
    -> observations:Content.Observed.t list
    -> issues:(Content.Root_id.t * Source.Portable_path.t * string) list
    -> (t, Agent_protocol.Error.t) result

  val roots : t -> Content.Root_id.t list
  val observations : t -> Content.Observed.t list
  val issues : t -> (Content.Root_id.t * Source.Portable_path.t * string) list
end
