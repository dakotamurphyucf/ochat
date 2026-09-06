(** Serialized, cancellable suggestion work. Never reads or mutates a UI model.
    At most one owned worker and one replaceable pending snapshot exist.
    Cancellation unwinds the previous switch before starting its replacement. *)

type snapshot =
  { identity : string
  ; epoch : int
  ; generation : int
  ; draft : string
  ; cursor : int
  ; input : Type_ahead_provider.input
  }

type event =
  | Ready of snapshot
  | Completed of snapshot * Type_ahead_provider.outcome

type t

val create
  :  sw:Eio.Switch.t
  -> sleep:(float -> unit)
  -> complete:
       (sw:Eio.Switch.t -> Type_ahead_provider.input -> Type_ahead_provider.outcome)
  -> emit:(event -> unit)
  -> t

(** [schedule t snapshot ~delay] emits [Ready] after debounce. The UI must
    recheck eligibility before calling [request]. *)
val schedule : t -> snapshot -> delay:float -> unit

val request : t -> snapshot -> unit
val cancel : t -> unit

(** [close t] cancels and joins all owned work. Idempotent. *)
val close : t -> unit
