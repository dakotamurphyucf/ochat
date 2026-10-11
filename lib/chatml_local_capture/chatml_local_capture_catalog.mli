open Core
module Budget = Chatml_local_capture_budget

(** Private wrapper pairs shared immutable facts with local acquisition counters.
    Acquisition alone constructs this wrapper after all shared validation succeeds. *)
type t

val create : Chatml_catalog.t -> accounting:Budget.Accounting.t -> t
val catalog : t -> Chatml_catalog.t
val accounting : t -> Budget.Accounting.t
