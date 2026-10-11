open Core
module Build = Chatml_project_build
module Accounting = Project_prepare_accounting

(** Immutable completed selection and whole-attempt receipt. Contains no clock,
    callbacks, admission ledger, native owner or publication capability. *)
type t

(** Private completion boundary: the caller supplies a successfully completed
    Build and nonnegative final receipt from its invocation-owned ledger. *)
val create : Build.t -> accounting:Accounting.t -> t

val build : t -> Build.t
val accounting : t -> Accounting.t
