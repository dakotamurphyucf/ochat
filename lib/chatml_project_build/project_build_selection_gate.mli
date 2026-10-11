open Core
module Model = Project_build_model
module Project = Chatml_project

(** Immutable previous-selection policy, decoded once per preparation. No host
    acquisition or live authority. Hosts call [check_dependency] before each
    acquisition; complete composition repeats the checks against admitted facts. *)
type t

val create
  :  Model.Operation.t
  -> document:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Model.Error.t) Result.t

val check_root
  :  t
  -> origin:Project.Lock.Catalog.origin
  -> project:Source.Portable_path.t
  -> budget:Project_build_budget.t
  -> (unit, Model.Error.t) Result.t

val previous_lock : t -> Project.Lock.t option

(** Initial/update operations admit explicit full pins. Build requires a previous
    owner-local association and matching Git repository/format/full pin/project
    member. Explicit environment/namespace selections must match; omitted ones
    are additionally reconciled against the actual target declaration later.
    Replacing a previously selected Git dependency with a local dependency also
    requires update. Fresh/changed local dependencies may otherwise be recaptured.
    Irrelevant removed dependencies do not require acquisition or reappearance.
    Checkpoints propagate; no callback is retained. *)
val check_dependency
  :  t
  -> owner:Project.t
  -> owner_origin:Project.Lock.Catalog.origin
  -> Project.Dependency.t
  -> budget:Project_build_budget.t
  -> (unit, Model.Error.t) Result.t

val preserves_git_selections : t -> bool

(** After acquisition, normal builds must retain the exact pinned Git source
    inventory, including declaration and optional companion selections. A changed
    Git origin requires Update; changed bytes under the same pin reject as a
    receipt mismatch. Local captures may refresh. This checks expected metadata,
    not native/Git provenance, which belongs to the acquisition owner. *)
val validate_captures
  :  t
  -> budget:Project_build_budget.t
  -> root:Model.Catalog_input.t
  -> catalogs:Model.Catalog_input.t list
  -> (unit, Model.Error.t) Result.t
