open Core
module Project = Chatml_project
module Path = Source.Portable_path

(** Pure owner-relative source intent. No locator is opened and no origin grants
    native authority. Source IDs remain relative to their selected package root. *)
module Error : sig
  type t =
    | Invalid_repository_path
    | Repository_escape
  [@@deriving equal, sexp_of]
end

type t

(** Explicit Git selects its repository root. A relative Local locator owned by
    Git resolves lexically from the repository declaration's containing directory
    and stays in that same repository/pin. Dot/parent components are interpreted;
    crossing the repository root rejects. Other components use existing portable
    path validation. Absolute POSIX Local locators and Local-owned relative
    locators select Local; their native admission remains the host's responsibility.

    The requested project member stays relative to the newly selected package
    root. Repository paths have the existing 1024-byte portable bound. All scans
    poll; checkpoint exceptions propagate unchanged. No callback is retained. *)
val resolve
  :  Project.Dependency.t
  -> owner_origin:Project.Lock.Catalog.origin
  -> owner_declaration:Chatml_source.Id.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val origin : t -> Project.Lock.Catalog.origin
val project : t -> Path.t

(** Compare resolved immutable intent with a selected target. Namespace and own
    environment agreement remain composition-owned. No native identity proof. *)
val matches
  :  t
  -> origin:Project.Lock.Catalog.origin
  -> declaration:Chatml_source.Id.t
  -> checkpoint:(unit -> unit)
  -> bool

(** Translate a package-relative member into an exact repository tree member.
    None is the repository root. Does not rewrite the logical Source.Id. *)
val repository_member
  :  package_root:Path.t option
  -> member:Path.t
  -> checkpoint:(unit -> unit)
  -> (Path.t, Error.t) Result.t
