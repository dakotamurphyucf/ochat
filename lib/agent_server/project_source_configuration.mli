open Core

(** Administrative declarations only, never decoded from authored project files.
    The exposed representation is unadmitted; every resource-opening entry point
    calls validate even when a trusted embedder constructed the record directly.
    Administrative relative paths are resolved by Config_validator against its
    actual configuration source directory before construction. *)
module Repository : sig
  type t =
    | Https of
        { repository : string
        ; authorization_header_file : string option
        }
    | Local of string
  [@@deriving compare, equal, sexp]
end

module Git_deployment : sig
  type t =
    { helper_directory : string
    ; ca_bundle : string option
    ; memory_bytes_per_process : int
    ; repositories : Repository.t list
    }
  [@@deriving compare, equal, sexp]
end

type t =
  { external_roots : string list
  ; git : Git_deployment.t option
  }
[@@deriving compare, equal, sexp]

val local_only : t

(** Fixed administrative admission ceilings, independent of source/graph bounds.
    Config_validator checks these BEFORE resolving/copying individual values:
    32 external roots, 128 exact Git grants, 4096 bytes per administrative path or
    route, 64KiB summed authority metadata; credential bodies have a separate
    4096-byte live read ceiling. Counts/checked sums precede derived map/list
    construction. Paths must be absolute and NUL-free after config resolution;
    HTTPS route rules reuse Git.Grant syntax admission without network IO.
    Duplicate exact roots/routes reject. Native affinity/real filesystem/helper
    admission belongs to open_owned, not pure configuration validation.
    Errors are bounded field diagnostics, never file contents/credentials. *)
val maximum_path_bytes : int

val maximum_external_roots : int
val maximum_git_grants : int
val maximum_authority_bytes : int
val maximum_authorization_header_bytes : int

(** Count and byte admission before administrative path resolution. Allows
    relative path declarations; performs no filesystem work. Final [validate]
    independently repeats this admission for directly constructed records. *)
val validate_raw : t -> unit Or_error.t

val validate : t -> unit Or_error.t
