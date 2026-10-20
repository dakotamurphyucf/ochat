open Core

(** A two-root native Project on the daemon's actual Source owner. The returned
    policy consults one test-only current read grant; it does not mutate a
    configured grant or bypass Host capture/currentness. *)
type t

val create : env:Eio_unix.Stdenv.base -> fixture:Config_fixture.t -> t
val authoring_policy : t -> Agent_server.Authoring_installation.Host_policy.t
val capture_policy : t -> Agent_server.Authoring_context_service.Developer.Target_policy.t
val revoke_dependency : t -> unit
val restore_dependency : t -> unit
val project : t -> Source.Portable_path.t
val primary_root : t -> string
val dependency_root : t -> string

(** Add only physical source roots to the existing fixture Server config.
    Installed read grants are supplied by [authoring_policy], so the same
    running daemon can check a changed trusted policy decision. *)
val install_sources : t -> env:Eio_unix.Stdenv.base -> fixture:Config_fixture.t -> unit
