open Core

(** Run opt-in E2E-23 repair and migration cases. Journal/snapshot/index/schema
    mutations occur only after the real daemon exits, followed by child restart
    and protocol assertions. Legacy import and delegated permission/workspace
    cases retain their existing in-process setup. The missing-index case is a
    regression gate, not an expected-failure exemption. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
