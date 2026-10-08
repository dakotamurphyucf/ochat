open! Core

(** Explicit synthetic protected-key provisioning under the fixture's private
    HOME before daemon/local-stdio process launch. Existing authority is borrowed
    unchanged; opening never reenrolls or reenables an existing binding. No ambient
    credentials are read and no provider network request is made. *)
val provision : env:Eio_unix.Stdenv.base -> ?key:string -> Config_fixture.t -> unit
