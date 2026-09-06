open Core

(** Runs the consolidated canonical-runtime E2E-18 matrix. *)
val run_runtime : Eio_unix.Stdenv.base -> case:string option -> unit

(** Runs the consolidated jobs, schedules, and orchestration E2E-19 matrix. *)
val run_background : Eio_unix.Stdenv.base -> case:string option -> unit

(** Runs the consolidated blob, export, audit, and retention E2E-20 matrix. *)
val run_data : Eio_unix.Stdenv.base -> case:string option -> unit

(** Runs the consolidated graceful restart E2E-21 matrix. *)
val run_persistence : Eio_unix.Stdenv.base -> case:string option -> unit

(** Runs the consolidated security attack E2E-24 matrix. *)
val run_security : Eio_unix.Stdenv.base -> case:string option -> unit
