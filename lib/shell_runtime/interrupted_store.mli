open! Core

(** Reconstructs non-resumable interrupted-request metadata from the durable
    session audit log. This module never retries or executes a command. *)

val refresh
  :  env:Eio_unix.Stdenv.base
  -> session:Session.t
  -> (Session.t, string) result
