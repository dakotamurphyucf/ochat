(** Bounded discovery and validation of the physically owned private journal.
    The publication lock and scoped collector are retained by the caller. *)
open! Core

type entry =
  { filename : string
  ; record : Recovery_asset_record.t
  ; committed : bool
  }

type t

val intent_count : Retention_reader.t -> (int, Store_error.t) result
val has_entries : Retention_reader.t -> (bool, Store_error.t) result

val read
  :  Retention_reader.t
  -> owner:Agent_protocol.Session_ref.t
  -> (t, Store_error.t) result

val entries : t -> entry list

(** Remove interrupted atomic journal temporaries after bounded preflight.
    Their renamed final is the only publication authority. An interrupted
    intent write cannot authorize any staging. The caller already proved this
    inventory under the same physical owner and Blob retention scope. *)
val remove_temporaries
  :  t
  -> env:Eio_unix.Stdenv.base
  -> reader:Retention_reader.t
  -> (unit, Store_error.t) result
