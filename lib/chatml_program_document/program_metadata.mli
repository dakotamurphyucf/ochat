open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program

(** Private immutable structural projection. Byte ranges retain expected revision
    ownership; canonical coordinates are reconstructed only against exact sources. *)
type location =
  { source : Captured.Id.t
  ; digest : Captured.Digest.t
  ; start_byte : int
  ; end_byte : int
  }

type source =
  { id : Captured.Id.t
  ; digest : Captured.Digest.t
  ; byte_length : int
  }

type definition =
  { implementation : Captured.Id.t
  ; interface : Captured.Id.t option
  ; declared_at : location
  }

type binding =
  { name : string
  ; implementation : Captured.Id.t
  ; declared_at : location
  }

type scope =
  { source : Captured.Id.t
  ; selected_at : location option
  ; bindings : binding list
  }

type t =
  { fingerprint : Captured.Digest.t
  ; compiler_contract : Captured.Digest.t
  ; environment_name : string
  ; required_contract : Captured.Digest.t
  ; environment_declared_at : location option
  ; provenance : Prepared.Provenance.t
  ; entry : Captured.Id.t
  ; sources : source list
  ; definitions : definition list
  ; scopes : scope list
  }

val decode
  :  Document_schema.Json.t
  -> checkpoint:(unit -> unit)
  -> (t, Document_schema.Error.t) Result.t
