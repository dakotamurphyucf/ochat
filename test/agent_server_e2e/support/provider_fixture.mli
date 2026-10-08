open! Core

(** Owns the real trusted application platform and operator composition under
    the fixture's private HOME. Missing setup receives explicit setup and one
    protected synthetic-key enrollment. Existing authority is borrowed unchanged,
    including disabled/missing bindings; it is never reenrolled or repaired.
    The callback borrows the host until it returns. Closing joins runtime workers
    before the provider switch drains. No ambient credentials or provider requests.
    Expected setup failures raise only finite redacted typed diagnostics. *)
val with_host
  :  env:Eio_unix.Stdenv.base
  -> ?api_url:string
  -> ?key:string
  -> Config_fixture.t
  -> (Inference_host.t -> 'a)
  -> 'a

(** Explicit synthetic provisioning before daemon/local-stdio process launch. *)
val provision : env:Eio_unix.Stdenv.base -> ?key:string -> Config_fixture.t -> unit
