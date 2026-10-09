open! Core

(** Explicit trusted host filename ingress. Bounded read precedes structural
    decoding; missing/bad files reject without silently dropping declarations.
    Cancellation propagates. No credential or auth identity is loaded. *)
val load
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> ( Inference_host.Compatible_profile.t list
       , Agent_protocol.Provider_operator.Error.t )
       Result.t
