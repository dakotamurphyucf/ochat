open! Core
module P = Agent_protocol
module DTO = P.Prompt_dependency_selection

(** Pure projection of actual installed metadata. The caller supplies the state
    of the SAME watcher that owns publication. No filesystem or source access. *)
val status
  :  Prompt_catalog_host.t
  -> publication:Config_watcher.State.t
  -> (DTO.Status.t, P.Error.t) Result.t

(** Preallocate a response from authentic prepared history and exact target
    entries, before writing. Missing/duplicate/disabled targets reject. Unavailable
    source targets receive finite generic text; prompt.get retains their detailed
    diagnostics. Checks request affinity without guessing a next generation.
    Original checkpoint exceptions/cancellation propagate. *)
val prepared_update
  :  Prompt_catalog_host.Prepared.t
  -> request:DTO.Update_request.t
  -> checkpoint:(unit -> unit)
  -> (DTO.Update_result.t, P.Error.t) Result.t
