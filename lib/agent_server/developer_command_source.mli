open! Core

(** Private fresh-source bridge for the daemon-owned developer host. A stable
    requested route is comparison data; only a matching source/connection
    borrowed from the existing Authoring installation can prepare a Candidate.
    A historical same-key lookup never invokes this module. *)

(** Shared physical route binding for an original REPL epoch and a command.
    The fresh capability must match the stable route before source acquisition;
    the same Capture_host kernels perform all owner/contract checks. This does
    not create a command Candidate or grant interpreter effects. *)
val prepare_selection
  :  Authoring_context_service.Developer.t
  -> route:Developer_command_original.Route.t
  -> fresh:Developer_command_original.Fresh_source.t
  -> actor:Operator_authorization.t
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> ( Authoring_context_service.Developer.Source_selection.t
       , Agent_protocol.Error.t )
       result

val prepare
  :  Authoring_context_service.Developer.t
  -> original:Developer_command_original.t
  -> fresh:Developer_command_original.Fresh_source.t
  -> actor:Operator_authorization.t
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> (Authoring_context_service.Developer.Candidate.t, Agent_protocol.Error.t) result
