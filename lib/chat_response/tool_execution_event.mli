type outcome = Ochat_function.Trace.outcome =
  | Returned
  | Raised
  | Cancelled

type agent_page_kind =
  | Subagent
  | Shell_script
  (** Classification for top-level calls displayed by the Chat-TUI Agent page.
    OpenAI function/custom call kinds do not encode this declaration
    provenance. *)

type t =
  | Started of
      { call_id : string
      ; name : string
      ; kind : [ `Function | `Custom ]
      ; payload : string
      }
  | Progress of
      { call_id : string
      ; progress : Ochat_function.Progress.t
      }
  | Finished of
      { call_id : string
      ; outcome : outcome
      ; output : Openai.Responses.Tool_output.Output.t option
      }
  | Trace of
      { call_id : string
      ; trace : Ochat_function.Trace.t
      }
  (** Transient execution metadata for a tool invocation.

    Values must not be added to canonical response history or used as final
    tool output. [Started.payload] is the exact function arguments or custom
    tool input passed to the runner. [Finished.output] and [Trace] are
    non-authoritative display projections. *)

(** Explicit producer presentation seam. Runtime trace keeps its existing DTO;
    public/TUI models retain this neutral value after ingress. *)
val neutral_output
  :  Openai.Responses.Tool_output.Output.t
  -> History_entry.Payload.Output.t
