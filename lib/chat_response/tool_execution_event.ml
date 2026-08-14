type outcome = Ochat_function.Trace.outcome =
  | Returned
  | Raised
  | Cancelled

type agent_page_kind =
  | Subagent
  | Shell_script

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
