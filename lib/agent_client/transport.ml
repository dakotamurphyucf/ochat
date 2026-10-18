module Developer_response_bound = struct
  type t =
    | Bootstrap
    | Installed of Agent_protocol.Developer_common.Limits.t

  let max_result_bytes = function
    | Bootstrap -> Document_schema.Limits.max_bytes Document_schema.Limits.default
    | Installed limits ->
      Document_schema.Limits.max_bytes
        (Agent_protocol.Developer_common.Limits.response limits)
  ;;

  let max_error_bytes = 64 * 1024

  let error_within_bound error =
    Agent_protocol.Error.to_json error
    |> Jsonaf.to_string
    |> String.length
    |> fun length -> length <= max_error_bytes
  ;;
end

type t =
  { request :
      Agent_protocol.Command.t
      -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result
  ; request_developer :
      bound:Developer_response_bound.t
      -> Agent_protocol.Developer_wire.Request.t
      -> (Jsonaf.t, Agent_protocol.Error.t) result
  ; next_notification : unit -> Agent_protocol.Envelope.t option
  ; close : unit -> unit
  }

let developer_unavailable ~bound:_ _ =
  Error (Agent_protocol.Error.invalid_request "Developer transport is unavailable")
;;

let create ~request ~next_notification ~close =
  { request; request_developer = developer_unavailable; next_notification; close }
;;

let create_with_developer ~request ~request_developer ~next_notification ~close =
  { request; request_developer; next_notification; close }
;;

let request t command = t.request command
let request_developer t ~bound command = t.request_developer ~bound command
let next_notification t = t.next_notification ()
let close t = t.close ()
