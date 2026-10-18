module Developer_response_bound = struct
  type t =
    | Bootstrap
    | Installed of Agent_protocol.Developer_common.Limits.t

  let max_result_bytes = function
    | Bootstrap -> Agent_protocol.Developer_common.Capabilities.max_json_bytes
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

module Repl_response_bound = struct
  type t =
    | Bootstrap
    | Installed of Agent_protocol.Developer_repl_wire.Limits.t

  let max_result_bytes = function
    | Bootstrap -> 64 * 1024
    | Installed limits ->
      Document_schema.Limits.max_bytes
        (Agent_protocol.Developer_repl_wire.Limits.response limits)
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
  ; request_repl :
      max_request_bytes:int
      -> bound:Repl_response_bound.t
      -> Agent_protocol.Developer_repl_wire.Request.t
      -> (Jsonaf.t, Agent_protocol.Error.t) result
  ; next_notification : unit -> Agent_protocol.Envelope.t option
  ; close : unit -> unit
  }

let developer_unavailable ~bound:_ _ =
  Error (Agent_protocol.Error.invalid_request "Developer transport is unavailable")
;;

let repl_unavailable ~max_request_bytes:_ ~bound:_ _ =
  Error (Agent_protocol.Error.invalid_request "Developer REPL transport is unavailable")
;;

let create ~request ~next_notification ~close =
  { request
  ; request_developer = developer_unavailable
  ; request_repl = repl_unavailable
  ; next_notification
  ; close
  }
;;

let create_with_developer ~request ~request_developer ~next_notification ~close =
  { request
  ; request_developer
  ; request_repl = repl_unavailable
  ; next_notification
  ; close
  }
;;

let create_with_developer_repl
      ~request
      ~request_developer
      ~request_repl
      ~next_notification
      ~close
  =
  { request; request_developer; request_repl; next_notification; close }
;;

let request t command = t.request command
let request_developer t ~bound command = t.request_developer ~bound command

let request_repl t ~max_request_bytes ~bound request =
  t.request_repl ~max_request_bytes ~bound request
;;

let next_notification t = t.next_notification ()
let close t = t.close ()
