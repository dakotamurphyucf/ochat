open! Core

module Limits = struct
  type t = { max_active_calls : int }

  let create ~max_active_calls =
    if max_active_calls < 1 || max_active_calls > 64
    then Or_error.error_string "active selected Agent calls must be between 1 and 64"
    else Ok { max_active_calls }
  ;;

  let default = { max_active_calls = 8 }
  let max_active_calls t = t.max_active_calls
end

module Active = struct
  type t =
    { limits : Limits.t
    ; current : int
    }

  let start limits = { limits; current = 1 }

  let descend t =
    if t.current >= Limits.max_active_calls t.limits
    then
      Error
        (Agent_protocol.Error.create
           Agent_protocol.Error.Resource_limit
           ~message:"active selected Agent-call limit reached"
           ~retryable:false
           ())
    else Ok { t with current = t.current + 1 }
  ;;

  let current t = t.current
  let limits t = t.limits
end
