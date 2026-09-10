open Core
module P = Agent_protocol

type limits =
  { max_pending : int
  ; max_per_source : int
  ; max_retained : int
  ; max_payload_bytes : int
  ; max_payload_depth : int
  }

let default_limits =
  { max_pending = 256
  ; max_per_source = 64
  ; max_retained = 4096
  ; max_payload_bytes = 65_536
  ; max_payload_depth = 64
  }
;;

let validate_limits limits =
  match
    limits.max_pending > 0
    && limits.max_per_source > 0
    && limits.max_per_source <= limits.max_pending
    && limits.max_retained >= limits.max_pending
    && limits.max_payload_bytes > 0
    && limits.max_payload_depth > 0
  with
  | true -> Ok ()
  | false -> Error (P.Error.invalid_request "invalid notification limits")
;;

include Staged_mutations.Make (struct
    module Id = P.Id.Delivery

    type t = P.Delivery.t

    let name = "notification"
    let id value = value.P.Delivery.context.id
    let equal a b = Jsonaf.exactly_equal (P.Delivery.to_json a) (P.Delivery.to_json b)
    let validate_transition = P.Delivery.validate_transition

    let validate_staging value =
      match value.P.Delivery.context.ownership, value.status with
      | Some _, Pending -> Ok ()
      | _ ->
        Error
          (P.Error.invalid_request
             "notification staging requires an owned pending intent")
    ;;
  end)
