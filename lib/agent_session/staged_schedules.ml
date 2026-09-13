open Core
module P = Agent_protocol
module S = P.Schedule

type limits =
  { max_active : int
  ; max_per_source : int
  ; max_retained : int
  ; max_delay_ms : int
  ; max_payload_bytes : int
  ; max_payload_depth : int
  }

let default_limits =
  { max_active = 256
  ; max_per_source = 64
  ; max_retained = 4096
  ; max_delay_ms = 86_400_000
  ; max_payload_bytes = 65_536
  ; max_payload_depth = 64
  }
;;

let validate_limits limits =
  match
    limits.max_active > 0
    && limits.max_per_source > 0
    && limits.max_per_source <= limits.max_active
    && limits.max_retained >= limits.max_active
    && limits.max_delay_ms >= 0
    && limits.max_payload_bytes > 0
    && limits.max_payload_depth > 0
  with
  | true -> Ok ()
  | false -> Error (P.Error.invalid_request "invalid schedule limits")
;;

include Staged_mutations.Make (struct
    module Id = P.Id.Schedule

    type t = S.t

    let name = "schedule"
    let id value = value.S.id
    let equal a b = Jsonaf.exactly_equal (S.to_json a) (S.to_json b)
    let validate_transition = S.validate_transition

    let validate_staging value =
      match value.S.ownership with
      | Some _ -> Ok ()
      | None ->
        Error
          (P.Error.create
             Conflict
             ~message:"schedule has no creating moderator source"
             ~retryable:false
             ())
    ;;
  end)
