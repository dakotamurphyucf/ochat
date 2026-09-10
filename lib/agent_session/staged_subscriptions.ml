open Core
module P = Agent_protocol
module S = P.Subscription

type limits =
  { max_active : int
  ; max_retained : int
  ; default_lifetime_ms : int
  ; max_lifetime_ms : int
  }

let default_limits =
  { max_active = 64
  ; max_retained = 4096
  ; default_lifetime_ms = 3_600_000
  ; max_lifetime_ms = 86_400_000
  }
;;

let validate_limits limits =
  match
    limits.max_active > 0
    && limits.max_active <= 1024
    && limits.max_retained >= limits.max_active
    && limits.default_lifetime_ms > 0
    && limits.default_lifetime_ms <= limits.max_lifetime_ms
    && limits.max_lifetime_ms <= 86_400_000
  with
  | true -> Ok ()
  | false -> Error (P.Error.invalid_request "invalid subscription limits")
;;

include Staged_mutations.Make (struct
    module Id = P.Id.Subscription

    type t = S.t

    let name = "subscription"
    let id value = value.S.context.id
    let equal = S.equal
    let validate_transition = S.validate_transition

    let validate_staging value =
      match value.S.context.source with
      | Some _ -> Ok ()
      | None ->
        Error
          (P.Error.create
             Conflict
             ~message:"subscription has no creating moderator source"
             ~retryable:false
             ())
    ;;
  end)
