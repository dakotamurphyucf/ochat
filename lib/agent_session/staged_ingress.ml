open Core
module P = Agent_protocol
module I = External_ingress

type limits =
  { max_active : int
  ; max_retained : int
  ; max_retained_bytes : int
  ; registration : I.limits
  }

let default_limits =
  { max_active = 64
  ; max_retained = 256
  ; max_retained_bytes = 4 * 1024 * 1024
  ; registration = I.default_limits
  }
;;

let validate_limits limits =
  let r = limits.registration in
  match
    limits.max_active >= 0
    && limits.max_retained >= limits.max_active
    && limits.max_retained_bytes >= 0
    && r.max_payload_bytes > 0
    && r.max_payload_depth > 0
    && r.max_receipts >= 0
    && r.rate_count >= 0
    && r.rate_window_ms > 0
  with
  | true -> Ok ()
  | false -> Error (P.Error.invalid_request "invalid shared ingress limits")
;;

module Base = Staged_mutations.Make (struct
    module Id = P.Id.Capability

    type t = I.t

    let name = "ingress registration"
    let id value = value.I.context.id
    let equal = I.equal
    let validate_staging = I.validate

    let validate_transition ~previous:_ _ =
      Error (P.Error.invalid_request "ingress staging requires a subscription validator")
    ;;
  end)

type t = Base.t

let create = Base.create
let is_empty = Base.is_empty
let find = Base.find
let values = Base.values
let abort = Base.abort
let release_owner = Base.release_owner
let abort_all = Base.abort_all

let validate lookup ~previous next =
  let open Result.Let_syntax in
  let%bind subscription =
    lookup next.I.context.subscription_id next.context.epoch
    |> Result.of_option
         ~error:(P.Error.invalid_request "ingress subscription epoch is not selected")
  in
  let%bind () =
    match previous with
    | None -> Ok ()
    | Some before when List.equal I.equal_receipt before.I.receipts next.receipts -> Ok ()
    | Some _ ->
      Error (P.Error.invalid_request "moderator staging cannot submit external events")
  in
  I.validate_transition ~subscription ~previous next
;;

let stage t ~owner ~previous ~next ~subscription =
  Base.stage_with_validation
    ~validate_transition:(validate (fun _ _ -> Some subscription))
    t
    ~owner
    ~previous
    ~next
;;

let select t ~owner ~receipts ~lookup ~subscription =
  Base.select_with_validation
    ~validate_transition:(validate subscription)
    t
    ~owner
    ~receipts
    ~lookup
;;

let selected t ~owner ~lookup ~subscription =
  Base.selected_with_validation
    ~validate_transition:(validate subscription)
    t
    ~owner
    ~lookup
;;

let ordered_changes ~subscriptions ~registrations =
  let changes, remaining =
    List.fold
      subscriptions
      ~init:([], registrations)
      ~f:(fun (changes, remaining) subscription ->
        let before, remaining =
          List.partition_tf remaining ~f:(fun registration ->
            P.Id.Subscription.equal
              registration.I.context.subscription_id
              subscription.P.Subscription.context.id
            && registration.context.epoch < subscription.epoch)
        in
        let changes =
          List.fold before ~init:changes ~f:(fun acc value ->
            Session_delta.Ingress_changed value :: acc)
        in
        Session_delta.Subscription_changed subscription :: changes, remaining)
  in
  List.rev changes
  @ List.map remaining ~f:(fun value -> Session_delta.Ingress_changed value)
;;

let check_capacity ~limits ~generation ~now ~subscriptions ~values =
  let table = Hashtbl.create (module P.Id.Capability) in
  List.iter values ~f:(fun value ->
    let id = value.I.context.id in
    let size = String.length (Sexp.to_string_mach (I.sexp_of_t value)) in
    let active =
      value.context.generation = generation
      && Option.is_none value.revoked
      && P.Timestamp.compare now value.context.expires_at < 0
      && List.exists subscriptions ~f:(fun subscription ->
        P.Id.Subscription.equal
          subscription.P.Subscription.context.id
          value.context.subscription_id
        && subscription.epoch = value.context.epoch
        && Option.is_none subscription.result)
    in
    Hashtbl.update table id ~f:(function
      | None -> size, active
      | Some (previous, was_active) -> Int.max previous size, was_active || active));
  let active = Hashtbl.count table ~f:snd in
  let remaining =
    Hashtbl.fold
      table
      ~init:(Some limits.max_retained_bytes)
      ~f:(fun ~key:_ ~data:(size, _) remaining ->
        match remaining with
        | Some left when left >= 8192 && size <= left - 8192 -> Some (left - 8192 - size)
        | _ -> None)
  in
  match
    Hashtbl.length table <= limits.max_retained
    && active <= limits.max_active
    && Option.is_some remaining
  with
  | true -> Ok ()
  | false ->
    Error
      (P.Error.create
         Resource_limit
         ~message:"shared ingress registration or retained-byte capacity exhausted"
         ~retryable:false
         ())
;;
