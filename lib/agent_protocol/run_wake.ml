open! Core
module J = Json_codec

module Occurrence = struct
  type t =
    | Job_completion of
        { job_id : Id.Job.t
        ; attempt : int
        }
    | Delivered_timer of
        { schedule_id : Id.Schedule.t
        ; delivery_count : int
        ; creator : Job.launch_owner
        ; subscription : (Id.Subscription.t * int) option
        }
    | Subscription_delivery of
        { subscription_id : Id.Subscription.t
        ; epoch : int
        ; delivery_id : Id.Delivery.t
        ; creator : Job.launch_owner
        }
  [@@deriving equal]

  let creator_to_json = function
    | Job.Invocation id ->
      `Object [ "kind", `String "invocation"; "id", Id.Invocation.to_json id ]
    | Moderator_event id ->
      `Object
        [ "kind", `String "moderator_event"; "id", Id.Moderator_execution.to_json id ]
  ;;

  let creator_of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind kind = J.required_as f "kind" J.string in
    match kind with
    | "invocation" ->
      let%map id = J.required_as f "id" Id.Invocation.of_json in
      Job.Invocation id
    | "moderator_event" ->
      let%map id = J.required_as f "id" Id.Moderator_execution.of_json in
      Job.Moderator_event id
    | _ -> Error (Protocol_error.invalid_request "unsupported run wake creator")
  ;;

  let creator_validate = function
    | Job.Invocation id ->
      Extension_codec.validate_id Id.Invocation.to_json Id.Invocation.of_json id
    | Moderator_event id ->
      Extension_codec.validate_id
        Id.Moderator_execution.to_json
        Id.Moderator_execution.of_json
        id
  ;;

  let validate = function
    | Job_completion { job_id; attempt } ->
      let open Result.Let_syntax in
      let%bind () = Extension_codec.validate_id Id.Job.to_json Id.Job.of_json job_id in
      if attempt < 0
      then Error (Protocol_error.invalid_request "negative run wake attempt")
      else Ok ()
    | Delivered_timer { schedule_id; delivery_count; creator; subscription } ->
      let open Result.Let_syntax in
      let%bind () =
        Extension_codec.validate_id Id.Schedule.to_json Id.Schedule.of_json schedule_id
      in
      let%bind () = creator_validate creator in
      let%bind () =
        match subscription with
        | None -> Ok ()
        | Some (id, epoch) ->
          let%bind () =
            Extension_codec.validate_id Id.Subscription.to_json Id.Subscription.of_json id
          in
          if epoch < 0
          then Error (Protocol_error.invalid_request "negative run subscription epoch")
          else Ok ()
      in
      if delivery_count < 1
      then
        Error
          (Protocol_error.invalid_request
             "run timer wake must identify a delivered occurrence")
      else Ok ()
    | Subscription_delivery { subscription_id; epoch; delivery_id; creator } ->
      let open Result.Let_syntax in
      let%bind () =
        Extension_codec.validate_id
          Id.Subscription.to_json
          Id.Subscription.of_json
          subscription_id
      in
      let%bind () =
        Extension_codec.validate_id Id.Delivery.to_json Id.Delivery.of_json delivery_id
      in
      let%bind () = creator_validate creator in
      if epoch < 0
      then Error (Protocol_error.invalid_request "negative run subscription epoch")
      else Ok ()
  ;;

  let to_json = function
    | Job_completion { job_id; attempt } ->
      `Object
        [ "kind", `String "job_completion"
        ; "job_id", Id.Job.to_json job_id
        ; "attempt", `Number (Int.to_string attempt)
        ]
    | Delivered_timer { schedule_id; delivery_count; creator; subscription } ->
      `Object
        ([ "kind", `String "delivered_timer"
         ; "schedule_id", Id.Schedule.to_json schedule_id
         ; "delivery_count", `Number (Int.to_string delivery_count)
         ; "creator", creator_to_json creator
         ]
         @ Projection_codec.optional "subscription" subscription (fun (id, epoch) ->
           `Object
             [ "id", Id.Subscription.to_json id; "epoch", `Number (Int.to_string epoch) ])
        )
    | Subscription_delivery { subscription_id; epoch; delivery_id; creator } ->
      `Object
        [ "kind", `String "subscription_delivery"
        ; "subscription_id", Id.Subscription.to_json subscription_id
        ; "epoch", `Number (Int.to_string epoch)
        ; "delivery_id", Id.Delivery.to_json delivery_id
        ; "creator", creator_to_json creator
        ]
  ;;

  let nonnegative = J.bounded_int ~min:0 ~max:Int.max_value

  let subscription_of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind id = J.required_as f "id" Id.Subscription.of_json in
    let%map epoch = J.required_as f "epoch" nonnegative in
    id, epoch
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind kind = J.required_as f "kind" J.string in
    match kind with
    | "job_completion" ->
      let%bind job_id = J.required_as f "job_id" Id.Job.of_json in
      let%map attempt = J.required_as f "attempt" nonnegative in
      Job_completion { job_id; attempt }
    | "delivered_timer" ->
      let%bind schedule_id = J.required_as f "schedule_id" Id.Schedule.of_json in
      let%bind delivery_count =
        J.required_as f "delivery_count" (J.bounded_int ~min:1 ~max:Int.max_value)
      in
      let%bind creator = J.required_as f "creator" creator_of_json in
      let%map subscription = J.optional_as f "subscription" subscription_of_json in
      Delivered_timer { schedule_id; delivery_count; creator; subscription }
    | "subscription_delivery" ->
      let%bind subscription_id =
        J.required_as f "subscription_id" Id.Subscription.of_json
      in
      let%bind epoch = J.required_as f "epoch" nonnegative in
      let%bind delivery_id = J.required_as f "delivery_id" Id.Delivery.of_json in
      let%map creator = J.required_as f "creator" creator_of_json in
      Subscription_delivery { subscription_id; epoch; delivery_id; creator }
    | _ -> Error (Protocol_error.invalid_request "unsupported run wake occurrence")
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end

type t =
  { run_id : Id.Run.t
  ; source : Run_source.t
  ; occurrence : Occurrence.t
  }
[@@deriving equal]

let validate t =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_id Id.Run.to_json Id.Run.of_json t.run_id in
  let%bind () = Run_source.validate t.source in
  Occurrence.validate t.occurrence
;;

let create ~run_id ~source ~occurrence =
  let t = { run_id; source; occurrence } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let to_json t =
  `Object
    [ "run_id", Id.Run.to_json t.run_id
    ; "source", Run_source.to_json t.source
    ; "occurrence", Occurrence.to_json t.occurrence
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind run_id = J.required_as f "run_id" Id.Run.of_json in
  let%bind source = J.required_as f "source" Run_source.of_json in
  let%bind occurrence = J.required_as f "occurrence" Occurrence.of_json in
  create ~run_id ~source ~occurrence
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;
