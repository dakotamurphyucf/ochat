open! Core
module Error = Protocol_error
module J = Json_codec

module Key = struct
  type t =
    | Job of
        { id : Id.Job.t
        ; attempt : int
        }
    | Schedule of Id.Schedule.t
    | Invocation of Id.Invocation.t
    | Subscription of Id.Subscription.t
    | Delivery of Id.Delivery.t
    | Moderator_execution of Id.Moderator_execution.t
  [@@deriving compare, equal, sexp]

  let to_json = function
    | Job { id; attempt } ->
      `Object
        [ "kind", `String "job"
        ; "id", Id.Job.to_json id
        ; "attempt", `Number (Int.to_string attempt)
        ]
    | Schedule id -> `Object [ "kind", `String "schedule"; "id", Id.Schedule.to_json id ]
    | Invocation id ->
      `Object [ "kind", `String "invocation"; "id", Id.Invocation.to_json id ]
    | Subscription id ->
      `Object [ "kind", `String "subscription"; "id", Id.Subscription.to_json id ]
    | Delivery id -> `Object [ "kind", `String "delivery"; "id", Id.Delivery.to_json id ]
    | Moderator_execution id ->
      `Object
        [ "kind", `String "moderator_execution"; "id", Id.Moderator_execution.to_json id ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind kind = J.required_as f "kind" J.string in
    match kind with
    | "job" ->
      let%bind id = J.required_as f "id" Id.Job.of_json in
      let%map attempt =
        J.required_as f "attempt" (J.bounded_int ~min:0 ~max:Int.max_value)
      in
      Job { id; attempt }
    | "schedule" ->
      J.required_as f "id" Id.Schedule.of_json |> Result.map ~f:(fun id -> Schedule id)
    | "invocation" ->
      J.required_as f "id" Id.Invocation.of_json
      |> Result.map ~f:(fun id -> Invocation id)
    | "subscription" ->
      J.required_as f "id" Id.Subscription.of_json
      |> Result.map ~f:(fun id -> Subscription id)
    | "delivery" ->
      J.required_as f "id" Id.Delivery.of_json |> Result.map ~f:(fun id -> Delivery id)
    | "moderator_execution" ->
      J.required_as f "id" Id.Moderator_execution.of_json
      |> Result.map ~f:(fun id -> Moderator_execution id)
    | _ -> Error (Error.invalid_request "unsupported work kind")
  ;;

  let t_of_sexp sexp =
    let raw = t_of_sexp sexp in
    match of_json (to_json raw) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end

module Status = struct
  type t =
    | Accepted
    | Running
    | Waiting_approval
    | Waiting_work
    | Succeeded
    | Failed
    | Cancelled
    | Interrupted
    | Unsupported
  [@@deriving compare, equal, sexp]

  let values =
    [ "accepted", Accepted
    ; "running", Running
    ; "waiting_approval", Waiting_approval
    ; "waiting_work", Waiting_work
    ; "succeeded", Succeeded
    ; "failed", Failed
    ; "cancelled", Cancelled
    ; "interrupted", Interrupted
    ; "unsupported", Unsupported
    ]
  ;;

  let to_json = function
    | Accepted -> `String "accepted"
    | Running -> `String "running"
    | Waiting_approval -> `String "waiting_approval"
    | Waiting_work -> `String "waiting_work"
    | Succeeded -> `String "succeeded"
    | Failed -> `String "failed"
    | Cancelled -> `String "cancelled"
    | Interrupted -> `String "interrupted"
    | Unsupported -> `String "unsupported"
  ;;

  let of_json = J.enum ~name:"work status" values
end

module Delivery_state = struct
  type t =
    | Not_applicable
    | Pending
    | Acknowledged
    | Discarded
  [@@deriving compare, equal, sexp]

  let to_json = function
    | Not_applicable -> `String "not_applicable"
    | Pending -> `String "pending"
    | Acknowledged -> `String "acknowledged"
    | Discarded -> `String "discarded"
  ;;

  let of_json =
    J.enum
      ~name:"work delivery"
      [ "not_applicable", Not_applicable
      ; "pending", Pending
      ; "acknowledged", Acknowledged
      ; "discarded", Discarded
      ]
  ;;
end

type t =
  { session : Session_ref.t
  ; generation : int
  ; key : Key.t
  ; status : Status.t
  ; delivery : Delivery_state.t
  ; revision : int64
  }

let create ~session ~generation ~key ~status ~delivery ~revision =
  let open Result.Let_syntax in
  let%bind () =
    match key with
    | Key.Job { attempt; _ } when attempt < 0 ->
      Error (Error.invalid_request "negative work attempt")
    | Job _
    | Schedule _
    | Invocation _
    | Subscription _
    | Delivery _
    | Moderator_execution _ -> Ok ()
  in
  if generation < 0 || Int64.(revision < 0L)
  then Error (Error.invalid_request "negative work occurrence")
  else Ok { session; generation; key; status; delivery; revision }
;;

let compare_key left right = Key.compare left.key right.key

let to_json t =
  `Object
    [ "session", Session_ref.to_json t.session
    ; "generation", `Number (Int.to_string t.generation)
    ; "key", Key.to_json t.key
    ; "status", Status.to_json t.status
    ; "delivery", Delivery_state.to_json t.delivery
    ; "revision", `String (Int64.to_string t.revision)
    ]
;;

let revision_of_json = function
  | `String encoded ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value >= 0L) && String.equal (Int64.to_string value) encoded
       -> Ok value
     | Some _ | None -> Error (Error.invalid_request "invalid work revision"))
  | _ -> Error (Error.invalid_request "work revision must be a decimal string")
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind session = J.required_as f "session" Session_ref.of_json in
  let%bind generation =
    J.required_as f "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind key = J.required_as f "key" Key.of_json in
  let%bind status = J.required_as f "status" Status.of_json in
  let%bind delivery = J.required_as f "delivery" Delivery_state.of_json in
  let%bind revision = J.required_as f "revision" revision_of_json in
  create ~session ~generation ~key ~status ~delivery ~revision
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;

module Query = struct
  type t =
    { session : Session_ref.t
    ; page : Page.Request.t
    }

  let to_json t =
    `Object (("session", Session_ref.to_json t.session) :: Page.Request.to_fields t.page)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind session = J.required_as f "session" Session_ref.of_json in
    let%map page = Page.Request.of_fields f in
    { session; page }
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end
