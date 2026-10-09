open! Core
module J = Json_codec

module Key = struct
  type t =
    | Operation of Id.Operation.t
    | Retained of Session_work.Key.t
  [@@deriving compare, equal]

  let to_json = function
    | Operation id ->
      `Object [ "kind", `String "operation"; "id", Id.Operation.to_json id ]
    | Retained key ->
      `Object [ "kind", `String "retained"; "key", Session_work.Key.to_json key ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind kind = J.required_as f "kind" J.string in
    match kind with
    | "operation" ->
      let%map id = J.required_as f "id" Id.Operation.of_json in
      Operation id
    | "retained" ->
      let%map key = J.required_as f "key" Session_work.Key.of_json in
      Retained key
    | _ -> Error (Protocol_error.invalid_request "unsupported run work occurrence")
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end

type t =
  { key : Key.t
  ; generation : int
  }
[@@deriving compare, equal]

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    match t.key with
    | Key.Operation id ->
      Extension_codec.validate_id Id.Operation.to_json Id.Operation.of_json id
    | Retained (Session_work.Key.Job { id; attempt }) ->
      let%bind () = Extension_codec.validate_id Id.Job.to_json Id.Job.of_json id in
      if attempt < 0
      then Error (Protocol_error.invalid_request "negative run job attempt")
      else Ok ()
    | Retained (Schedule id) ->
      Extension_codec.validate_id Id.Schedule.to_json Id.Schedule.of_json id
    | Retained (Invocation id) ->
      Extension_codec.validate_id Id.Invocation.to_json Id.Invocation.of_json id
    | Retained (Subscription id) ->
      Extension_codec.validate_id Id.Subscription.to_json Id.Subscription.of_json id
    | Retained (Delivery id) ->
      Extension_codec.validate_id Id.Delivery.to_json Id.Delivery.of_json id
    | Retained (Moderator_execution id) ->
      Extension_codec.validate_id
        Id.Moderator_execution.to_json
        Id.Moderator_execution.of_json
        id
  in
  if t.generation < 0
  then Error (Protocol_error.invalid_request "negative run work generation")
  else Ok ()
;;

let create ~key ~generation =
  let t = { key; generation } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let to_json t =
  `Object [ "key", Key.to_json t.key; "generation", `Number (Int.to_string t.generation) ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind key = J.required_as f "key" Key.of_json in
  let%bind generation =
    J.required_as f "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  create ~key ~generation
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;

include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

module Terminal = struct
  type outcome =
    | Succeeded
    | Failed
    | Cancelled
    | Limited
    | Interrupted
    | Unconfirmed
  [@@deriving compare, equal, sexp]

  type work = t [@@deriving equal]

  type t =
    { work : work
    ; outcome : outcome
    ; revision : int64
    }
  [@@deriving equal]

  let outcome_to_json = function
    | Succeeded -> `String "succeeded"
    | Failed -> `String "failed"
    | Cancelled -> `String "cancelled"
    | Limited -> `String "limited"
    | Interrupted -> `String "interrupted"
    | Unconfirmed -> `String "unconfirmed"
  ;;

  let outcome_of_json =
    J.enum
      ~name:"run owned terminal outcome"
      [ "succeeded", Succeeded
      ; "failed", Failed
      ; "cancelled", Cancelled
      ; "limited", Limited
      ; "interrupted", Interrupted
      ; "unconfirmed", Unconfirmed
      ]
  ;;

  let work_to_json = to_json
  let work_of_json = of_json
  let validate_work = validate

  let validate t =
    let open Result.Let_syntax in
    let%bind () = validate_work t.work in
    if Int64.(t.revision < 0L)
    then Error (Protocol_error.invalid_request "negative run terminal work revision")
    else Ok ()
  ;;

  let create ~work ~outcome ~revision =
    let t = { work; outcome; revision } in
    Result.map (validate t) ~f:(fun () -> t)
  ;;

  let to_json t =
    `Object
      [ "work", work_to_json t.work
      ; "outcome", outcome_to_json t.outcome
      ; "revision", `String (Int64.to_string t.revision)
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind work = J.required_as f "work" work_of_json in
    let%bind outcome = J.required_as f "outcome" outcome_of_json in
    let%bind revision = J.required_as f "revision" History.Content_revision.of_json in
    create ~work ~outcome ~revision:(History.Content_revision.to_int64 revision)
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end
