open! Core
module Error = Protocol_error

module Revision = struct
  type t = int64 [@@deriving compare, equal, sexp]

  let zero = 0L

  let of_int64 value =
    if Int64.(value < 0L)
    then Error (Error.invalid_request "pending revision must be nonnegative")
    else Ok value
  ;;

  let to_int64 t = t

  let succ t =
    if Int64.equal t Int64.max_value
    then Error (Error.invalid_request "pending revision is exhausted")
    else Ok Int64.(t + 1L)
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    match of_int64 (unchecked_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;

  let to_json t = `String (Int64.to_string t)

  let of_json json =
    let open Result.Let_syntax in
    let%bind value =
      History.Content_revision.of_json json
      |> Result.map ~f:History.Content_revision.to_int64
    in
    of_int64 value
  ;;
end

module Timing = struct
  type t =
    | Safe_boundary
    | After_current_operation
  [@@deriving equal, sexp]

  let to_json = function
    | Safe_boundary -> `String "safe_boundary"
    | After_current_operation -> `String "after_current_operation"
  ;;

  let of_json =
    Json_codec.enum
      ~name:"pending input timing"
      [ "safe_boundary", Safe_boundary
      ; "after_current_operation", After_current_operation
      ]
  ;;
end

module Terminal_proof = struct
  type outcome =
    | Completed
    | Cancelled
    | Failed
    | Interrupted
  [@@deriving equal, sexp]

  type t =
    { operation_id : Id.Operation.t
    ; generation : int
    ; outcome : outcome
    }
  [@@deriving equal, sexp]

  let validate t =
    if t.generation < 0
    then Error (Error.invalid_request "pending terminal generation must be nonnegative")
    else Ok t
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    match validate (unchecked_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;

  let of_operation (operation : Operation.t) =
    let open Result.Let_syntax in
    let%bind outcome =
      match operation.kind, operation.state with
      | Turn _, Completed -> Ok Completed
      | Turn _, Cancelled -> Ok Cancelled
      | Turn _, Failed _ -> Ok Failed
      | Turn _, Interrupted _ -> Ok Interrupted
      | Turn _, (Starting | Running | Cancelling)
      | ( Compaction
        , ( Starting
          | Running
          | Cancelling
          | Completed
          | Cancelled
          | Failed _
          | Interrupted _ ) ) ->
        Error
          (Error.invalid_request "pending barrier requires an actual terminal Turn root")
    in
    validate { operation_id = operation.id; generation = operation.generation; outcome }
  ;;

  let operation_id t = t.operation_id
  let generation t = t.generation

  let outcome_to_json = function
    | Completed -> `String "completed"
    | Cancelled -> `String "cancelled"
    | Failed -> `String "failed"
    | Interrupted -> `String "interrupted"
  ;;

  let to_json t =
    `Object
      [ "operation_id", Id.Operation.to_json t.operation_id
      ; "generation", `Number (Int.to_string t.generation)
      ; "outcome", outcome_to_json t.outcome
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind operation_id =
      Json_codec.required_as fields "operation_id" Id.Operation.of_json
    in
    let%bind generation =
      Json_codec.required_as
        fields
        "generation"
        (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind outcome =
      Json_codec.required_as
        fields
        "outcome"
        (Json_codec.enum
           ~name:"pending terminal outcome"
           [ "completed", Completed
           ; "cancelled", Cancelled
           ; "failed", Failed
           ; "interrupted", Interrupted
           ])
    in
    validate { operation_id; generation; outcome }
  ;;
end

module Binding = struct
  type t =
    | Safe_boundary
    | Await_idle
    | After_root of
        { operation_id : Id.Operation.t
        ; generation : int
        ; terminal : Terminal_proof.t option
        }
  [@@deriving equal, sexp]

  let safe_boundary = Safe_boundary

  let validate = function
    | (Safe_boundary | Await_idle) as value -> Ok value
    | After_root { operation_id; generation; terminal } as value ->
      if
        generation < 0
        || Option.exists terminal ~f:(fun proof ->
          (not (Id.Operation.equal operation_id (Terminal_proof.operation_id proof)))
          || not (Int.equal generation (Terminal_proof.generation proof)))
      then
        Error
          (Error.invalid_request "pending barrier terminal proof differs from bound root")
      else Ok value
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    match validate (unchecked_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;

  let create timing ~generation ~operation =
    if generation < 0
    then Error (Error.invalid_request "pending generation must be nonnegative")
    else (
      match timing, operation with
      | Timing.Safe_boundary, _ -> Ok Safe_boundary
      | After_current_operation, None -> Ok Await_idle
      | After_current_operation, Some (operation : Operation.t) ->
        if not (Int.equal operation.generation generation)
        then Error (Error.invalid_request "pending root belongs to another generation")
        else (
          match operation.kind, operation.state with
          | Turn _, (Starting | Running | Cancelling) ->
            Ok (After_root { operation_id = operation.id; generation; terminal = None })
          | Compaction, (Starting | Running | Cancelling) -> Ok Await_idle
          | (Turn _ | Compaction), (Completed | Failed _ | Cancelled | Interrupted _) ->
            Error
              (Error.invalid_request
                 "pending timing cannot bind an already terminal active operation")))
  ;;

  let release t proof =
    match t with
    | Safe_boundary | Await_idle -> Ok t
    | After_root { operation_id; generation; terminal } ->
      if Id.Operation.equal operation_id (Terminal_proof.operation_id proof)
      then
        if not (Int.equal generation (Terminal_proof.generation proof))
        then Error (Error.invalid_request "pending release generation differs")
        else (
          match terminal with
          | None -> Ok (After_root { operation_id; generation; terminal = Some proof })
          | Some previous ->
            if Terminal_proof.equal previous proof
            then Ok t
            else Error (Error.invalid_request "pending terminal proof is immutable"))
      else Ok t
  ;;

  let to_json = function
    | Safe_boundary -> `Object [ "kind", `String "safe_boundary" ]
    | Await_idle -> `Object [ "kind", `String "await_idle" ]
    | After_root { operation_id; generation; terminal } ->
      `Object
        [ "kind", `String "after_root"
        ; "operation_id", Id.Operation.to_json operation_id
        ; "generation", `Number (Int.to_string generation)
        ; "terminal", Option.value_map terminal ~default:`Null ~f:Terminal_proof.to_json
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind kind = Json_codec.required_as fields "kind" Json_codec.string in
    match kind with
    | "safe_boundary" -> Ok Safe_boundary
    | "await_idle" -> Ok Await_idle
    | "after_root" ->
      let%bind operation_id =
        Json_codec.required_as fields "operation_id" Id.Operation.of_json
      in
      let%bind generation =
        Json_codec.required_as
          fields
          "generation"
          (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
      in
      let%bind terminal =
        match Json_codec.optional fields "terminal" with
        | Some `Null -> Ok None
        | Some value -> Result.map (Terminal_proof.of_json value) ~f:Option.some
        | None -> Error (Error.invalid_request "pending terminal field is required")
      in
      validate (After_root { operation_id; generation; terminal })
    | _ -> Error (Error.invalid_request "unsupported pending binding")
  ;;
end

type t =
  { entry : History.entry
  ; generation : int
  ; binding : Binding.t
  }
[@@deriving equal, sexp]

let create ~entry ~generation ~binding =
  if
    generation < 0
    ||
    match binding with
    | Binding.Safe_boundary | Await_idle -> false
    | After_root { generation = bound; _ } -> not (Int.equal generation bound)
  then
    Error (Error.invalid_request "pending entry generation differs from temporal binding")
  else
    let open Result.Let_syntax in
    let%map _ = History.entry_of_json (History.entry_to_json entry) in
    { entry; generation; binding }
;;

let unchecked_of_sexp = t_of_sexp

let t_of_sexp sexp =
  let value = unchecked_of_sexp sexp in
  match create ~entry:value.entry ~generation:value.generation ~binding:value.binding with
  | Ok value -> value
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;

let entry t = t.entry
let history_id t = t.entry.id
let generation t = t.generation
let binding t = t.binding
let with_binding t binding = create ~entry:t.entry ~generation:t.generation ~binding

let with_entry t entry =
  if not (History.Id.equal t.entry.id entry.History.id)
  then
    Error (Error.invalid_request "pending replacement cannot change occurrence identity")
  else create ~entry ~generation:t.generation ~binding:t.binding
;;

let to_json t =
  `Object
    [ "id", History.Id.to_json t.entry.id
    ; "entry", History.entry_to_json t.entry
    ; "generation", `Number (Int.to_string t.generation)
    ; "binding", Binding.to_json t.binding
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id = Json_codec.required_as fields "id" History.Id.of_json in
  let%bind entry = Json_codec.required_as fields "entry" History.entry_of_json in
  let%bind generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind binding = Json_codec.required_as fields "binding" Binding.of_json in
  let%bind () =
    if History.Id.equal id entry.id
    then Ok ()
    else Error (Error.invalid_request "pending envelope identity differs from entry")
  in
  create ~entry ~generation ~binding
;;
