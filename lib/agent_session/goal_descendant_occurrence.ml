open! Core
module P = Agent_protocol
module X = Persistence_codec

let invalid message = Error (P.Error.invalid_request message)

module Frame = struct
  type t =
    { session : P.Session_ref.t
    ; generation : int
    ; candidate_session_revision : int64
    }
  [@@deriving equal, sexp_of]

  let create ~session ~generation ~candidate_session_revision =
    if generation < 0 || Int64.(candidate_session_revision <= 0L)
    then invalid "invalid descendant occurrence frame"
    else Ok { session; generation; candidate_session_revision }
  ;;

  let session t = t.session
  let generation t = t.generation
  let candidate_session_revision t = t.candidate_session_revision

  let to_json t =
    `Object
      [ "session", P.Session_ref.to_json t.session
      ; "generation", X.integer_json t.generation
      ; "candidate_session_revision", X.int64_json t.candidate_session_revision
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let%bind session = X.required fields "session" P.Session_ref.of_json in
    let%bind generation = X.required fields "generation" X.integer in
    let%bind candidate_session_revision =
      X.required fields "candidate_session_revision" X.nonnegative_int64
    in
    create ~session ~generation ~candidate_session_revision
  ;;

  let shape =
    X.shape_exn
      [ "session", X.fields_shape [ "server_id"; "session_id" ]
      ; "generation", Document_schema.Shape.value
      ; "candidate_session_revision", Document_schema.Shape.value
      ]
  ;;
end

module Kind = struct
  type t =
    | Attempt of
        { ordinal : int64
        ; operation_id : P.Id.Operation.t option
        ; invocation_id : P.Id.Invocation.t option
        }
    | Host_turn of { operation_id : P.Id.Operation.t }
  [@@deriving equal, sexp_of]
end

type t =
  { frame : Frame.t
  ; kind : Kind.t
  }
[@@deriving equal, sexp_of]

let frame t = t.frame
let kind t = t.kind

let attempt frame ~ordinal ~operation_id ~invocation_id =
  if Int64.(ordinal <= 0L)
  then invalid "descendant attempt ordinal must be positive"
  else Ok { frame; kind = Kind.Attempt { ordinal; operation_id; invocation_id } }
;;

let host_turn frame ~operation_id = Ok { frame; kind = Kind.Host_turn { operation_id } }

let dimension t =
  match t.kind with
  | Attempt { ordinal = _; operation_id = _; invocation_id = _ } ->
    Goal_admission_policy.Attempt
  | Host_turn { operation_id = _ } -> Goal_admission_policy.Host_turn
;;

let to_json t =
  let fields = [ "frame", Frame.to_json t.frame ] in
  `Object
    (match t.kind with
     | Attempt { ordinal; operation_id; invocation_id } ->
       fields
       @ [ "kind", `String "attempt"
         ; "ordinal", X.int64_json ordinal
         ; "operation_id", X.option_json P.Id.Operation.to_json operation_id
         ; "invocation_id", X.option_json P.Id.Invocation.to_json invocation_id
         ]
     | Host_turn { operation_id } ->
       fields
       @ [ "kind", `String "host_turn"
         ; "operation_id", P.Id.Operation.to_json operation_id
         ])
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind fields = X.object_ raw in
  let%bind frame = X.required fields "frame" Frame.of_json in
  let%bind kind = X.required fields "kind" P.Json_codec.string in
  match kind with
  | "attempt" ->
    let%bind ordinal = X.required fields "ordinal" X.nonnegative_int64 in
    let%bind operation_id =
      X.required fields "operation_id" (X.nullable P.Id.Operation.of_json)
    in
    let%bind invocation_id =
      X.required fields "invocation_id" (X.nullable P.Id.Invocation.of_json)
    in
    attempt frame ~ordinal ~operation_id ~invocation_id
  | "host_turn" ->
    let%bind operation_id = X.required fields "operation_id" P.Id.Operation.of_json in
    host_turn frame ~operation_id
  | _ -> invalid "unknown descendant occurrence kind"
;;

let shape =
  X.tagged_shape_exn
    ~discriminator:"kind"
    [ ( "attempt"
      , X.shape_exn
          [ "kind", Document_schema.Shape.value
          ; "frame", Frame.shape
          ; "ordinal", Document_schema.Shape.value
          ; "operation_id", Document_schema.Shape.nullable Document_schema.Shape.value
          ; "invocation_id", Document_schema.Shape.nullable Document_schema.Shape.value
          ] )
    ; ( "host_turn"
      , X.shape_exn
          [ "kind", Document_schema.Shape.value
          ; "frame", Frame.shape
          ; "operation_id", Document_schema.Shape.value
          ] )
    ]
;;

let capacity_projection_for_kind kind =
  let fields =
    [ ( "frame"
      , `Object
          [ ( "session"
            , `Object
                [ "server_id", `String (String.make 96 's')
                ; "session_id", `String (String.make 96 's')
                ] )
          ; "generation", X.integer_json Int.max_value
          ; "candidate_session_revision", X.int64_json Int64.max_value
          ] )
    ; "operation_id", `String (String.make 96 'o')
    ]
  in
  `Object
    (match kind with
     | Kind.Attempt { ordinal = _; operation_id = _; invocation_id = _ } ->
       fields
       @ [ "kind", `String "attempt"
         ; "ordinal", X.int64_json Int64.max_value
         ; "invocation_id", `String (String.make 96 'i')
         ]
     | Host_turn { operation_id = _ } -> fields @ [ "kind", `String "host_turn" ])
;;

let capacity_projection_for t = capacity_projection_for_kind t.kind

let capacity_projection () =
  (* Maximum Attempt contains the shared frame and operation of maximum Host_turn
     plus an ordinal and invocation. Their encoded fields exceed the two extra
     Host_turn tag bytes; fields/nodes/depth are componentwise at least as large.
     Protocol IDs have a 96-byte hard bound. These invalid values are capacity
     witnesses only and are never decoded as canonical evidence. *)
  capacity_projection_for_kind
    (Kind.Attempt { ordinal = Int64.max_value; operation_id = None; invocation_id = None })
;;
