open! Core
module D = Document_schema
module G = Agent_protocol.Goal
module C = D.Extension_carrier
module X = Persistence_codec

type t = G.t C.t

let value = C.value
let v = D.Shape.value
let obj = X.shape_exn
let nullable = D.Shape.nullable

let tagged cases =
  match D.Shape.tagged_object ~discriminator:"kind" cases with
  | Ok shape -> shape
  | Error error -> failwith (Sexp.to_string_hum (D.Error.sexp_of_t error))
;;

let source = Run_record_shapes.source
let receipt = Run_record_shapes.receipt

let episode =
  obj [ "session", X.fields_shape [ "server_id"; "session_id" ]; "admission", receipt ]
;;

let settlement = obj [ "episode", episode; "receipt", receipt ]

let request =
  obj
    [ "episode", episode
    ; "receipt", receipt
    ; "evidence", v
    ; "requested_action", Run_record_shapes.action
    ]
;;

let hold =
  tagged
    [ "operator_pause", obj [ "kind", v ]
    ; "source_review", obj [ "kind", v ]
    ; "needs_input", obj [ "kind", v; "reason", v ]
    ; "budget_gate", obj [ "kind", v; "dimension", v ]
    ; "usage_uncertain", obj [ "kind", v; "component", v ]
    ]
;;

let admission =
  tagged [ "enabled", obj [ "kind", v ]; "held", obj [ "kind", v; "reason", hold ] ]
;;

let state =
  tagged
    [ "open", obj [ "kind", v; "admission", admission ]
    ; "achieved", obj [ "kind", v ]
    ; "abandoned", obj [ "kind", v; "reason", v ]
    ]
;;

let budget =
  obj
    [ "admitted_turns", nullable v
    ; "admitted_attempts", nullable v
    ; "observed_tokens", nullable (obj [ "component", v; "ceiling", v ])
    ; "deadline", nullable v
    ]
;;

let shape =
  obj
    [ "id", v
    ; "session", X.fields_shape [ "server_id"; "session_id" ]
    ; "creator", v
    ; "source", source
    ; "objective", v
    ; "progress", v
    ; "budget", budget
    ; "state", state
    ; "episode", nullable episode
    ; "achievement_request", nullable request
    ; "request_settlement", nullable settlement
    ; "previous_episode_settlement", nullable settlement
    ; "revision", v
    ]
;;

let limits =
  match
    D.Limits.create
      ~max_bytes:G.Capacity.max_document_bytes
      ~max_depth:64
      ~max_fields:8192
      ~max_nodes:16384
  with
  | Ok limits -> limits
  | Error error -> failwith (Sexp.to_string_hum (D.Error.sexp_of_t error))
;;

let domain result =
  Result.map_error result ~f:(fun error ->
    D.Error.Invalid_field { path = []; reason = error.Agent_protocol.Error.message })
;;

let codec =
  match
    D.Domain_codec.create
      ~limits
      ~kind:"ochat.goal"
      ~version:1
      ~shape
      ~supported_semantics:[]
      ~decode:(fun json -> domain (G.of_json json))
      ~encode:(fun value -> Ok (G.to_json value))
  with
  | Ok codec -> codec
  | Error error -> failwith (Sexp.to_string_hum (D.Error.sexp_of_t error))
;;

let validate_capacity document goal =
  let open Result.Let_syntax in
  let%bind occupied = domain (G.Capacity.measure (D.Document.json document)) in
  let%bind current = domain (G.Capacity.measure (G.to_json goal)) in
  let%bind reserved = domain (G.reserved_capacity goal) in
  let total get = get occupied + Int.max 0 (get reserved - get current) in
  if
    total G.Capacity.encoded_bytes > D.Limits.max_bytes limits
    || total G.Capacity.fields > D.Limits.max_fields limits
    || total G.Capacity.nodes > D.Limits.max_nodes limits
  then Error (D.Error.Limit_exceeded "goal document lacks reserved settlement capacity")
  else Ok ()
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind carrier = D.Domain_codec.decode codec document in
  let%map () = validate_capacity document (value carrier) in
  carrier
;;

let to_document carrier =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec carrier in
  let%map () = validate_capacity document (value carrier) in
  document
;;

let create goal =
  let open Result.Let_syntax in
  let carrier = C.of_authored_value goal in
  let%map _ = to_document carrier in
  carrier
;;

let update t incoming =
  let open Result.Let_syntax in
  let%bind () = domain (G.validate_update (value t) ~incoming) in
  let next = C.with_value t incoming in
  let%map _ = to_document next in
  next
;;

let adopt t ~incoming =
  let open Result.Let_syntax in
  let%bind () = domain (G.validate_update (value t) ~incoming:(value incoming)) in
  let%bind adopted = D.Domain_codec.adopt codec ~previous:t ~incoming in
  let%map _ = to_document adopted in
  adopted
;;
