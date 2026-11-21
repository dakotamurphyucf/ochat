open! Core
module P = Agent_protocol
module S = Transcript.Scope
module X = Persistence_codec
module J = P.Json_codec
module Terminal = Inference.Event.Terminal

type origin =
  | Returned
  | Interrupted
[@@deriving equal]

type settlement =
  { origin : origin
  ; delivery : Terminal.delivery
  }
[@@deriving equal]

type phase =
  | Allocated
  | Running
  | Settled of settlement
[@@deriving equal]

type phase_encoding =
  | Compact
  | Named
[@@deriving equal]

type t =
  { ordinal : int64
  ; generation : int
  ; scope : S.t
  ; tracked : bool
  ; phase : phase
  ; phase_encoding : phase_encoding
  }
[@@deriving equal]

let invalid message = Error (P.Error.invalid_request message)
let max_source_bytes = 128

let create ~ordinal ~generation ~scope ~tracked ~phase ~phase_encoding =
  let key = S.key scope in
  if
    Int64.(ordinal <= 0L)
    || generation < 0
    || String.length (Transcript.Source_id.to_string key.source) > max_source_bytes
    || not
         (String.equal
            (Transcript.Attempt_id.to_string key.attempt)
            ("ordinal:" ^ Int64.to_string ordinal))
  then invalid "recovery attempt identity is outside the admitted allocation"
  else (
    match scope.relation with
    | Root -> Ok { ordinal; generation; scope; tracked; phase; phase_encoding }
    | Nested _ -> invalid "recovery attempt must be an actual root")
;;

let allocate handle ~tracking =
  let module H = Inference_ledger.Handle in
  match H.operation_id handle, H.invocation_id handle with
  | Some _, None ->
    create
      ~ordinal:(H.ordinal handle)
      ~generation:(H.generation handle)
      ~scope:(H.scope handle)
      ~tracked:(Inference_ledger.equal_tracking tracking Tracked)
      ~phase:Allocated
      ~phase_encoding:Compact
  | _ -> invalid "recovery attempt must be bound to a foreground operation"
;;

let ordinal t = t.ordinal
let generation t = t.generation
let tracked t = t.tracked
let phase t = t.phase
let with_phase t phase = { t with phase }

let same_identity t other =
  Int64.equal t.ordinal other.ordinal
  && Int.equal t.generation other.generation
  && S.equal t.scope other.scope
  && Bool.equal t.tracked other.tracked
;;

let same_storage_reservation t other =
  same_identity t other
  && equal_phase_encoding t.phase_encoding Compact
  && equal_phase_encoding other.phase_encoding Compact
;;

let matches_handle t handle =
  let module H = Inference_ledger.Handle in
  Int64.equal t.ordinal (H.ordinal handle)
  && Int.equal t.generation (H.generation handle)
  && S.equal t.scope (H.scope handle)
;;

let belongs_to t scope =
  let module Scope = P.Recovery_window.Scope in
  let session = P.Session_ref.session_id (Scope.session scope) in
  Int.equal t.generation (Scope.generation scope)
  && String.is_prefix
       (Transcript.Source_id.to_string (S.key t.scope).source)
       ~prefix:(P.Id.Session.to_string session ^ ":")
;;

let matches_accounting t accounting =
  match t.phase, Inference.Observation.Attempt_record.state accounting with
  | Allocated, Prepared | Running, Running -> true
  | Settled { origin = Returned; delivery }, Terminal terminal ->
    Terminal.equal_delivery delivery (Terminal.delivery terminal)
  | Settled { origin = Interrupted; delivery }, Interrupted state ->
    Terminal.equal_delivery delivery state.delivery
  | _ -> false
;;

let check_transition previous ~incoming =
  if
    not
      (same_identity previous incoming
       && equal_phase_encoding previous.phase_encoding incoming.phase_encoding)
  then invalid "recovery attempt changed its actual allocation identity"
  else if equal_phase previous.phase incoming.phase
  then Ok ()
  else (
    match previous.phase, incoming.phase with
    | Allocated, Running
    | Allocated, Settled { origin = Interrupted; delivery = Definitely_not_submitted }
    | Running, Settled _ -> Ok ()
    | _ -> invalid "recovery attempt transition changed previously settled evidence")
;;

let recover t =
  let interrupted delivery = with_phase t (Settled { origin = Interrupted; delivery }) in
  match t.phase with
  | Allocated -> interrupted Definitely_not_submitted
  | Running -> interrupted Possibly_submitted
  | Settled _ -> t
;;

let origin_to_string = function
  | Returned -> "returned"
  | Interrupted -> "interrupted"
;;

let origin_of_json =
  J.enum
    ~name:"recovery attempt settlement"
    [ "returned", Returned; "interrupted", Interrupted ]
;;

let delivery_to_string = function
  | Terminal.Definitely_not_submitted -> "definitely_not_submitted"
  | Possibly_submitted -> "possibly_submitted"
  | Response_started -> "response_confirmed"
;;

let delivery_of_json =
  J.enum
    ~name:"recovery attempt delivery"
    [ "definitely_not_submitted", Terminal.Definitely_not_submitted
    ; "possibly_submitted", Possibly_submitted
    ; "response_confirmed", Response_started
    ]
;;

let phase_to_json = function
  | Allocated -> `Object [ "kind", `String "allocated" ]
  | Running -> `Object [ "kind", `String "running" ]
  | Settled { origin; delivery } ->
    `Object
      [ "kind", `String "settled"
      ; "origin", `String (origin_to_string origin)
      ; "delivery", `String (delivery_to_string delivery)
      ]
;;

let phase_of_json json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind kind = X.required fields "kind" J.string in
  match kind with
  | "allocated" -> Ok Allocated
  | "running" -> Ok Running
  | "settled" ->
    let%bind origin = X.required fields "origin" origin_of_json in
    let%map delivery = X.required fields "delivery" delivery_of_json in
    Settled { origin; delivery }
  | _ -> invalid "unknown recovery attempt phase"
;;

(* Each new phase occupies the same byte in a canonical Store snapshot. Once
   the actual attempt is admitted, its acknowledgement and terminal replace
   that reserved byte. The named beta format retains its original encoding. *)
let phase_codes : (string * phase) list =
  [ "0", Allocated
  ; "1", Running
  ; "2", Settled { origin = Returned; delivery = Definitely_not_submitted }
  ; "3", Settled { origin = Returned; delivery = Possibly_submitted }
  ; "4", Settled { origin = Returned; delivery = Response_started }
  ; "5", Settled { origin = Interrupted; delivery = Definitely_not_submitted }
  ; "6", Settled { origin = Interrupted; delivery = Possibly_submitted }
  ; "7", Settled { origin = Interrupted; delivery = Response_started }
  ]
;;

let phase_to_field t =
  match t.phase_encoding with
  | Named -> "phase", phase_to_json t.phase
  | Compact ->
    let code, _ =
      List.find_exn phase_codes ~f:(fun (_, phase) -> equal_phase phase t.phase)
    in
    "phase_code", `Number code
;;

let phase_from_fields json =
  let open Result.Let_syntax in
  match
    ( Document_schema.Json.field json ~name:"phase"
    , Document_schema.Json.field json ~name:"phase_code" )
  with
  | Value phase, Absent ->
    let%map phase = phase_of_json phase in
    phase, Named
  | Absent, Value (`Number code) ->
    let%map phase =
      List.Assoc.find phase_codes code ~equal:String.equal
      |> Result.of_option
           ~error:(P.Error.invalid_request "invalid recovery attempt phase code")
    in
    phase, Compact
  | _ -> invalid "recovery attempt requires exactly one phase encoding"
;;

let to_jsonaf t =
  `Object
    [ "ordinal", X.int64_json t.ordinal
    ; "generation", X.host_counter_to_json t.generation
    ; "scope", S.to_json t.scope
    ; ("tracked", if t.tracked then `True else `False)
    ; phase_to_field t
    ]
;;

let scope_of_json json =
  S.of_json json ~limits:Inference.Observation.Admission.observation
  |> Result.map_error ~f:(fun _ ->
    P.Error.invalid_request "invalid recovery attempt scope")
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind ordinal = X.required fields "ordinal" X.nonnegative_int64 in
  let%bind generation = X.required fields "generation" X.host_counter_of_json in
  let%bind scope = X.required fields "scope" scope_of_json in
  let%bind tracked = X.required fields "tracked" J.bool in
  let%bind phase, phase_encoding = phase_from_fields json in
  create ~ordinal ~generation ~scope ~tracked ~phase ~phase_encoding
;;

let shape =
  let module D = Document_schema.Shape in
  let scope =
    X.shape_exn [ "key", X.fields_shape [ "source"; "attempt" ]; "parent", D.value ]
  in
  let phase =
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ "allocated", X.fields_shape [ "kind" ]
      ; "running", X.fields_shape [ "kind" ]
      ; "settled", X.fields_shape [ "kind"; "origin"; "delivery" ]
      ]
  in
  X.shape_exn
    [ "ordinal", D.value
    ; "generation", D.value
    ; "scope", scope
    ; "tracked", D.value
    ; "phase", phase
    ; "phase_code", D.value
    ]
;;
