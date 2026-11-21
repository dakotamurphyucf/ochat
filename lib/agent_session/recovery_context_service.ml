open! Core
module P = Agent_protocol
module N = Native_tool_invocation.Admission
module C = Chat_response.Tool_capability
module Capacity = Session_context_capacity
module Assessment = Capacity.Capacity.Assessment
module Checkpoint = Recovery_authored_checkpoint

type effect_kind =
  | Remaining
  | New_window
  | Checkpoint
[@@deriving equal]

let name = function
  | Remaining -> "context_remaining"
  | New_window -> "context_new_window"
  | Checkpoint -> Checkpoint.name
;;

type access =
  { token : N.t
  ; binding : C.binding
  ; effect_kind : effect_kind
  ; actor : Operator_authorization.t
  }

let denied () =
  P.Error.create
    Permission_denied
    ~message:"context effect authority denied or expired"
    ~retryable:false
    ()
;;

let capture token ~binding ~effect_kind =
  let open Result.Let_syntax in
  let%bind () = N.check_now token ~binding in
  let%bind actor = Result.of_option (N.actor token) ~error:(denied ()) in
  if
    String.equal (C.reference binding).name (name effect_kind)
    && Operator_authorization.is_current actor
  then Ok { token; binding; effect_kind; actor }
  else Error (denied ())
;;

let check_now t (state : Session_state.t) ~effect_kind =
  let open Result.Let_syntax in
  let%bind () = N.check_now t.token ~binding:t.binding in
  let invocation = N.invocation t.token in
  let principal = Operator_authorization.principal t.actor in
  let permitted =
    match effect_kind with
    | Remaining -> P.Principal.has_scope principal View_session_transcript
    | New_window -> P.Principal.has_scope principal Send_messages
    | Checkpoint ->
      P.Principal.has_scope principal View_session_transcript
      && P.Principal.has_scope principal Send_messages
  in
  if
    equal_effect_kind effect_kind t.effect_kind
    && P.Id.Session.equal invocation.context.session_id state.identity.session_id
    && Int.equal invocation.context.generation state.identity.generation
    && Operator_authorization.is_current t.actor
    && permitted
  then Ok (principal.id, invocation)
  else Error (denied ())
;;

type observation =
  { window : P.Recovery_window.Scope.t option
  ; revision : int64
  ; capacity : Capacity.status
  }

let integer value = `Number (Int64.to_string value)
let string value = `String value

let nullable f = function
  | None -> `Null
  | Some value -> f value
;;

let mode = function
  | Capacity.Mode.Manual -> string "manual"
  | Automatic -> string "automatic"
;;

let capacity_json t =
  let open Capacity in
  let availability (value : availability) =
    [ "requested_mode", mode value.requested; "effective_mode", mode value.effective ]
  in
  let assessment t =
    match Assessment.view t with
    | Unavailable reason ->
      [ "evidence", string "unknown"
      ; "reason", string (Sexp.to_string (Assessment.sexp_of_unavailable reason))
      ]
    | Assessed
        { evidence
        ; total
        ; output
        ; hard_input
        ; soft_input
        ; warn_input
        ; margin
        ; remaining
        ; pressure
        } ->
      let evidence =
        match evidence with
        | Measured tokens -> [ "evidence", string "measured"; "input", integer tokens ]
        | Estimated estimate ->
          let module O = Inference.Observation in
          (match O.Count.view (O.Context_estimate.count estimate) with
           | Estimated { tokens; estimator } ->
             [ "evidence", string "estimated"
             ; "input", integer tokens
             ; ( "estimator"
               , string
                   (Sexp.to_string
                      (O.Estimator.sexp_of_method_ (O.Estimator.method_ estimator))) )
             ; ( "estimator_version"
               , `Number (Int.to_string (O.Estimator.version estimator)) )
             ]
           | Unknown _ | Actual _ -> [ "evidence", string "unknown" ])
      in
      let remaining =
        match remaining with
        | Available tokens -> [ "remaining", integer tokens ]
        | Exceeded_by tokens -> [ "exceeded_by", integer tokens ]
        | Excess_out_of_range -> [ "exceeded", `True ]
      in
      [ "total", integer total
      ; "output_reserve", integer output
      ; "hard_input", integer hard_input
      ; "soft_input", integer soft_input
      ; "warning_input", integer warn_input
      ; "estimate_margin", integer margin
      ; "pressure", string (Sexp.to_string (Assessment.sexp_of_pressure pressure))
      ]
      @ evidence
      @ remaining
  in
  `Object
    (match t with
     | Disabled -> [ "status", string "disabled" ]
     | Awaiting_preparation mode ->
       ("status", string "awaiting_preparation") :: availability mode
     | Stale_preparation mode ->
       ("status", string "stale_preparation") :: availability mode
     | Current { availability = mode; phase; configuration; assessment = value } ->
       let module C = Inference.Observation.Configuration in
       let module Session = P.Session_configuration in
       [ "status", string "current"
       ; "phase", string (Sexp.to_string (Session.sexp_of_phase phase))
       ; "model", string (C.model configuration)
       ; "profile", string (C.profile configuration)
       ; "preparation", string (Assessment.preparation_id value)
       ]
       @ availability mode
       @ assessment value)
;;

let to_json t =
  `Object
    [ "window", nullable P.Recovery_window.Scope.to_json t.window
    ; "revision", integer t.revision
    ; "capacity", capacity_json t.capacity
    ]
;;

type t =
  { remaining : access -> (observation, P.Error.t) result
  ; new_window :
      access -> P.Id.Context_window.t -> (Recovery_rollover.Receipt.t, P.Error.t) result
  ; checkpoint :
      access -> Checkpoint.Request.t -> (Checkpoint.Receipt.t, P.Error.t) result
  }
