open! Core
module W = Responses_wire
module D = Responses_driver
module E = Inference.Event
module O = Inference.Observation
module T = Transcript
module P = History_entry.Payload
module Runtime = Inference_runtime

type slot =
  { descriptor : T.Item.t
  ; candidate : E.t option
  ; bytes : int
  }

type t =
  { scope : T.Scope.t
  ; accounting_id : O.Observation_id.t
  ; origin : P.Origin.t
  ; limits : Runtime.Limits.t
  ; mutable slots : slot Int.Map.t
  ; mutable retained_bytes : int
  ; mutable started : bool
  }

let evidence = function
  | Ok value -> value
  | Error _ -> raise (Runtime.Contract_violation Invalid_candidate)
;;

let create ~target ~scope ~accounting_id ~limits =
  Result.map (Inference_input.origin target) ~f:(fun origin ->
    { scope
    ; accounting_id
    ; origin
    ; limits
    ; slots = Int.Map.empty
    ; retained_bytes = 0
    ; started = false
    })
;;

let stream t view =
  T.Stream.create view ~limits:(Runtime.Limits.event_limits t.limits) |> evidence
;;

let live t view =
  E.create (Live (stream t view)) ~limits:(Runtime.Limits.event_limits t.limits)
  |> evidence
;;

let retain t index descriptor candidate =
  let bytes =
    match candidate with
    | Some event -> E.encoded_bytes event
    | None -> T.Stream.encoded_bytes (stream t (Item_announced descriptor))
  in
  let previous =
    Option.value_map (Map.find t.slots index) ~default:0 ~f:(fun slot -> slot.bytes)
  in
  let remaining = t.retained_bytes - previous in
  if
    ((not (Map.mem t.slots index))
     && Map.length t.slots >= Runtime.Limits.max_candidates t.limits)
    || bytes > Runtime.Limits.max_evidence_bytes t.limits - remaining
  then raise (Runtime.Contract_violation Evidence_limit);
  t.slots <- Map.set t.slots ~key:index ~data:{ descriptor; candidate; bytes };
  t.retained_bytes <- remaining + bytes
;;

let descriptor t index ?(header = None) ?(call_name = None) () =
  let proposed =
    T.Item.create
      ~scope:t.scope
      ~id:(T.Item_id.of_string ("output:" ^ Int.to_string index) |> evidence)
      ~entry_id:None
      ~header
      ~call_name
    |> evidence
  in
  match Map.find t.slots index with
  | None ->
    retain t index proposed None;
    proposed
  | Some slot ->
    let descriptor = T.Item.refine slot.descriptor proposed |> evidence in
    retain t index descriptor slot.candidate;
    descriptor
;;

let payload t wire =
  let original = Responses_history.of_wire_item wire |> evidence in
  P.captured (P.semantic original) ~origin:t.origin ~raw:(W.Item.raw wire) |> evidence
;;

let from_wire t index wire =
  let semantic = P.semantic (payload t wire) in
  let call_name =
    match P.Semantic.view semantic with
    | Call { name; _ } -> Some name
    | Message _ | Result _ | Reasoning _ | Unknown _ -> None
  in
  descriptor t index ~header:(Some (T.Header.of_semantic semantic)) ~call_name ()
;;

let finalized t index wire ~allow_local =
  let payload = payload t wire in
  let item = from_wire t index wire in
  let previously_eligible =
    match Map.find t.slots index with
    | Some { candidate = Some event; _ } ->
      (match E.view event with
       | Candidate_ready { local_execution = Tool_candidate; _ } -> true
       | Candidate_ready { local_execution = Not_eligible; _ } | Live _ | Terminal _ ->
         false)
    | None | Some { candidate = None; _ } -> false
  in
  let local_execution =
    if (allow_local || previously_eligible) && Option.is_some (W.Item.local_call wire)
    then E.Tool_candidate
    else Not_eligible
  in
  let candidate =
    E.create
      (Candidate_ready { item; payload; local_execution })
      ~limits:(Runtime.Limits.event_limits t.limits)
    |> evidence
  in
  retain t index item (Some candidate);
  candidate
;;

let start t =
  if t.started
  then []
  else (
    t.started <- true;
    [ live t (Source_started { scope = t.scope; origin = t.origin }) ])
;;

let part t (location : W.Event.location) space kind =
  let item = descriptor t location.output_index () in
  let index =
    match location.part_index with
    | Some index -> index
    | None -> raise (Runtime.Contract_violation Invalid_candidate)
  in
  let namespace =
    match space with
    | W.Event.Part_space.Content -> "content:"
    | Summary -> "summary:"
  in
  T.Part.create
    ~item
    ~id:(T.Part_id.of_string (namespace ^ Int.to_string index) |> evidence)
    ~index:(Some index)
    ~kind
  |> evidence
;;

let part_kind part =
  match W.Part.view part with
  | Output_text _ -> T.Part.Text
  | Refusal _ -> Refusal
  | Summary_text _ -> Reasoning_summary
  | Reasoning_text _ -> Reasoning_text
  | Unknown kind -> Unknown kind
;;

let part_text part =
  match W.Part.view part with
  | Output_text { text; _ } | Refusal text | Summary_text text | Reasoning_text text ->
    Some text
  | Unknown _ -> None
;;

let change t location kind change =
  let target =
    match kind with
    | W.Event.Delta_kind.Function_arguments | Custom_input ->
      T.Stream.Target.Call_input (descriptor t location.W.Event.output_index ())
    | Text -> Content (part t location Content Text)
    | Refusal -> Content (part t location Content Refusal)
    | Reasoning_summary -> Content (part t location Summary Reasoning_summary)
    | Reasoning_text -> Content (part t location Content Reasoning_text)
  in
  live t (Changed { target; change })
;;

let update t event =
  match W.Event.view event with
  | Response _ -> []
  | Item_added { output_index; item } ->
    [ live t (Item_announced (from_wire t output_index item)) ]
  | Item_done { output_index; item } ->
    [ finalized t output_index item ~allow_local:true ]
  | Part_added { location; part_space; part = wire } ->
    [ live t (Part_announced (part t location part_space (part_kind wire))) ]
  | Part_done { location; part_space; part = wire } ->
    let part = part t location part_space (part_kind wire) in
    live t (Part_announced part)
    ::
    (match part_text wire with
     | None -> []
     | Some text -> [ live t (Changed { target = Content part; change = Replace text }) ])
  | Delta { location; kind; delta } -> [ change t location kind (Append delta) ]
  | Text_done { location; kind; text } -> [ change t location kind (Replace text) ]
  | Annotation_added _ | Unknown _ ->
    let provider_kind =
      match Document_schema.Json.field (W.Event.raw event) ~name:"type" with
      | Value (`String name) -> name
      | Absent | Null | Value _ -> "unavailable"
    in
    [ live t (Unknown_event { scope = t.scope; provider_kind; raw = W.Event.raw event }) ]
  | Terminal _ | Error _ -> []
;;

let event t event ~on_event =
  match event with
  | D.Event.Terminal _ | Diagnostic _ | Http_rejection _ -> ()
  | Finalized items ->
    List.iter (start t) ~f:on_event;
    List.iter items ~f:(fun (index, item) ->
      on_event (finalized t index item ~allow_local:true))
  | Update { disposition = Duplicate; _ } -> ()
  | Update { event; disposition = Accepted; newly_finalized = _ } ->
    List.iter (start t) ~f:on_event;
    List.iter (update t event) ~f:on_event
;;

let provider_failure (error : W.Provider_error.t) =
  match error.code with
  | Value ("invalid_request_error" | "invalid_request") ->
    E.Terminal.Provider_failure.Invalid_request
  | Value ("permission_denied" | "insufficient_permissions") -> Denied
  | Value ("rate_limit_exceeded" | "rate_limit_error") -> Rate_limited
  | Value ("server_error" | "service_unavailable") -> Unavailable
  | Absent | Null | Value _ -> Unknown
;;

let failure : D.Terminal.failure -> E.Terminal.transport_failure = function
  | Http_status status -> Http_status status
  | Invalid_http -> Invalid_http
  | Invalid_content_type -> Invalid_content_type
  | Body_limit -> Body_limit
  | Framing_limit -> Framing_limit
  | Protocol -> Protocol
  | Unsupported_transport -> Unsupported_transport
  | Session_closed -> Session_closed
  | Session_busy -> Session_busy
  | Connection -> Connection
  | Timeout -> Timeout
;;

let delivery : D.Terminal.delivery -> E.Terminal.delivery = function
  | Definitely_not_submitted -> Definitely_not_submitted
  | Possibly_submitted -> Possibly_submitted
  | Response_started -> Response_started
;;

let auth : D.Auth.error -> E.Terminal.auth_failure = function
  | Missing -> Missing
  | Denied -> Denied
  | Profile_changed -> Profile_changed
  | Reauthorization_required -> Reauthorization_required
  | Invalid_credential -> Invalid_credential
  | Timed_out -> Timed_out
;;

let response_outcome response =
  match W.Response.outcome response with
  | Completed -> E.Terminal.Completed
  | Refused -> Refused
  | Incomplete { reason } ->
    Incomplete
      (match reason with
       | Value "max_output_tokens" -> Output_limit
       | Value "content_filter" -> Filtered
       | Absent | Null -> Unavailable
       | Value _ -> Other)
  | Failed error ->
    Failed
      (Provider
         (match error with
          | Value error -> provider_failure error
          | Absent | Null -> Unknown))
  | Nonterminal _ -> raise (Runtime.Contract_violation Backend_terminal)
;;

let unknown reason = O.Count.create (Unknown reason) |> evidence
let actual value = O.Count.create (Actual value) |> evidence

let count_from_wire : int64 W.Presence.t -> O.Count.t = function
  | Absent -> unknown Not_reported
  | Null -> unknown Explicit_null
  | Value value -> actual value
;;

let usage t reported reason =
  let counts =
    match reported with
    | W.Presence.Absent | Null ->
      let value =
        unknown
          (match reported with
           | Null -> O.Count.Explicit_null
           | Absent -> reason
           | Value _ -> assert false)
      in
      O.Usage.
        { input = value
        ; output = value
        ; reported_total = value
        ; cached_input = value
        ; cache_write_input = value
        ; reasoning_output = value
        }
    | Value wire ->
      O.Usage.
        { input = actual (W.Usage.input_tokens wire)
        ; output = actual (W.Usage.output_tokens wire)
        ; reported_total = actual (W.Usage.total_tokens wire)
        ; cached_input = count_from_wire (W.Usage.cached_tokens wire)
        ; cache_write_input = count_from_wire (W.Usage.cache_write_tokens wire)
        ; reasoning_output = count_from_wire (W.Usage.reasoning_tokens wire)
        }
  in
  let inclusions =
    O.Usage.
      [ { subset = Cached_input; included_in = Input }
      ; { subset = Cache_write_input; included_in = Input }
      ; { subset = Reasoning_output; included_in = Output }
      ]
  in
  let value = O.Usage.create ~counts ~inclusions |> evidence in
  O.create
    ~scope:t.scope
    ~id:t.accounting_id
    ~revision:0L
    ~payload:(Usage value)
    ~limits:O.Admission.observation
  |> evidence
;;

let finish t result =
  let delivery, outcome, response, reason =
    match result with
    | Error error ->
      ( E.Terminal.Definitely_not_submitted
      , E.Terminal.Failed (Authentication (auth error))
      , None
      , O.Count.Not_submitted )
    | Ok (D.Terminal.Failed { delivery = progress; reason }) ->
      let progress = delivery progress in
      ( progress
      , E.Terminal.Failed (Transport (failure reason))
      , None
      , if E.Terminal.equal_delivery progress Definitely_not_submitted
        then O.Count.Not_submitted
        else Interrupted )
    | Ok (Provider (Error error)) ->
      ( E.Terminal.Response_started
      , E.Terminal.Failed (Provider (provider_failure error))
      , None
      , O.Count.Not_reported )
    | Ok (Provider (Response { response; terminal = _ })) ->
      ( E.Terminal.Response_started
      , response_outcome response
      , Some response
      , O.Count.Not_reported )
  in
  let terminal = E.Terminal.create ~scope:t.scope ~delivery ~outcome |> evidence in
  let output, coverage, reported =
    match response with
    | None ->
      ( List.filter_map (Map.data t.slots) ~f:(fun slot -> slot.candidate)
      , Runtime.Receipt.Observed_prefix
      , W.Presence.Absent )
    | Some response ->
      let allow_local =
        match W.Response.outcome response with
        | Completed | Refused -> true
        | Incomplete _ | Failed _ | Nonterminal _ -> false
      in
      ( List.mapi (W.Response.output response) ~f:(fun index wire ->
          (* Tracker has validated the terminal array against finalized stream
             slots. Keep the exact authoritative item.done candidate, including
             opaque replay bytes, instead of rebuilding it from the envelope. *)
          match Map.find t.slots index with
          | Some { candidate = Some candidate; _ } -> candidate
          | None | Some { candidate = None; _ } -> finalized t index wire ~allow_local)
      , Runtime.Receipt.Response_output
      , W.Response.usage response )
  in
  Runtime.Receipt.create
    ~terminal
    ~usage:(usage t reported reason)
    ~output
    ~output_coverage:coverage
    ~limits:t.limits
  |> function
  | Ok receipt -> receipt
  | Error error -> raise (Runtime.Contract_violation error)
;;
