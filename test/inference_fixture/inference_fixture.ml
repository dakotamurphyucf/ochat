open! Core
module R = Inference.Request
module O = Inference.Observation
module E = Inference.Event
module Runtime = Inference_runtime
module P = History_entry.Payload
module Res = Openai.Responses

type post_stream =
  sw:Eio.Switch.t -> inputs:Res.Item.t list -> Res.Response_stream.t Seq.t

type t =
  { default_model : string
  ; adapter : Runtime.Adapter.t
  ; identity : Chat_response.Neutral_turn.Identity.t
  }

let ok result =
  Result.map_error result ~f:(fun _ -> "synthetic fixture admission")
  |> Result.ok_or_failwith
;;

let limits = Document_schema.Limits.default

let descriptor ~scope ~index payload =
  let semantic = P.semantic payload in
  let call_name =
    match P.Semantic.view semantic with
    | Call { name; _ } -> Some name
    | Message _ | Result _ | Reasoning _ | Unknown _ -> None
  in
  Transcript.Item.create
    ~scope
    ~id:(Transcript.Item_id.of_string (Int.to_string index) |> ok)
    ~entry_id:None
    ~header:(Some (Transcript.Header.of_semantic semantic))
    ~call_name
  |> ok
;;

let candidate ~scope ~index item =
  let payload = Openai.Responses_history.of_item item |> ok in
  let semantic = P.semantic payload in
  let eligible =
    match P.Semantic.view semantic with
    | Call { namespace; async; _ } ->
      let namespace_absent =
        match namespace with
        | Absent -> true
        | Null | Value _ -> false
      in
      let synchronous =
        match async with
        | Absent | Value false -> true
        | Null | Value true -> false
      in
      let finalized =
        match (P.Semantic.metadata semantic).status with
        | Absent | Value "completed" -> true
        | Null | Value _ -> false
      in
      namespace_absent && synchronous && finalized
    | Message _ | Result _ | Reasoning _ | Unknown _ -> false
  in
  let event =
    E.create
      (Candidate_ready
         { item = descriptor ~scope ~index payload
         ; payload
         ; local_execution = (if eligible then Tool_candidate else Not_eligible)
         })
      ~limits
    |> ok
  in
  event, payload
;;

let field json key =
  match Document_schema.Json.field json ~name:key with
  | Value value -> value
  | Absent | Null -> failwith "missing synthetic stream field"
;;

let text json key =
  match field json key with
  | `String value -> value
  | _ -> failwith "synthetic string"
;;

let index json key =
  match field json key with
  | `Number value -> Int.of_string value
  | _ -> failwith "synthetic position"
;;

let run_stream
      post_stream
      request
      ~sw
      ~scope
      ~accounting_id
      ~note_delivery
      ~on_event
      ~on_observation:_
  =
  let inputs =
    R.history request
    |> List.map ~f:(fun entry ->
      Openai.Responses_history.to_item (History_entry.payload entry) |> ok)
  in
  let descriptors = ref Int.Map.empty in
  let output = ref Int.Map.empty in
  let outcome = ref E.Terminal.Completed in
  let live view =
    Transcript.Stream.create view ~limits:Transcript.Admission.default
    |> ok
    |> fun event -> E.create (Live event) ~limits |> ok |> on_event
  in
  let announce position item =
    let payload = Openai.Responses_history.of_item item |> ok in
    let item = descriptor ~scope ~index:position payload in
    descriptors := Map.set !descriptors ~key:position ~data:item;
    live (Item_announced item)
  in
  let finalize position item =
    let event, payload = candidate ~scope ~index:position item in
    (* Deliver every observed candidate to the runtime owner, which reconciles
       duplicates and rejects conflicts before another tool publication. The
       receipt retains the first candidate in each actual response position. *)
    (match Map.find !output position with
     | Some _ -> ()
     | None -> output := Map.set !output ~key:position ~data:(payload, event));
    on_event event
  in
  let change json ~kind ~call_input ~value ~append =
    let position = index json "output_index" in
    let item =
      match Map.find !descriptors position with
      | Some item -> item
      | None ->
        let item =
          Transcript.Item.create
            ~scope
            ~id:(Transcript.Item_id.of_string (Int.to_string position) |> ok)
            ~entry_id:None
            ~header:None
            ~call_name:None
          |> ok
        in
        descriptors := Map.set !descriptors ~key:position ~data:item;
        item
    in
    let target =
      if call_input
      then Transcript.Stream.Target.Call_input item
      else (
        let part_index =
          match Document_schema.Json.field json ~name:"content_index" with
          | Value (`Number value) -> Int.of_string value
          | Absent -> index json "summary_index"
          | Null | Value _ -> failwith "synthetic part position"
        in
        let part =
          Transcript.Part.create
            ~item
            ~id:(Transcript.Part_id.of_string (Int.to_string part_index) |> ok)
            ~index:(Some part_index)
            ~kind
          |> ok
        in
        Transcript.Stream.Target.Content part)
    in
    live (Changed { target; change = (if append then Append value else Replace value) })
  in
  live (Source_started { scope; origin = P.Origin.unavailable });
  post_stream ~sw ~inputs
  |> Stdlib.Seq.iter (fun event ->
    note_delivery E.Terminal.Response_started;
    let json = Res.Response_stream.jsonaf_of_t event in
    match text json "type" with
    | "response.output_item.added" ->
      announce (index json "output_index") (Res.Item.t_of_jsonaf (field json "item"))
    | "response.output_item.done" ->
      finalize (index json "output_index") (Res.Item.t_of_jsonaf (field json "item"))
    | "response.output_text.delta" ->
      change json ~kind:Text ~call_input:false ~value:(text json "delta") ~append:true
    | "response.output_text.done" ->
      change json ~kind:Text ~call_input:false ~value:(text json "text") ~append:false
    | "response.refusal.delta" ->
      change json ~kind:Refusal ~call_input:false ~value:(text json "delta") ~append:true
    | "response.refusal.done" ->
      change
        json
        ~kind:Refusal
        ~call_input:false
        ~value:(text json "refusal")
        ~append:false
    | "response.reasoning_summary_text.delta" ->
      change
        json
        ~kind:Reasoning_summary
        ~call_input:false
        ~value:(text json "delta")
        ~append:true
    | "response.function_call_arguments.delta" | "response.custom_tool_call_input.delta"
      -> change json ~kind:Text ~call_input:true ~value:(text json "delta") ~append:true
    | "response.function_call_arguments.done" ->
      change json ~kind:Text ~call_input:true ~value:(text json "arguments") ~append:false
    | "response.custom_tool_call_input.done" ->
      change json ~kind:Text ~call_input:true ~value:(text json "input") ~append:false
    | ("response.completed" | "response.incomplete" | "response.failed") as terminal ->
      (outcome
       := match terminal with
          | "response.completed" -> Completed
          | "response.incomplete" -> Incomplete Unavailable
          | _ -> Failed (Provider Unknown));
      (match field (field json "response") "output" with
       | `Array items ->
         List.iteri items ~f:(fun position item ->
           finalize position (Res.Item.t_of_jsonaf item))
       | _ -> failwith "synthetic response output")
    | provider_kind -> live (Unknown_event { scope; provider_kind; raw = json }));
  (* The test mock declares EOF to be completion, including an empty answer.
     This is synthetic response evidence, not a real provider wire assertion. *)
  note_delivery E.Terminal.Response_started;
  let unknown = O.Count.create (Unknown Not_reported) |> ok in
  let usage =
    O.Usage.create
      ~counts:
        { input = unknown
        ; output = unknown
        ; reported_total = unknown
        ; cached_input = unknown
        ; cache_write_input = unknown
        ; reasoning_output = unknown
        }
      ~inclusions:[]
    |> ok
  in
  let usage =
    O.create
      ~scope
      ~id:accounting_id
      ~revision:0L
      ~payload:(Usage usage)
      ~limits:O.Admission.observation
    |> ok
  in
  let terminal =
    E.Terminal.create ~scope ~delivery:Response_started ~outcome:!outcome |> ok
  in
  Runtime.Receipt.create
    ~terminal
    ~usage
    ~output:(Map.data !output |> List.map ~f:snd)
    ~output_coverage:Response_output
    ~limits:Runtime.Limits.default
  |> ok
;;

let create ~namespace ~default_model ~post_stream =
  let source = Transcript.Source_id.of_string namespace |> ok in
  let sequence = Atomic.make 0 in
  let next () = Atomic.fetch_and_add sequence 1 |> Int.to_string in
  let identity : Chat_response.Neutral_turn.Identity.t =
    { new_preparation_id = (fun () -> namespace ^ "/preparation/" ^ next ())
    ; with_attempt =
        (fun _ ~relation ~f ->
          let id = next () in
          let scope =
            Transcript.Scope.create
              ~source
              ~attempt:(Transcript.Attempt_id.of_string id |> ok)
              ~relation
            |> ok
          in
          let accounting_id =
            O.Observation_id.of_string (namespace ^ "/usage/" ^ id) |> ok
          in
          f ~scope ~accounting_id)
    }
  in
  let bind target =
    if
      String.equal (R.Target.adapter target) "fixture.responses"
      && String.equal (R.Target.profile target) "selected"
      && Option.is_none (R.Target.account target)
      && String.equal (R.Target.endpoint target) "fixture://responses"
    then Ok ()
    else Error Runtime.Preparation_error.Target_mismatch
  in
  let adapter =
    Runtime.Adapter.create
      ~id:"fixture.responses"
      ~limits:Runtime.Limits.default
      ~bind
      ~prepare:(fun ~preparation_id request ->
        let open Result.Let_syntax in
        let%bind configuration =
          O.Configuration.of_target
            (R.target request)
            ~preparation_id
            ~transport:In_process
            ~capabilities:[]
            ~limits:O.Admission.observation
          |> Result.map_error ~f:(fun _ -> Runtime.Preparation_error.Invalid_preparation)
        in
        Runtime.Plan.create
          ~request
          ~configuration
          ~fingerprint:preparation_id
          ~run:(run_stream post_stream request))
      ()
    |> ok
  in
  { default_model; adapter; identity }
;;

let capture_config t config =
  let open Result.Let_syntax in
  let%bind target =
    R.Target.create
      ~adapter:"fixture.responses"
      ~profile:"selected"
      ~profile_revision:None
      ~account:None
      ~endpoint:"fixture://responses"
      ~model:t.default_model
      ~settings:[]
      ~limits
    |> Result.map_error ~f:(fun error -> Runtime.Preparation_error.Invalid_request error)
  in
  Chat_response.Inference_config.apply_overrides target config ~limits
;;

let recapture_config t ~current (config : Chat_response.Config.t) =
  let open Result.Let_syntax in
  let%bind current =
    R.Target.with_model
      current
      ~model:(Option.value config.model ~default:t.default_model)
      ~limits
    |> Result.map_error ~f:(fun error -> Runtime.Preparation_error.Invalid_request error)
  in
  let%bind current =
    List.fold_result (R.Target.settings current) ~init:current ~f:(fun target setting ->
      if
        List.mem
          Chat_response.Inference_config.setting_names
          (R.Setting.name setting)
          ~equal:String.equal
      then (
        let%bind setting =
          R.Setting.with_value setting ~value:Absent ~provenance:Captured_prompt ~limits
          |> Result.map_error ~f:(fun error ->
            Runtime.Preparation_error.Invalid_request error)
        in
        R.Target.with_setting
          target
          ~name:(R.Setting.name setting)
          ~value:(R.Setting.value setting)
          ~provenance:(R.Setting.provenance setting)
          ~limits
        |> Result.map_error ~f:(fun error ->
          Runtime.Preparation_error.Invalid_request error))
      else Ok target)
  in
  Chat_response.Inference_config.apply_overrides current config ~limits
;;

let resolve t target = Runtime.Context.create t.adapter ~target
let identity t = t.identity

let ctx t ?(config = Chat_response.Config.default) ~env ~dir ~tool_dir ~cache () =
  let inference_context = capture_config t config |> ok |> resolve t |> ok in
  Chat_response.Ctx.create
    ~inference_context
    ~inference_identity:(identity t)
    ~on_inference_attempt:(fun _ -> ())
    ~on_inference_completion:(fun _ -> ())
    ~env
    ~dir
    ~tool_dir
    ~cache
    ()
;;
