open! Core
module Request = Inference.Request
module Event = Inference.Event
module Observation = Inference.Observation
module Session = Session

module Preparation_error = struct
  type t =
    | Invalid_request of Request.Error.t
    | Target_mismatch
    | Target_unavailable
    | Target_denied
    | Reauthorization_required
    | Unsupported_input
    | Unsupported_setting
    | Incompatible_replay
    | Asset_unavailable
    | Transport_unavailable
    | Session_closed
    | Invalid_preparation
    | Request_limit of Document_schema.Error.t
  [@@deriving equal, sexp_of]
end

module Contract_error = struct
  type t =
    | Scope_mismatch
    | Accounting_identity_mismatch
    | Configuration_mismatch
    | Conflicting_candidate
    | Missing_candidate
    | Invalid_candidate
    | Missing_transport
    | Conflicting_transport
    | Invalid_usage
    | Conflicting_usage
    | Backend_terminal
    | Delivery_regression
    | Evidence_limit
  [@@deriving equal, sexp_of]
end

exception Contract_violation of Contract_error.t

module Limits = struct
  type t =
    { event_limits : Document_schema.Limits.t
    ; max_candidates : int
    ; max_evidence_bytes : int
    }

  let create ~event_limits ~max_candidates ~max_evidence_bytes =
    if max_candidates <= 0 || max_evidence_bytes <= 0
    then Error Preparation_error.Invalid_preparation
    else Ok { event_limits; max_candidates; max_evidence_bytes }
  ;;

  let default =
    { event_limits = Transcript.Admission.default
    ; max_candidates = 100_000
    ; max_evidence_bytes = 64 * 1024 * 1024
    }
  ;;

  let event_limits t = t.event_limits
  let max_candidates t = t.max_candidates
  let max_evidence_bytes t = t.max_evidence_bytes
end

let check_scope expected actual =
  if Transcript.Scope.equal expected actual
  then Ok ()
  else Error Contract_error.Scope_mismatch
;;

let item_equal (a : Transcript.Item.t) (b : Transcript.Item.t) =
  Transcript.Scope.equal a.scope b.scope
  && Transcript.Item_id.equal a.id b.id
  && Option.equal History_entry.Id.equal a.entry_id b.entry_id
  && Option.equal Transcript.Header.equal a.header b.header
  && Option.equal String.equal a.call_name b.call_name
;;

let candidate_equal a b =
  match Event.view a, Event.view b with
  | ( Candidate_ready { item = a; payload = pa; local_execution = ea }
    , Candidate_ready { item = b; payload = pb; local_execution = eb } ) ->
    item_equal a b
    && Event.equal_local_execution ea eb
    && Document_schema.Json.equal
         (History_entry.Payload.to_json pa)
         (History_entry.Payload.to_json pb)
  | (Live _ | Terminal _ | Candidate_ready _), (Live _ | Terminal _ | Candidate_ready _)
    -> false
;;

(* A bounded, immutable evidence set. Receipt validation uses a fresh set so a
   repeated output slot is rejected even when its bytes happen to match. *)
module Candidates = struct
  type t =
    { events : Event.t Map.M(Transcript.Item.Key).t
    ; bytes : int
    }

  let empty = { events = Map.empty (module Transcript.Item.Key); bytes = 0 }

  let add t event ~scope ~(limits : Limits.t) ~allow_duplicate =
    let open Result.Let_syntax in
    let%bind () = check_scope scope (Event.scope event) in
    let%bind event =
      Event.create (Event.view event) ~limits:limits.event_limits
      |> Result.map_error ~f:(fun _ -> Contract_error.Invalid_candidate)
    in
    match Event.view event with
    | Live _ | Terminal _ -> Error Contract_error.Invalid_candidate
    | Candidate_ready { item; _ } ->
      let key = Transcript.Item.key item in
      (match Map.find t.events key with
       | Some previous ->
         if allow_duplicate && candidate_equal previous event
         then Ok (t, false)
         else Error Contract_error.Conflicting_candidate
       | None ->
         let bytes = Event.encoded_bytes event in
         if
           Map.length t.events >= limits.max_candidates
           || bytes > limits.max_evidence_bytes - t.bytes
         then Error Contract_error.Evidence_limit
         else
           Ok
             ( { events = Map.set t.events ~key ~data:event; bytes = t.bytes + bytes }
             , true ))
  ;;
end

module Receipt = struct
  type output_coverage =
    | Response_output
    | Observed_prefix
  [@@deriving equal, sexp_of]

  type t =
    { terminal : Event.Terminal.t
    ; usage : Observation.t
    ; output : Event.t list
    ; output_coverage : output_coverage
    ; candidates : Candidates.t
    }

  let create ~terminal ~usage ~output ~output_coverage ~limits =
    let open Result.Let_syntax in
    let scope = Event.Terminal.scope terminal in
    let%bind _ =
      Event.create (Terminal terminal) ~limits:limits.Limits.event_limits
      |> Result.map_error ~f:(fun _ -> Contract_error.Evidence_limit)
    in
    let%bind () = check_scope scope (Observation.scope usage) in
    let%bind () =
      if
        (not (List.is_empty output))
        && not
             (Event.Terminal.equal_delivery
                (Event.Terminal.delivery terminal)
                Response_started)
      then Error Contract_error.Delivery_regression
      else Ok ()
    in
    let%bind () =
      Observation.validate usage ~limits:Observation.Admission.observation
      |> Result.map_error ~f:(fun _ -> Contract_error.Invalid_usage)
    in
    let%bind () =
      match Observation.payload usage with
      | Usage _ -> Ok ()
      | Context_estimate _ | Configuration _ | Transport_selection _ | Diagnostic _ ->
        Error Contract_error.Invalid_usage
    in
    let%map candidates =
      List.fold_result output ~init:Candidates.empty ~f:(fun candidates event ->
        Candidates.add candidates event ~scope ~limits ~allow_duplicate:false
        |> Result.map ~f:fst)
    in
    { terminal; usage; output; output_coverage; candidates }
  ;;

  let terminal t = t.terminal
  let usage t = t.usage
  let output t = t.output
  let output_coverage t = t.output_coverage
end

let configuration_matches request configuration =
  match
    Observation.Configuration.of_target
      ?transport_policy:(Observation.Configuration.transport_policy configuration)
      (Request.target request)
      ~preparation_id:(Observation.Configuration.preparation_id configuration)
      ~transport:(Observation.Configuration.transport configuration)
      ~capabilities:(Observation.Configuration.capabilities configuration)
      ~limits:Observation.Admission.observation
  with
  | Error _ -> false
  | Ok expected -> Observation.Configuration.equal expected configuration
;;

module Plan = struct
  type t =
    { request : Request.t
    ; configuration : Observation.Configuration.t
    ; fingerprint : string
    ; run :
        sw:Eio.Switch.t
        -> scope:Transcript.Scope.t
        -> accounting_id:Observation.Observation_id.t
        -> note_delivery:(Event.Terminal.delivery -> unit)
        -> on_event:(Event.t -> unit)
        -> on_observation:(Observation.t -> unit)
        -> Receipt.t
    }

  let create ~request ~configuration ~fingerprint ~run =
    if String.is_empty fingerprint || not (configuration_matches request configuration)
    then Error Preparation_error.Invalid_preparation
    else Ok { request; configuration; fingerprint; run }
  ;;
end

module Adapter = struct
  type prepare =
    preparation_id:string -> Request.t -> (Plan.t, Preparation_error.t) Result.t

  type t =
    { id : string
    ; limits : Limits.t
    ; bind : Request.Target.t -> (unit, Preparation_error.t) Result.t
    ; prepare : policy:Observation.Transport_policy.t -> prepare
    ; open_session :
        (Session.t
         -> policy:Observation.Transport_policy.t
         -> (prepare, Preparation_error.t) Result.t)
          option
    }

  let create ?prepare_with_policy ?open_session ~id ~limits ~bind ~prepare () =
    match
      Document_schema.Json.validate ~limits:Document_schema.Limits.default (`String id)
    with
    | Error _ -> Error Preparation_error.Invalid_preparation
    | Ok () ->
      if String.is_empty id
      then Error Preparation_error.Invalid_preparation
      else (
        let prepare =
          Option.value
            prepare_with_policy
            ~default:(fun ~policy ~preparation_id request ->
              match policy with
              | Observation.Transport_policy.Http_sse -> prepare ~preparation_id request
              | Prefer_websocket | Require_websocket ->
                Error Preparation_error.Transport_unavailable)
        in
        Ok { id; limits; bind; prepare; open_session })
  ;;
end

let contract_exn = function
  | Ok value -> value
  | Error error -> raise (Contract_violation error)
;;

let delivery_rank : Event.Terminal.delivery -> int = function
  | Definitely_not_submitted -> 0
  | Possibly_submitted -> 1
  | Response_started -> 2
;;

module Attempt = struct
  type t =
    { plan : Plan.t
    ; limits : Limits.t
    ; scope : Transcript.Scope.t
    ; accounting_id : Observation.Observation_id.t
    ; started : bool Atomic.t
    ; delivery : Event.Terminal.delivery Atomic.t
    }

  type run_error = Already_started [@@deriving equal, sexp_of]

  let scope t = t.scope
  let accounting_id t = t.accounting_id
  let configuration t = t.plan.configuration
  let delivery t = Atomic.get t.delivery

  let note_delivery t next =
    let previous = delivery t in
    if delivery_rank next < delivery_rank previous
    then raise (Contract_violation Delivery_regression);
    Atomic.set t.delivery next
  ;;

  let run t ~sw ~on_event ~on_observation =
    if not (Atomic.compare_and_set t.started false true)
    then Error Already_started
    else (
      Eio.Switch.check sw;
      let candidates = ref Candidates.empty in
      let latest_usage = ref None in
      let selected_transport = ref None in
      let check_observation observation =
        contract_exn (check_scope t.scope (Observation.scope observation));
        let limits =
          match Observation.payload observation with
          | Diagnostic _ -> Observation.Admission.diagnostic
          | Usage _ | Context_estimate _ | Configuration _ | Transport_selection _ ->
            Observation.Admission.observation
        in
        (match Observation.validate observation ~limits with
         | Ok () -> ()
         | Error _ -> raise (Contract_violation Invalid_usage));
        match Observation.payload observation with
        | Usage _ ->
          if
            not
              (Observation.Observation_id.equal
                 t.accounting_id
                 (Observation.id observation))
          then raise (Contract_violation Accounting_identity_mismatch);
          (match !latest_usage with
           | None -> `Publish
           | Some previous ->
             let comparison =
               Int64.compare
                 (Observation.revision observation)
                 (Observation.revision previous)
             in
             if comparison < 0
             then `Stale
             else if comparison > 0
             then `Publish
             else if Observation.equal previous observation
             then `Duplicate
             else raise (Contract_violation Conflicting_usage))
        | Transport_selection selection ->
          if Event.Terminal.equal_delivery (delivery t) Response_started
          then raise (Contract_violation Conflicting_transport);
          if
            not
              (Observation.Observation_id.equal
                 t.accounting_id
                 (Observation.Transport_selection.accounting_id selection))
          then raise (Contract_violation Accounting_identity_mismatch);
          (match Observation.Configuration.transport_policy t.plan.configuration with
           | Some policy
             when Observation.Transport_policy.equal
                    policy
                    (Observation.Transport_selection.requested selection) -> ()
           | Some _ | None -> raise (Contract_violation Configuration_mismatch));
          (match !selected_transport with
           | None -> `Publish
           | Some previous when Observation.equal previous observation -> `Duplicate
           | Some _ -> raise (Contract_violation Conflicting_transport))
        | Configuration configuration ->
          if not (Observation.Configuration.equal t.plan.configuration configuration)
          then raise (Contract_violation Configuration_mismatch);
          `Publish
        | Context_estimate context ->
          if
            not
              (String.equal
                 (Observation.Context_estimate.preparation_id context)
                 (Observation.Configuration.preparation_id t.plan.configuration))
          then raise (Contract_violation Configuration_mismatch);
          `Publish
        | Diagnostic _ -> `Publish
      in
      let publish_observation observation =
        match check_observation observation with
        | `Duplicate | `Stale -> ()
        | `Publish ->
          (match Observation.payload observation with
           | Usage _ -> latest_usage := Some observation
           | Transport_selection _ -> selected_transport := Some observation
           | Context_estimate _ | Configuration _ | Diagnostic _ -> ());
          on_observation observation
      in
      let publish_event event =
        contract_exn (check_scope t.scope (Event.scope event));
        match Event.view event with
        | Terminal _ -> raise (Contract_violation Backend_terminal)
        | Live _ ->
          let event =
            Event.create (Event.view event) ~limits:t.limits.event_limits
            |> Result.map_error ~f:(fun _ -> Contract_error.Evidence_limit)
            |> contract_exn
          in
          on_event event
        | Candidate_ready _ ->
          let next, publish =
            Candidates.add
              !candidates
              event
              ~scope:t.scope
              ~limits:t.limits
              ~allow_duplicate:true
            |> contract_exn
          in
          candidates := next;
          note_delivery t Response_started;
          if publish then on_event event
      in
      (* No catch encloses caller callbacks: cancellation and observer failures
         keep their original exception and backtrace. *)
      Atomic.set t.delivery Possibly_submitted;
      let receipt =
        t.plan.run
          ~sw
          ~scope:t.scope
          ~accounting_id:t.accounting_id
          ~note_delivery:(note_delivery t)
          ~on_event:publish_event
          ~on_observation:publish_observation
      in
      let receipt =
        Receipt.create
          ~terminal:receipt.terminal
          ~usage:receipt.usage
          ~output:receipt.output
          ~output_coverage:receipt.output_coverage
          ~limits:t.limits
        |> contract_exn
      in
      contract_exn (check_scope t.scope (Event.Terminal.scope receipt.terminal));
      (* Reconcile the entire receipt before any terminal-only call can reach the
         host. A missing/conflicting early candidate must not hide tool evidence. *)
      Map.iteri !candidates.events ~f:(fun ~key ~data:previous ->
        match Map.find receipt.candidates.events key with
        | None -> raise (Contract_violation Missing_candidate)
        | Some candidate ->
          if not (candidate_equal previous candidate)
          then raise (Contract_violation Conflicting_candidate));
      (match check_observation receipt.usage with
       | `Stale -> raise (Contract_violation Conflicting_usage)
       | `Duplicate | `Publish -> ());
      let final_delivery = Event.Terminal.delivery receipt.terminal in
      if
        Option.is_some (Observation.Configuration.transport_policy t.plan.configuration)
        && (not (Event.Terminal.equal_delivery final_delivery Definitely_not_submitted))
        && Option.is_none !selected_transport
      then raise (Contract_violation Missing_transport);
      if
        Event.Terminal.equal_delivery (delivery t) Response_started
        && not (Event.Terminal.equal_delivery final_delivery Response_started)
      then raise (Contract_violation Delivery_regression);
      Atomic.set t.delivery final_delivery;
      List.iter receipt.output ~f:publish_event;
      publish_observation receipt.usage;
      let terminal_event =
        Event.create (Terminal receipt.terminal) ~limits:t.limits.event_limits
        |> Result.map_error ~f:(fun _ -> Contract_error.Evidence_limit)
        |> contract_exn
      in
      on_event terminal_event;
      Ok receipt)
  ;;
end

module Prepared = struct
  type t =
    { plan : Plan.t
    ; limits : Limits.t
    }

  let target t = Request.target t.plan.request
  let preparation_id t = Observation.Configuration.preparation_id t.plan.configuration
  let configuration t = t.plan.configuration
  let fingerprint t = t.plan.fingerprint

  let start t ~scope ~accounting_id =
    Ok
      { Attempt.plan = t.plan
      ; limits = t.limits
      ; scope
      ; accounting_id
      ; started = Atomic.make false
      ; delivery = Atomic.make Event.Terminal.Definitely_not_submitted
      }
  ;;
end

let request_equal a b =
  Request.Target.equal (Request.target a) (Request.target b)
  && List.equal
       (fun a b ->
          History_entry.Id.equal (History_entry.id a) (History_entry.id b)
          && Document_schema.Json.equal
               (History_entry.Payload.to_json (History_entry.payload a))
               (History_entry.Payload.to_json (History_entry.payload b)))
       (Request.history a)
       (Request.history b)
  && List.equal Request.Tool_spec.equal (Request.tools a) (Request.tools b)
  && List.equal Request.Asset.equal (Request.assets a) (Request.assets b)
;;

module Context = struct
  type t =
    { adapter : Adapter.t
    ; target : Request.Target.t
    ; policy : Observation.Transport_policy.t
    ; session : Session.t option
    ; prepare : Adapter.prepare
    }

  let create adapter ~target =
    let open Result.Let_syntax in
    if not (String.equal adapter.Adapter.id (Request.Target.adapter target))
    then Error Preparation_error.Target_mismatch
    else (
      let%map () = adapter.bind target in
      let policy = Observation.Transport_policy.Http_sse in
      { adapter; target; policy; session = None; prepare = adapter.prepare ~policy })
  ;;

  let target t = t.target
  let transport_policy t = t.policy
  let detach t = { t with session = None; prepare = t.adapter.prepare ~policy:t.policy }

  let with_transport_policy t policy =
    { t with policy; session = None; prepare = t.adapter.prepare ~policy }
  ;;

  let with_session t session =
    if Session.is_closed session
    then Error Preparation_error.Session_closed
    else (
      match t.adapter.open_session with
      | None -> Ok { t with session = Some session }
      | Some open_session ->
        Result.map (open_session session ~policy:t.policy) ~f:(fun prepare ->
          { t with session = Some session; prepare }))
  ;;

  let closed_receipt ~scope ~accounting_id ~limits =
    let count =
      Observation.Count.create (Unknown Not_submitted)
      |> Result.map_error ~f:(fun _ -> Contract_error.Invalid_usage)
      |> contract_exn
    in
    let counts : Observation.Usage.counts =
      { input = count
      ; output = count
      ; reported_total = count
      ; cached_input = count
      ; cache_write_input = count
      ; reasoning_output = count
      }
    in
    let usage =
      Observation.Usage.create ~counts ~inclusions:[]
      |> Result.map_error ~f:(fun _ -> Contract_error.Invalid_usage)
      |> contract_exn
    in
    let usage =
      Observation.create
        ~scope
        ~id:accounting_id
        ~revision:0L
        ~payload:(Usage usage)
        ~limits:Observation.Admission.observation
      |> Result.map_error ~f:(fun _ -> Contract_error.Invalid_usage)
      |> contract_exn
    in
    let terminal =
      Event.Terminal.create
        ~scope
        ~delivery:Definitely_not_submitted
        ~outcome:(Failed (Transport Session_closed))
      |> Result.map_error ~f:(fun _ -> Contract_error.Backend_terminal)
      |> contract_exn
    in
    Receipt.create ~terminal ~usage ~output:[] ~output_coverage:Observed_prefix ~limits
    |> contract_exn
  ;;

  let prepare t ~preparation_id request =
    let open Result.Let_syntax in
    if Option.exists t.session ~f:Session.is_closed
    then Error Preparation_error.Session_closed
    else if not (Request.Target.equal t.target (Request.target request))
    then Error Preparation_error.Target_mismatch
    else (
      let%bind plan = t.prepare ~preparation_id request in
      if
        (not (request_equal plan.request request))
        || (not
              (String.equal
                 preparation_id
                 (Observation.Configuration.preparation_id plan.configuration)))
        || not (configuration_matches request plan.configuration)
      then Error Preparation_error.Invalid_preparation
      else (
        let original = plan.Plan.run in
        let plan =
          { plan with
            Plan.run =
              (fun ~sw ~scope ~accounting_id ~note_delivery ~on_event ~on_observation ->
                if Option.exists t.session ~f:Session.is_closed
                then closed_receipt ~scope ~accounting_id ~limits:t.adapter.limits
                else
                  original
                    ~sw
                    ~scope
                    ~accounting_id
                    ~note_delivery
                    ~on_event
                    ~on_observation)
          }
        in
        Ok { Prepared.plan; limits = t.adapter.limits }))
  ;;

  let identity target =
    match Request.Target.to_json target with
    | `Object fields ->
      `Object
        (List.filter fields ~f:(fun (name, _) ->
           not (String.equal name "model" || String.equal name "settings")))
    | `Null | `True | `False | `Number _ | `String _ | `Array _ -> assert false
  ;;

  let derive_in_session t ~target =
    if Document_schema.Json.equal (identity t.target) (identity target)
    then Result.map (t.adapter.bind target) ~f:(fun () -> { t with target })
    else Error Preparation_error.Target_mismatch
  ;;

  let derive t ~target = derive_in_session (detach t) ~target
end

type resolver = Request.Target.t -> (Context.t, Preparation_error.t) Result.t
