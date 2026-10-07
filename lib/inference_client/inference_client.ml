open! Core
module R = Inference_runtime

module Error = struct
  type t =
    | Preparation of R.Preparation_error.t
    | Attempt of R.Attempt.run_error
  [@@deriving equal, sexp_of]
end

module Completion = struct
  type outcome =
    | Returned of Inference.Event.Terminal.t
    | Interrupted of
        { reason : Inference.Observation.Attempt_record.interruption
        ; delivery : Inference.Event.Terminal.delivery
        }

  type t =
    { attempt : R.Attempt.t
    ; outcome : outcome
    }

  let attempt t = t.attempt
  let outcome t = t.outcome
end

module Identity = struct
  type t =
    { new_preparation_id : unit -> string
    ; with_attempt :
        'a.
        R.Prepared.t
        -> relation:Transcript.Scope.relation
        -> f:
             (scope:Transcript.Scope.t
              -> accounting_id:Inference.Observation.Observation_id.t
              -> 'a)
        -> 'a
    }
end

let run
      context
      ~sw
      ~(identity : Identity.t)
      ~relation
      ~request
      ~before_dispatch
      ~on_attempt
      ~on_completion
      ~on_event
      ~on_observation
  =
  let open Result.Let_syntax in
  let preparation_id = identity.new_preparation_id () in
  let%bind prepared =
    R.Context.prepare context ~preparation_id request
    |> Result.map_error ~f:(fun error -> Error.Preparation error)
  in
  before_dispatch prepared;
  identity.with_attempt prepared ~relation ~f:(fun ~scope ~accounting_id ->
    let%bind expected_scope =
      Transcript.Scope.create
        ~source:scope.key.source
        ~attempt:scope.key.attempt
        ~relation
      |> Result.map_error ~f:(fun _ ->
        Error.Preparation R.Preparation_error.Invalid_preparation)
    in
    let%bind () =
      if Transcript.Scope.equal scope expected_scope
      then Ok ()
      else Error (Error.Preparation R.Preparation_error.Invalid_preparation)
    in
    let%bind attempt =
      R.Prepared.start prepared ~scope ~accounting_id
      |> Result.map_error ~f:(fun error -> Error.Preparation error)
    in
    let result =
      try
        on_attempt attempt;
        `Result
          (R.Attempt.run attempt ~sw ~on_event ~on_observation
           |> Result.map_error ~f:(fun error -> Error.Attempt error))
      with
      | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let interrupted reason =
      { Completion.attempt
      ; outcome = Interrupted { reason; delivery = R.Attempt.delivery attempt }
      }
    in
    match result with
    | `Result (Ok receipt) ->
      (* Outside the exception boundary: a failed terminal acknowledgement cannot
       produce a second, contradictory interruption callback. *)
      on_completion
        { Completion.attempt; outcome = Returned (R.Receipt.terminal receipt) };
      Ok receipt
    | `Result (Error error) ->
      on_completion (interrupted Host_interrupted);
      Error error
    | `Raised (exn, backtrace) ->
      let reason =
        match exn with
        | Eio.Cancel.Cancelled _ -> Inference.Observation.Attempt_record.Cancelled
        | _ -> Host_interrupted
      in
      (try Eio.Cancel.protect (fun () -> on_completion (interrupted reason)) with
       | _ -> ());
      Exn.raise_with_original_backtrace exn backtrace)
;;

module Text = struct
  module P = History_entry.Payload

  type t =
    { receipt : R.Receipt.t
    ; messages : string list
    ; refusals : string list
    }

  let history ~namespace messages =
    let open Result.Let_syntax in
    let%bind allocator = History_entry.Allocator.create ~namespace ~next_sequence:0 in
    List.map messages ~f:(fun (role, text) ->
      let%bind semantic =
        P.Semantic.create
          (Message
             { form = Input
             ; role
             ; content = [ Text { text; annotations = []; logprobs = Absent } ]
             ; phase = Absent
             })
          ~metadata:P.Metadata.empty
      in
      History_entry.create ~allocator (P.authored semantic))
    |> Result.all
  ;;

  let of_receipt receipt =
    let messages, refusals =
      List.fold
        (R.Receipt.output receipt)
        ~init:([], [])
        ~f:(fun (messages, refusals) event ->
          match Inference.Event.view event with
          | Live _ | Terminal _ -> messages, refusals
          | Candidate_ready { payload; _ } ->
            (match P.Semantic.view (P.semantic payload) with
             | Message { form = Output; role = Assistant; content; phase = _ } ->
               let parts, refusals =
                 List.fold
                   content
                   ~init:([], refusals)
                   ~f:(fun (parts, refusals) -> function
                   | P.Content.Text { text; _ } -> text :: parts, refusals
                   | Refusal text -> parts, text :: refusals
                   | Image _ | Unknown _ -> parts, refusals)
               in
               let messages =
                 if List.is_empty parts
                 then messages
                 else String.concat (List.rev parts) :: messages
               in
               messages, refusals
             | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ ->
               messages, refusals))
    in
    { receipt; messages = List.rev messages; refusals = List.rev refusals }
  ;;

  let receipt t = t.receipt
  let messages t = t.messages
  let refusals t = t.refusals
end

module Execution = struct
  type t =
    { context : R.Context.t
    ; identity : Identity.t
    ; relation : Transcript.Scope.relation
    ; before_dispatch : R.Prepared.t -> unit
    ; on_attempt : R.Attempt.t -> unit
    ; on_completion : Completion.t -> unit
    ; on_observation : Inference.Observation.t -> unit
    }

  let create
        ~context
        ~identity
        ~relation
        ~before_dispatch
        ~on_attempt
        ~on_completion
        ~on_observation
    =
    { context
    ; identity
    ; relation
    ; before_dispatch
    ; on_attempt
    ; on_completion
    ; on_observation
    }
  ;;

  let context t = t.context

  let run t ~sw ~request ~on_event =
    run
      t.context
      ~sw
      ~identity:t.identity
      ~relation:t.relation
      ~request
      ~before_dispatch:t.before_dispatch
      ~on_attempt:t.on_attempt
      ~on_completion:t.on_completion
      ~on_event
      ~on_observation:t.on_observation
  ;;

  module Completion_error = struct
    type t =
      | Dispatch of Error.t
      | Outcome of Inference.Event.Terminal.outcome
      | No_text
    [@@deriving equal, sexp_of]
  end

  let complete_text t ~sw ?model ~settings ~messages () =
    let open Result.Let_syntax in
    let preparation result =
      Result.map_error result ~f:(fun error ->
        Completion_error.Dispatch (Error.Preparation error))
    in
    let request_error result =
      Result.map_error result ~f:(fun error -> R.Preparation_error.Invalid_request error)
      |> preparation
    in
    let limits = Document_schema.Limits.default in
    let%bind () =
      match
        List.find_a_dup
          (List.map settings ~f:Inference.Request.Setting.name)
          ~compare:String.compare
      with
      | None -> Ok ()
      | Some name ->
        Error (Inference.Request.Error.Duplicate_setting name) |> request_error
    in
    let target = R.Context.target t.context in
    let%bind target =
      match model with
      | None -> Ok target
      | Some model ->
        Inference.Request.Target.with_model target ~model ~limits |> request_error
    in
    let%bind target =
      List.fold_result settings ~init:target ~f:(fun target setting ->
        Inference.Request.Target.with_setting
          target
          ~name:(Inference.Request.Setting.name setting)
          ~value:(Inference.Request.Setting.value setting)
          ~provenance:(Inference.Request.Setting.provenance setting)
          ~limits
        |> request_error)
    in
    let%bind target =
      if
        List.exists (Inference.Request.Target.settings target) ~f:(fun setting ->
          String.equal (Inference.Request.Setting.name setting) "tool_choice")
      then
        Inference.Request.Target.with_setting
          target
          ~name:"tool_choice"
          ~value:Absent
          ~provenance:Execution_override
          ~limits
        |> request_error
      else Ok target
    in
    let%bind target =
      match
        List.find (Inference.Request.Target.settings target) ~f:(fun setting ->
          String.equal (Inference.Request.Setting.name setting) "text")
      with
      | None -> Ok target
      | Some setting ->
        (match Inference.Request.Setting.value setting with
         | Value (`Object fields) ->
           let constrained =
             match List.Assoc.find fields "format" ~equal:String.equal with
             | Some (`Object format) ->
               (match List.Assoc.find format "type" ~equal:String.equal with
                | Some (`String ("json_schema" | "json_object")) -> true
                | _ -> false)
             | _ -> false
           in
           if not constrained
           then Ok target
           else (
             let fields =
               List.map fields ~f:(fun (name, value) ->
                 ( name
                 , if String.equal name "format"
                   then `Object [ "type", `String "text" ]
                   else value ))
             in
             Inference.Request.Target.with_setting
               target
               ~name:"text"
               ~value:(Value (`Object fields))
               ~provenance:Execution_override
               ~limits
             |> request_error)
         | Absent | Null | Value _ -> Ok target)
    in
    let%bind context = R.Context.derive t.context ~target |> preparation in
    let namespace = t.identity.new_preparation_id () in
    let%bind history =
      Text.history ~namespace messages
      |> Result.map_error ~f:(fun _ -> R.Preparation_error.Unsupported_input)
      |> preparation
    in
    let%bind request =
      Inference.Request.create ~target ~history ~tools:[] ~assets:[] ~limits
      |> request_error
    in
    let%bind receipt =
      run { t with context } ~sw ~request ~on_event:ignore
      |> Result.map_error ~f:(fun error -> Completion_error.Dispatch error)
    in
    match Inference.Event.Terminal.outcome (R.Receipt.terminal receipt) with
    | Completed ->
      (match Text.messages (Text.of_receipt receipt) with
       | [] -> Error Completion_error.No_text
       | messages -> Ok (String.concat ~sep:"\n" messages))
    | (Refused | Incomplete _ | Failed _) as outcome ->
      Error (Completion_error.Outcome outcome)
  ;;
end
