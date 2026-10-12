open Core
module Baseline = Prompt_project_baseline
module Store = Prompt_project_baseline_store
module Sources = Prompt_script_preparation
module Owner = Prompt_source_owner
module Builder = Prompt_revision_builder
module Plan = Chat_response.Script_contract_plan
module Startup = Chat_response.Startup_compilation
module Operation = Chatml_project_build.Operation

module Mode = struct
  type t =
    | Build
    | Update
  [@@deriving equal, sexp_of]
end

module Prepared = struct
  type t =
    { programs : Chatml_prompt_programs.t
    ; association : Baseline.Prepared.t
    }

  let programs t = t.programs
  let association t = t.association
end

type previous =
  | First_admission
  | Established of Baseline.Entry.Prior.t
  | Untracked

let baseline_message = function
  | Baseline.Error.Limit_exceeded -> "Dependency history exceeds the host allowance"
  | Invalid_association message -> message
  | Ambiguous_association -> "Dependency history has ambiguous script roles"
  | Foreign_artifact -> "Dependency history does not match its saved artifact"
  | Foreign_operator_intent -> "Dependency history belongs to a different source intent"
  | Missing_program_bundle -> "Dependency history lacks captured program custody"
  | Document _ -> "Dependency history document is invalid"
  | Custody _ | Retained _ -> "Dependency history program custody is invalid"
;;

let store_message = function
  | Store.Error.Busy -> "Dependency history is busy"
  | Closed -> "Dependency history is closed"
  | Uncertain | Publication_uncertain _ -> "Dependency history publication is uncertain"
  | Stale_snapshot -> "Dependency history changed during preparation"
  | Foreign_snapshot -> "Dependency history snapshot belongs to another owner"
  | Untracked _ -> "Dependency history is unavailable; explicit recovery is required"
  | Baseline error -> baseline_message error
  | Store error -> Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error)
;;

let capture_with
      policy
      ?startup
      ~mode
      ~snapshot
      ~artifact_store
      ~baseline_limits
      ~custody_limits
      ~capture_sources
      ~host
      ~scope
      ~capture
      ~provided
      ~env
      ~checkpoint
      ()
  =
  let source =
    Builder.Capture.root (Owner.Capture.value capture)
    |> Prompt.Chat_markdown.Parsed.root_input
    |> Chatmd_parser_input.path
  in
  let diagnostic code message =
    [ Builder.Diagnostic.{ code; message; source = Some source } ]
  in
  let module Abort = struct
    exception Stop of Builder.Diagnostic.t list
  end
  in
  let reject code message = raise (Abort.Stop (diagnostic code message)) in
  let admit_baseline result =
    match result with
    | Ok value -> value
    | Error error -> reject "prompt.project_history" (baseline_message error)
  in
  Startup.with_context
    ?startup
    ~limits:(Sources.Policy.compilation policy)
    ~env
    ~checkpoint
    ~map_error:(fun error -> diagnostic error.Chatml_compilation.code error.message)
    ~f:(fun startup ->
      Startup.within
        startup
        ~map_error:(fun error -> diagnostic error.Chatml_compilation.code error.message)
        ~f:(fun () ->
          let remaining = ref (Baseline.Limits.max_work_steps baseline_limits) in
          let poll () =
            Startup.checkpoint startup;
            if !remaining <= 0
            then
              reject
                "prompt.project_history_limit"
                "Project preparation allowance exhausted";
            decr remaining;
            match Owner.Scope.check_capture scope capture with
            | Ok () -> ()
            | Error _ ->
              reject "prompt.project_source_owner" "Original source owner is unavailable"
          in
          try
            poll ();
            let previous =
              match
                Store.Snapshot.previous
                  snapshot
                  ~receipt:(Owner.Capture.receipt capture)
                  ~checkpoint:poll
              with
              | Error error -> reject "prompt.project_history" (store_message error)
              | Ok First_admission -> First_admission
              | Ok (Untracked _) -> Untracked
              | Ok (Established entry) ->
                poll ();
                let artifact =
                  match
                    Agent_store.Prompt_artifact_store.load
                      artifact_store
                      (Baseline.Entry.revision_id entry)
                  with
                  | Ok artifact -> artifact
                  | Error error ->
                    reject
                      "prompt.project_history_artifact"
                      (Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
                in
                poll ();
                Established
                  (Baseline.Entry.reconcile
                     entry
                     ~artifact
                     ~custody_limits
                     ~limits:baseline_limits
                     ~checkpoint:poll
                   |> admit_baseline)
            in
            let operation program =
              poll ();
              match previous with
              | Untracked ->
                reject
                  "prompt.project_history_untracked"
                  "Configured projects require authoritative dependency history"
              | First_admission ->
                (match mode with
                 | Mode.Build -> Operation.Initial_prepare
                 | Update -> Operation.Update { previous_lock = None })
              | Established prior ->
                let previous_lock =
                  match
                    Baseline.Entry.Prior.find
                      prior
                      ~slot:
                        (Chatml_prompt_programs.Binding.slot
                           (Plan.Program.binding program))
                      ~role:(Plan.Program.role program)
                      ~limits:baseline_limits
                      ~checkpoint:poll
                    |> admit_baseline
                  with
                  | New_slot | Provided -> None
                  | Selected lock -> Some lock
                in
                (match mode, previous_lock with
                 | Mode.Update, previous_lock -> Operation.Update { previous_lock }
                 | Build, None -> Operation.Build_without_previous
                 | Build, Some previous_lock -> Operation.Build { previous_lock })
            in
            let open Result.Let_syntax in
            let%map prepared =
              capture_sources
                policy
                ?startup:(Some startup)
                ~host
                ~scope
                ~capture
                ~operation
                ~provided
                ~env
                ~checkpoint:poll
                ()
            in
            let programs = Sources.Prepared.programs prepared in
            let association =
              Baseline.Prepared.create
                programs
                ~capture:(Owner.Capture.value capture)
                ~plans:(Sources.Prepared.captured_plans prepared)
                ~limits:baseline_limits
                ~checkpoint:poll
              |> admit_baseline
            in
            poll ();
            { Prepared.programs; association }
          with
          | Abort.Stop diagnostics -> Error diagnostics))
    ()
;;

let capture
      policy
      ?startup
      ~mode
      ~snapshot
      ~artifact_store
      ~baseline_limits
      ~custody_limits
      ~root_role
      ~host
      ~scope
      ~capture
      ~provided
      ~env
      ~checkpoint
      ()
  =
  capture_with
    policy
    ?startup
    ~mode
    ~snapshot
    ~artifact_store
    ~baseline_limits
    ~custody_limits
    ~capture_sources:(Sources.capture ~root_role)
    ~host
    ~scope
    ~capture
    ~provided
    ~env
    ~checkpoint
    ()
;;

let capture_child
      policy
      ?startup
      ~mode
      ~snapshot
      ~artifact_store
      ~baseline_limits
      ~custody_limits
      ~invocation
      ~host
      ~scope
      ~capture
      ~provided
      ~env
      ~checkpoint
      ()
  =
  capture_with
    policy
    ?startup
    ~mode
    ~snapshot
    ~artifact_store
    ~baseline_limits
    ~custody_limits
    ~capture_sources:(Sources.capture_child ~invocation)
    ~host
    ~scope
    ~capture
    ~provided
    ~env
    ~checkpoint
    ()
;;
