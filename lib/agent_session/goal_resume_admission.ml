open! Core
module P = Agent_protocol
module G = P.Goal

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let find_receipt (state : Session_state.t) ~request ~principal_id =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Id.Session.equal
        (Goal_resume_request.session_id request)
        state.identity.session_id
    then Ok ()
    else Error (P.Error.invalid_request "Goal resume targets another actor")
  in
  let%bind request_sha256 = Goal_resume_request.digest request in
  Goal_catalog.receipt
    (Option.value state.goal_catalog ~default:Goal_catalog.empty)
    ~principal_id
    ~key:(Goal_resume_request.key request)
    ~request_sha256
;;

module Basis = struct
  type t =
    { session_id : P.Id.Session.t
    ; generation : int
    ; revision : int64
    ; catalog_revision : int64
    ; goal : G.t
    ; run : P.Run.t
    ; admission : P.Run_receipt.t
    ; link : Run_goal_link.t
    ; installation : Run_source_installation.t
    ; accounting : Goal_accounting.t
    ; ledger_revision : int64
    }

  let equal before after =
    P.Id.Session.equal before.session_id after.session_id
    && Int.equal before.generation after.generation
    && Int64.equal before.revision after.revision
    && Int64.equal before.catalog_revision after.catalog_revision
    && G.equal before.goal after.goal
    && P.Run.equal before.run after.run
    && P.Run_receipt.equal before.admission after.admission
    && Run_goal_link.equal before.link after.link
    && Run_source_installation.equal before.installation after.installation
    && Goal_accounting.equal before.accounting after.accounting
    && Int64.equal before.ledger_revision after.ledger_revision
  ;;
end

type t =
  { request : Goal_resume_request.t
  ; session : P.Session_ref.t
  ; basis : Basis.t
  ; catalog : Goal_catalog.t
  ; receipt : Goal_command_receipt.t
  ; goal : G.t
  }

module Decision = struct
  type plan = t

  type t =
    | Retained of Goal_command_receipt.t
    | Commit of plan
end

let catalog t = t.catalog
let receipt t = t.receipt
let goal t = t.goal
let run t = t.basis.run
let admission t = t.basis.admission
let link t = t.basis.link

let select_basis (state : Session_state.t) ~request ~session =
  let open Result.Let_syntax in
  let%bind catalog =
    Result.of_option
      state.goal_catalog
      ~error:(P.Error.invalid_request "Goal resume catalog is absent")
  in
  let%bind index =
    Result.of_option
      state.run_state
      ~error:(P.Error.invalid_request "Goal resume Run index is absent")
  in
  let%bind goal =
    Result.of_option
      (Goal_catalog.find catalog (Goal_resume_request.goal_id request))
      ~error:(P.Error.invalid_request "Goal resume target is absent")
  in
  let%bind run =
    Result.of_option
      (Run_state.find index (Goal_resume_request.run_id request))
      ~error:(P.Error.invalid_request "Goal resume Run is absent")
  in
  let%bind () =
    if
      P.Id.Session.equal
        (Goal_resume_request.session_id request)
        state.identity.session_id
      && P.Id.Session.equal (P.Session_ref.session_id session) state.identity.session_id
      && P.Session_ref.equal session (G.session goal)
      && P.Session_ref.equal session run.session
      && Int.equal (Goal_resume_request.generation request) state.identity.generation
      && Int64.equal
           (Goal_resume_request.expected_catalog_revision request)
           (Goal_catalog.revision catalog)
      && Int64.equal
           (Goal_resume_request.expected_goal_revision request)
           (G.revision goal)
      && Int64.equal (Goal_resume_request.expected_run_revision request) run.revision
      && P.Run.Mode.equal run.mode Workflow
      && P.Id.Principal.equal (G.creator goal) run.principal_id
      && P.Run_source.equal (Goal_resume_request.source request) (G.source goal)
      && P.Run_source.equal (Goal_resume_request.source request) run.source
      && Int.equal run.source.generation state.identity.generation
    then Ok ()
    else conflict "Goal resume original session, source or episode frontier changed"
  in
  let%bind () =
    match G.state goal, run.lifecycle with
    | Open (Held _), (Admitted | Active | Waiting _) -> Ok ()
    | (Open (Enabled | Held _) | Closed _), (Admitted | Active | Waiting _ | Terminal _)
      -> conflict "Goal resume requires its held nonterminal Workflow episode"
  in
  let%bind link =
    Result.of_option
      (Run_state.goal_link index run.id)
      ~error:(P.Error.invalid_request "Goal resume immutable Run link is absent")
  in
  let%bind episode =
    Result.of_option
      (G.episode goal)
      ~error:(P.Error.invalid_request "Goal resume episode is absent")
  in
  let admission = P.Goal_episode.admission episode in
  let%bind retained =
    Run_state.receipt
      index
      ~principal_id:admission.principal_id
      ~key:admission.key
      ~request_sha256:admission.request_sha256
  in
  let%bind () =
    if
      P.Id.Goal.equal (Run_goal_link.goal_id link) (G.id goal)
      && P.Goal_episode.equal (Run_goal_link.episode link) episode
      && P.Goal_episode.matches_run episode run
      && Option.exists retained ~f:(P.Run_receipt.equal admission)
    then Ok ()
    else conflict "Goal resume immutable episode or Admission evidence changed"
  in
  (* Reuse the real checkpoint/installation law without adopting its candidate.
     A missing/replaced installation is not a cold-loading opportunity to rebase
     this existing Run's source. No runtime or initializer is invoked here. *)
  let%bind captured_index, source = Moderator_source_installation.capture state in
  let%bind () =
    if Run_state.equal index captured_index && P.Run_source.equal source run.source
    then Ok ()
    else conflict "Goal resume requires its unchanged full source installation"
  in
  let%bind account =
    Option.bind state.goal_accounts ~f:(fun accounts ->
      Goal_accounts.find accounts (G.id goal))
    |> Result.of_option
         ~error:(P.Error.invalid_request "Goal resume canonical accounting is absent")
  in
  let%map accounting =
    Goal_accounting_document.resolve account ~goal ~ledger:state.inference_ledger
  in
  ( catalog
  , { Basis.session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; revision = state.counters.revision
    ; catalog_revision = Goal_catalog.revision catalog
    ; goal
    ; run
    ; admission
    ; link
    ; installation = Run_state.installation index
    ; accounting
    ; ledger_revision = Inference_ledger.revision state.inference_ledger
    } )
;;

let prepare (state : Session_state.t) ~request ~principal_id ~session ~now =
  let open Result.Let_syntax in
  let%bind retained = find_receipt state ~request ~principal_id in
  match retained with
  | Some receipt -> Ok (Decision.Retained receipt)
  | None ->
    let%bind catalog, basis = select_basis state ~request ~session in
    let%bind goal = G.resume basis.goal in
    let%bind eligibility =
      Goal_admission_policy.check_activation
        goal
        ~accounting:basis.accounting
        ~ledger:state.inference_ledger
        ~now
    in
    let%bind () =
      match eligibility with
      | Allow -> Ok ()
      | Hold hold ->
        Error
          (P.Error.create
             Permission_denied
             ~message:
               ("Goal resume policy denies activation: "
                ^ Sexp.to_string_hum (G.Hold.sexp_of_t hold))
             ~retryable:false
             ())
    in
    let%bind previous_document =
      Result.of_option
        (Goal_catalog.document catalog (G.id goal))
        ~error:(P.Error.invalid_request "Goal resume preserved document is absent")
    in
    let%bind document =
      Goal_document.update previous_document goal
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request
          (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
    in
    let%bind () =
      if Int64.equal (Goal_catalog.revision catalog) Int64.max_value
      then conflict "Goal resume catalog revision exhausted"
      else Ok ()
    in
    let%bind request_sha256 = Goal_resume_request.digest request in
    let%bind receipt =
      Goal_command_receipt.create
        ~principal_id
        ~key:(Goal_resume_request.key request)
        ~request_sha256
        ~goal_id:(G.id goal)
        ~goal_revision:(G.revision goal)
        ~catalog_revision:(Int64.succ (Goal_catalog.revision catalog))
        ~source:(Some (Goal_resume_request.source request))
        ~accepted_at:now
    in
    let%bind catalog = Goal_catalog.commit catalog ~goal:document ~receipt in
    let%map () =
      Goal_run_evidence.validate_transition
        ~previous_runs:state.run_state
        ~catalog:(Some catalog)
        ~runs:state.run_state
    in
    Decision.Commit { request; session; basis; catalog; receipt; goal }
;;

let check_basis t ~state =
  let open Result.Let_syntax in
  let%bind _, current = select_basis state ~request:t.request ~session:t.session in
  if Basis.equal t.basis current
  then Ok ()
  else conflict "Goal resume candidate no longer matches its actual session basis"
;;
