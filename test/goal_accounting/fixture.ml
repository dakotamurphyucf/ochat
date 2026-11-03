open! Core
module P = Agent_protocol
module G = P.Goal
module L = Agent_session.Inference_ledger
module A = Agent_session.Goal_accounting
module U = Agent_session.Goal_usage
module O = Inference.Observation
module D = Document_schema

let protocol result =
  Result.map_error result ~f:(fun (error : P.Error.t) -> error.message)
  |> Result.ok_or_failwith
;;

let observed result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (O.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let ledger result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (L.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let account result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (A.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let sid = P.Id.Session.of_string "ses_charge" |> protocol

let session =
  P.Session_ref.create
    ~session_id:sid
    ~server_id:(P.Id.Server.of_string "srv_charge" |> protocol)
;;

let principal = P.Id.Principal.of_string "pri_charge" |> protocol
let timestamp = P.Timestamp.of_string "2026-11-03T00:00:00Z" |> protocol

let source ?(generation = 0) ?(epoch = 1L) () =
  P.Run_source.create
    ~observer:{ script_id = "accounting"; source_sha256 = String.make 64 'a' }
    ~generation
    ~installation_epoch:epoch
  |> protocol
;;

let configuration =
  let target =
    Inference.Request.Target.create
      ~adapter:"synthetic"
      ~profile:"test"
      ~profile_revision:None
      ~account:None
      ~endpoint:"https://invalid.test"
      ~model:"test"
      ~settings:[]
      ~limits:Transcript.Admission.default
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  O.Configuration.of_target
    target
    ~preparation_id:"prepared"
    ~transport:In_process
    ~capabilities:[]
    ~limits:O.Admission.observation
  |> observed
;;

let make_ledger ?(attempts = 3) ?(turns = 3) () =
  let document_limits =
    Transcript.Admission.limits ~max_bytes:(4 * 1024 * 1024)
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let limits =
    L.Limits.create
      ~max_attempts:attempts
      ~max_turns:turns
      ~max_retained_bytes:(4 * 1024 * 1024)
      ~document_limits
    |> ledger
  in
  L.create ~session_id:sid ~generation:0 ~before_tracking_unknown:false ~limits |> ledger
;;

let goal ?(suffix = "one") () =
  let run =
    P.Run.create
      ~id:(P.Id.Run.of_string ("run_" ^ suffix) |> protocol)
      ~session
      ~principal_id:principal
      ~source:(source ())
      ~mode:Workflow
      ~lifecycle:Admitted
      ~revision:0L
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:timestamp
      ~updated_at:timestamp
    |> protocol
  in
  let receipt =
    P.Run_receipt.create
      ~run_id:run.id
      ~principal_id:principal
      ~source:run.source
      ~key:(P.Idempotency_key.of_string ("admit_" ^ suffix) |> protocol)
      ~request_sha256:(String.make 64 'a')
      ~kind:Admission
      ~run_revision:0L
      ~session_revision:0L
      ~committed_at:timestamp
    |> protocol
  in
  let episode = G.Episode.create ~run ~admission:receipt |> protocol in
  let goal =
    G.create
      ~id:(P.Id.Goal.of_string ("goal_" ^ suffix) |> protocol)
      ~session
      ~creator:principal
      ~source:(source ())
      ~objective:(G.Objective.of_string "Account work" |> protocol)
      ~budget:G.Budget.unlimited
    |> protocol
  in
  G.bind_episode goal episode ~previous:None |> protocol, episode
;;

let admit current =
  L.admit
    current
    ~source:(Transcript.Source_id.of_string "dispatch" |> Result.ok_or_failwith)
    ~relation:Root
    ~operation_id:None
    ~invocation_id:None
    ~configuration
  |> ledger
;;

let step accounting goal before after admission =
  A.plan accounting ~goal ~before ~after ~admission |> account |> A.Plan.candidate
;;

let admitted accounting goal episode before =
  let after, handle, tracking = admit before in
  let accounting =
    step
      accounting
      goal
      before
      after
      (Some (A.Admission.Attempt { episode; handle; tracking }))
  in
  after, accounting, handle, tracking
;;

let components =
  [ O.Usage.Component.Input
  ; Output
  ; Reported_total
  ; Cached_input
  ; Cache_write_input
  ; Reasoning_output
  ]
;;

let count view = O.Count.create view |> observed

let usage ?(inclusions = []) handle ~revision views =
  let get component =
    List.Assoc.find_exn views component ~equal:O.Usage.Component.equal |> count
  in
  let counts : O.Usage.counts =
    { input = get Input
    ; output = get Output
    ; reported_total = get Reported_total
    ; cached_input = get Cached_input
    ; cache_write_input = get Cache_write_input
    ; reasoning_output = get Reasoning_output
    }
  in
  let usage = O.Usage.create ~counts ~inclusions |> observed in
  O.create
    ~scope:(L.Handle.scope handle)
    ~id:(L.Handle.accounting_id handle)
    ~revision
    ~payload:(Usage usage)
    ~limits:O.Admission.observation
  |> observed
;;

let actual_views tokens =
  List.map components ~f:(fun component -> component, O.Count.Actual tokens)
;;

let put current handle revision views =
  L.observe current handle (usage handle ~revision views) |> ledger |> fst
;;

let metric accounting current component =
  U.component (A.usage accounting ~ledger:current |> account) component
;;

let interrupted =
  O.Attempt_record.Interrupted
    { reason = Host_interrupted; delivery = Possibly_submitted }
;;

let operation index : P.Operation.t =
  { id = P.Id.Operation.of_string ("op_turn_" ^ Int.to_string index) |> protocol
  ; generation = 0
  ; kind = Turn User_submit
  ; state = Starting
  ; started_at = timestamp
  ; updated_at = timestamp
  }
;;
