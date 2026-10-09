open! Core
module P = Agent_protocol

let get = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let source =
  P.Run_source.create
    ~observer:{ script_id = "workflow"; source_sha256 = String.make 64 'a' }
    ~generation:2
    ~installation_epoch:3L
  |> get
;;

let run_id = P.Id.Run.of_string "run_protocol_fixture" |> get
let job_id = P.Id.Job.of_string "job_protocol_fixture" |> get

let work =
  P.Run_work.create ~key:(Retained (Job { id = job_id; attempt = 1 })) ~generation:2
  |> get
;;

let now = P.Timestamp.of_string "2026-10-09T00:00:00Z" |> get

let make
      ?(revision = 3L)
      ?(owned_work = [ work ])
      ?(relinquished_work = [])
      ?(terminal_work = [])
      lifecycle
  =
  P.Run.create
    ~id:run_id
    ~session:
      (P.Session_ref.create
         ~server_id:(P.Id.Server.of_string "srv_fixture" |> get)
         ~session_id:(P.Id.Session.of_string "ses_fixture" |> get))
    ~principal_id:(P.Id.Principal.of_string "pri_fixture" |> get)
    ~source
    ~mode:Workflow
    ~lifecycle
    ~revision
    ~owned_work
    ~relinquished_work
    ~terminal_work
    ~created_at:now
    ~updated_at:now
;;

let%expect_test
    "run terminal cannot erase adverse owned occurrence or unresolved obligation"
  =
  let evidence outcome = P.Run_work.Terminal.create ~work ~outcome ~revision:3L |> get in
  let failed = evidence Failed in
  let succeeded = evidence Succeeded in
  print_s
    [%sexp
      { unresolved_rejected = (Result.is_error (make (Terminal (Completed None))) : bool)
      ; adverse_rejected =
          (Result.is_error (make ~terminal_work:[ failed ] (Terminal (Completed None)))
           : bool)
      ; adverse_transfer_rejected =
          (Result.is_error
             (make
                ~owned_work:[]
                ~relinquished_work:[ work ]
                ~terminal_work:[ failed ]
                (Terminal (Completed None)))
           : bool)
      ; successful_accepted =
          (Result.is_ok (make ~terminal_work:[ succeeded ] (Terminal (Completed None)))
           : bool)
      }];
  [%expect
    {|
    ((unresolved_rejected true) (adverse_rejected true)
     (adverse_transfer_rejected true) (successful_accepted true))
    |}]
;;

let%expect_test "immutable failure evidence survives retry and sexp entrypoints validate" =
  let failed = P.Run_work.Terminal.create ~work ~outcome:Failed ~revision:3L |> get in
  let previous = make ~terminal_work:[ failed ] Active |> get in
  let next = make ~revision:4L Active |> get in
  let malformed =
    match P.Run.to_json previous with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "revision" then `String "-1" else value))
    | _ -> assert false
  in
  let sexp_rejected =
    try
      ignore (P.Run.t_of_sexp (Jsonaf.sexp_of_t malformed) : P.Run.t);
      false
    with
    | Sexplib.Conv.Of_sexp_error _ -> true
  in
  print_s
    [%sexp
      { erased_evidence_rejected =
          (Result.is_error (P.Run.validate_transition ~previous next) : bool)
      ; malformed_sexp_rejected = (sexp_rejected : bool)
      ; roundtrip =
          (Jsonaf.exactly_equal
             (P.Run.to_json previous)
             (P.Run.to_json (P.Run.t_of_sexp (P.Run.sexp_of_t previous)))
           : bool)
      }];
  [%expect
    {|
    ((erased_evidence_rejected true) (malformed_sexp_rejected true)
     (roundtrip true))
    |}]
;;

let%expect_test "exact wakes reject fabricated kinds and preserve occurrence witnesses" =
  let wake =
    P.Run_wake.create ~run_id ~source ~occurrence:(Job_completion { job_id; attempt = 1 })
    |> get
  in
  let decoded = P.Run_wake.of_json (P.Run_wake.to_json wake) |> get in
  let fabricated =
    `Object [ "kind", `String "future_event"; "name", `String "eventually" ]
  in
  print_s
    [%sexp
      { roundtrip = (P.Run_wake.equal wake decoded : bool)
      ; fabricated_rejected =
          (Result.is_error (P.Run_wake.Occurrence.of_json fabricated) : bool)
      ; identical_continue =
          (Result.is_ok (P.Run_action.combine (Some Continue) (Some Continue)) : bool)
      ; incompatible =
          (Result.is_error (P.Run_action.combine (Some Continue) (Some (Wait wake)))
           : bool)
      }];
  [%expect
    {|
    ((roundtrip true) (fabricated_rejected true) (identical_continue true)
     (incompatible true))
    |}]
;;

let%expect_test "interrupted custody retains ambiguous effects without claiming success" =
  let ambiguous =
    P.Run_work.Terminal.create ~work ~outcome:Unconfirmed ~revision:3L |> get
  in
  print_s
    [%sexp
      { interrupted_retained =
          (Result.is_ok (make ~terminal_work:[ ambiguous ] (Terminal Interrupted)) : bool)
      ; completed_rejected =
          (Result.is_error (make ~terminal_work:[ ambiguous ] (Terminal (Completed None)))
           : bool)
      }];
  [%expect {| ((interrupted_retained true) (completed_rejected true)) |}]
;;

let%expect_test
    "raw occurrence bound precedes nested decoding and direct admission copies"
  =
  let count = P.Run_limits.max_occurrences + 1 in
  let raw =
    P.Run_limits.list P.Run_work.of_json (`Array (List.init count ~f:(fun _ -> `Null)))
  in
  let count_error =
    match raw with
    | Error error -> String.equal error.message "run occurrence bound exceeded"
    | Ok _ -> false
  in
  let direct = make ~owned_work:(List.init count ~f:(fun _ -> work)) Active in
  print_s
    [%sexp
      { raw_bound_before_decode = (count_error : bool)
      ; direct_bound = (Result.is_error direct : bool)
      }];
  [%expect {| ((raw_bound_before_decode true) (direct_bound true)) |}]
;;
