open! Core
module P = Agent_protocol
module D = Document_schema
module Request = P.Goal_native_start_request
module Intent = P.Goal_native_start_intent
module Action = P.Goal_native_action_intent
module Existing = Goal_resume_request_tests

let ok = Existing.ok
let timestamp = Goal_native_intent_tests.timestamp
let source = Existing.make_source ()
let principal = P.Id.Principal.of_string "pri_native_start" |> ok

let fields = function
  | `Object fields -> fields
  | _ -> failwith "expected protocol object"
;;

let replace value name field =
  `Object (List.Assoc.add (fields value) ~equal:String.equal name field)
;;

let request
      ?(generation = 0)
      ?(source = source)
      ?(goal = "goal_native_start")
      ?(goal_revision = 0L)
      ?(catalog_revision = 0L)
      ?(input = P.Run_start.Input.Authored_start)
      ?(key = "native-start")
      ()
  =
  Request.create
    ~generation
    ~source
    ~goal_id:(P.Id.Goal.of_string goal |> ok)
    ~expected_goal_revision:goal_revision
    ~expected_catalog_revision:catalog_revision
    ~input
    ~key:(P.Idempotency_key.of_string key |> ok)
;;

let submission text =
  P.Run_start.Input.User_submission { kind = Plain_text; text; attachments = [] }
;;

let%expect_test "native Start freezes every command field and the complete real input" =
  let base = request () |> ok in
  let digest value = Request.digest value |> ok in
  let alternatives =
    [ request ~generation:1 ~source:(Existing.make_source ~generation:1 ()) ()
    ; request ~source:(Existing.make_source ~installation_epoch:2L ()) ()
    ; request ~source:(Existing.make_source ~script_id:"different" ()) ()
    ; request ~source:(Existing.make_source ~source_sha256:(String.make 64 'b') ()) ()
    ; request ~goal:"goal_other_start" ()
    ; request ~goal_revision:1L ()
    ; request ~catalog_revision:1L ()
    ; request ~input:(submission "actual input") ()
    ; request ~key:"other-native-start" ()
    ]
    |> List.map ~f:ok
  in
  let digests = List.map (base :: alternatives) ~f:digest in
  [%test_eq: int] (List.length digests) (Set.length (Set.of_list (module String) digests));
  List.iter (base :: alternatives) ~f:(fun value ->
    assert (Request.equal value (Request.of_json (Request.to_json value) |> ok)));
  List.iter
    [ request ~generation:(-1) ()
    ; request ~generation:1 ()
    ; request ~goal_revision:(-1L) ()
    ; request ~catalog_revision:(-1L) ()
    ; request ~input:(submission "") ()
    ]
    ~f:(fun value -> assert (Result.is_error value));
  let wire = Request.to_json base in
  List.iter
    ([ "session_id"; "principal_id"; "expected_revision"; "run_id"; "future" ]
     |> List.map ~f:(fun name -> replace wire name `Null))
    ~f:(fun value -> assert (Result.is_error (Request.of_json value)));
  List.iter
    [ replace wire "generation" (`String "00")
    ; replace wire "expected_goal_revision" (`Number "0")
    ; replace wire "kind" (`String "goal_start_episode_v2")
    ; replace wire "input" `Null
    ; `Object (("generation", `String "0") :: fields wire)
    ]
    ~f:(fun value -> assert (Result.is_error (Request.of_json value)));
  let maximum_counters =
    request
      ~generation:Int.max_value
      ~source:
        (Existing.make_source
           ~generation:Int.max_value
           ~installation_epoch:Int64.max_value
           ())
      ~goal_revision:Int64.max_value
      ~catalog_revision:Int64.max_value
      ()
    |> ok
  in
  assert (
    Request.equal
      maximum_counters
      (Request.of_json (Request.to_json maximum_counters) |> ok));
  (* Find the actual public Input codec's boundary instead of copying its
     serialization overhead or assuming the old Resume4KiB bound. *)
  let input n = submission (String.make n 'x') in
  let input_fits n =
    Result.is_ok (P.Run_start.Input.of_json (P.Run_start.Input.to_json (input n)))
  in
  let rec largest lower upper =
    if upper - lower = 1
    then lower
    else (
      let middle = lower + ((upper - lower) / 2) in
      if input_fits middle then largest middle upper else largest lower middle)
  in
  let maximum = largest 1 P.Run_limits.max_document_bytes in
  assert (maximum > 1_000_000);
  assert (input_fits maximum);
  assert (not (input_fits (maximum + 1)));
  let escaped_source = Existing.make_source ~script_id:(String.make 256 '\000') () in
  let large =
    request ~source:escaped_source ~input:(input maximum) ~key:(String.make 256 'k') ()
    |> ok
  in
  assert (Request.equal large (Request.of_json (Request.to_json large) |> ok));
  assert (
    String.length (Jsonaf.to_string (Request.to_json large))
    > P.Run_limits.max_document_bytes);
  assert (Result.is_error (request ~input:(input (maximum + 1)) ()));
  assert (
    Result.is_error
      (Request.of_json
         (replace
            (Request.to_json large)
            "input"
            (P.Run_start.Input.to_json (input (maximum + 1))))));
  print_endline
    "complete immutable command digest; strict caller-free request; actual near-1MiB \
     input boundary has symmetric create/decode";
  [%expect
    {| complete immutable command digest; strict caller-free request; actual near-1MiB input boundary has symmetric create/decode |}]
;;

(* Protocol DATA consistency examples only: this helper is never an Actor
   admission, custody token, writer ACK or execution fixture. *)
let admission
      ?(run = "run_native_start")
      ?(principal_id = principal)
      ?source
      ?key
      ?digest
      ?(kind = P.Run_receipt.Kind.Admission)
      ?(run_revision = 0L)
      ?(session_revision = 1L)
      ?(committed_at = timestamp "2026-11-05T00:00:00Z")
      request
  =
  P.Run_receipt.create
    ~run_id:(P.Id.Run.of_string run |> ok)
    ~principal_id
    ~source:(Option.value source ~default:(Request.source request))
    ~key:(Option.value key ~default:(Request.key request))
    ~request_sha256:(Option.value digest ~default:(Request.digest request |> ok))
    ~kind
    ~run_revision
    ~session_revision
    ~committed_at
  |> ok
;;

let%expect_test "closed Start phases preserve identity, claim times and admitted receipt" =
  let request = request () |> ok in
  let staged = Intent.create ~request ~principal_id:principal |> ok in
  let accepted_at = timestamp "2026-11-05T00:00:01Z" in
  let claimed_at = timestamp "2026-11-05T00:00:00Z" in
  let accepted = Intent.accept staged ~now:accepted_at |> ok in
  let claimed = Intent.claim accepted ~now:claimed_at |> ok in
  let receipt = admission request in
  let admitted = Intent.admit claimed ~admission:receipt |> ok in
  let discarded = Intent.discard accepted ~reason:"actual callback retired" |> ok in
  let primary =
    P.Error.create
      Permission_denied
      ~message:(String.make 100_000 'x')
      ~retryable:false
      ()
  in
  let rejected = Intent.reject claimed primary |> ok in
  let phases = [ staged; accepted; claimed; admitted; discarded; rejected ] in
  let edges = [ 0, 1; 0, 4; 1, 2; 1, 4; 2, 3; 2, 5 ] in
  List.iteri phases ~f:(fun i previous ->
    assert (Intent.equal previous (Intent.of_json (Intent.to_json previous) |> ok));
    List.iteri phases ~f:(fun j incoming ->
      let allowed =
        Int.equal i j
        || List.exists edges ~f:(fun (from, to_) -> Int.equal i from && Int.equal j to_)
      in
      [%test_eq: bool] allowed (Result.is_ok (Intent.validate_update previous ~incoming))));
  (match Intent.phase admitted with
   | Admitted actual ->
     assert (P.Timestamp.equal actual.accepted_at accepted_at);
     assert (P.Timestamp.equal actual.claimed_at claimed_at);
     assert (P.Run_receipt.equal actual.admission receipt)
   | Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _ -> assert false);
  (match Intent.phase rejected with
   | Rejected error ->
     [%test_eq: P.Error.code] Permission_denied error.code;
     assert (not error.retryable);
     assert (String.length error.message < String.length primary.message)
   | Staged | Accepted _ | Dispatch_claimed _ | Admitted _ | Discarded _ -> assert false);
  List.iter
    [ admission ~kind:Action request
    ; admission ~principal_id:(P.Id.Principal.of_string "pri_other_start" |> ok) request
    ; admission ~source:(Existing.make_source ~installation_epoch:2L ()) request
    ; admission ~key:(P.Idempotency_key.of_string "other-start" |> ok) request
    ; admission ~digest:(String.make 64 '0') request
    ; admission ~run_revision:1L request
    ; admission ~session_revision:0L request
    ]
    ~f:(fun receipt ->
      assert (Result.is_error (Intent.admit claimed ~admission:receipt));
      let wire = Intent.to_json admitted in
      let phase =
        Jsonaf.member_exn "phase" wire
        |> fun phase -> replace phase "admission" (P.Run_receipt.to_json receipt)
      in
      assert (Result.is_error (Intent.of_json (replace wire "phase" phase))));
  assert (Result.is_error (Intent.admit accepted ~admission:receipt));
  assert (Result.is_error (Intent.discard claimed ~reason:"late"));
  assert (Result.is_error (Intent.reject admitted primary));
  assert (Result.is_error (Intent.discard admitted ~reason:"late"));
  let admitted_wire = Intent.to_json admitted in
  let phase = Jsonaf.member_exn "phase" admitted_wire in
  List.iter [ "accepted_at"; "claimed_at" ] ~f:(fun name ->
    let incoming =
      replace
        admitted_wire
        "phase"
        (replace phase name (P.Timestamp.to_json (timestamp "2026-11-06T00:00:00Z")))
      |> Intent.of_json
      |> ok
    in
    assert (Result.is_error (Intent.validate_update claimed ~incoming)));
  let foreign =
    Intent.create ~request ~principal_id:(P.Id.Principal.of_string "pri_other_start" |> ok)
    |> ok
  in
  assert (Result.is_error (Intent.validate_update staged ~incoming:foreign));
  let foreign_request =
    Request.of_json
      (replace (Request.to_json request) "expected_goal_revision" (`String "1"))
    |> ok
  in
  let foreign = Intent.create ~request:foreign_request ~principal_id:principal |> ok in
  assert (Result.is_error (Intent.validate_update staged ~incoming:foreign));
  List.iter
    [ replace admitted_wire "receipt" (P.Run_receipt.to_json receipt)
    ; replace admitted_wire "phase" (replace phase "kind" (`String "future"))
    ; replace admitted_wire "phase" (replace phase "optional_receipt" `Null)
    ]
    ~f:(fun wire -> assert (Result.is_error (Intent.of_json wire)));
  print_endline
    "only legal phase edges; no rearm; immutable identity and exact claim times; \
     admitted command receipt is terminal DATA";
  [%expect
    {| only legal phase edges; no rearm; immutable identity and exact claim times; admitted command receipt is terminal DATA |}]
;;

module Measurement = struct
  type t =
    { bytes : int
    ; fields : int
    ; nodes : int
    ; depth : int
    }
  [@@deriving sexp_of]

  let of_json json =
    let rec visit = function
      | `Object fields ->
        let children = List.map fields ~f:(fun (_, value) -> visit value) in
        combine children (List.length fields)
      | `Array values -> combine (List.map values ~f:visit) 0
      | `Null | `True | `False | `Number _ | `String _ -> 0, 1, 1
    and combine children own_fields =
      List.fold
        children
        ~init:(own_fields, 1, 1)
        ~f:(fun (fields, nodes, depth) (f, n, d) ->
          fields + f, nodes + n, Int.max depth (d + 1))
    in
    let fields, nodes, depth = visit json in
    { bytes = String.length (Jsonaf.to_string json); fields; nodes; depth }
  ;;

  let maximum values =
    List.fold values ~init:{ bytes = 0; fields = 0; nodes = 0; depth = 0 } ~f:(fun a b ->
      { bytes = Int.max a.bytes b.bytes
      ; fields = Int.max a.fields b.fields
      ; nodes = Int.max a.nodes b.nodes
      ; depth = Int.max a.depth b.depth
      })
  ;;

  let dominates a b =
    a.bytes >= b.bytes && a.fields >= b.fields && a.nodes >= b.nodes && a.depth >= b.depth
  ;;
end

let document_ok = Goal_native_intent_tests.document_ok

let%expect_test
    "action carrier preserves Resume payload and reserves genuine future Start structure"
  =
  let principal = P.Id.Principal.of_string ("pri_" ^ String.make 92 'p') |> ok in
  let request =
    request
      ~source:(Existing.make_source ~script_id:(String.make 256 '\000') ())
      ~key:(String.make 256 'k')
      ()
    |> ok
  in
  let staged = Intent.create ~request ~principal_id:principal |> ok in
  let start = Action.Start staged in
  let resume_request = Existing.request () |> ok in
  let resume_intent =
    P.Goal_native_resume_intent.create ~request:resume_request ~principal_id:principal
    |> ok
  in
  let resume = Action.Resume resume_intent in
  List.iter [ start; resume ] ~f:(fun action ->
    assert (Action.equal action (Action.of_json (Action.to_json action) |> ok));
    assert (Action.equal action (Action.t_of_sexp (Action.sexp_of_t action))));
  assert (
    Jsonaf.exactly_equal
      (Jsonaf.member_exn "intent" (Action.to_json resume))
      (P.Goal_native_resume_intent.to_json resume_intent));
  (match Action.of_json (Action.to_json resume) |> ok with
   | Resume actual ->
     [%test_eq: string]
       (P.Goal_resume_request.digest resume_request |> ok)
       (P.Goal_resume_request.digest (P.Goal_native_resume_intent.request actual) |> ok)
   | Start _ -> assert false);
  [%test_eq: string] "goal.native-start.v1" (Action.persistence_semantics start);
  [%test_eq: string] "goal.native-resume.v1" (Action.persistence_semantics resume);
  assert (Result.is_error (Action.validate_update start ~incoming:resume));
  assert (Result.is_error (Action.validate_update resume ~incoming:start));
  let codec =
    D.Domain_codec.create
      ~limits:D.Limits.default
      ~kind:"test.goal-action"
      ~version:1
      ~shape:Action.shape
      ~supported_semantics:[]
      ~decode:(fun raw ->
        Result.map_error (Action.of_json raw) ~f:(fun error ->
          D.Error.Invalid_field { path = []; reason = error.message }))
      ~encode:(fun value -> Ok (Action.to_json value))
    |> document_ok
  in
  List.iter [ start; resume ] ~f:(fun action ->
    let wire = Action.to_json action in
    let intent = Jsonaf.member_exn "intent" wire in
    let phase =
      Jsonaf.member_exn "phase" intent
      |> fun phase -> replace phase "future_phase_note" (`String "retained")
    in
    let enriched =
      replace
        wire
        "intent"
        (replace
           (replace intent "future_intent_note" (`Array [ `Number "17" ]))
           "phase"
           phase)
    in
    assert (Result.is_error (Action.of_json enriched));
    let document =
      D.Document.create
        ~limits:D.Limits.default
        ~kind:"test.goal-action"
        ~version:1
        ~payload:enriched
      |> document_ok
    in
    let carrier = D.Domain_codec.decode codec document |> document_ok in
    assert (Action.equal action (D.Extension_carrier.value carrier));
    let encoded =
      D.Domain_codec.encode codec carrier |> document_ok |> D.Document.payload
    in
    assert (D.Json.equal enriched encoded);
    let request_wire =
      Jsonaf.member_exn "request" intent |> fun raw -> replace raw "future_grant" `True
    in
    let protected = replace wire "intent" (replace intent "request" request_wire) in
    let document =
      D.Document.create
        ~limits:D.Limits.default
        ~kind:"test.goal-action"
        ~version:1
        ~payload:protected
      |> document_ok
    in
    assert (Result.is_error (D.Domain_codec.decode codec document)));
  let at = timestamp "2026-11-05T00:00:00Z" in
  let accepted = Intent.accept staged ~now:at |> ok in
  let claimed = Intent.claim accepted ~now:at |> ok in
  let maximum_time =
    P.Timestamp.of_time_ns (Time_ns.of_int63_ns_since_epoch Int63.max_value)
  in
  let receipt =
    admission
      ~run:("run_" ^ String.make 92 'r')
      ~principal_id:principal
      ~session_revision:Int64.max_value
      ~committed_at:maximum_time
      request
  in
  let admitted = Intent.admit claimed ~admission:receipt |> ok in
  let error data message =
    P.Error.create Resource_limit ~message ~retryable:true ~data ()
  in
  let rec nested n value =
    if n = 0 then value else `Object [ "x", nested (n - 1) value ]
  in
  let errors =
    [ error `Null (String.make 200 '\000')
    ; error (`Array (List.init 800 ~f:(fun _ -> `Number "0"))) "nodes"
    ; error (`Object (List.init 180 ~f:(fun i -> Int.to_string i, `Number "0"))) "fields"
    ; error (nested 5 `Null) "depth"
    ]
  in
  let rejected =
    List.map errors ~f:(fun error ->
      let rejected = Intent.reject claimed error |> ok in
      (match Intent.phase rejected with
       | Rejected actual ->
         assert (Jsonaf.exactly_equal (P.Error.to_json actual) (P.Error.to_json error))
       | Staged | Accepted _ | Dispatch_claimed _ | Admitted _ | Discarded _ ->
         assert false);
      rejected)
  in
  let discarded = Intent.discard accepted ~reason:(String.make 160 '\000') |> ok in
  let probes = Action.capacity_projections start in
  let reserved = List.map probes ~f:Measurement.of_json |> Measurement.maximum in
  List.iter (accepted :: claimed :: admitted :: discarded :: rejected) ~f:(fun value ->
    let actual = Action.to_json (Start value) |> Measurement.of_json in
    if not (Measurement.dominates reserved actual)
    then
      raise_s
        [%sexp
          "capacity reserve missed a valid later phase"
        , (reserved : Measurement.t)
        , (actual : Measurement.t)]);
  (* Identity codec checks only the real structured ownership contract. These
     deliberately non-domain reserve witnesses are never admission evidence. *)
  let capacity_codec =
    D.Domain_codec.create
      ~limits:D.Limits.default
      ~kind:"test.goal-capacity"
      ~version:1
      ~shape:Action.shape
      ~supported_semantics:[]
      ~decode:(fun raw -> Ok raw)
      ~encode:(fun raw -> Ok raw)
    |> document_ok
  in
  List.iter probes ~f:(fun probe ->
    let document =
      D.Document.create
        ~limits:D.Limits.default
        ~kind:"test.goal-capacity"
        ~version:1
        ~payload:probe
      |> document_ok
    in
    D.Domain_codec.decode capacity_codec document
    |> document_ok
    |> D.Domain_codec.encode capacity_codec
    |> document_ok
    |> ignore);
  List.iter [ admitted; discarded ] ~f:(fun intent ->
    assert (List.is_empty (Action.capacity_projections (Start intent))));
  List.iter rejected ~f:(fun intent ->
    assert (List.is_empty (Action.capacity_projections (Start intent))));
  print_endline
    "exact current Resume payload/digest; compatible extensions survive carrier; request \
     stays strict; capacity DATA dominates real escaped receipt and diagnostic phases";
  [%expect
    {| exact current Resume payload/digest; compatible extensions survive carrier; request stays strict; capacity DATA dominates real escaped receipt and diagnostic phases |}]
;;
