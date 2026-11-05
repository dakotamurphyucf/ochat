open! Core
module P = Agent_protocol
module Request = Agent_session.Goal_resume_request

let ok = function
  | Ok value -> value
  | Error (error : P.Error.t) -> raise_s [%sexp (error : P.Error.t)]
;;

let make_source
      ?(script_id = "goal")
      ?(source_sha256 = String.make 64 'a')
      ?(generation = 0)
      ?(installation_epoch = 1L)
      ()
  =
  P.Run_source.create
    ~observer:{ script_id; source_sha256 }
    ~generation
    ~installation_epoch
  |> ok
;;

let source = make_source ()

let request
      ?(session = "ses_resume")
      ?(goal = "goal_resume")
      ?(run = "run_resume")
      ?(key = "resume")
      ?(generation = 0)
      ?(source = source)
      ?(goal_revision = 0L)
      ?(catalog_revision = 0L)
      ?(run_revision = 0L)
      ()
  =
  Request.create
    ~session_id:(P.Id.Session.of_string session |> ok)
    ~goal_id:(P.Id.Goal.of_string goal |> ok)
    ~run_id:(P.Id.Run.of_string run |> ok)
    ~key:(P.Idempotency_key.of_string key |> ok)
    ~generation
    ~source
    ~expected_goal_revision:goal_revision
    ~expected_catalog_revision:catalog_revision
    ~expected_run_revision:run_revision
;;

let%expect_test "resume identity retains every original fence and rejects invalid bounds" =
  let base = request () |> ok in
  let digest value = Request.digest value |> ok in
  let digest_base = digest base in
  [%test_eq: string] digest_base (request () |> ok |> digest);
  let alternatives =
    [ request ~session:"ses_other" ()
    ; request ~goal:"goal_other" ()
    ; request ~run:"run_other" ()
    ; request ~key:"resume_other" ()
    ; request ~generation:1 ~source:(make_source ~generation:1 ()) ()
    ; request ~source:(make_source ~installation_epoch:2L ()) ()
    ; request ~source:(make_source ~script_id:"other" ()) ()
    ; request ~source:(make_source ~source_sha256:(String.make 64 'b') ()) ()
    ; request ~goal_revision:1L ()
    ; request ~catalog_revision:1L ()
    ; request ~run_revision:1L ()
    ]
    |> List.map ~f:(fun value -> ok value |> digest)
  in
  let identities = Set.of_list (module String) (digest_base :: alternatives) in
  [%test_eq: int] (List.length alternatives + 1) (Set.length identities);
  List.iter
    [ request ~generation:(-1) ()
    ; request ~generation:1 ()
    ; request ~goal_revision:(-1L) ()
    ; request ~catalog_revision:(-1L) ()
    ; request ~run_revision:(-1L) ()
    ]
    ~f:(function
      | Error error -> [%test_eq: P.Error.code] Invalid_request error.code
      | Ok _ -> failwith "invalid resume fence was admitted");
  let maximum =
    request
      ~generation:Int.max_value
      ~source:
        (make_source ~generation:Int.max_value ~installation_epoch:Int64.max_value ())
      ~goal_revision:Int64.max_value
      ~catalog_revision:Int64.max_value
      ~run_revision:Int64.max_value
      ()
    |> ok
  in
  [%test_eq: int] Int.max_value (Request.generation maximum);
  [%test_eq: int64] Int64.max_value (Request.expected_goal_revision maximum);
  [%test_eq: int64] Int64.max_value (Request.expected_catalog_revision maximum);
  [%test_eq: int64] Int64.max_value (Request.expected_run_revision maximum);
  assert (not (String.equal digest_base (digest maximum)));
  print_endline
    "resume identity preserves every fence; invalid bounds or source are rejected";
  [%expect
    {| resume identity preserves every fence; invalid bounds or source are rejected |}]
;;
