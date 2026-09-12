open Core
module A = Chat_response.Authoring_materialization
module Q = Chat_response.Authoring_context
module V = Chat_response.Authoring_validation
module P = Chat_response.Authoring_policy
module C = Chat_response.Tool_capability
module M = Chatmd_shell_spec.Authoring_metadata
module Presence = Chat_response.Authoring_presence
module H = Agent_protocol.History
module G = Agent_protocol.Authoring_guidance
module Codec = Agent_session.History_codec

let digest = Chatmd_shell_spec.Source_ref.digest
let ok = Result.ok_or_failwith

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let cap_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : C.error)]
;;

let policy_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.error)]
;;

let make_host
      ?(targets = [ V.One_off_script; Standalone_tool; Moderator; Generated_chatmd ])
      ?(runtime = "materialization-test")
      ()
  =
  V.create_host
    ~runtime_identity:runtime
    ~targets
    ~moderator_surface:Ordinary
    ~compilation:Chatml_compilation.default_limits
  |> ok
;;

let fixture f =
  Mirage_crypto_rng_unix.use_default ();
  let context = Q.create ~secret:"materialization-fixture-key" () |> ok in
  let host = make_host () in
  let catalog = A.catalog context ~host |> ok in
  let authoring target = M.{ authoring = Some (V.help target); helper = None } in
  let metadata =
    [ "script", authoring One_off_script
    ; "child", authoring Generated_chatmd
    ; ("ordinary", M.{ authoring = None; helper = None })
    ; (M.helper_name Reference, M.{ authoring = None; helper = Some Reference })
    ; (M.helper_name Validation, M.{ authoring = None; helper = Some Validation })
    ]
  in
  let functions =
    List.map metadata ~f:(fun (name, _) ->
      let module Definition = struct
        type input = string

        let name = name
        let description = None
        let type_ = "function"
        let parameters = `True
        let input_of_string input = input
      end
      in
      ( digest name
      , Ochat_function.create_function
          (module Definition)
          (fun _ -> failwith "guidance must not execute tools") ))
  in
  let ceiling =
    C.create ~metadata ~owner:"fixture" ~resource_fingerprint:(digest "scope") functions
    |> cap_ok
  in
  let plan ?(selected_names = [ "script"; "child" ]) policy =
    P.resolve ~catalog ~policy ~ceiling ~selected_names () |> policy_ok
  in
  f context host ceiling plan
;;

let entries ?(start = 0) messages =
  List.mapi messages ~f:(fun index message ->
    A.entry
      message
      ~id:(History_entry.Id.create ~namespace:"reference" ~sequence:(start + index) |> ok))
;;

let restored entries =
  List.map entries ~f:(fun entry ->
    H.entry_to_json entry |> H.entry_of_json |> protocol_ok)
  |> fun previous ->
  Codec.all_of_protocol previous |> protocol_ok |> Codec.all_to_protocol ~previous
;;

let%expect_test
    "installed policy materialization survives actual history codecs and is scope-bound"
  =
  fixture (fun context host _ plan ->
    let policy = plan Auto in
    let make ?(host = host) ?(scope = "session-a:1") () =
      A.create ~context ~host ~policy ~capabilities:(P.capabilities policy) ~scope ()
      |> ok
    in
    let materialized = make () in
    let initial = A.initial materialized in
    let effective = entries initial |> restored in
    let refresh t effective =
      A.refresh t ~known:[] ~effective |> protocol_ok |> List.length
    in
    print_s
      [%sexp
        (List.length initial : int)
      , (refresh materialized effective : int)
      , (refresh materialized [] : int)
      , (refresh (make ~scope:"session-b:1" ()) effective : int)
      , (refresh (make ~scope:"session-a:2" ()) effective : int)
      , (refresh (make ~host:(make_host ~runtime:"updated-runtime" ()) ()) effective
         : int)];
    let changed f = List.map effective ~f in
    print_s
      [%sexp
        (refresh materialized (changed (fun e -> { e with H.redacted = true })) : int)
      , (refresh materialized (changed (fun e -> { e with H.provenance = Canonical }))
         : int)
      , (refresh
           materialized
           (changed (fun e -> { e with H.payload = `String "summary" }))
         : int)];
    print_s [%sexp (A.estimated_tokens initial : int)];
    List.iter effective ~f:(fun e -> assert (H.equal_role e.role User)));
  [%expect
    {|
    (1 0 1 1 1 1)
    (1 1 1)
    953
    |}]
;;

let%expect_test
    "preload shares prerequisites but does not treat primer pointers as complete guides"
  =
  fixture (fun context host _ plan ->
    let policy = plan (Preload [ "chatml.tasks"; "chatml.task-effects" ]) in
    let t =
      A.create
        ~context
        ~host
        ~policy
        ~capabilities:(P.capabilities policy)
        ~scope:"session:1"
        ()
      |> ok
    in
    let messages = A.initial t in
    let ids =
      List.concat_map messages ~f:(fun message ->
        List.map message.A.guidance.topics ~f:(fun topic -> topic.G.id))
    in
    assert (Option.is_none (List.find_a_dup ids ~compare:String.compare));
    let effective = entries messages |> restored in
    let known = Presence.remember ~previous:[] ~history:effective |> protocol_ok in
    let show effective =
      A.refresh t ~known ~effective
      |> protocol_ok
      |> List.concat_map ~f:(fun message ->
        List.map message.A.guidance.topics ~f:(fun t -> t.G.id))
      |> [%sexp_of: string list]
      |> print_s
    in
    print_s [%sexp (ids : string list)];
    show effective;
    show (List.take effective 1);
    show
      (List.filter effective ~f:(fun e ->
         not (H.Id.equal e.id (List.last_exn effective).id)));
    let budget = A.estimated_tokens messages in
    (match
       A.create
         ~max_tokens:(budget - 1)
         ~context
         ~host
         ~policy
         ~capabilities:(P.capabilities policy)
         ~scope:"session:1"
         ()
     with
     | Ok _ -> failwith "preload was silently truncated"
     | Error error -> assert (String.is_substring error ~substring:"needs at least"));
    let changed_policy = plan Auto in
    let t =
      A.create
        ~context
        ~host
        ~policy:changed_policy
        ~capabilities:(P.capabilities changed_policy)
        ~scope:"session:1"
        ()
      |> ok
    in
    print_s [%sexp (A.refresh t ~known ~effective |> protocol_ok |> List.length : int)]);
  [%expect
    {|
    (authoring.primer chatml.introduction chatml.syntax.calls
     chatml.syntax.containers chatml.types chatml.operators chatml.tasks
     chatml.task-effects)
    ()
    (chatml.introduction chatml.syntax.calls chatml.syntax.containers
     chatml.types chatml.operators chatml.tasks chatml.task-effects)
    (chatml.task-effects)
    1
    |}]
;;

let%expect_test
    "manual and ordinary policies add nothing; stale capability and target selections \
     fail"
  =
  fixture (fun context host ceiling plan ->
    let make policy =
      A.create
        ~context
        ~host
        ~policy
        ~capabilities:(P.capabilities policy)
        ~scope:"session:1"
        ()
      |> ok
    in
    let manual = make (plan Manual) in
    let ordinary = make (plan ~selected_names:[ "ordinary" ] Auto) in
    print_s
      [%sexp
        (List.length (A.initial manual) : int), (List.length (A.initial ordinary) : int)];
    let policy = plan Auto in
    let show = function
      | Ok _ -> failwith "expected incompatible materialization rejection"
      | Error error -> print_endline error
    in
    show (A.create ~context ~host ~policy ~capabilities:ceiling ~scope:"session:1" ());
    show
      (A.create
         ~context
         ~host:(make_host ~targets:[ One_off_script ] ())
         ~policy
         ~capabilities:(P.capabilities policy)
         ~scope:"session:1"
         ()));
  [%expect
    {|
    (0 0)
    invalid authoring materialization scope, budget or capabilities
    unavailable authoring task: child_agent
    |}]
;;

let%expect_test
    "only complete installed guidance satisfies a required topic, and primer purpose \
     matters"
  =
  fixture (fun context host ceiling plan ->
    let policy = plan Auto in
    let t =
      A.create
        ~context
        ~host
        ~policy
        ~capabilities:(P.capabilities policy)
        ~scope:"session:1"
        ()
      |> ok
    in
    let message = List.hd_exn (A.initial t) in
    let topic = List.hd_exn message.guidance.topics in
    let check label purpose topic =
      let guidance =
        G.create
          ~context_identity:(A.context_identity t)
          ~policy_fingerprint:(P.fingerprint policy)
          ~purpose
          ~topics:[ topic ]
          ~payload:message.payload
        |> protocol_ok
      in
      let original = List.hd_exn (entries [ message ]) in
      let effective = [ { original with H.provenance = Runtime_authoring guidance } ] in
      print_s
        [%sexp
          (label : string)
        , (A.refresh t ~known:[] ~effective |> protocol_ok |> List.length : int)]
    in
    check "complete primer" Primer topic;
    check "ordinary reference is not the shared primer" Reference topic;
    check "incomplete" Primer { topic with complete = false };
    check "rediscovery" Rediscovery { topic with complete = false };
    check "different document" Primer { topic with document_sha256 = digest "other" };
    check "authored prose" Primer { topic with source = Authored (digest "author") };
    let stale_catalog =
      P.catalog
        ~identity:(digest "outdated corpus")
        ~packages:[ V.help One_off_script ]
        ~topics:
          (List.map (V.help One_off_script).topics ~f:(fun id -> id, [ M.One_off_script ]))
      |> policy_ok
    in
    let policy =
      P.resolve ~catalog:stale_catalog ~ceiling ~selected_names:[ "script" ] ()
      |> policy_ok
    in
    match
      A.create
        ~context
        ~host
        ~policy
        ~capabilities:(P.capabilities policy)
        ~scope:"session:1"
        ()
    with
    | Ok _ -> failwith "stale corpus was accepted"
    | Error error -> print_endline error);
  [%expect
    {|
    ("complete primer" 0)
    ("ordinary reference is not the shared primer" 1)
    (incomplete 1)
    (rediscovery 1)
    ("different document" 1)
    ("authored prose" 1)
    authoring policy refers to a different installed corpus
    |}]
;;
