open! Core

let domain () = (Stdlib.Domain.self () :> int)

let error () =
  Agent_protocol.Error.create
    Conflict
    ~message:"failed startup request belongs to another owner or domain"
    ~retryable:false
    ()
;;

module Receipt = struct
  type t =
    { domain : int
    ; startup : Failed_startup_data.t
    }

  let check_domain_exn t =
    if not (Int.equal t.domain (domain ()))
    then invalid_arg "failed startup receipt belongs to another domain"
  ;;

  let same_startup t startup =
    check_domain_exn t;
    phys_equal t.startup startup
  ;;
end

type t =
  { issuer : unit ref
  ; domain : int
  ; startup : Failed_startup_data.t
  ; mutable released : Receipt.t option
  }

let create ~issuer ~domain ~startup = { issuer; domain; startup; released = None }

let belongs_to t ~issuer ~startup =
  Int.equal t.domain (domain ())
  && phys_equal t.issuer issuer
  && phys_equal t.startup startup
;;

let released t =
  if Int.equal t.domain (domain ()) then Ok t.released else Error (error ())
;;

module Gate = struct
  type request = t

  type state =
    | Open
    | Released of request

  type t =
    { domain : int
    ; mutable state : state
    }

  let create () = { domain = domain (); state = Open }

  let is_closed t =
    if not (Int.equal t.domain (domain ()))
    then invalid_arg "failed startup gate belongs to another domain";
    match t.state with
    | Open -> false
    | Released _ -> true
  ;;

  let allows (t : t) (request : request) =
    Int.equal t.domain (domain ())
    && Int.equal request.domain t.domain
    &&
    match t.state with
    | Open -> true
    | Released original -> phys_equal original request
  ;;

  let release (t : t) (request : request) ~issuer ~startup =
    if not (allows t request && belongs_to request ~issuer ~startup)
    then Error (error ())
    else (
      match t.state with
      | Released _ ->
        (match request.released with
         | Some receipt -> Ok receipt
         | None -> invalid_arg "released startup gate lost its original receipt")
      | Open ->
        let receipt : Receipt.t = { domain = t.domain; startup } in
        request.released <- Some receipt;
        t.state <- Released request;
        Ok receipt)
  ;;
end

let failed_before_ack ~progress ~outcome =
  Option.is_some (Failed_startup_data.writer_failure ~progress ~outcome)
;;

(* The observation remains opaque here: selection returns the SAME original
   observation only after the required typed receipts. Tests may supply neutral
   observation DATA; this selector cannot create an Actor custody receipt. *)
module Original_input_facts = struct
  type 'observation root =
    { requested_candidate : bool
    ; observation : 'observation
    }

  type 'observation t =
    { producer_exit : unit Actor_reply.t option
    ; close : Failed_startup_data.Input.close option
    ; root : 'observation root option
    }

  let eligible_root ~original_end input =
    match original_end, input with
    | ( Some (Actor_reply.Returned (Error _) | Actor_reply.Raised _)
      , Some
          { producer_exit = Some _
          ; close =
              Some { normal_root_close = true; outcome = Actor_reply.Returned (Ok ()) }
          ; root = Some { requested_candidate = true; observation }
          } ) -> Some observation
    | _ -> None
  ;;
end

let original_input_joined startup =
  match Failed_startup_data.original_end startup, Failed_startup_data.input startup with
  | ( (Some (Actor_reply.Returned (Error _) | Actor_reply.Raised _) as original_end)
    , Some input ) ->
    let producer_exit, close, root =
      ( Failed_startup_data.Input.producer_exit input
      , Failed_startup_data.Input.close input
      , Failed_startup_data.Input.root input )
    in
    let facts : _ Original_input_facts.t =
      { producer_exit
      ; close
      ; root =
          Option.map root ~f:(fun { requested_candidate; observation } ->
            Original_input_facts.{ requested_candidate; observation })
      }
    in
    Option.value_map
      (Original_input_facts.eligible_root ~original_end (Some facts))
      ~default:false
      ~f:(fun observation ->
        failed_before_ack
          ~progress:(Session_commit_observation.progress observation)
          ~outcome:(Session_commit_observation.outcome observation))
  | _ -> false
;;

type authored_cut =
  | First_lifecycle of Failed_startup_data.Authored.t
  | Final_aggregate of
      Failed_startup_data.Authored.t * Failed_startup_data.Authored.settled_lifecycle

let same_failure (original : unit Actor_reply.t) (producer : unit Actor_reply.t) =
  match original, producer with
  | Returned (Error original), Returned (Error producer) -> phys_equal original producer
  | Raised (original, _), Raised (producer, _) -> phys_equal original producer
  | (Returned _ | Raised _), (Returned _ | Raised _) -> false
;;

let authored_callback_joined completion =
  let open Result.Let_syntax in
  let%map snapshot = Callback_cleanup_data.Completion.snapshot completion in
  match
    ( Callback_cleanup_data.Completion_snapshot.producer_exit snapshot
    , Callback_cleanup_data.Completion_snapshot.finish_call snapshot
    , Callback_cleanup_data.Completion_snapshot.release snapshot )
  with
  | Returned (Ok ()), Completed (Ok ()), Completed (Ok ()) ->
    Callback_cleanup_data.Completion_snapshot.original_finish_settled snapshot
  | _ -> false
;;

let authored_lifecycle_joined
      actual
      (confirmed : Failed_startup_data.Authored.settled_lifecycle)
  =
  match actual, confirmed with
  | Failed_startup_data.Authored.Legacy_acknowledged, Legacy -> Ok true
  | Callback { id; completion }, Extension confirmed
    when Agent_protocol.Id.Moderator_execution.equal id confirmed ->
    authored_callback_joined completion
  | (Uninstalled | Installed | Legacy_acknowledged | Callback _), _ -> Ok false
;;

let original_authored_joined startup =
  let module A = Failed_startup_data.Authored in
  match
    Failed_startup_data.original_end startup, Failed_startup_data.authored startup
  with
  | Some (Returned (Error _) | Raised _), Some attempt ->
    (match A.producer_exit attempt with
     | Some producer ->
       (match A.lifecycle attempt, A.final_aggregate_failure attempt with
        | Uninstalled, None
          when Option.exists (A.first_lifecycle_failure attempt) ~f:(fun original ->
                 same_failure original producer) -> Ok (Some (First_lifecycle attempt))
        | actual, Some (confirmed, original) when same_failure original producer ->
          Result.map (authored_lifecycle_joined actual confirmed) ~f:(function
            | true -> Some (Final_aggregate (attempt, confirmed))
            | false -> None)
        | _ -> Ok None)
     | None -> Ok None)
  | (None | Some (Returned (Ok ()) | Returned (Error _) | Raised _)), _ -> Ok None
;;

let rec authored_graphs_joined graphs ~issuer ~generation =
  let module Graph = Inference_graph_owner.Host in
  let open Result.Let_syntax in
  match graphs with
  | [] -> Ok true
  | graph :: remaining ->
    (match Graph.phase graph with
     | Finished
       when phys_equal issuer (Graph.issuing_actor graph)
            && Int.equal generation (Graph.generation graph) ->
       let%bind joined = Graph.Native_join.current graph in
       (match joined with
        | None -> Ok false
        | Some acknowledgement ->
          let%bind () = Graph.Native_join.check_acknowledgement graph acknowledgement in
          authored_graphs_joined remaining ~issuer ~generation)
     | Open | Sealed | Finished -> Ok false)
;;

let%expect_test "failed startup facts and exact request publication remain fail closed" =
  let ok = function
    | Ok value -> value
    | Error _ -> failwith "unexpected startup refusal"
  in
  (* Receipt classification only. These neutral facts do not create an actual
     journal ACK, activation, release permission or custody receipt. *)
  let failure = error () in
  let raised =
    Actor_reply.Raised (Failure "input fact", Stdlib.Printexc.get_raw_backtrace ())
  in
  let payload = "original root DATA" in
  let transaction : Session_commit_observation.Transaction.t =
    { session_id =
        Agent_protocol.Id.Session.create_with
          (Agent_protocol.Id.Generator.create ~bytes:(fun count -> String.make count 'x'))
    ; generation = 0
    ; sequence = 1L
    ; revision = 1L
    ; previous_hash = None
    ; payload
    ; hash = Digestif.SHA256.(to_hex (digest_string payload))
    ; max_payload_length = String.length payload
    }
  in
  let entered = Session_commit_observation.Progress.Writer_entered transaction in
  let failed = Session_commit_observation.Outcome.Returned (Error failure) in
  let root : _ Original_input_facts.root =
    { requested_candidate = true; observation = entered, failed }
  in
  let complete : _ Original_input_facts.t =
    { producer_exit = Some (Actor_reply.Returned (Ok ()))
    ; close = Some { normal_root_close = true; outcome = Actor_reply.Returned (Ok ()) }
    ; root = Some root
    }
  in
  let qualifies original_end input =
    Option.exists
      (Original_input_facts.eligible_root ~original_end input)
      ~f:(fun (progress, outcome) -> failed_before_ack ~progress ~outcome)
  in
  let original_end = Some (Actor_reply.Returned (Error failure)) in
  assert (qualifies original_end (Some complete));
  assert (qualifies (Some raised) (Some complete));
  List.iter [ None; Some (Actor_reply.Returned (Ok ())) ] ~f:(fun original_end ->
    assert (not (qualifies original_end (Some complete))));
  assert (not (qualifies original_end None));
  List.iter
    [ { complete with producer_exit = None }
    ; { complete with close = None }
    ; { complete with
        close = Some { normal_root_close = false; outcome = Actor_reply.Returned (Ok ()) }
      }
    ; { complete with
        close =
          Some
            { normal_root_close = true; outcome = Actor_reply.Returned (Error failure) }
      }
    ; { complete with close = Some { normal_root_close = true; outcome = raised } }
    ; { complete with root = None }
    ; { complete with root = Some { root with requested_candidate = false } }
    ]
    ~f:(fun incomplete -> assert (not (qualifies original_end (Some incomplete))));
  List.iter
    [ Actor_reply.Returned (Ok ()); Actor_reply.Returned (Error failure); raised ]
    ~f:(fun exit ->
      assert (qualifies original_end (Some { complete with producer_exit = Some exit })));
  List.iter
    [ Session_commit_observation.Progress.Unobserved; Before_writer ]
    ~f:(fun progress -> assert (not (failed_before_ack ~progress ~outcome:failed)));
  List.iter
    [ Session_commit_observation.Outcome.Not_called; In_progress; Returned (Ok ()) ]
    ~f:(fun outcome -> assert (not (failed_before_ack ~progress:entered ~outcome)));
  assert (failed_before_ack ~progress:entered ~outcome:failed);
  assert (
    failed_before_ack
      ~progress:entered
      ~outcome:
        (Session_commit_observation.Outcome.Raised
           (Failure "writer fact", Stdlib.Printexc.get_raw_backtrace ())));
  let incomplete = Failed_startup_data.create () in
  assert (not (original_input_joined incomplete));
  Failed_startup_data.record_original_end
    incomplete
    (Actor_reply.Returned (Error failure));
  assert (not (original_input_joined incomplete));
  let input = Failed_startup_data.Input.create () in
  Failed_startup_data.install_input incomplete input;
  assert (not (original_input_joined incomplete));
  Failed_startup_data.Input.record_producer_exit input (Actor_reply.Returned (Ok ()));
  Failed_startup_data.Input.record_close
    input
    ~normal_root_close:true
    (Actor_reply.Returned (Ok ()));
  assert (not (original_input_joined incomplete));
  let startup = Failed_startup_data.create () in
  let issuer = ref () in
  let gate = Gate.create () in
  let original = create ~issuer ~domain:(domain ()) ~startup in
  let equivalent = create ~issuer ~domain:(domain ()) ~startup in
  assert (not (Gate.is_closed gate));
  assert (Result.is_error (Gate.release gate original ~issuer:(ref ()) ~startup));
  assert (Option.is_none (ok (released original)));
  let receipt = ok (Gate.release gate original ~issuer ~startup) in
  (* Observe only the retained cell after publication. This private DATA law
     does not claim an actual lost RPC or exercise Owner prefix ordering. *)
  let observed = Option.value_exn (ok (released original)) in
  assert (phys_equal receipt observed);
  assert (Gate.is_closed gate);
  assert (Gate.allows gate original);
  assert (not (Gate.allows gate equivalent));
  assert (Result.is_error (Gate.release gate equivalent ~issuer ~startup));
  assert (Option.is_none (ok (released equivalent)));
  assert (phys_equal receipt (ok (Gate.release gate original ~issuer ~startup)));
  let foreign = create ~issuer ~domain:(Int.succ (domain ())) ~startup in
  assert (Result.is_error (released foreign));
  assert (Result.is_error (Gate.release gate foreign ~issuer ~startup));
  assert (Receipt.same_startup receipt startup);
  assert (not (Receipt.same_startup receipt (Failed_startup_data.create ())));
  [%expect {| |}]
;;
