open! Core

module Failure = struct
  type t =
    | Protocol of Agent_protocol.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace
end

module Outcome = struct
  type t = (unit, Failure.t) Result.t

  let capture f =
    match f () with
    | Ok () -> Ok ()
    | Error failure -> Error (Failure.Protocol failure)
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      Error (Failure.Raised (exn, backtrace))
  ;;

  let resume = function
    | Ok () -> Ok ()
    | Error (Failure.Protocol failure) -> Error failure
    | Error (Failure.Raised (exn, backtrace)) ->
      Exn.raise_with_original_backtrace exn backtrace
  ;;
end

module Attempt = struct
  type t = unit ref

  let create () = ref ()
  let equal (t : t) (other : t) = phys_equal t other
end

let on_domain domain = Int.equal domain (Stdlib.Domain.self () :> int)

let domain_error () =
  Agent_protocol.Error.create
    Conflict
    ~message:"callback cleanup belongs to another creating domain"
    ~retryable:false
    ()
;;

let check_domain_exn domain =
  if not (on_domain domain) then invalid_arg "callback cleanup accessed on another domain"
;;

module Completion_snapshot = struct
  module Failure_slot = struct
    type kind =
      | Release
      | Lexical_handler
      | Reply_boundary
      | Capture
    [@@deriving equal]

    type t =
      { kind : kind
      ; failure : Failure.t
      }

    let kind t = t.kind
    let failure t = t.failure
  end

  type producer_exit =
    | Not_returned
    | Returned of Outcome.t

  type finish_call =
    | Not_attempted
    | Awaiting_reply
    | Completed of Outcome.t

  type release =
    | Not_released
    | Finishing
    | Completed of Outcome.t

  type t =
    { producer_exit : producer_exit
    ; finish_call : finish_call
    ; release : release
    ; failure_slots : Failure_slot.t list
    ; capture_failure : Failure.t option
    ; original_finish_settled : bool
    }

  let producer_exit t = t.producer_exit
  let finish_call t = t.finish_call
  let release t = t.release
  let failure_slots t = t.failure_slots
  let failures t = List.map t.failure_slots ~f:Failure_slot.failure
  let capture_failure t = t.capture_failure
  let original_finish_settled t = t.original_finish_settled

  let pending t =
    (match t.producer_exit with
     | Not_returned -> true
     | Returned _ -> false)
    || (match t.finish_call with
        | Not_attempted | Awaiting_reply -> true
        | Completed _ -> false)
    ||
    match t.release with
    | Not_released | Finishing -> true
    | Completed _ -> false
  ;;

  let positive t =
    let exited =
      match t.producer_exit with
      | Not_returned -> false
      | Returned _ -> true
    in
    let call_positive =
      match t.finish_call with
      | Completed (Ok ()) -> true
      | Not_attempted | Awaiting_reply | Completed (Error _) -> false
    in
    let release_positive =
      match t.release with
      | Completed (Ok ()) -> true
      | Not_released | Finishing | Completed (Error _) -> false
    in
    exited && call_positive && release_positive && List.is_empty t.failure_slots
  ;;
end

module Completion = struct
  module Reply_origin = struct
    type t =
      | Handler
      | Rejected
      | Await_failed
  end

  module Release = struct
    type t =
      { attempt : Attempt.t
      ; mutable outcome : Outcome.t option
      }
  end

  module Call = struct
    type t =
      { attempt : Attempt.t
      ; mutable handler : Outcome.t option
      ; mutable reply : (Reply_origin.t * Outcome.t) option
      }
  end

  type t =
    { issuer : unit ref
    ; domain : int
    ; mutable producer_exit : Completion_snapshot.producer_exit
    ; mutable release : Release.t option
    ; mutable call : Call.t option
    ; mutable latest_finish : Outcome.t option
    ; mutable capture_failure : Failure.t option
    }

  let create ~issuer ~domain =
    { issuer
    ; domain
    ; producer_exit = Not_returned
    ; release = None
    ; call = None
    ; latest_finish = None
    ; capture_failure = None
    }
  ;;

  let belongs_to t ~issuer ~domain =
    on_domain t.domain && Int.equal t.domain domain && phys_equal t.issuer issuer
  ;;

  let producer_exit t =
    check_domain_exn t.domain;
    t.producer_exit
  ;;

  let latest_finish t =
    check_domain_exn t.domain;
    t.latest_finish
  ;;

  let record_capture_failure t failure =
    check_domain_exn t.domain;
    match t.capture_failure with
    | None -> t.capture_failure <- Some failure
    | Some _ -> invalid_arg "callback capture failure recorded twice"
  ;;

  let record_producer_exit t outcome =
    check_domain_exn t.domain;
    match t.producer_exit with
    | Not_returned -> t.producer_exit <- Returned outcome
    | Returned _ -> invalid_arg "callback producer exit recorded twice"
  ;;

  let record_release t ~attempt =
    check_domain_exn t.domain;
    match t.release with
    | None -> t.release <- Some { Release.attempt; outcome = None }
    | Some _ -> invalid_arg "callback release recorded twice"
  ;;

  let release t : Completion_snapshot.release =
    check_domain_exn t.domain;
    match t.release with
    | None -> Not_released
    | Some { outcome = None; _ } -> Finishing
    | Some { outcome = Some outcome; _ } -> Completed outcome
  ;;

  let record_finish t ~attempt outcome =
    check_domain_exn t.domain;
    t.latest_finish <- Some outcome;
    (match t.release with
     | Some released when Attempt.equal released.attempt attempt ->
       if Option.is_none released.outcome then released.outcome <- Some outcome
     | None | Some _ -> ());
    match t.call with
    | Some call when Attempt.equal call.attempt attempt ->
      if Option.is_none call.handler then call.handler <- Some outcome
    | None | Some _ -> ()
  ;;

  let begin_finish_call t =
    check_domain_exn t.domain;
    match t.call with
    | None ->
      let attempt = Attempt.create () in
      t.call <- Some { Call.attempt; handler = None; reply = None };
      attempt
    | Some _ -> invalid_arg "callback lexical finish invocation recorded twice"
  ;;

  let original_call_exn t ~attempt =
    check_domain_exn t.domain;
    match t.call with
    | Some call when Attempt.equal call.attempt attempt -> call
    | None | Some _ -> invalid_arg "callback reply differs from lexical finish invocation"
  ;;

  let reply_origin t ~attempt =
    let call = original_call_exn t ~attempt in
    match call.handler with
    | Some _ -> Reply_origin.Handler
    | None -> Rejected
  ;;

  let record_reply t ~attempt ~origin outcome =
    let call = original_call_exn t ~attempt in
    (match origin, call.handler with
     | Reply_origin.Handler, None ->
       invalid_arg "callback handler reply has no actual handler outcome"
     | Handler, Some _ | Rejected, _ | Await_failed, _ -> ());
    match call.reply with
    | None -> call.reply <- Some (origin, outcome)
    | Some _ -> invalid_arg "callback lexical reply recorded twice"
  ;;

  let snapshot_unchecked t =
    let failure kind = function
      | Ok () -> []
      | Error failure -> [ { Completion_snapshot.Failure_slot.kind; failure } ]
    in
    let release_failures =
      match t.release with
      | None | Some { outcome = None; _ } -> []
      | Some { outcome = Some outcome; _ } -> failure Release outcome
    in
    let lexical_failures, boundary_failures, finish_call =
      match t.call with
      | None -> [], [], Completion_snapshot.Not_attempted
      | Some call ->
        let same_release =
          Option.exists t.release ~f:(fun released ->
            Attempt.equal released.attempt call.attempt)
        in
        let lexical_failures =
          if same_release
          then []
          else Option.value_map call.handler ~default:[] ~f:(failure Lexical_handler)
        in
        let boundary_failures, finish_call =
          match call.reply with
          | None -> [], Completion_snapshot.Awaiting_reply
          | Some (Handler, outcome) -> [], Completed outcome
          | Some ((Rejected | Await_failed), outcome) ->
            failure Reply_boundary outcome, Completed outcome
        in
        lexical_failures, boundary_failures, finish_call
    in
    { Completion_snapshot.producer_exit = t.producer_exit
    ; finish_call
    ; release = release t
    ; failure_slots =
        release_failures
        @ lexical_failures
        @ boundary_failures
        @ List.map (Option.to_list t.capture_failure) ~f:(fun failure ->
          { Completion_snapshot.Failure_slot.kind = Capture; failure })
    ; capture_failure = t.capture_failure
    ; original_finish_settled =
        (match t.call with
         | Some { handler = Some _; _ }
         | Some { handler = None; reply = Some (Rejected, _); _ } -> true
         | None
         | Some { handler = None; reply = None; _ }
         | Some { handler = None; reply = Some ((Handler | Await_failed), _); _ } -> false)
    }
  ;;

  let snapshot t =
    if on_domain t.domain then Ok (snapshot_unchecked t) else Error (domain_error ())
  ;;
end

let%expect_test "original callback finish completeness is frozen at observation" =
  let domain = (Stdlib.Domain.self () :> int) in
  let snapshot completion =
    match Completion.snapshot completion with
    | Ok snapshot -> snapshot
    | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
  in
  let sentinel = Failure "original awaited handler" in
  let failed = Error (Failure.Raised (sentinel, Stdlib.Printexc.get_callstack 8)) in
  let completion = Completion.create ~issuer:(ref ()) ~domain in
  let unstarted = snapshot completion in
  assert (not (Completion_snapshot.original_finish_settled unstarted));
  Completion.record_producer_exit completion (Ok ());
  let attempt = Completion.begin_finish_call completion in
  let awaiting = snapshot completion in
  assert (not (Completion_snapshot.original_finish_settled awaiting));
  Completion.record_reply completion ~attempt ~origin:Await_failed failed;
  let incomplete = snapshot completion in
  assert (not (Completion_snapshot.original_finish_settled incomplete));
  Completion.record_release completion ~attempt;
  Completion.record_finish completion ~attempt failed;
  let completed = snapshot completion in
  assert (Completion_snapshot.original_finish_settled completed);
  assert (not (Completion_snapshot.original_finish_settled incomplete));
  let ordinary = Completion.create ~issuer:(ref ()) ~domain in
  Completion.record_producer_exit ordinary (Ok ());
  let ordinary_attempt = Completion.begin_finish_call ordinary in
  Completion.record_release ordinary ~attempt:ordinary_attempt;
  Completion.record_finish ordinary ~attempt:ordinary_attempt (Ok ());
  let handled = snapshot ordinary in
  assert (Completion_snapshot.original_finish_settled handled);
  assert (Completion_snapshot.pending handled);
  let rejected = Completion.create ~issuer:(ref ()) ~domain in
  let rejected_attempt = Completion.begin_finish_call rejected in
  Completion.record_reply rejected ~attempt:rejected_attempt ~origin:Rejected failed;
  let rejection = snapshot rejected in
  assert (Completion_snapshot.original_finish_settled rejection);
  assert (Completion_snapshot.pending rejection);
  let module Slot = Completion_snapshot.Failure_slot in
  let kinds snapshot =
    List.map (Completion_snapshot.failure_slots snapshot) ~f:Slot.kind
  in
  let check_kinds snapshot expected =
    assert (List.equal Slot.equal_kind (kinds snapshot) expected)
  in
  (* The actual original handler can fail before bookkeeping release. A later
     independent releasing attempt must prepend its own slot without changing
     the old snapshot or relabeling the original lexical failure. *)
  let retained = Completion.create ~issuer:(ref ()) ~domain in
  Completion.record_producer_exit retained (Ok ());
  let lexical_attempt = Completion.begin_finish_call retained in
  let lexical_failure = Failure.Raised (sentinel, Stdlib.Printexc.get_callstack 8) in
  Completion.record_finish retained ~attempt:lexical_attempt (Error lexical_failure);
  Completion.record_reply
    retained
    ~attempt:lexical_attempt
    ~origin:Handler
    (Error lexical_failure);
  let before_release = snapshot retained in
  check_kinds before_release [ Lexical_handler ];
  let release_attempt = Attempt.create () in
  let release_failure = Failure.Raised (sentinel, Stdlib.Printexc.get_callstack 8) in
  Completion.record_release retained ~attempt:release_attempt;
  Completion.record_finish retained ~attempt:release_attempt (Error release_failure);
  let after_release = snapshot retained in
  check_kinds after_release [ Release; Lexical_handler ];
  check_kinds before_release [ Lexical_handler ];
  let slots = Completion_snapshot.failure_slots after_release in
  assert (phys_equal (Slot.failure (List.nth_exn slots 0)) release_failure);
  assert (phys_equal (Slot.failure (List.nth_exn slots 1)) lexical_failure);
  assert (
    List.equal
      phys_equal
      (Completion_snapshot.failures before_release)
      [ lexical_failure ]);
  assert (
    List.equal
      phys_equal
      (Completion_snapshot.failures after_release)
      [ release_failure; lexical_failure ]);
  (* SAME releasing/lexical invocation is suppressed, while its independent
     await boundary remains a distinct fact even with the same payload. *)
  check_kinds completed [ Release; Reply_boundary ];
  check_kinds incomplete [ Reply_boundary ];
  check_kinds handled [];
  check_kinds rejection [ Reply_boundary ];
  Completion.record_capture_failure retained lexical_failure;
  let captured = snapshot retained in
  check_kinds captured [ Release; Lexical_handler; Capture ];
  check_kinds after_release [ Release; Lexical_handler ];
  assert (
    phys_equal
      (Slot.failure (List.last_exn (Completion_snapshot.failure_slots captured)))
      lexical_failure);
  print_endline "unresolved original handler stays incomplete; rejection is not release";
  [%expect {| unresolved original handler stays incomplete; rejection is not release |}]
;;

module Completion_observation = struct
  type t =
    { completion : Completion.t
    ; snapshot : Completion_snapshot.t
    }

  let completion t = t.completion
  let snapshot t = t.snapshot

  let observe completion =
    Result.map (Completion.snapshot completion) ~f:(fun snapshot ->
      { completion; snapshot })
  ;;
end

module Cleanup_snapshot = struct
  type t =
    { registration_closed : bool
    ; pending : bool
    ; failures : Failure.t list
    ; completions : Completion_snapshot.t list
    ; claims : Completion_observation.t list
    }

  let registration_closed t = t.registration_closed
  let pending t = t.pending
  let failures t = t.failures
  let completions t = t.completions
  let claims t = t.claims
end

module Parent = struct
  module Child = struct
    type t =
      { manager : Chat_response.Moderator_manager.t
      ; completion : Completion.t
      }
  end

  type t =
    { issuer : unit ref
    ; domain : int
    ; mutable registration_closed : bool
    ; mutable children : Child.t list
    }

  let create ~issuer ~domain =
    { issuer; domain; registration_closed = false; children = [] }
  ;;

  let compact t =
    t.children
    <- List.filter t.children ~f:(fun child ->
         not
           (Completion_snapshot.positive (Completion.snapshot_unchecked child.completion)))
  ;;

  let check t ~issuer =
    if not (on_domain t.domain)
    then Error (domain_error ())
    else if (not (phys_equal t.issuer issuer)) || t.registration_closed
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"callback parent registration ended or changed"
           ~retryable:false
           ())
    else Ok ()
  ;;

  let install t ~issuer ~manager completion =
    match check t ~issuer with
    | Error _ -> invalid_arg "callback ACK installed after parent registration closed"
    | Ok () ->
      if not (Completion.belongs_to completion ~issuer ~domain:t.domain)
      then invalid_arg "callback ACK belongs to another Actor/domain";
      compact t;
      (match
         List.find t.children ~f:(fun child -> phys_equal child.completion completion)
       with
       | Some child when phys_equal child.manager manager ->
         invalid_arg "callback ACK registered twice"
       | Some _ -> invalid_arg "callback ACK Manager association changed"
       | None -> t.children <- t.children @ [ { Child.manager; completion } ])
  ;;

  let close t =
    check_domain_exn t.domain;
    t.registration_closed <- true
  ;;

  let snapshot ?manager t =
    if not (on_domain t.domain)
    then Error (domain_error ())
    else (
      compact t;
      let claims =
        List.filter t.children ~f:(fun child ->
          Option.value_map manager ~default:true ~f:(phys_equal child.manager))
        |> List.map ~f:(fun child ->
          { Completion_observation.completion = child.completion
          ; snapshot = Completion.snapshot_unchecked child.completion
          })
      in
      let completions = List.map claims ~f:Completion_observation.snapshot in
      Ok
        { Cleanup_snapshot.registration_closed = t.registration_closed
        ; pending =
            (not t.registration_closed)
            || List.exists completions ~f:Completion_snapshot.pending
        ; failures = List.concat_map completions ~f:Completion_snapshot.failures
        ; completions
        ; claims
        })
  ;;
end

module Initialization = struct
  type t =
    { scope : unit ref
    ; parent : Parent.t
    }

  let snapshot t = Parent.snapshot t.parent
end

module Operation = struct
  type terminal_outcome =
    ( (unit, Agent_protocol.Error.t) Result.t
      , exn * Stdlib.Printexc.raw_backtrace )
      Result.t

  type t =
    { operation : Agent_protocol.Operation.t
    ; parent : Parent.t
    ; mutable terminal : terminal_outcome option
    }

  let operation t =
    if on_domain t.parent.domain then Ok t.operation else Error (domain_error ())
  ;;

  let snapshot t =
    Result.map (Parent.snapshot t.parent) ~f:(fun snapshot ->
      { snapshot with pending = snapshot.pending || Option.is_none t.terminal })
  ;;

  let snapshot_for_manager t ~manager =
    Result.map (Parent.snapshot ~manager t.parent) ~f:(fun snapshot ->
      { snapshot with pending = snapshot.pending || Option.is_none t.terminal })
  ;;

  let terminal t =
    if on_domain t.parent.domain then Ok t.terminal else Error (domain_error ())
  ;;
end

module Host = struct
  let initialization ~issuer ~domain ~scope =
    { Initialization.scope; parent = Parent.create ~issuer ~domain }
  ;;

  let operation ~issuer ~domain ~operation =
    { Operation.operation; parent = Parent.create ~issuer ~domain; terminal = None }
  ;;

  let check_initialization_registration (t : Initialization.t) ~issuer ~scope =
    let open Result.Let_syntax in
    let%bind () = Parent.check t.parent ~issuer in
    if phys_equal t.scope scope
    then Ok ()
    else
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"callback initialization parent changed"
           ~retryable:false
           ())
  ;;

  let check_operation_registration
        (t : Operation.t)
        ~issuer
        ~(operation : Agent_protocol.Operation.t)
    =
    let open Result.Let_syntax in
    let%bind () = Parent.check t.parent ~issuer in
    if
      Agent_protocol.Id.Operation.equal t.operation.id operation.id
      && Int.equal t.operation.generation operation.generation
      && Agent_protocol.Operation.equal_kind t.operation.kind operation.kind
    then Ok ()
    else
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"callback operation parent changed"
           ~retryable:false
           ())
  ;;

  let install_initialization_callback
        (t : Initialization.t)
        ~issuer
        ~scope
        ~manager
        completion
    =
    Result.ok_or_failwith
      (Result.map_error (check_initialization_registration t ~issuer ~scope) ~f:(fun _ ->
         "callback initialization ACK parent changed"));
    Parent.install t.parent ~issuer ~manager completion
  ;;

  let install_operation_callback (t : Operation.t) ~issuer ~operation ~manager completion =
    Result.ok_or_failwith
      (Result.map_error (check_operation_registration t ~issuer ~operation) ~f:(fun _ ->
         "callback operation ACK parent changed"));
    Parent.install t.parent ~issuer ~manager completion
  ;;

  let end_initialization_registration (t : Initialization.t) = Parent.close t.parent
  let end_operation_registration (t : Operation.t) = Parent.close t.parent

  let record_operation_terminal (t : Operation.t) outcome =
    check_domain_exn t.parent.domain;
    match t.terminal with
    | None -> t.terminal <- Some outcome
    | Some _ -> ()
  ;;
end
