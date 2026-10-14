open Core
module H = Agent_protocol.Host_invocation
module I = Agent_protocol.Invocation
module O = Agent_protocol.Execution_owner
module E = Agent_protocol.Error
module C = Chat_response.Tool_capability
module Job_scope = Developer_job_scope

type storage =
  { read : Agent_protocol.Id.Invocation.t -> ((int * H.t) option, E.t) result
  ; compare_and_set :
      id:Agent_protocol.Id.Invocation.t
      -> expected_revision:int option
      -> after:H.t
      -> (int, E.t) result
  }

type identity =
  { owner : O.developer
  ; capture_fingerprint : Chatml_source.Digest.t
  ; ceiling : C.t
  ; selected : unit -> C.t
  ; current :
      owner:O.developer
      -> capture_fingerprint:Chatml_source.Digest.t
      -> (unit, E.t) result
  ; now : unit -> Agent_protocol.Timestamp.t
  }

type t =
  { identity : identity
  ; storage : storage
  }

type active =
  { issuer : identity
  ; context : H.context
  ; job_parent : Job_scope.Parent.t option
  ; parent_live_now : unit -> bool
  ; domain_id : int
  ; live : bool Atomic.t
  }

type parent =
  | Root
  | Child of active
  | Job_child of Job_scope.Parent.t

let invalid message = Error (E.invalid_request message)

let validate_selection ~ceiling selected =
  List.fold_result (C.references selected) ~init:() ~f:(fun () reference ->
    C.resolve ceiling ~id:reference.id ~fingerprint:reference.fingerprint
    |> Result.map ~f:ignore
    |> Result.map_error ~f:(fun error -> E.invalid_request error.C.message))
;;

let create
      ~(owner : O.developer)
      ~capture_fingerprint
      ~ceiling
      ~selected
      ~current
      ~now
      ~storage
  =
  let open Result.Let_syntax in
  let%bind () = O.validate (O.Developer owner) in
  let%bind () = current ~owner ~capture_fingerprint in
  let%map () = validate_selection ~ceiling (selected ()) in
  { identity = { owner; capture_fingerprint; ceiling; selected; current; now }; storage }
;;

let owner t = t.identity.owner
let capture_fingerprint t = t.identity.capture_fingerprint
let ceiling t = t.identity.ceiling

let selected_identity identity =
  let selected = identity.selected () in
  Result.map (validate_selection ~ceiling:identity.ceiling selected) ~f:(fun () ->
    selected)
;;

let selected t = selected_identity t.identity

let current_identity t =
  t.current ~owner:t.owner ~capture_fingerprint:t.capture_fingerprint
;;

let current t = current_identity t.identity

let same_owner t context =
  match context.H.owner with
  | O.Developer owner -> O.equal_developer owner t.owner
  | Conversation _ -> false
;;

let same_domain active = Int.equal active.domain_id (Stdlib.Domain.self () :> int)

let job_parent_id = function
  | None -> None
  | Some parent -> Some (Job_scope.Parent.job_id parent)
;;

let inherited_parent = function
  | Root -> None
  | Child active -> active.job_parent
  | Job_child parent -> Some parent
;;

let valid_parent_shape t ~parent (context : H.context) =
  let open Result.Let_syntax in
  match parent, context.parent_invocation, context.parent_job with
  | Root, None, None -> Ok ()
  | Root, None, Some _ -> invalid "developer root invocation cannot claim a job parent"
  | Root, Some _, _ -> invalid "developer root invocation cannot claim a parent"
  | Job_child claimed, None, Some job_id ->
    if Agent_protocol.Id.Job.equal job_id (Job_scope.Parent.job_id claimed)
    then Ok ()
    else invalid "developer job child has a foreign claimed job parent"
  | Job_child _, Some _, _ ->
    invalid "developer job child cannot claim an invocation parent"
  | Job_child _, None, None -> invalid "developer job child requires its claimed job"
  | Child active, Some id, claimed_job_id ->
    let parent_context = active.context in
    let%bind () =
      if
        phys_equal active.issuer t
        && same_domain active
        && Atomic.get active.live
        && active.parent_live_now ()
      then Ok ()
      else invalid "developer invocation parent claim expired or belongs to another owner"
    in
    let%bind () =
      if
        Agent_protocol.Id.Invocation.equal id parent_context.id
        && O.equal context.owner parent_context.owner
        && Option.equal
             Agent_protocol.Id.Job.equal
             claimed_job_id
             (job_parent_id active.job_parent)
      then Ok ()
      else invalid "developer invocation parent is foreign"
    in
    (match parent_context.deadline, context.deadline with
     | None, _ -> Ok ()
     | Some _, None -> invalid "developer child has no deadline under a timed parent"
     | Some parent_deadline, Some child_deadline
       when Agent_protocol.Timestamp.compare child_deadline parent_deadline <= 0 -> Ok ()
     | Some _, Some _ -> invalid "developer invocation deadline exceeds parent")
  | Child _, None, _ -> invalid "developer child invocation requires its direct parent"
;;

let valid_parent_now t ~parent context =
  let open Result.Let_syntax in
  let%bind () = valid_parent_shape t ~parent context in
  match inherited_parent parent with
  | None -> Ok ()
  | Some claimed ->
    Eio.Fiber.check ();
    Job_scope.Parent.check_now
      claimed
      ~owner:t.owner
      ~capture_fingerprint:t.capture_fingerprint
;;

let valid_parent t ~parent context =
  let open Result.Let_syntax in
  let%bind () = valid_parent_shape t ~parent context in
  let%bind () =
    match inherited_parent parent with
    | None -> Ok ()
    | Some claimed ->
      Job_scope.Parent.check
        claimed
        ~owner:t.owner
        ~capture_fingerprint:t.capture_fingerprint
  in
  valid_parent_now t ~parent context
;;

let check_parent_now t ~parent invocation =
  let open Result.Let_syntax in
  let context = H.context invocation in
  let%bind () = valid_parent_now t ~parent context in
  match context.deadline with
  | Some deadline when Agent_protocol.Timestamp.compare deadline (t.now ()) <= 0 ->
    invalid "developer invocation deadline has expired"
  | Some _ | None -> Ok ()
;;

let check_invocation_identity t ~parent invocation =
  let open Result.Let_syntax in
  let%bind () = H.validate invocation in
  let context = H.context invocation in
  let%bind () =
    if same_owner t context
    then Ok ()
    else invalid "developer invocation has foreign owner"
  in
  let%bind () = current_identity t in
  let%bind () = valid_parent t ~parent context in
  check_parent_now t ~parent invocation
;;

let check_invocation t ~parent invocation =
  check_invocation_identity t.identity ~parent invocation
;;

let validate_initial t ~parent invocation =
  let open Result.Let_syntax in
  let%bind () = check_invocation_identity t ~parent invocation in
  let%bind () =
    if I.equal_status (H.status invocation) I.Admitted
    then Ok ()
    else invalid "developer invocation must start admitted"
  in
  Ok (H.context invocation)
;;

let interrupted message = E.create Interrupted ~message ~retryable:false ()

let persistence_uncertain context =
  E.create
    Persistence_error
    ~message:"developer invocation terminal save is uncertain; inspect the durable record"
    ~retryable:false
    ~data:(`Object [ "invocation_id", Agent_protocol.Id.Invocation.to_json context.H.id ])
    ()
;;

let failure_outcome (error : E.t) =
  I.Fail
    { code = E.code_to_string error.code
    ; message = String.prefix error.message 16384
    ; retryable = error.retryable
    ; details = `Null
    }
;;

let exception_outcome =
  I.Fail
    { code = "invocation.callback_failed"
    ; message = "Developer tool execution failed."
    ; retryable = false
    ; details = `Null
    }
;;

type 'a callback_result =
  | Returned of 'a
  | Failed of E.t
  | Raised of exn * Stdlib.Printexc.raw_backtrace

let run_callback identity ~parent ~parent_live_now ~context ~dispatched callback =
  let active =
    { issuer = identity
    ; context
    ; job_parent = inherited_parent parent
    ; parent_live_now
    ; domain_id = (Stdlib.Domain.self () :> int)
    ; live = Atomic.make true
    }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set active.live false)
    ~f:(fun () ->
      match
        let open Result.Let_syntax in
        let%bind () = check_invocation_identity identity ~parent dispatched in
        callback ~active ~dispatched
      with
      | Ok value -> Returned value
      | Error error -> Failed error
      | exception exn -> Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
;;

let resolve_or_fail dispatched outcome =
  match H.resolve dispatched outcome with
  | Ok terminal -> terminal
  | Error _ ->
    (match H.resolve dispatched exception_outcome with
     | Ok terminal -> terminal
     | Error _ -> failwith "validated dispatched invocation cannot resolve")
;;

let terminal_for_error dispatched = function
  | Returned outcome -> resolve_or_fail dispatched outcome
  | Failed error -> resolve_or_fail dispatched (failure_outcome error)
  | Raised (Eio.Cancel.Cancelled _, _) ->
    (match H.cancel dispatched ~reason:"cancelled" with
     | Ok terminal -> terminal
     | Error _ -> failwith "validated dispatched invocation cannot cancel")
  | Raised _ -> resolve_or_fail dispatched exception_outcome
;;

let saved_revision previous next =
  let valid =
    next > 0
    && next < Int.max_value
    &&
    match previous with
    | None -> true
    | Some prior -> prior > 0 && prior < Int.max_value && next > prior
  in
  if valid then Ok next else invalid "developer invocation revision is invalid"
;;

let save t ~id ~expected_revision after =
  let open Result.Let_syntax in
  let%bind revision = t.storage.compare_and_set ~id ~expected_revision ~after in
  saved_revision expected_revision revision
;;

let prepare_dispatch t ~parent ~invocation ~read =
  let open Result.Let_syntax in
  let%bind context = validate_initial t ~parent invocation in
  let%bind existing = read context.id in
  let%bind () = check_invocation_identity t ~parent invocation in
  let%bind expected_revision =
    match existing with
    | None -> Ok None
    | Some (revision, prior) ->
      let%bind _ = saved_revision None revision in
      if not (H.equal_context (H.context prior) context)
      then invalid "developer invocation ID is bound to another context"
      else (
        match H.status prior with
        | I.Admitted -> Ok (Some revision)
        | Dispatching -> Error (interrupted "developer invocation is already dispatching")
        | Resolved _ | Published _ -> invalid "developer invocation is already terminal")
  in
  let%bind () = check_invocation_identity t ~parent invocation in
  let%bind dispatched = H.dispatch invocation in
  Ok (context, expected_revision, dispatched)
;;

let with_invocation t ~parent ~invocation callback =
  let open Result.Let_syntax in
  let%bind context, expected_revision, dispatched =
    prepare_dispatch t.identity ~parent ~invocation ~read:t.storage.read
  in
  let%bind dispatch_revision = save t ~id:context.id ~expected_revision dispatched in
  let callback_result =
    run_callback
      t.identity
      ~parent
      ~parent_live_now:(fun () -> true)
      ~context
      ~dispatched
      callback
  in
  let terminal = Ok (terminal_for_error dispatched callback_result) in
  let final =
    Result.bind terminal ~f:(fun after ->
      match
        Eio.Cancel.protect (fun () ->
          save t ~id:context.id ~expected_revision:(Some dispatch_revision) after)
      with
      | Ok _ -> Ok after
      | Error _ -> Error (persistence_uncertain context)
      | exception _ -> Error (persistence_uncertain context))
  in
  match callback_result with
  | Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
  | Returned _ | Failed _ -> final
;;

module Selected = struct
  type nonrec t = identity
  type nonrec active = active

  type nonrec parent = parent =
    | Root
    | Child of active
    | Job_child of Job_scope.Parent.t

  type dispatch_storage =
    { read : Agent_protocol.Id.Invocation.t -> ((int * H.t) option, E.t) result
    ; save_dispatch : expected_revision:int option -> dispatched:H.t -> (int, E.t) result
    }

  type cause =
    | Cause_normal
    | Cause_typed_error of E.t
    | Cause_raised of exn * Stdlib.Printexc.raw_backtrace

  type dispatch =
    | Awaiting_reconciliation
    | Installed of int

  type 'selection completion =
    { dispatch : dispatch
    ; terminal : H.t
    ; selected : 'selection option
    ; cause : cause
    ; context : H.context
    }

  let create ~owner ~capture_fingerprint ~ceiling ~selected ~current ~now =
    let open Result.Let_syntax in
    let%bind () = O.validate (O.Developer owner) in
    let%bind () = current ~owner ~capture_fingerprint in
    let%map () = validate_selection ~ceiling (selected ()) in
    { owner; capture_fingerprint; ceiling; selected; current; now }
  ;;

  let ceiling t = t.ceiling
  let same_issuer = phys_equal
  let selected = selected_identity
  let check_invocation = check_invocation_identity
  let check_parent_now = check_parent_now

  let inherited_job_parent t active =
    if
      phys_equal active.issuer t
      && same_domain active
      && Atomic.get active.live
      && active.parent_live_now ()
    then (
      match active.job_parent with
      | None -> Ok None
      | Some parent ->
        Result.map
          (Job_scope.Parent.check_now
             parent
             ~owner:t.owner
             ~capture_fingerprint:t.capture_fingerprint)
          ~f:(fun () -> Some parent))
    else invalid "developer invocation parent claim expired or belongs to another owner"
  ;;

  let terminal_for_cause dispatched = function
    | Cause_normal -> resolve_or_fail dispatched exception_outcome
    | Cause_typed_error error -> resolve_or_fail dispatched (failure_outcome error)
    | Cause_raised (Eio.Cancel.Cancelled _, _) ->
      (match H.cancel dispatched ~reason:"cancelled" with
       | Ok terminal -> terminal
       | Error _ -> failwith "validated dispatched invocation cannot cancel")
    | Cause_raised _ -> resolve_or_fail dispatched exception_outcome
  ;;

  let completion ~dispatch ~dispatched ~selected ~cause ~frozen =
    let open Result.Let_syntax in
    let outcome, selected =
      match frozen with
      | Some (outcome, selected) -> outcome, Some selected
      | None ->
        (match cause with
         | Cause_normal -> exception_outcome, selected
         | Cause_typed_error error -> failure_outcome error, None
         | Cause_raised (Eio.Cancel.Cancelled _, _) -> I.Cancelled "cancelled", None
         | Cause_raised _ -> exception_outcome, None)
    in
    let%map terminal =
      match frozen, cause with
      | Some _, _ -> H.resolve dispatched outcome
      | None, Cause_raised (Eio.Cancel.Cancelled _, _) ->
        Ok (terminal_for_cause dispatched cause)
      | None, _ -> Ok (resolve_or_fail dispatched outcome)
    in
    { dispatch; terminal; selected; cause; context = H.context dispatched }
  ;;

  let with_invocation t ~parent ~invocation ~dispatch ~parent_live_now ~frozen callback =
    let open Result.Let_syntax in
    let%bind context, expected_revision, dispatched =
      prepare_dispatch t ~parent ~invocation ~read:dispatch.read
    in
    let saved =
      match dispatch.save_dispatch ~expected_revision ~dispatched with
      | Ok revision ->
        (match saved_revision expected_revision revision with
         | Ok revision -> `Installed revision
         | Error error -> `Unknown (Cause_typed_error error))
      | Error error -> `Unknown (Cause_typed_error error)
      | exception exn ->
        `Unknown (Cause_raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
    in
    match saved with
    | `Unknown cause ->
      completion
        ~dispatch:Awaiting_reconciliation
        ~dispatched
        ~selected:None
        ~cause
        ~frozen:None
    | `Installed revision ->
      let callback_result =
        run_callback t ~parent ~parent_live_now ~context ~dispatched callback
      in
      (match callback_result with
       | Returned (outcome, selected) ->
         (match frozen () with
          | Some (frozen_outcome, frozen_selected)
            when (not (I.equal_outcome outcome frozen_outcome))
                 || not
                      (Option.value_map
                         selected
                         ~default:false
                         ~f:(phys_equal frozen_selected)) ->
            completion
              ~dispatch:(Installed revision)
              ~dispatched
              ~selected:None
              ~cause:
                (Cause_typed_error
                   (E.invalid_request
                      "selected native outcome changed after physical sealing"))
              ~frozen:(Some (frozen_outcome, frozen_selected))
          | Some _ | None ->
            let%map terminal =
              match selected with
              | Some _ -> H.resolve dispatched outcome
              | None -> Ok (resolve_or_fail dispatched outcome)
            in
            { dispatch = Installed revision
            ; terminal
            ; selected
            ; cause = Cause_normal
            ; context
            })
       | Failed error ->
         let frozen = frozen () in
         completion
           ~dispatch:(Installed revision)
           ~dispatched
           ~selected:None
           ~cause:(Cause_typed_error error)
           ~frozen
       | Raised (exn, backtrace) ->
         let frozen = frozen () in
         completion
           ~dispatch:(Installed revision)
           ~dispatched
           ~selected:None
           ~cause:(Cause_raised (exn, backtrace))
           ~frozen)
  ;;

  module Completion = struct
    type 'selection t = 'selection completion

    type nonrec dispatch = dispatch =
      | Awaiting_reconciliation
      | Installed of int

    type settlement =
      | Terminal_saved of int
      | Recovered_terminal_saved of
          { dispatch_revision : int
          ; terminal_revision : int
          }
      | Dispatch_absent
      | Uncertain of E.t

    let dispatch t = t.dispatch
    let terminal t = t.terminal
    let selected t = t.selected

    let finish t ~settlement =
      let settled =
        match t.dispatch, settlement with
        | Installed dispatch_revision, Terminal_saved terminal_revision
          when Result.is_ok (saved_revision (Some dispatch_revision) terminal_revision) ->
          Ok t.terminal
        | ( (Awaiting_reconciliation | Installed _)
          , Recovered_terminal_saved { dispatch_revision; terminal_revision } )
          when Result.is_ok (saved_revision None dispatch_revision)
               && Result.is_ok (saved_revision (Some dispatch_revision) terminal_revision)
               &&
               match t.dispatch with
               | Awaiting_reconciliation -> true
               | Installed known -> Int.equal known dispatch_revision -> Ok t.terminal
        | Awaiting_reconciliation, Dispatch_absent ->
          Error
            (E.create
               Persistence_error
               ~message:"developer invocation dispatch was not installed"
               ~retryable:false
               ())
        | ( (Awaiting_reconciliation | Installed _)
          , (Terminal_saved _ | Recovered_terminal_saved _ | Dispatch_absent | Uncertain _)
          ) -> Error (persistence_uncertain t.context)
      in
      match t.cause with
      | Cause_raised (exn, backtrace) ->
        Stdlib.Printexc.raise_with_backtrace exn backtrace
      | Cause_typed_error error ->
        (match t.dispatch, settlement with
         | Awaiting_reconciliation, Dispatch_absent -> Error error
         | ( (Awaiting_reconciliation | Installed _)
           , ( Terminal_saved _
             | Recovered_terminal_saved _
             | Dispatch_absent
             | Uncertain _ ) ) -> settled)
      | Cause_normal -> settled
    ;;
  end
end
