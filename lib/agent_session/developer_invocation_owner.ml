open Core
module H = Agent_protocol.Host_invocation
module I = Agent_protocol.Invocation
module O = Agent_protocol.Execution_owner
module E = Agent_protocol.Error
module C = Chat_response.Tool_capability

type storage =
  { read : Agent_protocol.Id.Invocation.t -> ((int * H.t) option, E.t) result
  ; compare_and_set :
      id:Agent_protocol.Id.Invocation.t
      -> expected_revision:int option
      -> after:H.t
      -> (int, E.t) result
  }

type t =
  { owner : O.developer
  ; capture_fingerprint : Chatml_source.Digest.t
  ; ceiling : C.t
  ; selected : unit -> C.t
  ; current :
      owner:O.developer
      -> capture_fingerprint:Chatml_source.Digest.t
      -> (unit, E.t) result
  ; now : unit -> Agent_protocol.Timestamp.t
  ; storage : storage
  }

type active =
  { issuer : t
  ; context : H.context
  ; domain_id : int
  ; live : bool Atomic.t
  }

type parent =
  | Root
  | Child of active

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
  { owner; capture_fingerprint; ceiling; selected; current; now; storage }
;;

let owner t = t.owner
let capture_fingerprint t = t.capture_fingerprint
let ceiling t = t.ceiling

let selected t =
  let selected = t.selected () in
  Result.map (validate_selection ~ceiling:t.ceiling selected) ~f:(fun () -> selected)
;;

let current t = t.current ~owner:t.owner ~capture_fingerprint:t.capture_fingerprint

let same_owner t context =
  match context.H.owner with
  | O.Developer owner -> O.equal_developer owner t.owner
  | Conversation _ -> false
;;

let same_domain active = Int.equal active.domain_id (Stdlib.Domain.self () :> int)

let valid_parent t ~parent (context : H.context) =
  let open Result.Let_syntax in
  let%bind () =
    if Option.is_some context.parent_job
    then invalid "developer invocation job parent requires an owned job claim"
    else Ok ()
  in
  match parent, context.parent_invocation with
  | Root, None -> Ok ()
  | Root, Some _ -> invalid "developer root invocation cannot claim a parent"
  | Child active, Some id ->
    let parent_context = active.context in
    let%bind () =
      if phys_equal active.issuer t && same_domain active && Atomic.get active.live
      then Ok ()
      else invalid "developer invocation parent claim expired or belongs to another owner"
    in
    let%bind () =
      if
        Agent_protocol.Id.Invocation.equal id parent_context.id
        && O.equal context.owner parent_context.owner
      then Ok ()
      else invalid "developer invocation parent is foreign"
    in
    (match parent_context.deadline, context.deadline with
     | None, _ -> Ok ()
     | Some _, None -> invalid "developer child has no deadline under a timed parent"
     | Some parent_deadline, Some child_deadline
       when Agent_protocol.Timestamp.compare child_deadline parent_deadline <= 0 -> Ok ()
     | Some _, Some _ -> invalid "developer invocation deadline exceeds parent")
  | Child _, None -> invalid "developer child invocation requires its direct parent"
;;

let check_invocation t ~parent invocation =
  let open Result.Let_syntax in
  let%bind () = H.validate invocation in
  let context = H.context invocation in
  let%bind () =
    if same_owner t context
    then Ok ()
    else invalid "developer invocation has foreign owner"
  in
  let%bind () = current t in
  let%bind () = valid_parent t ~parent context in
  let%bind () =
    match context.deadline with
    | Some deadline when Agent_protocol.Timestamp.compare deadline (t.now ()) <= 0 ->
      invalid "developer invocation deadline has expired"
    | Some _ | None -> Ok ()
  in
  Ok ()
;;

let validate_initial t ~parent invocation =
  let open Result.Let_syntax in
  let%bind () = check_invocation t ~parent invocation in
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

let with_invocation t ~parent ~invocation callback =
  let open Result.Let_syntax in
  let%bind context = validate_initial t ~parent invocation in
  let%bind existing = t.storage.read context.id in
  let%bind () = check_invocation t ~parent invocation in
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
  let%bind () = check_invocation t ~parent invocation in
  let%bind dispatched = H.dispatch invocation in
  let%bind dispatch_revision = save t ~id:context.id ~expected_revision dispatched in
  let active =
    { issuer = t
    ; context
    ; domain_id = (Stdlib.Domain.self () :> int)
    ; live = Atomic.make true
    }
  in
  let callback_result =
    Exn.protect
      ~finally:(fun () -> Atomic.set active.live false)
      ~f:(fun () ->
        match
          let%bind () = check_invocation t ~parent dispatched in
          callback ~active ~dispatched
        with
        | Ok outcome -> `Outcome outcome
        | Error error -> `Outcome (failure_outcome error)
        | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  let terminal =
    match callback_result with
    | `Outcome outcome ->
      (match H.resolve dispatched outcome with
       | Ok _ as success -> success
       | Error _ -> H.resolve dispatched exception_outcome)
    | `Raised (Eio.Cancel.Cancelled _, _) -> H.cancel dispatched ~reason:"cancelled"
    | `Raised _ -> H.resolve dispatched exception_outcome
  in
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
  | `Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
  | `Outcome _ -> final
;;
