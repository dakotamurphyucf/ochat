open! Core
module P = Agent_protocol
module W = P.Authoring.Wire
module Ledger = Chatml_authoring_context
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Sources = Authoring_source_host
module Upload = Authoring_upload_host
module Captured = Chatml_source
module Build = Chatml_project_build
module Activity = Authoring_context_activity
open Activity

module Limits = struct
  type t =
    { max_connections : int
    ; max_active_requests_per_connection : int
    ; max_roots_per_connection : int
    ; max_response_bytes : int
    ; max_provisional_results : int
    }

  let create
        ~max_connections
        ~max_active_requests_per_connection
        ~max_roots_per_connection
        ~max_response_bytes
        ~max_provisional_results
    =
    if
      max_connections <= 0
      || max_active_requests_per_connection <= 0
      || max_roots_per_connection <= 0
      || max_response_bytes <= 0
      || max_provisional_results <= 0
      || max_provisional_results > 1024
    then Or_error.error_string "authoring service limits must be positive"
    else
      Ok
        { max_connections
        ; max_active_requests_per_connection
        ; max_roots_per_connection
        ; max_response_bytes
        ; max_provisional_results
        }
  ;;
end

module Selection_policy = struct
  type t =
    { project_contract :
        Operator_authorization.t
        -> root:Sources.Root.t
        -> project:Source.Portable_path.t
        -> selection:P.Authoring.Context.Selection.t
        -> Chatml.Chatml_compiler_contract.t option
    ; document_static :
        Operator_authorization.t -> requested_environment:string option -> Build.t option
    }

  let create ~project_contract ~document_static = { project_contract; document_static }
end

module Supplied_document_policy = struct
  type t = { authorize : Operator_authorization.t -> bool }

  let create ~authorize = { authorize }
end

module Selected_feature = struct
  type t =
    | Core_authoring
    | Bundle_upload
    | Semantic
    | Lexical
    | Assistance
end

module Error = struct
  type t =
    | Uninstalled
    | Unselected_feature
    | Unauthorized
    | Expired_actor
    | Foreign_connection
    | Stale_negotiation
    | Stale_context
    | Stale_source
    | Source_unavailable
    | Selection_unavailable
    | Busy
    | Closed
    | Resource_limit
    | Invalid_position of P.Authoring.Failure.invalid_position
    | Invalid_request
    | Analysis_unavailable
    | Storage_unavailable
    | Upload_failure of Upload.Error.t

  let to_protocol error =
    let code, message, reason =
      match error with
      | Uninstalled | Unselected_feature ->
        P.Error.Method_not_found, "authoring method unavailable", None
      | Unauthorized | Expired_actor | Foreign_connection ->
        ( P.Error.Permission_denied
        , "authoring access denied"
        , Some P.Authoring.Failure.Unauthorized )
      | Stale_negotiation | Closed ->
        ( P.Error.Lease_stale
        , "authoring connection expired"
        , Some P.Authoring.Failure.Context_lease_stale )
      | Stale_context ->
        ( P.Error.Conflict
        , "authoring context changed"
        , Some P.Authoring.Failure.Stale_context )
      | Stale_source ->
        ( P.Error.Conflict
        , "authoring source changed"
        , Some P.Authoring.Failure.Stale_source )
      | Source_unavailable ->
        ( P.Error.Invalid_request
        , "authoring source unavailable"
        , Some P.Authoring.Failure.Source_unavailable )
      | Selection_unavailable ->
        ( P.Error.Invalid_request
        , "authoring selection unavailable"
        , Some P.Authoring.Failure.Environment_unavailable )
      | Busy -> P.Error.Conflict, "authoring operation busy", None
      | Resource_limit ->
        ( P.Error.Resource_limit
        , "authoring limit reached"
        , Some P.Authoring.Failure.Work_limit )
      | Invalid_position reason ->
        ( P.Error.Invalid_request
        , "invalid authoring position"
        , Some (P.Authoring.Failure.Invalid_position reason) )
      | Invalid_request -> P.Error.Invalid_request, "invalid authoring request", None
      | Analysis_unavailable ->
        P.Error.Internal_error, "authoring analysis unavailable", None
      | Storage_unavailable ->
        P.Error.Internal_error, "authoring storage unavailable", None
      | Upload_failure upload_error ->
        (match upload_error with
         | Upload.Error.Unauthorized | Expired_actor | Foreign_domain ->
           ( P.Error.Permission_denied
           , "bundle access denied"
           , Some P.Authoring.Failure.Unauthorized )
         | Closed ->
           ( P.Error.Lease_stale
           , "bundle host closed"
           , Some P.Authoring.Failure.Context_lease_stale )
         | Busy | Store Busy -> P.Error.Conflict, "bundle operation busy", None
         | Selection_unavailable ->
           ( P.Error.Invalid_request
           , "bundle selection unavailable"
           , Some P.Authoring.Failure.Environment_unavailable )
         | Stale_capture ->
           ( P.Error.Conflict
           , "bundle capture changed"
           , Some P.Authoring.Failure.Stale_source )
         | Invalid_closure | Store Invalid_manifest ->
           P.Error.Invalid_request, "invalid bundle closure", None
         | Resource_limit | Store Limit_exceeded ->
           ( P.Error.Resource_limit
           , "bundle limit reached"
           , Some P.Authoring.Failure.Work_limit )
         | Store Idempotency_conflict ->
           P.Error.Idempotency_conflict, "bundle idempotency conflict", None
         | Store Invalid_offset -> P.Error.Invalid_request, "invalid bundle offset", None
         | Store Incomplete -> P.Error.Conflict, "bundle incomplete", None
         | Store Sealed -> P.Error.Conflict, "bundle sealed", None
         | Store Expired -> P.Error.Lease_stale, "bundle expired", None
         | Store Missing_or_foreign -> P.Error.Invalid_request, "bundle unavailable", None
         | Store Storage_failure ->
           P.Error.Internal_error, "bundle storage unavailable", None)
    in
    let error = P.Error.create code ~message ~retryable:false () in
    match reason with
    | None -> error
    | Some reason ->
      (match P.Authoring.Failure.attach error reason with
       | Ok error -> error
       | Error _ -> error)
  ;;
end

type phase = Activity.phase =
  | Active
  | Revoking
  | Closed

type context_owner = Activity.context_owner =
  | Host_project of Sources.Context.t
  | Host_document of Sources.Document_context.t
  | Uploaded_project of Upload.Context_capture.t
  | Supplied_document of Operator_authorization.t

type context_entry = Activity.context_entry
type connection_entry = Activity.connection_entry

module Retained_document_capture = struct
  type t =
    { context : context_entry
    ; identity : Ledger.Identity.t
    ; generation : Ledger.Generation.t
    ; source_id : Captured.Id.t
    ; source_digest : Captured.Digest.t
    ; source_bytes : int
    ; capture : Sources.Document_context.Parsed_capture.t
    ; project_capture : Sources.Document_context.Project_capture.t option
    }

  let same_source left right =
    Captured.Id.equal (Captured.id left) (Captured.id right)
    && Captured.Digest.equal (Captured.digest left) (Captured.digest right)
    && Int.equal (Captured.byte_length left) (Captured.byte_length right)
  ;;

  let prepare ~context ~inputs ~capture ~metadata ~max_sources ~max_bytes ~checkpoint =
    let source = Ledger.Document.Inputs.source inputs in
    let captured = Sources.Document_context.Parsed_capture.source capture in
    let bundle = Sources.Document_context.Parsed_capture.bundle capture in
    if
      max_sources <= 0
      || max_bytes <= 0
      || (not (same_source source captured))
      || not
           (match metadata with
            | None -> true
            | Some (declaration, project) ->
              Sources.Document_context.Project_capture.matches
                project
                ~parsed:capture
                ~declaration)
    then Error Error.Stale_source
    else (
      let rec visit sources count bytes =
        match sources with
        | [] -> Ok (count, bytes)
        | (_, contents) :: rest ->
          checkpoint ();
          let length = String.length contents in
          if count >= max_sources || length > max_bytes - bytes
          then Error Error.Resource_limit
          else visit rest (count + 1) (bytes + length)
      in
      let open Result.Let_syntax in
      let metadata_source =
        Option.map metadata ~f:(fun (_, project) ->
          Sources.Document_context.Project_capture.source project)
      in
      let bundle_sources = Chatmd_source_bundle.sources bundle in
      let metadata_bytes =
        Option.value_map metadata_source ~default:0 ~f:Captured.byte_length
      in
      let metadata_count = Option.value_map metadata_source ~default:0 ~f:(fun _ -> 1) in
      let%map _, _ =
        if metadata_bytes > max_bytes
        then Error Error.Resource_limit
        else visit bundle_sources metadata_count metadata_bytes
      in
      { context
      ; identity = Ledger.Document.Inputs.identity inputs
      ; generation = Ledger.Document.Inputs.generation inputs
      ; source_id = Captured.id source
      ; source_digest = Captured.digest source
      ; source_bytes = Captured.byte_length source
      ; capture
      ; project_capture = Option.map metadata ~f:snd
      })
  ;;

  let matches_import_owner t ~context ~inputs =
    phys_equal t.context context
    && Ledger.Identity.equal t.identity (Ledger.Document.Inputs.identity inputs)
    && Captured.Id.equal t.source_id (Captured.id (Ledger.Document.Inputs.source inputs))
  ;;

  let matches_current t ~context ~inputs =
    matches_import_owner t ~context ~inputs
    && Ledger.Generation.equal t.generation (Ledger.Document.Inputs.generation inputs)
    && Captured.Digest.equal
         t.source_digest
         (Captured.digest (Ledger.Document.Inputs.source inputs))
    && Int.equal
         t.source_bytes
         (Captured.byte_length (Ledger.Document.Inputs.source inputs))
  ;;

  let capture t = t.capture
  let project_capture t = t.project_capture

  let same_head left right =
    match left, right with
    | None, None -> true
    | Some left, Some right -> phys_equal left right
    | None, Some _ | Some _, None -> false
  ;;

  let recompose_bundle t ~inputs ~checkpoint =
    let bundle = Sources.Document_context.Parsed_capture.bundle t.capture in
    let root_file = Chatmd_source_bundle.root_file bundle in
    let root = Ledger.Document.Inputs.source inputs in
    let imports =
      Chatmd_source_bundle.sources bundle
      |> List.filter ~f:(fun (member, _) ->
        checkpoint ();
        not (String.equal member root_file))
    in
    Chatmd_source_bundle.create
      ~limits:(Chatmd_source_bundle.limits bundle)
      ~checkpoint
      ~root_file
      ~sources:((root_file, Captured.contents root) :: imports)
      ()
    |> Result.map_error ~f:(fun _ -> Error.Resource_limit)
  ;;
end

type t =
  { sw : Eio.Switch.t
  ; domain : int
  ; clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; host : P.Id.Server.t
  ; sources : Sources.t option
  ; upload : Upload.t option
  ; supplied_document : Supplied_document_policy.t option
  ; selection_policy : Selection_policy.t
  ; analysis_policy : Project.Policy.t
  ; protocol_limits : P.Authoring.Limits.t
  ; limits : Limits.t
  ; gate : Eio.Mutex.t
  ; mutable connections : connection_entry list
  ; mutable project_cache : Project.Project_cache.t
  ; mutable document_cache : Document.Document_cache.t
  ; mutable document_capture : Retained_document_capture.t option
  ; mutable analysis_index : Authoring_analysis_index.t
  ; mutable provisional_index : Authoring_provisional_index.t
  ; mutable closed : bool
  ; mutable release_hook : Eio.Switch.hook
  }

let with_gate t f =
  let outcome =
    Eio.Mutex.use_rw ~protect:false t.gate (fun () ->
      match f () with
      | result -> `Returned result
      | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | `Returned result -> result
  | `Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

(** Commit a prepared immutable candidate without waiting after its final
    nonyielding owner check. The callback is limited to pure gate-owned CAS and
    cache/ledger pointer replacement; never call host policy or Eio operations. *)
let try_with_gate t f =
  if not (Eio.Mutex.try_lock t.gate)
  then Error Error.Busy
  else Exn.protect ~finally:(fun () -> Eio.Mutex.unlock t.gate) ~f
;;

let with_lifecycle = Activity.with_lifecycle

let check_domain t =
  if Int.equal t.domain (Stdlib.Domain.self () :> int) then Ok () else Error Error.Closed
;;

let find_connection t connection =
  List.find t.connections ~f:(fun entry -> phys_equal entry.connection connection)
;;

let find_context = Activity.find_context
let valid_negotiation = Activity.valid_negotiation

let check_entry t entry =
  if t.closed
  then Error Error.Closed
  else (
    match entry.phase with
    | Active -> Ok ()
    | Revoking -> Error Error.Stale_negotiation
    | Closed -> Error Error.Closed)
;;

let in_active = Activity.in_active
let with_active = Activity.with_active

exception Revoked of Error.t

let check_actor connection actor =
  match Connection_context.request_actor connection (Some actor) with
  | Error _ -> Error Error.Foreign_connection
  | Ok actor ->
    if
      Operator_authorization.is_current actor
      && Operator_authorization.is_current (Connection_context.actor connection)
    then Ok ()
    else Error Error.Expired_actor
;;

let same_principal left right =
  P.Id.Principal.equal left.P.Principal.id right.P.Principal.id
  && String.equal left.authentication_kind right.authentication_kind
  && P.Scope.Set.equal left.scopes right.scopes
  && List.equal
       (fun (left_name, left_value) (right_name, right_value) ->
          String.equal left_name right_name && String.equal left_value right_value)
       left.attributes
       right.attributes
;;

let check_actor_context original current =
  if
    (not (Operator_authorization.is_current original))
    || not (Operator_authorization.is_current current)
  then Error Error.Expired_actor
  else if
    same_principal
      (Operator_authorization.principal original)
      (Operator_authorization.principal current)
  then Ok ()
  else Error Error.Unauthorized
;;

let remove_active entry cancel =
  entry.active
  <- List.filter entry.active ~f:(fun active -> not (phys_equal active cancel));
  if List.is_empty entry.active
  then (
    let wake = entry.idle_waker in
    entry.idle <- None;
    entry.idle_waker <- None;
    wake)
  else None
;;

let with_request
      ?(feature = Selected_feature.Core_authoring)
      ?(final_check = fun () -> Ok ())
      ?(commit = fun result -> Ok result)
      ?after_commit
      ?(abort = fun () -> ())
      t
      ~actor
      ~connection
      ~checkpoint
      f
  =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  let%bind () = check_actor connection actor in
  let%bind entry, negotiation =
    with_gate t (fun () ->
      match find_connection t connection with
      | None -> Error Error.Stale_negotiation
      | Some entry ->
        let%bind () = check_entry t entry in
        let selected_feature =
          match feature with
          | Selected_feature.Core_authoring -> P.Authoring.Features.core
          | Bundle_upload -> P.Authoring.Features.bundle
          | Semantic -> P.Authoring.Features.semantic
          | Lexical -> P.Authoring.Features.lexical
          | Assistance -> P.Authoring.Features.assistance
        in
        if
          not
            (Option.exists entry.negotiation ~f:(fun negotiation ->
               Connection_context.Authoring_negotiation.selected
                 negotiation
                 selected_feature))
        then Error Error.Unselected_feature
        else Ok (entry, Option.value_exn entry.negotiation))
  in
  let%bind () =
    if valid_negotiation entry then Ok () else Error Error.Stale_negotiation
  in
  Eio.Cancel.sub (fun cancel ->
    let idle, idle_waker = Eio.Promise.create () in
    let%bind () =
      with_gate t (fun () ->
        let%bind () = check_entry t entry in
        if
          (not (Option.exists entry.negotiation ~f:(phys_equal negotiation)))
          || List.length entry.active >= t.limits.max_active_requests_per_connection
        then Error Error.Busy
        else (
          if List.is_empty entry.active
          then (
            entry.idle <- Some idle;
            entry.idle_waker <- Some idle_waker);
          entry.active <- cancel :: entry.active;
          Ok ()))
    in
    let registered = ref true in
    let committed = ref false in
    let unregister () =
      let wake =
        with_gate t (fun () ->
          let wake = remove_active entry cancel in
          registered := false;
          wake)
      in
      Option.iter wake ~f:(fun wake -> Eio.Promise.resolve wake ())
    in
    let operation () =
      let%bind result =
        try
          with_active entry (fun () ->
            let guarded_checkpoint () =
              (match check_actor connection actor with
               | Ok () -> ()
               | Error error -> raise (Revoked error));
              checkpoint ();
              match check_actor connection actor with
              | Ok () -> ()
              | Error error -> raise (Revoked error)
            in
            f entry ~checkpoint:guarded_checkpoint)
        with
        | Revoked error -> Error error
      in
      let%bind () = check_actor connection actor in
      let%bind () =
        if valid_negotiation entry then Ok () else Error Error.Stale_negotiation
      in
      let%bind () =
        with_gate t (fun () ->
          let%bind () = check_entry t entry in
          if Option.exists entry.negotiation ~f:(phys_equal negotiation)
          then Ok ()
          else Error Error.Stale_negotiation)
      in
      let%bind () = check_actor connection actor in
      let%bind () = final_check () in
      let%map response, wake =
        try_with_gate t (fun () ->
          let%bind () = check_entry t entry in
          let%bind () =
            if
              valid_negotiation entry
              && Option.exists entry.negotiation ~f:(phys_equal negotiation)
            then Ok ()
            else Error Error.Stale_negotiation
          in
          let%map response = commit result in
          committed := true;
          let wake =
            match after_commit with
            | None ->
              let wake = remove_active entry cancel in
              registered := false;
              wake
            | Some _ -> None
          in
          response, wake)
      in
      Option.iter wake ~f:(fun wake -> Eio.Promise.resolve wake ());
      Option.iter after_commit ~f:(fun settle ->
        Eio.Cancel.protect settle;
        unregister ());
      response
    in
    let outcome =
      try `Returned (operation ()) with
      | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let cleanup_error =
      if !registered
      then
        Eio.Cancel.protect (fun () ->
          let abort_error =
            try
              if not !committed then abort ();
              None
            with
            | exn -> Some (exn, Stdlib.Printexc.get_raw_backtrace ())
          in
          unregister ();
          abort_error)
      else None
    in
    match outcome, cleanup_error with
    | `Returned result, None -> result
    | `Returned _, Some (exn, backtrace) ->
      Stdlib.Printexc.raise_with_backtrace exn backtrace
    | `Raised (exn, backtrace), _ -> Stdlib.Printexc.raise_with_backtrace exn backtrace)
;;

let transition t contexts =
  t.project_cache <- Project.Project_cache.transition t.project_cache ~contexts;
  t.document_cache <- Document.Document_cache.transition t.document_cache ~contexts;
  t.analysis_index
  <- Authoring_analysis_index.transition
       t.analysis_index
       ~project:t.project_cache
       ~document:t.document_cache;
  t.provisional_index
  <- Authoring_provisional_index.transition t.provisional_index ~contexts;
  Option.iter t.document_capture ~f:(fun retained ->
    match Ledger.find_document contexts ~identity:retained.identity with
    | Some view
      when Captured.Id.equal
             retained.source_id
             (Captured.id (Ledger.Document.View.source view)) -> ()
    | Some _ | None -> t.document_capture <- None)
;;

module Current = struct
  type basis =
    | Project
    | Document

  type t =
    { owner : Ledger.t
    ; domain : int
    ; context : context_entry
    ; expected : P.Authoring.Context.Expected.t
    ; live : bool ref
    ; retained_document_capture : Retained_document_capture.t option
    ; borrowed_project_cache : Project.Project_cache.t
    ; borrowed_document_cache : Document.Document_cache.t
    ; borrowed_analysis_index : Authoring_analysis_index.t
    ; borrowed_provisional_index : Authoring_provisional_index.t
    }

  let basis t =
    match Ledger.basis t.owner ~identity:t.context.identity with
    | Some Ledger.Basis.Project -> Project
    | Some Ledger.Basis.Document | None -> Document
  ;;

  let expected t = t.expected

  let analysis_index t =
    if (not !(t.live)) || not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then Error Error.Stale_context
    else Ok t.borrowed_analysis_index
  ;;

  let provisional_index t =
    if (not !(t.live)) || not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then Error Error.Stale_context
    else Ok t.borrowed_provisional_index
  ;;

  let analysis_heads t =
    if (not !(t.live)) || not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then Error Error.Stale_context
    else
      Ok (t.borrowed_project_cache, t.borrowed_document_cache, t.borrowed_analysis_index)
  ;;

  let project_inputs t ~checkpoint =
    if (not !(t.live)) || not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then Error Error.Stale_context
    else
      Ledger.inputs t.owner ~identity:t.context.identity ~checkpoint
      |> Result.map_error ~f:(fun _ -> Error.Stale_context)
  ;;

  let document_inputs t ~checkpoint =
    if (not !(t.live)) || not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then Error Error.Stale_context
    else
      Ledger.document_inputs t.owner ~identity:t.context.identity ~checkpoint
      |> Result.map_error ~f:(fun _ -> Error.Stale_context)
  ;;

  let document_capture t ~checkpoint =
    let open Result.Let_syntax in
    let%map inputs = document_inputs t ~checkpoint in
    Option.bind t.retained_document_capture ~f:(fun retained ->
      if Retained_document_capture.matches_current retained ~context:t.context ~inputs
      then Some (Retained_document_capture.capture retained)
      else None)
  ;;

  let project_capture t ~checkpoint =
    let open Result.Let_syntax in
    let%map inputs = document_inputs t ~checkpoint in
    Option.bind t.retained_document_capture ~f:(fun retained ->
      if Retained_document_capture.matches_current retained ~context:t.context ~inputs
      then Retained_document_capture.project_capture retained
      else None)
  ;;

  let frozen_document_bundle t ~checkpoint =
    let open Result.Let_syntax in
    let%bind inputs = document_inputs t ~checkpoint in
    match t.retained_document_capture with
    | Some retained
      when Retained_document_capture.matches_import_owner
             retained
             ~context:t.context
             ~inputs ->
      Retained_document_capture.recompose_bundle retained ~inputs ~checkpoint
      |> Result.map ~f:Option.some
    | Some _ | None -> Ok None
  ;;
end

let close_owner = Activity.close_owner

let join_entry t entry =
  Eio.Cancel.protect (fun () ->
    let rec loop () =
      match with_gate t (fun () -> entry.idle) with
      | None -> ()
      | Some idle ->
        Eio.Promise.await idle;
        loop ()
    in
    loop ())
;;

let clear_contexts t entry =
  let contexts, retired_owners =
    with_gate t (fun () -> entry.contexts, entry.retired_owners)
  in
  List.iter contexts ~f:(fun context -> close_owner context.owner);
  List.iter retired_owners ~f:close_owner;
  with_gate t (fun () ->
    let open Result.Let_syntax in
    let%bind ledger =
      List.fold_result
        contexts
        ~init:(Project.Project_cache.contexts t.project_cache)
        ~f:(fun ledger context ->
          Ledger.close_context ledger ~identity:context.identity
          |> Result.map_error ~f:(fun _ -> Error.Stale_context))
    in
    transition t ledger;
    entry.contexts <- [];
    entry.retired_owners <- [];
    entry.roots <- [];
    Ok ())
;;

let close_connection_and_wait t connection =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match with_gate t (fun () -> find_connection t connection) with
  | None ->
    Connection_context.close_authoring connection;
    Ok ()
  | Some entry ->
    if in_active entry
    then Error Error.Busy
    else (
      let cancels =
        Connection_context.close_authoring connection;
        with_gate t (fun () ->
          entry.phase <- Closed;
          entry.active)
      in
      List.iter cancels ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
      with_lifecycle entry (fun () ->
        join_entry t entry;
        let open Result.Let_syntax in
        let%map () = clear_contexts t entry in
        with_gate t (fun () ->
          t.connections
          <- List.filter t.connections ~f:(fun current -> not (phys_equal current entry)))))
;;

let close_and_wait t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  let entries = with_gate t (fun () -> t.connections) in
  if List.exists entries ~f:in_active
  then Error Error.Busy
  else (
    let connections =
      with_gate t (fun () ->
        t.closed <- true;
        List.map t.connections ~f:(fun entry -> entry.connection))
    in
    let result =
      List.fold_result connections ~init:() ~f:(fun () connection ->
        close_connection_and_wait t connection)
    in
    ignore (Eio.Switch.try_remove_hook t.release_hook : bool);
    result)
;;

let create
      ~sw
      ~clock
      ~host
      ~sources
      ~upload
      ~supplied_document
      ~selection_policy
      ~context_limits
      ~analysis_policy
      ~protocol_limits
      ~(limits : Limits.t)
  =
  Eio.Switch.check sw;
  Option.iter upload ~f:(fun upload ->
    if not (P.Id.Server.equal host (Upload.host_id upload))
    then invalid_arg "authoring upload host does not match service host");
  let contexts = Ledger.create ~limits:context_limits in
  let analysis_index =
    Authoring_analysis_index.create
      ~owner_nonce:(P.Id.Transaction.create ())
      ~max_rows:(Project.Policy.max_results analysis_policy)
    |> Or_error.ok_exn
  in
  let provisional_index =
    Authoring_provisional_index.create
      ~owner_nonce:(P.Id.Transaction.create ())
      ~max_rows:limits.max_provisional_results
    |> Or_error.ok_exn
  in
  let service =
    { sw
    ; domain = (Stdlib.Domain.self () :> int)
    ; clock
    ; host
    ; sources
    ; upload
    ; supplied_document
    ; selection_policy
    ; analysis_policy
    ; protocol_limits
    ; limits
    ; gate = Eio.Mutex.create ()
    ; connections = []
    ; project_cache = Project.Project_cache.create ~contexts ~policy:analysis_policy
    ; document_cache = Document.Document_cache.create ~policy:analysis_policy
    ; document_capture = None
    ; analysis_index
    ; provisional_index
    ; closed = false
    ; release_hook = Eio.Switch.null_hook
    }
  in
  service.release_hook
  <- Eio.Switch.on_release_cancellable sw (fun () ->
       ignore (close_and_wait service : (unit, Error.t) result));
  service
;;

let publish_initialize_and_wait t ~connection ~response =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then Error Error.Closed
  else if
    (not (P.Id.Server.equal response.P.Initialize.Response.server_id t.host))
    || not (same_principal response.principal (Connection_context.principal connection))
  then Error Error.Foreign_connection
  else if Connection_context.authoring_closed connection
  then Error Error.Closed
  else
    let open Result.Let_syntax in
    let%bind entry =
      with_gate t (fun () ->
        if t.closed
        then Error Error.Closed
        else (
          match find_connection t connection with
          | Some entry -> Ok entry
          | None ->
            if List.length t.connections >= t.limits.max_connections
            then Error Error.Resource_limit
            else (
              let entry = Activity.create connection in
              t.connections <- entry :: t.connections;
              Ok entry)))
    in
    if in_active entry
    then Error Error.Busy
    else (
      let result =
        with_lifecycle entry (fun () ->
          let%bind cancels =
            if Connection_context.authoring_closed connection
            then Error Error.Closed
            else
              with_gate t (fun () ->
                if t.closed
                then Error Error.Closed
                else if
                  Option.exists (find_connection t connection) ~f:(phys_equal entry)
                  && phys_equal entry.connection connection
                  &&
                  match entry.phase with
                  | Active | Revoking -> true
                  | Closed -> false
                then (
                  entry.phase <- Revoking;
                  Ok entry.active)
                else Error Error.Closed)
          in
          List.iter cancels ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
          join_entry t entry;
          let%bind () = clear_contexts t entry in
          let%bind () =
            if
              Connection_context.try_mark_initialized
                ~version:response.selected_version
                ~enabled_features:response.enabled_features
                connection
            then Ok ()
            else Error Error.Closed
          in
          let negotiation = Connection_context.authoring_negotiation connection in
          with_gate t (fun () ->
            if
              t.closed
              || (match entry.phase with
                  | Closed -> true
                  | Active | Revoking -> false)
              || not (Option.exists (find_connection t connection) ~f:(phys_equal entry))
            then Error Error.Closed
            else (
              entry.negotiation <- negotiation;
              entry.phase <- Active;
              Ok ())))
      in
      match result with
      | Ok () -> Ok ()
      | Error _ as failure ->
        if Connection_context.authoring_closed connection
        then
          with_gate t (fun () ->
            if List.is_empty entry.contexts && List.is_empty entry.active
            then
              t.connections
              <- List.filter t.connections ~f:(fun current ->
                   not (phys_equal current entry)));
        failure)
;;
