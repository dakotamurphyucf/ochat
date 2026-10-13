open Core
module Sources = Project_source_host
module Roots = Chatml_local_roots
module Local = Chatml_local_source
module Project = Chatml_project_prepare
module Build = Chatml_project_build
module Captured = Chatml_source
module Path = Source.Portable_path
module Actor = Operator_authorization

module Read_policy = struct
  type t = { authorize : Actor.t -> root_index:int -> bool }

  let create ~authorize = { authorize }
  let deny_all = { authorize = (fun _ ~root_index:_ -> false) }
end

module Document_policy = struct
  type t = { authorize : Actor.t -> bool }

  let create ~authorize = { authorize }
  let deny_all = { authorize = (fun _ -> false) }
end

module Limits = struct
  type t =
    { max_contexts : int
    ; max_context_requests : int
    ; max_manifest_entries : int
    ; max_browse_entries : int
    ; max_read_bytes : int
    ; max_context_read_bytes : int
    }

  let create
        ~max_contexts
        ~max_context_requests
        ~max_manifest_entries
        ~max_browse_entries
        ~max_read_bytes
        ~max_context_read_bytes
    =
    if
      List.exists
        [ max_contexts
        ; max_context_requests
        ; max_manifest_entries
        ; max_browse_entries
        ; max_read_bytes
        ; max_context_read_bytes
        ]
        ~f:(fun value -> value <= 0)
    then Or_error.error_string "invalid authoring source host limit"
    else
      Ok
        { max_contexts
        ; max_context_requests
        ; max_manifest_entries
        ; max_browse_entries
        ; max_read_bytes
        ; max_context_read_bytes
        }
  ;;
end

module Error = struct
  type t =
    | Unauthorized
    | Expired_actor
    | Foreign_root
    | Closed
    | Busy
    | Foreign_domain
    | Context_limit
    | Result_limit
    | Unsupported_source_selection
    | Sources of Sources.Error.t
    | Root of Roots.Error.t
    | Local of Local.Error.t
    | Project of Project.Error.t
    | Source_capture of Core.Error.t

  let sexp_of_t = function
    | Unauthorized -> Sexp.Atom "Unauthorized"
    | Expired_actor -> Sexp.Atom "Expired_actor"
    | Foreign_root -> Sexp.Atom "Foreign_root"
    | Closed -> Sexp.Atom "Closed"
    | Busy -> Sexp.Atom "Busy"
    | Foreign_domain -> Sexp.Atom "Foreign_domain"
    | Context_limit -> Sexp.Atom "Context_limit"
    | Result_limit -> Sexp.Atom "Result_limit"
    | Unsupported_source_selection -> Sexp.Atom "Unsupported_source_selection"
    | Sources error -> [%sexp "Sources", (error : Sources.Error.t)]
    | Root error -> [%sexp "Root", (Roots.Error.code error : Roots.Error.code)]
    | Local error -> [%sexp "Local", (error : Local.Error.t)]
    | Project _ -> Sexp.Atom "Project"
    | Source_capture _ -> Sexp.Atom "Source_capture"
  ;;
end

let domain () = (Stdlib.Domain.self () :> int)

module Root = struct
  type t =
    { host_identity : unit ref
    ; original_actor : Actor.t
    ; index : int
    }

  let index t = t.index
end

module Capture = struct
  type t = { build : Build.t }

  let build t = t.build
  let prepared t = Build.prepared t.build
  let fingerprint t = Chatml_prepared_program.fingerprint (prepared t)
  let sources t = Chatml_prepared_program.sources (prepared t)
end

module Entry = struct
  type t = { member : Path.t }

  let member t = t.member
end

module Document_capture = struct
  type t = { source : Captured.t }

  let source t = t.source
end

type grant =
  | Roots of Root.t list
  | Supplied

type t =
  { domain : int
  ; source_host : Sources.t
  ; read_policy : Read_policy.t
  ; document_policy : Document_policy.t
  ; limits : Limits.t
  ; identity : unit ref
  ; mutable closed : bool
  ; mutable contexts : context list
  ; mutable roots_busy : bool
  ; mutable roots_cancel : Eio.Cancel.t option
  ; roots_idle : Eio.Condition.t
  ; mutable release_hook : Eio.Switch.hook
  }

and context =
  { host : t
  ; original_actor : Actor.t
  ; grant : grant
  ; mutable closed : bool
  ; mutable busy : bool
  ; mutable attempts : int
  ; mutable read_bytes : int
  ; mutable cancel : Eio.Cancel.t option
  ; idle : Eio.Condition.t
  }

type project_context =
  { common : context
  ; root : Root.t
  ; dependency_roots : Root.t list
  ; project : Path.t
  ; selection : Chatml_project.Selection.request
  ; contract : Chatml.Chatml_compiler_contract.t
  }

type document_context =
  { common : context
  ; root : Root.t option
  ; member : Path.t
  ; source_id : Captured.Id.t
  }

type host = t

type active_operation =
  | Root_enumeration of host
  | Source_context of context

let active_operations : active_operation list Eio.Fiber.key = Eio.Fiber.create_key ()

let with_active operation f =
  let stack = Option.value (Eio.Fiber.get active_operations) ~default:[] in
  Eio.Fiber.with_binding active_operations (operation :: stack) f
;;

let same_principal a b =
  Agent_protocol.Id.Principal.equal (Actor.principal a).id (Actor.principal b).id
;;

let check_root host ~actor root =
  if not (phys_equal host.identity root.Root.host_identity)
  then Error Error.Foreign_root
  else if (not (Actor.is_current root.original_actor)) || not (Actor.is_current actor)
  then Error Error.Expired_actor
  else if not (same_principal root.original_actor actor)
  then Error Error.Unauthorized
  else if
    (not (host.read_policy.authorize root.original_actor ~root_index:root.index))
    || not (host.read_policy.authorize actor ~root_index:root.index)
  then Error Error.Unauthorized
  else Ok ()
;;

let check_host host =
  if not (Int.equal host.domain (domain ()))
  then Error Error.Foreign_domain
  else if host.closed
  then Error Error.Closed
  else Ok ()
;;

let remove_context host context =
  host.contexts
  <- List.filter host.contexts ~f:(fun current -> not (phys_equal current context))
;;

let reject_self_join context =
  let stack = Option.value (Eio.Fiber.get active_operations) ~default:[] in
  if
    List.exists stack ~f:(function
      | Source_context active -> phys_equal active context
      | Root_enumeration _ -> false)
  then invalid_arg "Authoring_source_host: cannot close own active operation"
;;

let reject_host_self_join host =
  let stack = Option.value (Eio.Fiber.get active_operations) ~default:[] in
  if
    List.exists stack ~f:(function
      | Root_enumeration active -> phys_equal active host
      | Source_context active -> phys_equal active.host host)
  then invalid_arg "Authoring_source_host: cannot close own active operation"
;;

let close_context_and_wait context =
  if not (Int.equal context.host.domain (domain ()))
  then invalid_arg "Authoring_source_host.Context: foreign domain";
  reject_self_join context;
  context.closed <- true;
  Option.iter context.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
  Eio.Cancel.protect (fun () ->
    while context.busy do
      Eio.Condition.await_no_mutex context.idle
    done);
  remove_context context.host context
;;

let close_all_and_wait host =
  if not (Int.equal host.domain (domain ()))
  then invalid_arg "Authoring_source_host: foreign domain";
  reject_host_self_join host;
  List.iter host.contexts ~f:reject_self_join;
  host.closed <- true;
  Option.iter host.roots_cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
  List.iter host.contexts ~f:(fun context ->
    Option.iter context.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit));
  Eio.Cancel.protect (fun () ->
    while host.roots_busy do
      Eio.Condition.await_no_mutex host.roots_idle
    done);
  List.iter (List.rev host.contexts) ~f:close_context_and_wait;
  ignore (Eio.Switch.try_remove_hook host.release_hook : bool)
;;

let create source_host ~sw ~read_policy ~document_policy ~limits =
  Eio.Switch.check sw;
  let host =
    { domain = domain ()
    ; source_host
    ; read_policy
    ; document_policy
    ; limits
    ; identity = ref ()
    ; closed = false
    ; contexts = []
    ; roots_busy = false
    ; roots_cancel = None
    ; roots_idle = Eio.Condition.create ()
    ; release_hook = Eio.Switch.null_hook
    }
  in
  host.release_hook
  <- Eio.Switch.on_release_cancellable sw (fun () -> close_all_and_wait host);
  host
;;

let roots host ~actor ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = check_host host in
  if not (Actor.is_current actor)
  then Error Error.Expired_actor
  else if host.roots_busy
  then Error Error.Busy
  else (
    host.roots_busy <- true;
    Exn.protect
      ~finally:(fun () ->
        host.roots_cancel <- None;
        host.roots_busy <- false;
        Eio.Condition.broadcast host.roots_idle)
      ~f:(fun () ->
        with_active (Root_enumeration host) (fun () ->
          Eio.Cancel.sub (fun cancel ->
            host.roots_cancel <- Some cancel;
            Sources.with_authoring
              host.source_host
              ~f:(fun ~roots ~policy:_ ~limits:_ ~max_request_bytes:_ ->
                List.filter_mapi roots ~f:(fun index _ ->
                  checkpoint ();
                  if
                    Actor.is_current actor
                    && host.read_policy.authorize actor ~root_index:index
                  then
                    Some
                      { Root.host_identity = host.identity
                      ; original_actor = actor
                      ; index
                      }
                  else None))
            |> Result.map_error ~f:(fun error -> Error.Sources error)
            |> Result.bind ~f:(fun selected ->
              let open Result.Let_syntax in
              let%bind () = check_host host in
              if not (Actor.is_current actor)
              then Error Error.Expired_actor
              else
                List.fold_result selected ~init:() ~f:(fun () root ->
                  check_root host ~actor root)
                |> Result.map ~f:(fun () -> selected))))))
;;

let check_all context actor =
  let open Result.Let_syntax in
  let%bind () = check_host context.host in
  let%bind () = if context.closed then Error Error.Closed else Ok () in
  let%bind () =
    if (not (Actor.is_current context.original_actor)) || not (Actor.is_current actor)
    then Error Error.Expired_actor
    else if not (same_principal context.original_actor actor)
    then Error Error.Unauthorized
    else Ok ()
  in
  match context.grant with
  | Roots roots ->
    List.fold_result roots ~init:() ~f:(fun () root ->
      let%bind () = check_root context.host ~actor:context.original_actor root in
      check_root context.host ~actor root)
  | Supplied ->
    if
      context.host.document_policy.authorize context.original_actor
      && context.host.document_policy.authorize actor
    then Ok ()
    else Error Error.Unauthorized
;;

let make_context host ~actor ~grant =
  let open Result.Let_syntax in
  let%bind () = check_host host in
  let context =
    { host
    ; original_actor = actor
    ; grant
    ; closed = false
    ; busy = false
    ; attempts = 0
    ; read_bytes = 0
    ; cancel = None
    ; idle = Eio.Condition.create ()
    }
  in
  let%bind () = check_all context actor in
  if List.length host.contexts >= host.limits.max_contexts
  then Error Error.Context_limit
  else (
    host.contexts <- context :: host.contexts;
    Ok context)
;;

exception Revoked of Error.t

let with_operation context ~actor f =
  let open Result.Let_syntax in
  let%bind () = check_all context actor in
  if context.busy
  then Error Error.Busy
  else if context.attempts >= context.host.limits.max_context_requests
  then Error Error.Result_limit
  else (
    context.attempts <- context.attempts + 1;
    context.busy <- true;
    let poll checkpoint () =
      (match check_all context actor with
       | Ok () -> ()
       | Error error -> raise (Revoked error));
      checkpoint ();
      match check_all context actor with
      | Ok () -> ()
      | Error error -> raise (Revoked error)
    in
    try
      Exn.protect
        ~finally:(fun () ->
          context.cancel <- None;
          context.busy <- false;
          Eio.Condition.broadcast context.idle)
        ~f:(fun () ->
          with_active (Source_context context) (fun () ->
            Eio.Cancel.sub (fun cancel ->
              context.cancel <- Some cancel;
              let result = f (poll : (unit -> unit) -> unit -> unit) in
              match check_all context actor with
              | Ok () -> result
              | Error error -> Error error)))
    with
    | Revoked error -> Error error)
;;

let selected_anchors all selected =
  Result.all
    (List.map selected ~f:(fun root ->
       match List.nth all root.Root.index with
       | None -> Error Error.Foreign_root
       | Some pair -> Ok pair))
;;

let with_local_root context root ~actor ~checkpoint f =
  with_operation context ~actor (fun poll ->
    let poll = poll checkpoint in
    Sources.with_authoring
      context.host.source_host
      ~f:(fun ~roots ~policy ~limits:_ ~max_request_bytes:_ ->
        let open Result.Let_syntax in
        poll ();
        let%bind selected = selected_anchors roots [ root ] in
        let prefix, anchor = List.hd_exn selected in
        Roots.with_anchors
          [ anchor ]
          ~limits:(Project.Policy.root_limits policy)
          ~checkpoint:poll
          ~f:(fun roots ->
            match Roots.resolve roots ~base:None ~locator:prefix ~checkpoint:poll with
            | Error error -> Error (Error.Root error)
            | Ok cursor -> f (Roots.Cursor.owner cursor) ~policy ~poll)
        |> Result.map_error ~f:(function
          | `Roots error -> Error.Root error
          | `Callback error -> error))
    |> Result.map_error ~f:(fun error -> Error.Sources error)
    |> Result.bind ~f:Fn.id)
;;

let list_directory_on_root context root ~actor ~directory ~checkpoint =
  with_local_root context root ~actor ~checkpoint (fun owner ~policy ~poll ->
    let maximum =
      Int.min
        context.host.limits.max_browse_entries
        (Chatml_local_capture.Limits.directory_entries
           (Project.Policy.local_limits policy))
    in
    let entries =
      Local.fold_directory
        owner
        ~directory
        ~max_entries:maximum
        ~init:[]
        ~checkpoint:poll
        ~f:(fun reversed ~name ->
          let member =
            match directory with
            | None -> name
            | Some directory -> Path.to_string directory ^ "/" ^ name
          in
          match Path.create member with
          | Error _ -> Ok reversed
          | Ok member -> Ok ({ Entry.member } :: reversed))
    in
    let open Result.Let_syntax in
    let%map entries, _ =
      Result.map_error entries ~f:(function
        | `Filesystem error -> Error.Local error
        | `Callback error -> error)
    in
    List.sort (List.rev entries) ~compare:(fun a b -> Path.compare a.member b.member))
;;

let read_local context owner ~policy ~member ~poll =
  let remaining = context.host.limits.max_context_read_bytes - context.read_bytes in
  if remaining <= 0
  then Error Error.Result_limit
  else (
    let maximum =
      Int.min
        (Int.min context.host.limits.max_read_bytes remaining)
        (Chatml_local_capture.Limits.source_bytes (Project.Policy.local_limits policy))
    in
    let open Result.Let_syntax in
    let%map contents =
      Local.read owner ~member ~max_bytes:maximum ~checkpoint:poll
      |> Result.map_error ~f:(fun error -> Error.Local error)
    in
    context.read_bytes <- context.read_bytes + String.length contents;
    contents)
;;

let read_member_on_root context root ~actor ~member ~checkpoint =
  with_local_root context root ~actor ~checkpoint (fun owner ~policy ~poll ->
    read_local context owner ~policy ~member ~poll)
;;

module Context = struct
  type t = project_context

  let root (t : t) = t.root
  let check_current (t : t) ~actor = check_all t.common actor

  let open_root host ~actor ~root ~dependency_roots ~project ~selection ~contract =
    let open Result.Let_syntax in
    let selected = root :: dependency_roots in
    let rec validate count seen = function
      | [] -> Ok ()
      | candidate :: rest ->
        if count >= Project_source_configuration.maximum_external_roots
        then Error Error.Result_limit
        else if Set.mem seen candidate.Root.index
        then Error Error.Foreign_root
        else validate (count + 1) (Set.add seen candidate.index) rest
    in
    let%bind () = validate 0 Int.Set.empty selected in
    let%map common = make_context host ~actor ~grant:(Roots selected) in
    { common; root; dependency_roots; project; selection; contract }
  ;;

  let capture (t : t) ~actor ~clock ~checkpoint =
    with_operation t.common ~actor (fun poll ->
      let poll = poll checkpoint in
      Sources.with_authoring
        t.common.host.source_host
        ~f:(fun ~roots ~policy ~limits ~max_request_bytes ->
          let open Result.Let_syntax in
          poll ();
          let%bind selected = selected_anchors roots (t.root :: t.dependency_roots) in
          let root_prefix, _ = List.hd_exn selected in
          let anchors = List.map selected ~f:snd in
          let%bind source_host =
            Project.Host.create
              anchors
              ~git_store:None
              ~git_grants:[]
              ~max_grants:0
              ~max_grant_bytes:0
            |> Result.map_error ~f:(fun _ -> Error.Unsupported_source_selection)
          in
          let%bind lock_id =
            Captured.Id.create
              ~package_namespace:"@ochat.authoring.lock"
              ~relative_path:"authoring.lock"
            |> Result.map_error ~f:(fun _ -> Error.Unsupported_source_selection)
          in
          let%bind request =
            Project.Request.create
              (Project.Root.Local { locator = root_prefix; project = t.project })
              ~operation:Build.Operation.Build_without_previous
              ~selection:t.selection
              ~entry:Build.Entry.Declared
              ~contract:t.contract
              ~lock_id
              ~max_request_bytes
            |> Result.map_error ~f:(fun _ -> Error.Unsupported_source_selection)
          in
          let%bind batch =
            Project.prepare_sources
              source_host
              [ request ]
              ~policy
              ~limits
              ~clock
              ~checkpoint:poll
            |> Result.map_error ~f:(fun error -> Error.Project error)
          in
          let%bind build =
            match Project.Source_batch.preparations batch with
            | [ prepared ] -> Ok (Project.Source_preparation.build prepared)
            | _ -> Error Error.Unsupported_source_selection
          in
          let capture = { Capture.build } in
          if
            List.length (Capture.sources capture)
            > t.common.host.limits.max_manifest_entries
          then Error Error.Result_limit
          else (
            poll ();
            Ok capture))
      |> Result.map_error ~f:(fun error -> Error.Sources error)
      |> Result.bind ~f:Fn.id)
  ;;

  let list_directory (t : t) ~actor ~directory ~checkpoint =
    list_directory_on_root t.common t.root ~actor ~directory ~checkpoint
  ;;

  let read_member (t : t) ~actor ~member ~checkpoint =
    read_member_on_root t.common t.root ~actor ~member ~checkpoint
  ;;

  let close_and_wait (t : t) = close_context_and_wait t.common
end

module Document_context = struct
  type t = document_context

  module Origin = struct
    type t =
      | Host_member
      | Supplied
    [@@deriving equal, sexp_of]
  end

  let origin t = if Option.is_some t.root then Origin.Host_member else Origin.Supplied
  let source_id t = t.source_id
  let check_current t ~actor = check_all t.common actor

  let mint_source_id member =
    let nonce = Agent_protocol.Id.Transaction.(create () |> to_string) in
    Captured.Id.create
      ~package_namespace:("@ochat.authoring.document." ^ nonce)
      ~relative_path:(Path.to_string member)
    |> Result.map_error ~f:(fun error -> Error.Source_capture error)
  ;;

  let close_failed common f =
    match f () with
    | Ok capture -> Ok capture
    | Error error ->
      close_context_and_wait common;
      Error error
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      close_context_and_wait common;
      Exn.raise_with_original_backtrace exn backtrace
  ;;

  let capture_host t ~actor ~checkpoint =
    match t.root with
    | None -> Error Error.Unsupported_source_selection
    | Some root ->
      with_local_root t.common root ~actor ~checkpoint (fun owner ~policy ~poll ->
        let open Result.Let_syntax in
        let%bind contents = read_local t.common owner ~policy ~member:t.member ~poll in
        let%map source =
          Captured.create
            t.source_id
            ~contents
            ~expected_digest:None
            ~max_bytes:(Some t.common.host.limits.max_read_bytes)
            ~checkpoint:poll
          |> Result.map_error ~f:(fun error -> Error.Source_capture error)
        in
        { Document_capture.source })
  ;;

  let open_host host ~actor ~root ~member ~checkpoint =
    let open Result.Let_syntax in
    let%bind source_id = mint_source_id member in
    let%bind common = make_context host ~actor ~grant:(Roots [ root ]) in
    let document = { common; root = Some root; member; source_id } in
    let%map capture =
      close_failed common (fun () -> capture_host document ~actor ~checkpoint)
    in
    document, capture
  ;;

  let open_supplied host ~actor ~member ~contents ~checkpoint =
    let open Result.Let_syntax in
    let%bind source_id = mint_source_id member in
    let%bind common = make_context host ~actor ~grant:Supplied in
    let document = { common; root = None; member; source_id } in
    let%map capture =
      close_failed common (fun () ->
        with_operation common ~actor (fun poll ->
          let poll = poll checkpoint in
          let bytes = String.length contents in
          if
            bytes > host.limits.max_read_bytes
            || bytes > host.limits.max_context_read_bytes
          then Error Error.Result_limit
          else (
            let%map source =
              Captured.create
                source_id
                ~contents
                ~expected_digest:None
                ~max_bytes:(Some host.limits.max_read_bytes)
                ~checkpoint:poll
              |> Result.map_error ~f:(fun error -> Error.Source_capture error)
            in
            common.read_bytes <- bytes;
            { Document_capture.source })))
    in
    document, capture
  ;;

  let refresh t ~actor ~checkpoint =
    let open Result.Let_syntax in
    let%bind () = check_current t ~actor in
    capture_host t ~actor ~checkpoint
  ;;

  let list_directory t ~actor ~directory ~checkpoint =
    let open Result.Let_syntax in
    let%bind () = check_current t ~actor in
    match t.root with
    | None -> Error Error.Unsupported_source_selection
    | Some root -> list_directory_on_root t.common root ~actor ~directory ~checkpoint
  ;;

  let close_and_wait t = close_context_and_wait t.common
end

let close_and_wait = close_all_and_wait
