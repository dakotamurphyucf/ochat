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
    | Document_parser of Prompt.Chat_markdown.Generated.error
    | Document_bundle_limit
    | Project_declaration_unavailable
    | Invalid_project_declaration
    | Selected_script_unavailable

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
    | Document_parser error ->
      [%sexp "Document_parser", (error : Prompt.Chat_markdown.Generated.error)]
    | Document_bundle_limit -> Sexp.Atom "Document_bundle_limit"
    | Project_declaration_unavailable -> Sexp.Atom "Project_declaration_unavailable"
    | Invalid_project_declaration -> Sexp.Atom "Invalid_project_declaration"
    | Selected_script_unavailable -> Sexp.Atom "Selected_script_unavailable"
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

let check_root_current host ~actor root =
  let open Result.Let_syntax in
  let%bind () = check_host host in
  check_root host ~actor root
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

module Retained_owner = struct
  type t = { common : context }

  let check_current t ~actor = check_all t.common actor
  let close_and_wait t = close_context_and_wait t.common
end

let retain_owner context ~actor =
  let open Result.Let_syntax in
  let%bind () = check_all context actor in
  let%map common =
    make_context context.host ~actor:context.original_actor ~grant:context.grant
  in
  { Retained_owner.common }
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

let list_directory_with_owner host owner ~policy ~directory ~poll =
  let maximum =
    Int.min
      host.limits.max_browse_entries
      (Chatml_local_capture.Limits.directory_entries (Project.Policy.local_limits policy))
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
  List.sort (List.rev entries) ~compare:(fun a b -> Path.compare a.member b.member)
;;

let list_directory_on_root context root ~actor ~directory ~checkpoint =
  with_local_root context root ~actor ~checkpoint (fun owner ~policy ~poll ->
    list_directory_with_owner context.host owner ~policy ~directory ~poll)
;;

let read_local ?max_bytes context owner ~policy ~member ~poll =
  let remaining = context.host.limits.max_context_read_bytes - context.read_bytes in
  if remaining <= 0
  then Error Error.Result_limit
  else (
    let maximum =
      Int.min
        (Int.min context.host.limits.max_read_bytes remaining)
        (Chatml_local_capture.Limits.source_bytes (Project.Policy.local_limits policy))
    in
    let maximum = Option.value_map max_bytes ~default:maximum ~f:(Int.min maximum) in
    if maximum <= 0
    then Error Error.Result_limit
    else
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

let prepare_local_project
      ~selected
      ~project
      ~selection
      ~entry
      ~contract
      ~policy
      ~limits
      ~max_request_bytes
      ~clock
      ~poll
  =
  let open Result.Let_syntax in
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
    Project.Request.create_with_entry
      (Project.Root.Local { locator = root_prefix; project })
      ~operation:Build.Operation.Build_without_previous
      ~selection
      ~entry
      ~contract
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
  match Project.Source_batch.preparations batch with
  | [ prepared ] -> Ok (Project.Source_preparation.build prepared)
  | _ -> Error Error.Unsupported_source_selection
;;

module Context = struct
  type t = project_context

  let root (t : t) = t.root
  let contract (t : t) = t.contract
  let check_current (t : t) ~actor = check_all t.common actor
  let retain_owner (t : t) ~actor = retain_owner t.common ~actor

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
          let%bind build =
            prepare_local_project
              ~selected
              ~project:t.project
              ~selection:t.selection
              ~entry:Project.Request.Entry.Declared
              ~contract:t.contract
              ~policy
              ~limits
              ~max_request_bytes
              ~clock
              ~poll
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

  let equal_bytes_bounded left right ~poll =
    if String.length left <> String.length right
    then false
    else (
      let rec visit offset =
        poll ();
        if offset >= String.length left
        then true
        else (
          let finish = Int.min (offset + 4096) (String.length left) in
          let rec compare byte =
            if byte >= finish
            then visit finish
            else if Char.equal left.[byte] right.[byte]
            then compare (byte + 1)
            else false
          in
          compare offset)
      in
      visit 0)
  ;;

  let bind_parser_inputs
        ~source
        ~bundle
        ~parsed
        ~reuse_imports
        ~strict_reuse
        ~max_source_bytes
        ~poll
    =
    let open Result.Let_syntax in
    let source_id = Captured.id source in
    let member_contents =
      Chatmd_source_bundle.sources bundle |> String.Map.of_alist_exn
    in
    let generated = Prompt.Chat_markdown.Generated.parsed parsed in
    let parsed_units = generated.parsed_root :: List.map generated.parsed_agents ~f:snd in
    let source_cache =
      ref (Map.set reuse_imports ~key:(Captured.Id.relative_path source_id) ~data:source)
    in
    let rec visit_units count compared_bytes reversed = function
      | [] -> Ok (List.rev reversed)
      | unit :: remaining ->
        visit_inputs
          count
          compared_bytes
          reversed
          (Prompt.Chat_markdown.Parsed.inputs unit)
          remaining
    and visit_inputs count compared_bytes reversed inputs remaining_units =
      match inputs with
      | [] -> visit_units count compared_bytes reversed remaining_units
      | input :: rest ->
        poll ();
        let input_bytes = Chatmd_parser_input.authored_byte_length input in
        if count >= 1024 || input_bytes > (8 * 1024 * 1024) - compared_bytes
        then Error Error.Document_bundle_limit
        else (
          let member = Chatmd_parser_input.path input in
          let%bind contents =
            match Map.find member_contents member with
            | Some contents -> Ok contents
            | None -> Error Error.Document_bundle_limit
          in
          if not (Chatmd_parser_input.authored_matches input contents ~checkpoint:poll)
          then Error Error.Document_bundle_limit
          else (
            let%bind captured =
              match Map.find !source_cache member with
              | Some captured -> Ok captured
              | None when strict_reuse -> Error Error.Document_bundle_limit
              | None ->
                let%bind id =
                  Captured.Id.create
                    ~package_namespace:(Captured.Id.package_namespace source_id)
                    ~relative_path:member
                  |> Result.map_error ~f:(fun error -> Error.Source_capture error)
                in
                let%map captured =
                  Captured.create
                    id
                    ~contents
                    ~expected_digest:None
                    ~max_bytes:(Some max_source_bytes)
                    ~checkpoint:poll
                  |> Result.map_error ~f:(fun error -> Error.Source_capture error)
                in
                source_cache := Map.set !source_cache ~key:member ~data:captured;
                captured
            in
            visit_inputs
              (count + 1)
              (compared_bytes + input_bytes)
              ((input, captured) :: reversed)
              rest
              remaining_units))
    in
    visit_units 0 0 [] parsed_units
  ;;

  module Parsed_capture = struct
    type t =
      { source : Captured.t
      ; bundle : Chatmd_source_bundle.t
      ; parsed : Prompt.Chat_markdown.Generated.receipt
      ; input_sources : (Chatmd_parser_input.t * Captured.t) list
      }

    let source t = t.source
    let bundle t = t.bundle
    let parsed t = t.parsed
    let input_sources t = t.input_sources

    let script_source
          t
          ~declaration
          ~max_script_bytes
          ~max_locator_bytes
          ~max_path_components
          ~checkpoint
      =
      Authoring_document_script_route.admit_source
        ~parsed:t.parsed
        ~input_sources:t.input_sources
        ~bundle:t.bundle
        ~declaration
        ~root_source:t.source
        ~max_script_bytes
        ~max_locator_bytes
        ~max_path_components
        ~checkpoint
      |> Result.map ~f:Authoring_document_script_route.Source_only.source
      |> Result.map_error ~f:(function
        | Limit -> Error.Result_limit
        | Foreign_declaration
        | Invalid_project_route
        | Missing_external_source
        | Conflicting_external_source -> Error.Selected_script_unavailable)
    ;;

    let parse_root_only ~(limits : Chatmd_source_bundle.limits) ~source ~dir ~checkpoint =
      let open Result.Let_syntax in
      let defaults = Chatmd_source_bundle.default_limits in
      let root_file = Captured.Id.relative_path (Captured.id source) in
      let bytes = Captured.byte_length source in
      if
        limits.max_source_bytes <= 0
        || limits.max_bundle_bytes <= 0
        || limits.max_files <= 0
        || limits.max_source_bytes > defaults.max_source_bytes
        || limits.max_bundle_bytes > defaults.max_bundle_bytes
        || limits.max_files > defaults.max_files
        || bytes > limits.max_source_bytes
        || bytes > limits.max_bundle_bytes
      then Error Error.Document_bundle_limit
      else (
        checkpoint ();
        let%bind bundle =
          Chatmd_source_bundle.create
            ~limits
            ~checkpoint
            ~root_file
            ~sources:[ root_file, Captured.contents source ]
            ()
          |> Result.map_error ~f:(fun _ -> Error.Document_bundle_limit)
        in
        let%bind parsed =
          Prompt.Chat_markdown.Generated.parse_source_bundle
            ~dir
            bundle
            ~checkpoint
            ~before_read:(fun ~member:_ ~byte_length:_ -> checkpoint ())
          |> Result.map_error ~f:(fun error -> Error.Document_parser error)
        in
        let%map input_sources =
          bind_parser_inputs
            ~source
            ~bundle
            ~parsed
            ~reuse_imports:String.Map.empty
            ~strict_reuse:false
            ~max_source_bytes:limits.max_source_bytes
            ~poll:checkpoint
        in
        { source; bundle; parsed; input_sources })
    ;;

    let reparse_frozen
          ~previous
          ~source
          ~bundle
          ~(limits : Chatmd_source_bundle.limits)
          ~dir
          ~checkpoint
      =
      let open Result.Let_syntax in
      let defaults = Chatmd_source_bundle.default_limits in
      let source_id = Captured.id source in
      let root_file = Captured.Id.relative_path source_id in
      if
        limits.max_source_bytes <= 0
        || limits.max_bundle_bytes <= 0
        || limits.max_files <= 0
        || limits.max_source_bytes > defaults.max_source_bytes
        || limits.max_bundle_bytes > defaults.max_bundle_bytes
        || limits.max_files > defaults.max_files
        || (not (Captured.Id.equal source_id (Captured.id previous.source)))
        || (not (String.equal root_file (Chatmd_source_bundle.root_file previous.bundle)))
        || (not (String.equal root_file (Chatmd_source_bundle.root_file bundle)))
        || Captured.byte_length source > limits.max_source_bytes
      then Error Error.Document_bundle_limit
      else (
        let%bind bundle =
          Chatmd_source_bundle.create
            ~limits
            ~checkpoint
            ~root_file
            ~sources:(Chatmd_source_bundle.sources bundle)
            ()
          |> Result.map_error ~f:(fun _ -> Error.Document_bundle_limit)
        in
        let prior =
          Chatmd_source_bundle.sources previous.bundle |> String.Map.of_alist_exn
        in
        let current = Chatmd_source_bundle.sources bundle in
        if Map.length prior <> List.length current
        then Error Error.Document_bundle_limit
        else (
          let%bind () =
            List.fold_result current ~init:() ~f:(fun () (member, bytes) ->
              checkpoint ();
              match Map.find prior member with
              | None -> Error Error.Document_bundle_limit
              | Some _ when String.equal member root_file ->
                if equal_bytes_bounded bytes (Captured.contents source) ~poll:checkpoint
                then Ok ()
                else Error Error.Document_bundle_limit
              | Some old_bytes ->
                if equal_bytes_bounded bytes old_bytes ~poll:checkpoint
                then Ok ()
                else Error Error.Document_bundle_limit)
          in
          let%bind reuse_imports =
            List.fold_result
              previous.input_sources
              ~init:String.Map.empty
              ~f:(fun retained (input, captured) ->
                checkpoint ();
                let member = Chatmd_parser_input.path input in
                if String.equal member root_file
                then Ok retained
                else (
                  let id = Captured.id captured in
                  if
                    (not
                       (String.equal
                          (Captured.Id.package_namespace id)
                          (Captured.Id.package_namespace source_id)))
                    || not (String.equal (Captured.Id.relative_path id) member)
                  then Error Error.Document_bundle_limit
                  else (
                    match Map.find prior member with
                    | None -> Error Error.Document_bundle_limit
                    | Some bytes ->
                      if
                        not
                          (equal_bytes_bounded
                             bytes
                             (Captured.contents captured)
                             ~poll:checkpoint)
                      then Error Error.Document_bundle_limit
                      else (
                        match Map.find retained member with
                        | Some prior_captured
                          when (not
                                  (Captured.Id.equal
                                     (Captured.id prior_captured)
                                     (Captured.id captured)))
                               || not
                                    (Captured.Digest.equal
                                       (Captured.digest prior_captured)
                                       (Captured.digest captured)) ->
                          Error Error.Document_bundle_limit
                        | Some _ -> Ok retained
                        | None -> Ok (Map.set retained ~key:member ~data:captured)))))
          in
          let%bind parsed =
            Prompt.Chat_markdown.Generated.parse_source_bundle
              ~dir
              bundle
              ~checkpoint
              ~before_read:(fun ~member:_ ~byte_length:_ -> checkpoint ())
            |> Result.map_error ~f:(fun error -> Error.Document_parser error)
          in
          let%map input_sources =
            bind_parser_inputs
              ~source
              ~bundle
              ~parsed
              ~reuse_imports
              ~strict_reuse:true
              ~max_source_bytes:limits.max_source_bytes
              ~poll:checkpoint
          in
          { source; bundle; parsed; input_sources }))
    ;;
  end

  module Project_capture = struct
    type t =
      { source : Captured.t
      ; project : Chatml_project.t
      ; root : Root.t
      ; issuer : Project_source_host.Identity.t
      ; member : Path.t
      ; parsed : Parsed_capture.t
      ; declaration : Prompt.Chat_markdown.Declaration.t
      ; identity : Captured.Digest.t
      }

    let source t = t.source
    let project t = t.project
    let member t = t.member
    let identity t = t.identity

    let belongs_to_root t candidate =
      phys_equal t.root.host_identity candidate.Root.host_identity
      && Int.equal t.root.index candidate.index
      && same_principal t.root.original_actor candidate.original_actor
    ;;

    let root_index_if_bound t ~issuer ~original_actor =
      if
        Project_source_host.Identity.equal t.issuer issuer
        && same_principal t.root.original_actor original_actor
        && String.equal
             (Actor.principal t.root.original_actor).authentication_kind
             (Actor.principal original_actor).authentication_kind
      then Some t.root.index
      else None
    ;;

    let matches t ~parsed ~declaration =
      phys_equal t.parsed parsed && phys_equal t.declaration declaration
    ;;

    let matches_project_contents t ~build ~max_compare_bytes ~checkpoint =
      Authoring_project_declaration.matches_project_contents
        ~source:t.source
        ~build
        ~max_compare_bytes
        ~checkpoint
      |> Result.map_error ~f:(function
        | Limit -> Error.Result_limit
        | Foreign_input | Unavailable_route -> Error.Foreign_root)
    ;;
  end

  module Script_capture = struct
    type t =
      { owner : document_context
      ; build : Build.t
      ; contract : Chatml.Chatml_compiler_contract.t
      ; target : Chatml.Chatml_compiler_contract.Target.t
      ; selection : Agent_protocol.Authoring.Context.Selection.t
      ; entry_source : Captured.t
      ; parsed : Parsed_capture.t
      ; declaration : Prompt.Chat_markdown.Declaration.t
      ; project : Project_capture.t
      }

    let build t = t.build
    let contract t = t.contract
    let entry_source t = t.entry_source

    let matches t ~parsed ~declaration ~project =
      phys_equal t.parsed parsed
      && phys_equal t.declaration declaration
      && phys_equal t.project project
    ;;
  end

  let check_selected_now t ~script ~actor ~select_contract =
    let module Contract = Chatml.Chatml_compiler_contract in
    let open Result.Let_syntax in
    let%bind () =
      if phys_equal script.Script_capture.owner t then Ok () else Error Error.Foreign_root
    in
    let%bind () = check_all t.common actor in
    let%bind root =
      match t.root with
      | Some root when Project_capture.belongs_to_root script.project root -> Ok root
      | None | Some _ -> Error Error.Foreign_root
    in
    let same_target actor =
      match
        select_contract
          actor
          ~root
          ~project:(Project_capture.member script.project)
          ~selection:script.selection
      with
      | Some contract ->
        (match Contract.kind contract with
         | Contract.Kind.Standard target -> Contract.Target.equal script.target target
         | Native -> false)
      | None -> false
    in
    let original = same_target t.common.original_actor in
    let current = same_target actor in
    let%bind () = check_all t.common actor in
    if original && current then Ok () else Error Error.Selected_script_unavailable
  ;;

  exception Project_work_limit

  let capture_project_declaration
        t
        ~actor
        ~parsed
        ~declaration
        ~schema_limits
        ~max_locator_bytes
        ~max_path_components
        ~max_identity_bytes
        ~max_work_steps
        ~checkpoint
    =
    let module Pure = Authoring_project_declaration in
    let open Result.Let_syntax in
    if
      max_locator_bytes <= 0
      || max_locator_bytes > 1024
      || max_path_components <= 0
      || max_path_components > 1024
      || max_identity_bytes <= 0
      || max_identity_bytes > 1024 * 1024
      || max_work_steps <= 0
    then Error Error.Result_limit
    else (
      match t.root with
      | None -> Error Error.Project_declaration_unavailable
      | Some root ->
        let remaining = ref max_work_steps in
        let budgeted_checkpoint () =
          if !remaining <= 0 then raise Project_work_limit;
          decr remaining;
          checkpoint ()
        in
        (try
           with_local_root
             t.common
             root
             ~actor
             ~checkpoint:budgeted_checkpoint
             (fun owner ~policy ~poll ->
                let charge = poll in
                charge ();
                let captured_root = Parsed_capture.source parsed in
                if
                  (not (Captured.Id.equal (Captured.id captured_root) t.source_id))
                  || not
                       (String.equal
                          (Captured.Id.relative_path (Captured.id captured_root))
                          (Path.to_string t.member))
                then Error Error.Foreign_root
                else (
                  let%bind input, member, selection =
                    Pure.admit
                      ~parsed:(Parsed_capture.parsed parsed)
                      ~input_sources:(Parsed_capture.input_sources parsed)
                      ~declaration
                      ~package_namespace:(Captured.Id.package_namespace t.source_id)
                      ~max_locator_bytes
                      ~max_path_components
                      ~checkpoint:charge
                    |> Result.map_error ~f:(function
                      | Foreign_input -> Error.Foreign_root
                      | Unavailable_route -> Error.Project_declaration_unavailable
                      | Limit -> Error.Result_limit)
                  in
                  let max_bytes = Document_schema.Limits.max_bytes schema_limits in
                  let%bind contents =
                    read_local ~max_bytes t.common owner ~policy ~member ~poll:charge
                    |> Result.map_error ~f:(function
                      | Error.Local error ->
                        (match Local.Error.code error with
                         | Too_large | Invalid_limit | Entry_limit -> Error.Result_limit
                         | Missing
                         | Denied
                         | Unsupported_provider
                         | Unsupported_kind
                         | Symlink_or_non_directory
                         | Changed
                         | Not_retained_directory
                         | Closed
                         | Unavailable -> Error.Project_declaration_unavailable)
                      | error -> error)
                  in
                  let%bind id =
                    Captured.Id.create
                      ~package_namespace:(Captured.Id.package_namespace t.source_id)
                      ~relative_path:(Path.to_string member)
                    |> Result.map_error ~f:(fun error -> Error.Source_capture error)
                  in
                  let%bind source =
                    Captured.create
                      id
                      ~contents
                      ~expected_digest:None
                      ~max_bytes:(Some max_bytes)
                      ~checkpoint:charge
                    |> Result.map_error ~f:(fun error -> Error.Source_capture error)
                  in
                  let%bind project =
                    Chatml_project.decode source ~limits:schema_limits ~checkpoint:charge
                    |> Result.map_error ~f:(fun error ->
                      match Chatml_project.Error.details error with
                      | Document_schema.Error.Limit_exceeded _ | Invalid_configuration _
                        -> Error.Result_limit
                      | Malformed _
                      | Duplicate_key _
                      | Unsupported_beta_format
                      | Unsupported_format _
                      | Invalid_field _
                      | Unsupported_kind _
                      | Unsupported_version _
                      | Missing_conversion _
                      | Wrong_kind _
                      | Wrong_version _
                      | Required_semantics_unknown _
                      | Extension_conflict _ -> Error.Invalid_project_declaration)
                  in
                  let%map identity =
                    Pure.identity
                      ~source
                      ~input
                      ~declaration
                      ~selection
                      ~max_identity_bytes
                      ~checkpoint:charge
                    |> Result.map_error ~f:(function
                      | Limit -> Error.Result_limit
                      | Foreign_input | Unavailable_route -> Error.Foreign_root)
                  in
                  { Project_capture.source
                  ; project
                  ; root
                  ; issuer = Project_source_host.identity t.common.host.source_host
                  ; member
                  ; parsed
                  ; declaration
                  ; identity
                  }))
         with
         | Project_work_limit -> Error Error.Result_limit))
  ;;

  exception Script_work_limit

  let prepare_selected_script
        t
        ~actor
        ~parsed
        ~declaration
        ~project_capture
        ~select_contract
        ~clock
        ~max_script_bytes
        ~max_locator_bytes
        ~max_path_components
        ~max_work_steps
        ~checkpoint
    =
    let module Contract = Chatml.Chatml_compiler_contract in
    let open Result.Let_syntax in
    if
      max_script_bytes <= 0
      || max_script_bytes > Chatmd_source_bundle.default_limits.max_source_bytes
      || max_locator_bytes <= 0
      || max_locator_bytes > 1024
      || max_path_components <= 0
      || max_path_components > 1024
      || max_work_steps <= 0
    then Error Error.Result_limit
    else if not (Project_capture.matches project_capture ~parsed ~declaration)
    then Error Error.Foreign_root
    else (
      match t.root with
      | None -> Error Error.Project_declaration_unavailable
      | Some root when not (Project_capture.belongs_to_root project_capture root) ->
        Error Error.Foreign_root
      | Some root ->
        let remaining = ref max_work_steps in
        let budgeted_checkpoint () =
          if !remaining <= 0 then raise Script_work_limit;
          decr remaining;
          checkpoint ()
        in
        let select actor ~selection =
          budgeted_checkpoint ();
          select_contract
            actor
            ~root
            ~project:(Project_capture.member project_capture)
            ~selection
        in
        let standard_target contract =
          match Contract.kind contract with
          | Contract.Kind.Standard target -> Some target
          | Native -> None
        in
        (try
           with_operation t.common ~actor (fun poll ->
             let poll = poll budgeted_checkpoint in
             Sources.with_authoring
               t.common.host.source_host
               ~f:(fun ~roots ~policy ~limits ~max_request_bytes ->
                 poll ();
                 let root_source = Parsed_capture.source parsed in
                 if
                   (not (Captured.Id.equal (Captured.id root_source) t.source_id))
                   || not
                        (String.equal
                           (Captured.Id.relative_path (Captured.id root_source))
                           (Path.to_string t.member))
                 then Error Error.Foreign_root
                 else (
                   let%bind route =
                     Authoring_document_script_route.admit
                       ~parsed:(Parsed_capture.parsed parsed)
                       ~input_sources:(Parsed_capture.input_sources parsed)
                       ~bundle:(Parsed_capture.bundle parsed)
                       ~declaration
                       ~project_member:(Project_capture.member project_capture)
                       ~root_source
                       ~max_script_bytes
                       ~max_locator_bytes
                       ~max_path_components
                       ~checkpoint:poll
                     |> Result.map_error ~f:(function
                       | Foreign_declaration -> Error.Foreign_root
                       | Invalid_project_route
                       | Missing_external_source
                       | Conflicting_external_source ->
                         Error.Project_declaration_unavailable
                       | Limit -> Error.Result_limit)
                   in
                   let%bind selection =
                     Agent_protocol.Authoring.Context.Selection.create
                       ~target:Library
                       ~environment:(Authoring_document_script_route.environment route)
                     |> Result.map_error ~f:(fun _ -> Error.Selected_script_unavailable)
                   in
                   let%bind original_contract =
                     select t.common.original_actor ~selection
                     |> Result.of_option ~error:Error.Selected_script_unavailable
                   in
                   let%bind contract =
                     select actor ~selection
                     |> Result.of_option ~error:Error.Selected_script_unavailable
                   in
                   let%bind selected_target =
                     match
                       standard_target original_contract, standard_target contract
                     with
                     | Some original, Some current
                       when Contract.Target.equal original current -> Ok current
                     | _ -> Error Error.Selected_script_unavailable
                   in
                   let%bind selected = selected_anchors roots [ root ] in
                   let%bind build =
                     prepare_local_project
                       ~selected
                       ~project:(Project_capture.member project_capture)
                       ~selection:
                         (Chatml_project.Selection.Library
                            { environment =
                                Authoring_document_script_route.environment route
                            })
                       ~entry:(Authoring_document_script_route.entry route)
                       ~contract
                       ~policy
                       ~limits
                       ~max_request_bytes
                       ~clock
                       ~poll
                   in
                   let%bind () =
                     List.fold_result
                       (Chatml_prepared_program.sources (Build.prepared build))
                       ~init:0
                       ~f:(fun count _ ->
                         poll ();
                         if count >= t.common.host.limits.max_manifest_entries
                         then Error Error.Result_limit
                         else Ok (count + 1))
                     |> Result.map ~f:(fun _ -> ())
                   in
                   let%bind same_project =
                     Project_capture.matches_project_contents
                       project_capture
                       ~build
                       ~max_compare_bytes:(64 * 1024 * 1024)
                       ~checkpoint:poll
                   in
                   if not same_project
                   then Error Error.Selected_script_unavailable
                   else (
                     poll ();
                     let%bind original_now =
                       select t.common.original_actor ~selection
                       |> Result.of_option ~error:Error.Selected_script_unavailable
                     in
                     let%bind current_now =
                       select actor ~selection
                       |> Result.of_option ~error:Error.Selected_script_unavailable
                     in
                     match standard_target original_now, standard_target current_now with
                     | Some original, Some current
                       when Contract.Target.equal selected_target original
                            && Contract.Target.equal selected_target current ->
                       Ok
                         { Script_capture.owner = t
                         ; build
                         ; contract
                         ; target = selected_target
                         ; selection
                         ; entry_source = Authoring_document_script_route.source route
                         ; parsed
                         ; declaration
                         ; project = project_capture
                         }
                     | _ -> Error Error.Selected_script_unavailable)))
             |> Result.map_error ~f:(fun error -> Error.Sources error)
             |> Result.bind ~f:Fn.id)
         with
         | Script_work_limit -> Error Error.Result_limit))
  ;;

  module Origin = struct
    type t =
      | Host_member
      | Supplied
    [@@deriving equal, sexp_of]
  end

  let origin t = if Option.is_some t.root then Origin.Host_member else Origin.Supplied
  let root t = t.root
  let source_id t = t.source_id
  let check_current t ~actor = check_all t.common actor
  let retain_owner t ~actor = retain_owner t.common ~actor

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

  let capture_parser_bundle
        ?(limits : Chatmd_source_bundle.limits = Chatmd_source_bundle.default_limits)
        t
        ~actor
        ~source
        ~dir
        ~checkpoint
    =
    let open Result.Let_syntax in
    let%bind () = check_all t.common actor in
    let source_id = Captured.id source in
    if
      (not (Captured.Id.equal source_id t.source_id))
      || not
           (String.equal (Captured.Id.relative_path source_id) (Path.to_string t.member))
    then Error Error.Foreign_root
    else (
      match t.root with
      | None -> Error Error.Unsupported_source_selection
      | Some root ->
        with_local_root t.common root ~actor ~checkpoint (fun owner ~policy ~poll ->
          let open Result.Let_syntax in
          let defaults = Chatmd_source_bundle.default_limits in
          if
            limits.max_source_bytes <= 0
            || limits.max_bundle_bytes <= 0
            || limits.max_files <= 0
            || limits.max_source_bytes > defaults.max_source_bytes
            || limits.max_bundle_bytes > defaults.max_bundle_bytes
            || limits.max_files > defaults.max_files
          then Error Error.Document_bundle_limit
          else (
            let%bind retained_limits =
              Source_loader.Generated_retained.Limits.create
                ~max_source_bytes:limits.max_source_bytes
                ~max_bundle_bytes:limits.max_bundle_bytes
                ~max_files:limits.max_files
              |> Result.map_error ~f:(fun _ -> Error.Document_bundle_limit)
            in
            if
              Captured.byte_length source > t.common.host.limits.max_read_bytes
              || Captured.byte_length source > limits.max_source_bytes
              || Captured.byte_length source > limits.max_bundle_bytes
            then Error Error.Document_bundle_limit
            else (
              let%bind retained =
                Source_loader.Generated_retained.create
                  ~root:dir
                  ~owner
                  ~root_file:t.member
                  ~root_contents:(Captured.contents source)
                  ~limits:retained_limits
                  ~before_import_read:(fun ~member:_ ->
                    poll ();
                    let remaining =
                      t.common.host.limits.max_context_read_bytes - t.common.read_bytes
                    in
                    if remaining <= 0
                    then -1
                    else
                      Int.min
                        (Int.min t.common.host.limits.max_read_bytes remaining)
                        (Chatml_local_capture.Limits.source_bytes
                           (Project.Policy.local_limits policy)))
                  ~on_import_read:(fun ~member:_ ~contents ->
                    t.common.read_bytes <- t.common.read_bytes + String.length contents;
                    poll ())
                  ~checkpoint:poll
                |> Result.map_error ~f:(fun _ -> Error.Document_bundle_limit)
              in
              let%bind parsed, bundle =
                Prompt.Chat_markdown.Generated.capture_retained_bundle
                  retained
                  ~checkpoint:poll
                |> Result.map_error ~f:(fun error -> Error.Document_parser error)
              in
              let%map input_sources =
                bind_parser_inputs
                  ~source
                  ~bundle
                  ~parsed
                  ~reuse_imports:String.Map.empty
                  ~strict_reuse:false
                  ~max_source_bytes:limits.max_source_bytes
                  ~poll
              in
              { Parsed_capture.source; bundle; parsed; input_sources }))))
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

let with_root_owner host ~actor ~root ~checkpoint f =
  let open Result.Let_syntax in
  let%bind () = check_host host in
  let%bind () = check_root host ~actor root in
  if host.roots_busy
  then Error Error.Busy
  else (
    host.roots_busy <- true;
    try
      Exn.protect
        ~finally:(fun () ->
          host.roots_cancel <- None;
          host.roots_busy <- false;
          Eio.Condition.broadcast host.roots_idle)
        ~f:(fun () ->
          with_active (Root_enumeration host) (fun () ->
            Eio.Cancel.sub (fun cancel ->
              host.roots_cancel <- Some cancel;
              let poll () =
                (match check_host host, check_root host ~actor root with
                 | Ok (), Ok () -> ()
                 | Error error, _ | _, Error error -> raise (Revoked error));
                checkpoint ();
                match check_host host, check_root host ~actor root with
                | Ok (), Ok () -> ()
                | Error error, _ | _, Error error -> raise (Revoked error)
              in
              Sources.with_authoring
                host.source_host
                ~f:(fun ~roots ~policy ~limits:_ ~max_request_bytes:_ ->
                  let%bind selected = selected_anchors roots [ root ] in
                  let prefix, anchor = List.hd_exn selected in
                  Roots.with_anchors
                    [ anchor ]
                    ~limits:(Project.Policy.root_limits policy)
                    ~checkpoint:poll
                    ~f:(fun roots ->
                      match
                        Roots.resolve roots ~base:None ~locator:prefix ~checkpoint:poll
                      with
                      | Error error -> Error (Error.Root error)
                      | Ok cursor -> f (Roots.Cursor.owner cursor) ~policy ~poll)
                  |> Result.map_error ~f:(function
                    | `Roots error -> Error.Root error
                    | `Callback error -> error))
              |> Result.map_error ~f:(fun error -> Error.Sources error)
              |> Result.bind ~f:(fun result ->
                poll ();
                result))))
    with
    | Revoked error -> Error error)
;;

let list_root_directory host ~actor ~root ~directory ~checkpoint =
  with_root_owner host ~actor ~root ~checkpoint (fun owner ~policy ~poll ->
    list_directory_with_owner host owner ~policy ~directory ~poll)
;;

let read_root_member host ~actor ~root ~member ~checkpoint =
  with_root_owner host ~actor ~root ~checkpoint (fun owner ~policy ~poll ->
    let maximum =
      Int.min
        host.limits.max_read_bytes
        (Chatml_local_capture.Limits.source_bytes (Project.Policy.local_limits policy))
    in
    let open Result.Let_syntax in
    let%bind contents =
      Local.read owner ~member ~max_bytes:maximum ~checkpoint:poll
      |> Result.map_error ~f:(fun error -> Error.Local error)
    in
    let%bind id = Document_context.mint_source_id member in
    let%map source =
      Captured.create
        id
        ~contents
        ~expected_digest:None
        ~max_bytes:(Some maximum)
        ~checkpoint:poll
      |> Result.map_error ~f:(fun error -> Error.Source_capture error)
    in
    { Document_capture.source })
;;

let close_and_wait = close_all_and_wait
