open Core
module Attempt = Project_prepare_attempt
module Host = Project_prepare_host
module Request = Project_prepare_request
module Build = Chatml_project_build
module Error = Project_prepare_error
module Stage = Project_prepare_stage
module State = Project_prepare_graph_state
module Location = State.Location
module Accounting = Project_prepare_accounting
module Project = Chatml_project
module Catalog = Chatml_catalog
module Roots = Chatml_local_roots
module Captured = Chatml_source
module Path = Source.Portable_path
module Reader = Chatml_git_catalog.Reader

let charge attempt delta = Attempt.charge attempt delta
let poll attempt () = Attempt.poll attempt

let resolve_local attempt roots ~base ~locator ~project ~declared_at =
  Roots.resolve roots ~base ~locator ~checkpoint:(poll attempt)
  |> Result.map ~f:(fun cursor -> Location.Local { cursor; project })
  |> Result.map_error ~f:(fun failure -> Error.create ?declared_at (Roots failure))
;;

let root_intent (request : Request.t) =
  match request.root with
  | Local { locator = _; project } -> Project.Lock.Catalog.Local, project
  | Git { repository; pin; package_root; project } ->
    Project.Lock.Catalog.Git { repository; pin; package_root }, project
;;

let root_location attempt roots (request : Request.t) =
  match request.root with
  | Local { locator; project } ->
    resolve_local attempt roots ~base:None ~locator ~project ~declared_at:None
  | Git { repository; pin; package_root; project } ->
    Ok (Location.Git { repository; pin; package_root; project })
;;

let declaration_base attempt roots cursor project ~declared_at =
  let member = Path.to_string project in
  match String.rsplit2 member ~on:'/' with
  | None -> Ok cursor
  | Some (directory, _) ->
    Roots.resolve roots ~base:(Some cursor) ~locator:directory ~checkpoint:(poll attempt)
    |> Result.map_error ~f:(fun failure -> Error.create ~declared_at (Roots failure))
;;

let dependency_location attempt roots node (job : State.job) dependency =
  let location = job.location in
  let owner = Catalog.project (State.catalog node) in
  let open Result.Let_syntax in
  let%bind resolved =
    Stage.pure attempt ~f:(fun ~limits ~checkpoint ->
      Build.Dependency.resolve
        dependency
        ~owner_origin:(Location.origin location)
        ~owner_declaration:(Captured.id (Project.source owner))
        ~limits
        ~checkpoint)
  in
  let project = Build.Dependency.project resolved in
  match Build.Dependency.origin resolved with
  | Git { repository; pin; package_root } ->
    Ok (Location.Git { repository; pin; package_root; project })
  | Local ->
    let declared_at = Project.Dependency.declared_at dependency in
    (match Project.Dependency.source dependency with
     | Git _ -> Error (Error.create ~declared_at Invalid_accounting)
     | Local { directory } ->
       let%bind base =
         if String.is_prefix directory ~prefix:"/"
         then Ok None
         else (
           match location with
           | Git _ -> Error (Error.create ~declared_at Invalid_accounting)
           | Local { cursor; project } ->
             declaration_base attempt roots cursor project ~declared_at
             |> Result.map ~f:Option.some)
       in
       resolve_local
         attempt
         roots
         ~base
         ~locator:directory
         ~project
         ~declared_at:(Some declared_at))
;;

let expand attempt roots state gate node (job : State.job) =
  let open Result.Let_syntax in
  let%bind first = State.visit state node job in
  if not first
  then Ok ()
  else (
    let catalog = State.catalog node in
    let%bind dependencies =
      Stage.pure attempt ~f:(fun ~limits ~checkpoint ->
        Build.selected_dependencies catalog ~limits ~checkpoint)
    in
    List.fold_result dependencies ~init:() ~f:(fun () dependency ->
      let%bind () =
        charge attempt { Accounting.zero with dependency_visits = 1; work_steps = 1 }
      in
      (* Every incoming prior route is checked before native resolution, transport
         or reuse. A previously captured alias does not erase a second route. *)
      let%bind previous_owner =
        Stage.pure attempt ~f:(fun ~limits ~checkpoint ->
          Build.Selection_gate.check_dependency
            gate
            ~previous_owner:job.previous_owner
            ~owner:(Catalog.project catalog)
            ~owner_origin:(Location.origin (State.location node))
            dependency
            ~limits
            ~checkpoint)
      in
      let%bind location = dependency_location attempt roots node job dependency in
      State.enqueue
        state
        ~location
        ~selection:
          (Project.Selection.Library
             { environment = Project.Dependency.environment dependency })
        ~namespace_override:(Project.Dependency.namespace_override dependency)
        ~previous_owner
        ~parent:(Some (node, dependency))))
;;

let capture attempt (job : State.job) reader =
  match job.location with
  | Local { cursor; project } ->
    Project_prepare_local.capture
      attempt
      ~owner:(Roots.Cursor.owner cursor)
      ~project
      ~namespace_override:job.namespace_override
      ~selection:job.selection
    |> Result.map ~f:Chatml_local_capture.catalog
  | Git { repository = _; pin = _; package_root; project } ->
    (match reader with
     | None -> Error (Error.create Invalid_accounting)
     | Some reader ->
       let open Result.Let_syntax in
       let%bind declaration =
         Reader.read_declaration
           reader
           ~package_root
           ~project
           ~namespace_override:job.namespace_override
         |> Result.map_error ~f:(fun failure ->
           Error.create
             ?declared_at:(Chatml_git_catalog.Error.declared_at failure)
             (Git_catalog failure))
       in
       Reader.capture reader declaration ~request:job.selection
       |> Result.map_error ~f:(fun failure ->
         Error.create
           ?declared_at:(Chatml_git_catalog.Error.declared_at failure)
           (Git_catalog failure)))
;;

module Work = struct
  type t =
    | Existing of State.node
    | Capture_admitted
end

let select_work attempt state (job : State.job) =
  let open Result.Let_syntax in
  let%bind existing = State.find state job.location in
  match existing with
  | Some node -> Ok (Work.Existing node)
  | None ->
    (* Package capacity precedes transport as well as declaration/source reads. *)
    let%map () = charge attempt { Accounting.zero with packages = 1; work_steps = 1 } in
    Work.Capture_admitted
;;

let process attempt roots state gate root (job : State.job) ~work ~reader =
  let open Result.Let_syntax in
  let%bind node =
    match work with
    | Work.Existing node -> Ok node
    | Capture_admitted ->
      let%bind catalog = capture attempt job reader in
      State.add state job.location ~catalog
  in
  let%bind () = State.check_selection state node job in
  let%bind () = State.connect state job node in
  (match job.parent with
   | None -> root := Some node
   | Some _ -> ());
  expand attempt roots state gate node job
;;

let acquire attempt host roots state gate root =
  let open Result.Let_syntax in
  (* Local work must wait until native/semantic reservations are released. *)
  let rec drain reader repository =
    let%bind next = State.take state ~repository:(Some repository) in
    match next with
    | None -> Ok ()
    | Some job ->
      let%bind work = select_work attempt state job in
      let%bind () =
        process attempt roots state gate root job ~work ~reader:(Some reader)
      in
      drain reader repository
  in
  let rec run () =
    let%bind next = State.take state ~repository:None in
    match next with
    | None -> Ok ()
    | Some job ->
      let%bind work = select_work attempt state job in
      let%bind () =
        match work, job.location with
        | Work.Existing _, _ | Capture_admitted, Local _ ->
          process attempt roots state gate root job ~work ~reader:None
        | Capture_admitted, Git { repository; pin; package_root = _; project = _ } ->
          Stage.with_git attempt ~host ~repository ~pin ~f:(fun reader ->
            let%bind () =
              process attempt roots state gate root job ~work ~reader:(Some reader)
            in
            drain reader (repository, pin))
      in
      run ()
  in
  run ()
;;

let prepare attempt ~host ~(request : Request.t) ~tools =
  let result =
    let open Result.Let_syntax in
    let%bind gate =
      Stage.pure attempt ~f:(fun ~limits ~checkpoint ->
        Build.Selection_gate.create request.operation ~limits ~checkpoint)
    in
    let origin, project = root_intent request in
    let%bind previous_owner =
      Stage.pure attempt ~f:(fun ~limits ~checkpoint ->
        Build.Selection_gate.check_root gate ~origin ~project ~limits ~checkpoint)
    in
    Stage.with_roots attempt ~anchors:host.Host.anchors ~f:(fun roots ->
      let state = State.create attempt in
      let root = ref None in
      let%bind location = root_location attempt roots request in
      let%bind () =
        State.enqueue
          state
          ~location
          ~selection:request.selection
          ~namespace_override:None
          ~previous_owner
          ~parent:None
      in
      let%bind () = acquire attempt host roots state gate root in
      match !root with
      | None -> Error (Error.create Invalid_accounting)
      | Some root ->
        let%bind root_input, catalogs = State.inputs state ~root in
        let%bind build =
          Stage.pure attempt ~f:(fun ~limits ~checkpoint ->
            Build.compose
              root_input
              ~catalogs
              ~entry:request.entry
              ~selection_gate:gate
              ~contract:request.contract
              ~lock_id:request.lock_id
              ~limits
              ~checkpoint
            |> Result.map ~f:(fun build -> build, Build.accounting build))
        in
        let names = Project.Lock.required_tools (Build.lock build) in
        let%bind () =
          List.fold_result names ~init:() ~f:(fun () name ->
            charge attempt { Accounting.zero with work_steps = 1 + String.length name })
        in
        Attempt.poll attempt;
        let%map _ =
          Chat_response.Tool_capability.select tools ~names
          |> Result.map_error ~f:(fun failure ->
            Error.create
              ~declared_at:(Project.declared_at (Catalog.project (State.catalog root)))
              (Tool_admission failure))
        in
        build)
  in
  match result with
  | Ok _ -> result
  | Error error -> Attempt.reject attempt error
;;
