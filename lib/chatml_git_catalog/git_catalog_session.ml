open Core
open Git_catalog_model
module Budget = Git_catalog_budget
module Raw = Git_reader
module Catalog = Chatml_catalog
module Captured = Chatml_source
module Operation = Git_catalog_operation

type state =
  { budget : Budget.t
  ; raw : Raw.t
  ; operations : Operation.t
  ; mutable failure : Error.t option
  ; mutable sources : Captured.t list
  ; mutable declarations : Project.t list
  }

type t =
  { witness : unit ref
  ; mutable state : state option
  }

type declaration =
  { owner : unit ref
  ; project : Project.t
  ; package_root : Path.t option
  ; project_member : Path.t
  ; root : Host.Object_id.t
  }

let project t = t.project
let package_root t = t.package_root

let operation t ~f =
  match t.state with
  | None -> Error (Error.create Reader_closed)
  | Some state ->
    (match state.failure with
     | Some _ -> Error (Error.create Reader_poisoned)
     | None ->
       (try
          match
            Operation.run state.operations ~f:(fun () ->
              Budget.context state.budget None;
              f state)
          with
          | Error Busy ->
            let failure = Error.create Reader_busy in
            state.failure <- Some failure;
            Error failure
          | Error Closed -> Error (Error.create Reader_closed)
          | Ok (Ok _ as result) -> result
          | Ok (Error failure as result) ->
            state.failure <- Some failure;
            result
        with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          state.failure <- Some (Budget.error state.budget Reader_poisoned);
          Exn.raise_with_original_backtrace exn backtrace))
;;

let with_repository repository ~limits ~checkpoint ~f =
  let outcome =
    Eio.Switch.run (fun switch ->
      Budget.with_budget repository ~limits ~checkpoint ~f:(fun budget ->
        let state =
          { budget
          ; raw = Raw.create repository ~budget
          ; operations = Operation.create switch
          ; failure = None
          ; sources = []
          ; declarations = []
          }
        in
        let reader = { witness = ref (); state = Some state } in
        Fun.protect
          ~finally:(fun () ->
            Operation.close state.operations;
            reader.state <- None)
          (fun () ->
             let result = f reader in
             match result with
             | Error _ -> Ok result
             | Ok _ when Operation.is_active state.operations ->
               Error (Error.create Reader_busy)
             | Ok _ ->
               (match state.failure with
                | None -> Ok result
                | Some error -> Error error))))
  in
  match outcome with
  | Error error -> Error (`Reader error)
  | Ok (Error error, _) -> Error (`Callback error)
  | Ok (Ok result, receipt) -> Ok (result, receipt)
;;

let check_path state ~package_root path =
  Budget.checkpoint state.budget;
  let own = Path.to_string path in
  let prefix =
    Option.value_map package_root ~default:0 ~f:(fun prefix ->
      String.length (Path.to_string prefix) + 1)
  in
  if prefix > 1024 - String.length own
  then Error (Budget.error state.budget (Limit_exceeded Repository_member_bytes))
  else (
    let components =
      List.length (String.split own ~on:'/')
      + Option.value_map package_root ~default:0 ~f:(fun prefix ->
        List.length (String.split (Path.to_string prefix) ~on:'/'))
    in
    if components > (Budget.limits state.budget).max_path_components
    then Error (Budget.error state.budget (Limit_exceeded Path_components))
    else Ok ())
;;

let same_id state first second =
  Budget.equal_string
    state.budget
    (Captured.Id.package_namespace first)
    (Captured.Id.package_namespace second)
  && String.equal (Captured.Id.relative_path first) (Captured.Id.relative_path second)
;;

let source state ~namespace ~package_root ~root path =
  let open Result.Let_syntax in
  let%bind () = check_path state ~package_root path in
  let%bind body = Raw.blob state.raw ~root ~path in
  match body with
  | None -> Ok None
  | Some contents ->
    (* Both inputs are validated, so this constructor has no recoverable failure. *)
    let id =
      Captured.Id.create ~package_namespace:namespace ~relative_path:(Path.to_string path)
      |> Or_error.ok_exn
    in
    let existing =
      List.find state.sources ~f:(fun source ->
        Budget.checkpoint state.budget;
        same_id state id (Captured.id source))
    in
    (match existing with
     | Some source ->
       if Budget.equal_string state.budget (Captured.contents source) contents
       then Ok (Some source)
       else Error (Budget.error state.budget (Conflicting_capture id))
     | None ->
       let%bind () = Budget.admit_source state.budget (String.length contents) in
       let source =
         Captured.create
           id
           ~contents
           ~expected_digest:None
           ~max_bytes:(Some (Budget.limits state.budget).max_source_bytes)
           ~checkpoint:(fun () -> Budget.checkpoint state.budget)
         |> Or_error.ok_exn
       in
       state.sources <- source :: state.sources;
       Ok (Some source))
;;

let declaration_error state failure =
  Error.create
    ~declared_at:
      (Project.Declared_at.create
         (Project.Error.source failure)
         ~field_path:(Project.Error.field_path failure)
         ~checkpoint:(fun () -> Budget.checkpoint state.budget))
    (Declaration failure)
;;

let catalog_error failure =
  Error.create
    ~declared_at:(Catalog.Error.declared_at failure)
    ?related:(Catalog.Error.related failure)
    (Catalog failure)
;;

let read_declaration t ~package_root ~project:project_member ~namespace_override =
  operation t ~f:(fun state ->
    let open Result.Let_syntax in
    let%bind () = check_path state ~package_root project_member in
    if Option.exists namespace_override ~f:String.is_empty
    then Error (Budget.error state.budget Invalid_namespace_override)
    else (
      let%bind root = Raw.root state.raw in
      let%bind root =
        Raw.subtree
          state.raw
          ~root
          ~components:
            (Option.value_map package_root ~default:[] ~f:(fun prefix ->
               String.split (Path.to_string prefix) ~on:'/'))
          ~path:project_member
      in
      let%bind body = Raw.blob state.raw ~root ~path:project_member in
      match body with
      | None -> Error (Budget.error state.budget (Missing_member project_member))
      | Some body ->
        let%bind () =
          if String.length body > (Budget.limits state.budget).max_source_bytes
          then Error (Budget.error state.budget (Limit_exceeded Source_bytes))
          else Ok ()
        in
        let%bind authored =
          Result.map_error
            (Project.Namespace.of_contents
               body
               ~limits:(Budget.limits state.budget).document
               ~checkpoint:(fun () -> Budget.checkpoint state.budget))
            ~f:(fun details -> Budget.error state.budget (Bootstrap details))
        in
        let namespace = Option.value namespace_override ~default:authored in
        let%bind captured = source state ~namespace ~package_root ~root project_member in
        let captured = Option.value_exn captured in
        Budget.context
          state.budget
          (Some
             (Project.Declared_at.create captured ~field_path:[] ~checkpoint:(fun () ->
                Budget.checkpoint state.budget)));
        let existing =
          List.find state.declarations ~f:(fun project ->
            Budget.checkpoint state.budget;
            same_id state (Captured.id captured) (Captured.id (Project.source project)))
        in
        let%map project =
          match existing with
          | Some project -> Ok project
          | None ->
            let%map project =
              Result.map_error
                (Project.decode
                   captured
                   ~limits:(Budget.limits state.budget).document
                   ~checkpoint:(fun () -> Budget.checkpoint state.budget))
                ~f:(declaration_error state)
            in
            state.declarations <- project :: state.declarations;
            project
        in
        { owner = t.witness; project; package_root; project_member; root }))
;;

let observations state declaration discovery =
  List.fold_result
    (Project.source_dirs declaration.project)
    ~init:[]
    ~f:(fun observations directory ->
      Budget.checkpoint state.budget;
      Budget.context state.budget (Some (Project.Source_dir.declared_at directory));
      let path, components =
        match Project.Source_dir.directory directory with
        | Root -> declaration.project_member, []
        | Relative path -> path, String.split (Path.to_string path) ~on:'/'
      in
      let open Result.Let_syntax in
      let%bind () = check_path state ~package_root:declaration.package_root path in
      let%bind tree = Raw.directory state.raw ~root:declaration.root ~components ~path in
      List.fold_result
        (Git_tree.entries tree)
        ~init:observations
        ~f:(fun observations entry ->
          Budget.checkpoint state.budget;
          let%map observation =
            Result.map_error
              (Catalog.Plan.Discovery.observe
                 discovery
                 ~directory
                 ~name:(Git_tree.Entry.name entry))
              ~f:catalog_error
          in
          match observation with
          | None -> observations
          | Some observation -> observation :: observations))
;;

let capture t declaration ~request =
  operation t ~f:(fun state ->
    if not (phys_equal declaration.owner t.witness)
    then Error (Budget.error state.budget Foreign_declaration)
    else (
      Budget.context state.budget (Some (Project.declared_at declaration.project));
      let open Result.Let_syntax in
      let%bind discovery =
        Result.map_error
          (Catalog.Plan.Discovery.create
             declaration.project
             ~request
             ~checkpoint:(fun () -> Budget.checkpoint state.budget))
          ~f:catalog_error
      in
      let%bind observations = observations state declaration discovery in
      let%bind plan =
        Result.map_error
          (Catalog.Plan.create discovery ~observations ~checkpoint:(fun () ->
             Budget.checkpoint state.budget))
          ~f:catalog_error
      in
      let namespace =
        Captured.Id.package_namespace (Captured.id (Project.source declaration.project))
      in
      let%bind sources =
        List.fold_result
          (Catalog.Plan.members plan)
          ~init:[ Project.source declaration.project ]
          ~f:(fun sources member ->
            Budget.context state.budget (Some (Catalog.Plan.Member.declared_at member));
            let path = Catalog.Plan.Member.path member in
            if Path.equal path declaration.project_member
            then Ok sources
            else (
              let%bind captured =
                source
                  state
                  ~namespace
                  ~package_root:declaration.package_root
                  ~root:declaration.root
                  path
              in
              match captured with
              | Some source -> Ok (source :: sources)
              | None ->
                (match Catalog.Plan.Member.requirement member with
                 | Optional -> Ok sources
                 | Required -> Error (Budget.error state.budget (Missing_member path)))))
      in
      Result.map_error
        (Catalog.create plan ~sources ~checkpoint:(fun () ->
           Budget.checkpoint state.budget))
        ~f:catalog_error))
;;

let accounting t =
  operation t ~f:(fun state ->
    Budget.checkpoint state.budget;
    Ok (Budget.snapshot state.budget))
;;
