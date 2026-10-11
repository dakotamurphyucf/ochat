open Core
module Model = Project_build_model
module Budget = Project_build_budget
module Assembly = Project_build_assembly
module Catalog_restore = Project_build_restore_catalog
module Closure = Project_build_closure
module Input = Model.Catalog_input
module Project = Chatml_project
module Lock = Project.Lock
module Captured = Chatml_source

let inventory budget ~limits ~lock_source ~sources =
  let fail problem = Error (Model.Error.create problem) in
  let check_bytes bytes =
    if
      Option.exists (Model.Limits.max_input_bytes limits) ~f:(fun maximum ->
        bytes > maximum)
    then fail Limit_exceeded
    else Ok bytes
  in
  let open Result.Let_syntax in
  let%bind initial_bytes = check_bytes (Captured.byte_length lock_source) in
  let%bind _ =
    List.fold_result sources ~init:initial_bytes ~f:(fun total source ->
      Budget.checkpoint budget;
      let bytes = Captured.byte_length source in
      if bytes > Int.max_value - total
      then fail Accounting_overflow
      else check_bytes (total + bytes))
  in
  let sources =
    List.sort sources ~compare:(fun left right ->
      Budget.compare_id budget (Captured.id left) (Captured.id right))
  in
  let rec unique previous = function
    | [] -> Ok sources
    | source :: rest ->
      Budget.checkpoint budget;
      let id = Captured.id source in
      if Budget.compare_id budget id (Captured.id lock_source) = 0
      then fail Invalid_lock_identity
      else if
        Option.exists previous ~f:(fun previous ->
          Budget.compare_id budget previous id = 0)
      then fail (Duplicate_capture id)
      else unique (Some id) rest
  in
  unique None sources
;;

let find_source budget sources id =
  List.find sources ~f:(fun source ->
    Budget.compare_id budget (Captured.id source) id = 0)
;;

let expected_inventory budget ~declared_at receipt sources =
  let fail problem = Error (Model.Error.create ~declared_at problem) in
  let entries =
    Lock.Entry.source (Lock.entry_selection receipt)
    :: List.concat_map (Lock.catalogs receipt) ~f:(fun catalog ->
      Budget.checkpoint budget;
      Lock.Catalog.sources catalog)
  in
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result entries ~init:() ~f:(fun () expected ->
      Budget.checkpoint budget;
      let id = Lock.Source_entry.id expected in
      match find_source budget sources id with
      | None -> fail (Missing_capture id)
      | Some actual ->
        if
          Captured.Digest.equal
            (Lock.Source_entry.digest expected)
            (Captured.digest actual)
          && Int.equal
               (Lock.Source_entry.byte_length expected)
               (Captured.byte_length actual)
        then Ok ()
        else fail Lock_mismatch)
  in
  let expected =
    List.map entries ~f:(fun entry ->
      Budget.checkpoint budget;
      Lock.Source_entry.id entry)
    |> List.dedup_and_sort ~compare:(Budget.compare_id budget)
  in
  let rec exact expected actual =
    Budget.checkpoint budget;
    match expected, actual with
    | [], [] -> Ok ()
    | [], _ :: _ -> fail Lock_mismatch
    | id :: _, [] -> fail (Missing_capture id)
    | id :: expected, source :: actual ->
      let comparison = Budget.compare_id budget id (Captured.id source) in
      if comparison = 0 then exact expected actual else fail Lock_mismatch
  in
  exact expected sources
;;

let catalog_input budget ~limits ~declared_at receipt sources selected =
  let checkpoint () = Budget.checkpoint budget in
  let fail problem = Error (Model.Error.create ~declared_at problem) in
  let get id =
    match find_source budget sources id with
    | None -> fail (Missing_capture id)
    | Some source -> Ok source
  in
  let declaration = Lock.Catalog.declaration selected in
  let open Result.Let_syntax in
  let%bind source = get declaration in
  let%bind project =
    Project.decode source ~limits:(Model.Limits.document limits) ~checkpoint
    |> Result.map_error ~f:(fun error -> Model.Error.create (Declaration error))
  in
  let environment = Some (Lock.Catalog.selected_environment selected) in
  let request =
    if Budget.compare_id budget declaration (Lock.root receipt) = 0
    then (
      match Lock.Entry.selection (Lock.entry_selection receipt) with
      | Declared name -> Project.Selection.Entry { name; environment }
      | Captured -> Library { environment })
    else Project.Selection.Library { environment }
  in
  let%bind captures =
    List.fold_result (Lock.Catalog.sources selected) ~init:[] ~f:(fun captures expected ->
      checkpoint ();
      let%map source = get (Lock.Source_entry.id expected) in
      source :: captures)
  in
  let%map catalog =
    Catalog_restore.create budget ~project ~request ~sources:(List.rev captures)
  in
  (* Each decoded catalog is reconstructed once. No restored input asserts a
     native acquisition identity; aliases refer to these unique declarations. *)
  { Input.catalog
  ; origin = Lock.Catalog.origin selected
  ; acquisition_key = None
  ; dependencies = Lock.Catalog.dependencies selected
  }
;;

let restore budget ~limits ~contract ~lock_source ~sources =
  let checkpoint () = Budget.checkpoint budget in
  checkpoint ();
  let open Result.Let_syntax in
  let%bind sources = inventory budget ~limits ~lock_source ~sources in
  let%bind receipt =
    Lock.decode lock_source ~limits:(Model.Limits.document limits) ~checkpoint
    |> Result.map_error ~f:(fun error -> Model.Error.create (Declaration error))
  in
  let declared_at = Project.Declared_at.create lock_source ~field_path:[] ~checkpoint in
  let%bind () = expected_inventory budget ~declared_at receipt sources in
  let%bind inputs =
    List.fold_result (Lock.catalogs receipt) ~init:[] ~f:(fun inputs selected ->
      checkpoint ();
      let%map input =
        catalog_input budget ~limits ~declared_at receipt sources selected
      in
      input :: inputs)
  in
  let root_id = Lock.root receipt in
  let roots, catalogs =
    List.partition_tf inputs ~f:(fun input ->
      Budget.compare_id budget (Input.declaration input) root_id = 0)
  in
  let%bind root =
    match roots with
    | [ root ] -> Ok root
    | [] -> Error (Model.Error.create ~declared_at (Missing_catalog root_id))
    | _ :: _ -> Error (Model.Error.create ~declared_at Lock_mismatch)
  in
  let%bind entry =
    match Lock.Entry.selection (Lock.entry_selection receipt) with
    | Declared _ -> Ok Model.Entry.Declared
    | Captured ->
      let id = Lock.entry receipt in
      (match find_source budget sources id with
       | Some source -> Ok (Model.Entry.Captured source)
       | None -> Error (Model.Error.create ~declared_at (Missing_capture id)))
  in
  let%bind catalogs = Closure.select budget root ~catalogs in
  Assembly.create
    budget
    root
    ~catalogs
    ~entry
    ~contract
    ~lock:(Restore { source = lock_source; receipt })
    ~limits
;;
