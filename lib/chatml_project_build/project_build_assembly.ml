open Core
module Model = Project_build_model
module Budget = Project_build_budget
module Receipt = Project_build_receipt
module Entry = Project_build_entry
module Input = Model.Catalog_input
module Project = Chatml_project
module Lock = Project.Lock
module Catalog = Chatml_catalog
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

type t =
  { prepared : Prepared.t
  ; lock : Lock.t
  ; lock_source : Captured.t
  }

let prepared t = t.prepared
let lock t = t.lock
let lock_source t = t.lock_source

module Lock_input = struct
  type t =
    | Generate of Captured.Id.t
    | Restore of
        { source : Captured.t
        ; receipt : Lock.t
        }
end

let reconcile_entry budget entry ~sources ~receipts ~scopes ~declared_at =
  let same_id left right = Budget.compare_id budget left right = 0 in
  let source = Entry.source entry in
  let id = Captured.id source in
  let fail () = Error (Model.Error.create ~declared_at Invalid_entry_selection) in
  match List.find sources ~f:(fun candidate -> same_id (Captured.id candidate) id) with
  | None -> Ok (source :: sources, Entry.scope entry :: scopes)
  | Some previous ->
    let existing =
      List.find_map receipts ~f:(fun receipt ->
        Budget.checkpoint budget;
        List.find
          (Lock.Catalog.scopes (Receipt.catalog receipt))
          ~f:(fun scope -> same_id (Lock.File_scope.source scope) id))
    in
    let prepared_scope =
      List.find scopes ~f:(fun scope -> same_id (Prepared.Scope.source scope) id)
    in
    (match existing, prepared_scope with
     | None, _ | _, None -> fail ()
     | Some existing, Some prepared_scope ->
       let bindings = Prepared.Scope.bindings (Entry.scope entry) in
       let same_location left right =
         (* Locations are validated against exact bytes, so matching revision and
            byte endpoints also fix their canonical line/column coordinates. *)
         let left_span = Captured.Location.span left in
         let right_span = Captured.Location.span right in
         same_id (Captured.Location.source left) (Captured.Location.source right)
         && Captured.Digest.equal
              (Captured.Location.source_digest left)
              (Captured.Location.source_digest right)
         && Int.equal left_span.left.offset right_span.left.offset
         && Int.equal left_span.right.offset right_span.right.offset
       in
       let same_binding
             (expected : Prepared.Scope.binding)
             (actual : Prepared.Scope.binding)
         =
         Budget.compare_string budget expected.name actual.name = 0
         && same_location expected.declared_at actual.declared_at
         && same_id
              (Prepared.Module_id.implementation expected.module_id)
              (Prepared.Module_id.implementation actual.module_id)
       in
       if
         Budget.equal_source budget previous source
         && Budget.compare_string
              budget
              (Lock.File_scope.environment existing)
              (Entry.environment entry)
            = 0
         && Option.equal
              same_location
              (Prepared.Scope.selected_at prepared_scope)
              (Prepared.Scope.selected_at (Entry.scope entry))
         && List.equal same_binding (Prepared.Scope.bindings prepared_scope) bindings
       then Ok (sources, scopes)
       else fail ())
;;

let create budget root ~catalogs ~entry ~contract ~lock:lock_input ~limits =
  let checkpoint () = Budget.checkpoint budget in
  let declared_at = Input.declared_at root in
  let fail problem = Error (Model.Error.create ~declared_at problem) in
  let declaration result =
    Result.map_error result ~f:(fun error -> Model.Error.create (Declaration error))
  in
  let invalid result =
    Result.map_error result ~f:(fun _ -> Model.Error.create ~declared_at Lock_mismatch)
  in
  let open Result.Let_syntax in
  let lock_id =
    match lock_input with
    | Lock_input.Generate id -> id
    | Restore { source; receipt = _ } -> Captured.id source
  in
  let fingerprint = Contract.fingerprint contract ~checkpoint in
  let%bind receipts =
    List.fold_result catalogs ~init:[] ~f:(fun receipts input ->
      let%map receipt = Receipt.create budget input ~catalogs ~contract ~fingerprint in
      receipt :: receipts)
    |> Result.map ~f:List.rev
  in
  let%bind entry = Entry.create budget root ~catalogs ~entry ~contract in
  let sources =
    List.concat_map catalogs ~f:(fun input ->
      checkpoint ();
      Catalog.sources input.Input.catalog)
  in
  let scopes =
    List.concat_map receipts ~f:(fun receipt ->
      checkpoint ();
      Receipt.scopes receipt)
  in
  let%bind sources, scopes =
    reconcile_entry budget entry ~sources ~receipts ~scopes ~declared_at
  in
  let%bind () =
    if
      List.exists sources ~f:(fun source ->
        Budget.compare_id budget (Captured.id source) lock_id = 0)
    then fail Invalid_lock_identity
    else Ok ()
  in
  List.iter sources ~f:(fun source ->
    Budget.charge_bytes budget (Captured.byte_length source));
  let required_tools =
    List.concat_map catalogs ~f:(fun input ->
      checkpoint ();
      List.map
        (Project.required_tools (Catalog.project input.Input.catalog))
        ~f:(fun (name, _) ->
          checkpoint ();
          name))
    |> List.dedup_and_sort ~compare:(Budget.compare_string budget)
  in
  let environment = Project.Selection.environment (Catalog.selection root.catalog) in
  let%bind lock =
    Lock.create
      ~declared_at
      ~root:(Input.declaration root)
      ~entry:(Entry.receipt entry)
      ~environment:(Project.Scope_spec.name environment)
      ~compiler_family:(Contract.compiler_family contract)
      ~target_name:(Contract.target_name contract)
      ~compiler_contract:fingerprint
      ~required_tools
      ~catalogs:(List.map receipts ~f:Receipt.catalog)
      ~checkpoint
      ()
    |> declaration
  in
  let%bind contents =
    Lock.to_string lock ~limits:(Model.Limits.document limits) ~checkpoint |> declaration
  in
  let%bind lock, lock_source =
    match lock_input with
    | Generate _ ->
      Budget.charge_bytes budget (String.length contents);
      let%map source =
        Captured.create
          lock_id
          ~contents
          ~expected_digest:None
          ~max_bytes:(Model.Limits.max_input_bytes limits)
          ~checkpoint
        |> invalid
      in
      lock, source
    | Restore { source; receipt } ->
      Budget.charge_bytes budget (Captured.byte_length source);
      let%bind expected =
        Lock.to_string receipt ~limits:(Model.Limits.document limits) ~checkpoint
        |> declaration
      in
      if Budget.compare_string budget contents expected = 0
      then Ok (receipt, source)
      else fail Lock_mismatch
  in
  let%bind environment =
    Prepared.Environment.create
      ~name:(Project.Scope_spec.name environment)
      ~required_contract:fingerprint
      ~declared_at:
        (Option.map
           (Project.Scope_spec.declared_at environment)
           ~f:Project.Declared_at.location)
    |> invalid
  in
  let definitions =
    List.concat_map catalogs ~f:(fun input ->
      checkpoint ();
      Catalog.definitions input.Input.catalog)
  in
  let%map prepared =
    Prepared.create
      ~max_input_bytes:(Model.Limits.max_input_bytes limits)
      ~compiler_contract:fingerprint
      ~environment
      ~provenance:(Project { declaration = Input.declaration root; lock = Some lock_id })
      ~entry:(Captured.id (Entry.source entry))
      ~sources:(lock_source :: sources)
      ~definitions
      ~scopes
      ~checkpoint
      ()
    |> Result.map_error ~f:(fun error -> Model.Error.create (Prepared error))
  in
  { prepared; lock; lock_source }
;;
