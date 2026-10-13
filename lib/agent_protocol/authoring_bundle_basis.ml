open! Core
open! Authoring_bundle_basis_common
module A = Authoring_common
module Context = Authoring_context.Context
module Captured = Chatml_source
module Project = Chatml_project
module Path = Source.Portable_path
module Route = Authoring_bundle_basis_project.Route
module Project_basis = Authoring_bundle_basis_project.Project_basis
module Document_member = Authoring_bundle_basis_document.Document_member
module Script = Authoring_bundle_basis_document.Script
module Chatmd_document = Authoring_bundle_basis_document.Chatmd_document

type t =
  | Project of Project_basis.t
  | Chatmd_document of Chatmd_document.t

let equal left right =
  match left, right with
  | Project left, Project right -> Project_basis.equal left right
  | Chatmd_document left, Chatmd_document right -> Chatmd_document.equal left right
  | Project _, Chatmd_document _ | Chatmd_document _, Project _ -> false
;;

let to_json = function
  | Project project ->
    obj [ "kind", str "project"; "project", Project_basis.to_json project ]
  | Chatmd_document document ->
    obj [ "kind", str "chatmd_document"; "document", Chatmd_document.to_json document ]
;;

let of_json json =
  let* value = fields json [ "kind"; "project"; "document" ] in
  let* kind = required value "kind" (C.string ~max_bytes:24) in
  match kind with
  | "project" ->
    let* _ = fields json [ "kind"; "project" ] in
    let* project = required value "project" Project_basis.of_json in
    Ok (Project project)
  | "chatmd_document" ->
    let* _ = fields json [ "kind"; "document" ] in
    let* document = required value "document" Chatmd_document.of_json in
    Ok (Chatmd_document document)
  | _ -> invalid "invalid bundle basis"
;;

let validate_inventory basis ~member_sources =
  let members =
    List.fold
      member_sources
      ~init:(Map.empty (module Captured.Id))
      ~f:(fun acc source -> Map.set acc ~key:(A.Source.id source) ~data:source)
  in
  let metadata_nodes = ref 0 in
  let metadata_bytes = ref 0 in
  let charge ~nodes ~bytes =
    if
      nodes < 0
      || bytes < 0
      || nodes > 4096 - !metadata_nodes
      || bytes > (4 * 1024 * 1024) - !metadata_bytes
    then invalid "bundle metadata exceeds aggregate limit"
    else (
      metadata_nodes := !metadata_nodes + nodes;
      metadata_bytes := !metadata_bytes + bytes;
      Ok ())
  in
  let charge_id id =
    let namespace = String.length (Captured.Id.package_namespace id) in
    let path = String.length (Captured.Id.relative_path id) in
    if namespace > Int.max_value - path
    then invalid "bundle metadata byte overflow"
    else charge ~nodes:1 ~bytes:(namespace + path)
  in
  let member id =
    match Map.find members id with
    | Some source -> Ok source
    | None -> invalid "bundle basis references missing member"
  in
  let validate_project ~top ~selector project =
    let open Project_basis in
    let* () = charge_id (root_source project) in
    let* subset =
      List.fold_result
        (source_members project)
        ~init:(Set.empty (module Captured.Id))
        ~f:(fun subset id ->
          let* () = charge_id id in
          let* _ = member id in
          Ok (Set.add subset id))
    in
    let* () =
      if Set.mem subset (root_source project)
      then Ok ()
      else invalid "bundle project root is outside its captured subset"
    in
    let route_ids =
      List.fold
        (routes project)
        ~init:(Set.empty (module Captured.Id))
        ~f:(fun ids route -> Set.add ids (Route.declaration route))
    in
    let* () =
      List.fold_result (routes project) ~init:() ~f:(fun () route ->
        let declaration = Route.declaration route in
        let* () = charge_id declaration in
        let* () =
          if Set.mem subset declaration
          then Ok ()
          else invalid "bundle route is outside its captured subset"
        in
        let* () =
          match Route.origin route with
          | Project.Lock.Catalog.Local -> charge ~nodes:1 ~bytes:0
          | Project.Lock.Catalog.Git { repository; pin; package_root } ->
            let package_bytes =
              Option.value_map package_root ~default:0 ~f:(fun path ->
                String.length (Path.to_string path))
            in
            charge
              ~nodes:1
              ~bytes:
                (String.length repository
                 + String.length (Project.Git_pin.commit pin)
                 + package_bytes)
        in
        List.fold_result (Route.dependencies route) ~init:() ~f:(fun () dependency ->
          let target = Project.Lock.Dependency_selection.declaration dependency in
          let* () =
            charge
              ~nodes:1
              ~bytes:(String.length (Project.Lock.Dependency_selection.name dependency))
          in
          let* () = charge_id target in
          if Set.mem subset target && Set.mem route_ids target
          then Ok ()
          else invalid "bundle dependency target lacks captured project route"))
    in
    let* () =
      if
        List.exists (routes project) ~f:(fun route ->
          Captured.Id.equal (Route.declaration route) (root_source project))
      then Ok ()
      else invalid "bundle project root route is absent"
    in
    let* () =
      match entry project with
      | Declared -> Ok ()
      | Captured captured ->
        let source = Captured_entry.source captured in
        let id = A.Source.id source in
        let* () = charge_id id in
        (match Captured_entry.delivery captured with
         | Uploaded_member ->
           let* actual = member id in
           if Set.mem subset id && A.Source.equal source actual
           then Ok ()
           else invalid "uploaded bundle entry does not match member"
         | Derived_script ->
           let* () =
             if top || Map.mem members id || Set.mem subset id
             then invalid "invalid derived bundle entry ownership"
             else Ok ()
           in
           (match selector with
            | None -> invalid "derived bundle entry lacks script selector"
            | Some selector ->
              if
                Captured.Digest.equal
                  (A.Source.digest source)
                  (Script.Selector.body_digest selector)
                && Int.equal
                     (A.Source.byte_length source)
                     (Script.Selector.body_byte_length selector)
              then Ok ()
              else invalid "derived bundle entry differs from script body"))
    in
    match mode project with
    | Exact_lock id ->
      let* () = charge_id id in
      let* _ = member id in
      if Set.mem subset id
      then Ok ()
      else invalid "exact selected lock is outside project subset"
    | Compose { selected_previous_lock; generated_lock } ->
      let* () = charge_id generated_lock in
      let* () =
        if Map.mem members generated_lock || Set.mem subset generated_lock
        then invalid "generated bundle lock collides with captured source"
        else Ok ()
      in
      (match selected_previous_lock with
       | None -> Ok ()
       | Some previous ->
         let id = A.Source.id previous in
         let* () = charge_id id in
         if Captured.Id.equal id generated_lock || Set.mem subset id
         then invalid "selected previous lock collides with project capture"
         else Ok ())
  in
  match basis with
  | Project project -> validate_project ~top:true ~selector:None project
  | Chatmd_document document ->
    let root = Chatmd_document.root_source document in
    let namespace = Captured.Id.package_namespace root in
    let* () = charge_id root in
    let document_ids =
      List.fold
        (Chatmd_document.document_members document)
        ~init:(Set.empty (module Captured.Id))
        ~f:(fun ids document_member ->
          Set.add ids (Document_member.source document_member))
    in
    let* () =
      if Set.mem document_ids root
      then Ok ()
      else invalid "bundle document root is not mapped"
    in
    let* _document_bytes =
      List.fold_result
        (Chatmd_document.document_members document)
        ~init:0
        ~f:(fun total document_member ->
          let id = Document_member.source document_member in
          let* () = charge_id id in
          let* () =
            charge
              ~nodes:1
              ~bytes:
                (String.length (Path.to_string (Document_member.member document_member)))
          in
          let* source = member id in
          let length = A.Source.byte_length source in
          if
            (not (String.equal namespace (Captured.Id.package_namespace id)))
            || length > 1024 * 1024
            || length > (8 * 1024 * 1024) - total
          then invalid "invalid captured document inventory"
          else Ok (total + length))
    in
    let derived = ref (Map.empty (module Captured.Id)) in
    List.fold_result (Chatmd_document.scripts document) ~init:() ~f:(fun () script ->
      let selector = Script.selector script in
      let registration = Script.Selector.registration_source selector in
      let declaration = Script.Selector.declaration_source selector in
      let* () = charge_id registration in
      let* () = charge_id declaration in
      let* () =
        charge ~nodes:1 ~bytes:(String.length (Script.Selector.script_id selector))
      in
      let* () =
        if Set.mem document_ids registration && Set.mem document_ids declaration
        then Ok ()
        else invalid "bundle script owner is outside document closure"
      in
      let* owner = member declaration in
      let* () =
        if Script.Selector.end_byte selector <= A.Source.byte_length owner
        then Ok ()
        else invalid "bundle script range exceeds declaration source"
      in
      match Script.choice script with
      | Source_only -> Ok ()
      | Selected_project project ->
        let* () = validate_project ~top:false ~selector:(Some selector) project in
        (match Project_basis.entry project with
         | Declared -> invalid "selected bundle script requires captured entry"
         | Captured entry ->
           let source = Project_basis.Captured_entry.source entry in
           let* () =
             if
               Captured.Digest.equal
                 (A.Source.digest source)
                 (Script.Selector.body_digest selector)
               && Int.equal
                    (A.Source.byte_length source)
                    (Script.Selector.body_byte_length selector)
             then Ok ()
             else invalid "selected bundle script body mismatch"
           in
           (match Project_basis.Captured_entry.delivery entry with
            | Uploaded_member -> Ok ()
            | Derived_script ->
              let id = A.Source.id source in
              (match Map.find !derived id with
               | None ->
                 derived := Map.set !derived ~key:id ~data:source;
                 Ok ()
               | Some prior when A.Source.equal prior source -> Ok ()
               | Some _ -> invalid "conflicting derived bundle script identity"))))
;;
