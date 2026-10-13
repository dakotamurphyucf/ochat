open! Core
open! Authoring_bundle_basis_common
module A = Authoring_common
module Context = Authoring_context.Context
module Captured = Chatml_source
module Project = Chatml_project
module Path = Source.Portable_path
module Project_basis = Authoring_bundle_basis_project.Project_basis

module Document_member = struct
  type t =
    { source : Captured.Id.t
    ; member : Path.t
    }

  let source t = t.source
  let member t = t.member

  let create ~source ~member =
    let* () = C.validate_source_id source in
    if String.length (Path.to_string member) > 1024
    then invalid "bundle document member exceeds wire limit"
    else Ok { source; member }
  ;;

  let equal left right =
    Captured.Id.equal left.source right.source && Path.equal left.member right.member
  ;;

  let to_json t =
    obj [ "source", C.source_id_json t.source; "member", path_json t.member ]
  ;;

  let of_json json =
    let* value = fields json [ "source"; "member" ] in
    let* source = required value "source" C.source_id in
    let* member = required value "member" path_of_json in
    create ~source ~member
  ;;
end

module Script = struct
  module Selector = struct
    type t =
      { registration_source : Captured.Id.t
      ; declaration_source : Captured.Id.t
      ; start_byte : int
      ; end_byte : int
      ; script_id : string
      ; body_digest : Captured.Digest.t
      ; body_byte_length : int
      }

    let registration_source t = t.registration_source
    let declaration_source t = t.declaration_source
    let start_byte t = t.start_byte
    let end_byte t = t.end_byte
    let script_id t = t.script_id
    let body_digest t = t.body_digest
    let body_byte_length t = t.body_byte_length

    let key_compare left right =
      let first =
        Captured.Id.compare left.registration_source right.registration_source
      in
      if first <> 0
      then first
      else (
        let second =
          Captured.Id.compare left.declaration_source right.declaration_source
        in
        if second <> 0
        then second
        else (
          let third = Int.compare left.start_byte right.start_byte in
          if third <> 0
          then third
          else (
            let fourth = Int.compare left.end_byte right.end_byte in
            if fourth <> 0 then fourth else String.compare left.script_id right.script_id)))
    ;;

    let equal left right =
      Int.equal (key_compare left right) 0
      && Captured.Digest.equal left.body_digest right.body_digest
      && Int.equal left.body_byte_length right.body_byte_length
    ;;

    let create
          ~registration_source
          ~declaration_source
          ~start_byte
          ~end_byte
          ~script_id
          ~body_digest
          ~body_byte_length
      =
      let* () = C.validate_source_id registration_source in
      let* () = C.validate_source_id declaration_source in
      let* () =
        if
          start_byte < 0
          || end_byte <= start_byte
          || end_byte > C.maximum_wire_integer
          || body_byte_length < 0
          || body_byte_length > 1024 * 1024
          || String.is_empty script_id
          || String.length script_id > 256
          || not (Stdlib.String.is_valid_utf_8 script_id)
        then invalid "invalid bundle script selector"
        else Ok ()
      in
      Ok
        { registration_source
        ; declaration_source
        ; start_byte
        ; end_byte
        ; script_id
        ; body_digest
        ; body_byte_length
        }
    ;;

    let to_json t =
      obj
        [ "registration_source", C.source_id_json t.registration_source
        ; "declaration_source", C.source_id_json t.declaration_source
        ; "start_byte", C.integer t.start_byte
        ; "end_byte", C.integer t.end_byte
        ; "script_id", str t.script_id
        ; "body_digest", C.digest_json t.body_digest
        ; "body_byte_length", C.integer t.body_byte_length
        ]
    ;;

    let of_json json =
      let* value =
        fields
          json
          [ "registration_source"
          ; "declaration_source"
          ; "start_byte"
          ; "end_byte"
          ; "script_id"
          ; "body_digest"
          ; "body_byte_length"
          ]
      in
      let* registration_source = required value "registration_source" C.source_id in
      let* declaration_source = required value "declaration_source" C.source_id in
      let* start_byte = required value "start_byte" C.nonnegative in
      let* end_byte = required value "end_byte" C.nonnegative in
      let* script_id = required value "script_id" (C.utf8_text ~max_bytes:256) in
      let* body_digest = required value "body_digest" C.digest in
      let* body_byte_length = required value "body_byte_length" C.nonnegative in
      create
        ~registration_source
        ~declaration_source
        ~start_byte
        ~end_byte
        ~script_id
        ~body_digest
        ~body_byte_length
    ;;
  end

  type choice =
    | Source_only
    | Selected_project of Project_basis.t

  type t =
    { selector : Selector.t
    ; choice : choice
    }

  let selector t = t.selector
  let choice t = t.choice

  let create ~selector ~choice =
    let* () =
      match choice with
      | Source_only -> Ok ()
      | Selected_project project ->
        (match Project_basis.entry project with
         | Project_basis.Declared ->
           invalid "selected bundle script requires captured entry"
         | Project_basis.Captured entry ->
           let source = Project_basis.Captured_entry.source entry in
           if
             Captured.Digest.equal
               (A.Source.digest source)
               (Selector.body_digest selector)
             && Int.equal
                  (A.Source.byte_length source)
                  (Selector.body_byte_length selector)
           then Ok ()
           else invalid "selected bundle script body mismatch")
    in
    Ok { selector; choice }
  ;;

  let equal_choice left right =
    match left, right with
    | Source_only, Source_only -> true
    | Selected_project left, Selected_project right -> Project_basis.equal left right
    | Source_only, Selected_project _ | Selected_project _, Source_only -> false
  ;;

  let equal left right =
    Selector.equal left.selector right.selector && equal_choice left.choice right.choice
  ;;

  let to_json t =
    let fields = [ "selector", Selector.to_json t.selector ] in
    match t.choice with
    | Source_only -> obj (("kind", str "source_only") :: fields)
    | Selected_project project ->
      obj
        [ "kind", str "selected_project"
        ; "selector", Selector.to_json t.selector
        ; "project", Project_basis.to_json project
        ]
  ;;

  let of_json json =
    let* value = fields json [ "kind"; "selector"; "project" ] in
    let* kind = required value "kind" (C.string ~max_bytes:24) in
    let* selector = required value "selector" Selector.of_json in
    let* choice =
      match kind with
      | "source_only" ->
        let* _ = fields json [ "kind"; "selector" ] in
        Ok Source_only
      | "selected_project" ->
        let* project = required value "project" Project_basis.of_json in
        Ok (Selected_project project)
      | _ -> invalid "invalid bundle script choice"
    in
    create ~selector ~choice
  ;;
end

module Chatmd_document = struct
  type t =
    { root_source : Captured.Id.t
    ; document_members : Document_member.t list
    ; scripts : Script.t list
    }

  let root_source t = t.root_source
  let document_members t = t.document_members
  let scripts t = t.scripts

  let create ~root_source ~document_members ~scripts =
    let* () = C.validate_source_id root_source in
    if not (bounded_count ~max:256 document_members && bounded_count ~max:256 scripts)
    then invalid "too many bundle document members or scripts"
    else
      let* document_members =
        List.fold_result
          document_members
          ~init:(Map.empty (module Captured.Id))
          ~f:(fun seen member ->
            if Map.mem seen (Document_member.source member)
            then invalid "duplicate bundle document member"
            else Ok (Map.set seen ~key:(Document_member.source member) ~data:member))
        |> Result.map ~f:Map.data
      in
      let paths = List.map document_members ~f:Document_member.member in
      let* () =
        if Option.is_some (Path.first_collision paths ~checkpoint:Fn.id)
        then invalid "colliding bundle document paths"
        else Ok ()
      in
      let owners =
        List.fold
          document_members
          ~init:(Set.empty (module Captured.Id))
          ~f:(fun ids member -> Set.add ids (Document_member.source member))
      in
      let* () =
        if Set.mem owners root_source
        then Ok ()
        else invalid "bundle document root is not mapped"
      in
      let* () =
        List.fold_result scripts ~init:() ~f:(fun () script ->
          let selector = Script.selector script in
          if
            Set.mem owners (Script.Selector.registration_source selector)
            && Set.mem owners (Script.Selector.declaration_source selector)
          then Ok ()
          else invalid "bundle script owner is outside document closure")
      in
      let scripts =
        List.sort scripts ~compare:(fun left right ->
          Script.Selector.key_compare (Script.selector left) (Script.selector right))
      in
      let rec unique = function
        | [] | [ _ ] -> true
        | left :: (right :: _ as rest) ->
          Script.Selector.key_compare (Script.selector left) (Script.selector right) <> 0
          && unique rest
      in
      if not (unique scripts)
      then invalid "duplicate bundle script selector"
      else Ok { root_source; document_members; scripts }
  ;;

  let equal left right =
    Captured.Id.equal left.root_source right.root_source
    && List.equal Document_member.equal left.document_members right.document_members
    && List.equal Script.equal left.scripts right.scripts
  ;;

  let to_json t =
    obj
      [ "root_source", C.source_id_json t.root_source
      ; "document_members", C.array Document_member.to_json t.document_members
      ; "scripts", C.array Script.to_json t.scripts
      ]
  ;;

  let of_json json =
    let* value = fields json [ "root_source"; "document_members"; "scripts" ] in
    let* root_source = required value "root_source" C.source_id in
    let* document_members =
      required value "document_members" (C.list ~max_items:256 Document_member.of_json)
    in
    let* scripts = required value "scripts" (C.list ~max_items:256 Script.of_json) in
    create ~root_source ~document_members ~scripts
  ;;
end
