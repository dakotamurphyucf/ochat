open! Core
module P = Agent_protocol
module C = Recurrence_command
module E = Recurrence_entry
module F = Recurrence_definition
module I = Recurrence_identity
module R = Recurrence_command_receipt

type decision =
  | Retained of R.t
  | Commit of
      { catalog : Recurrence_catalog.t
      ; receipt : R.t
      }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let prepare
      catalog
      ~(request : C.t)
      ~principal_id
      ~session
      ~generation
      ~current_source
      ~current_lifecycle_revision
      ~now
  =
  let open Result.Let_syntax in
  let%bind request_sha256 = C.digest request in
  let%bind retained =
    Recurrence_catalog.receipt catalog ~principal_id ~key:request.key ~request_sha256
  in
  match retained with
  | Some receipt -> Ok (Retained receipt)
  | None ->
    let needs_source = C.Mutation.needs_source request.mutation in
    let%bind () =
      if
        P.Id.Session.equal request.session_id (P.Session_ref.session_id session)
        && Int.equal request.generation generation
        && Int64.equal
             request.expected_catalog_revision
             (Recurrence_catalog.revision catalog)
        && ((not needs_source)
            || (Option.equal P.Run_source.equal request.source current_source
                && Option.equal
                     P.Session_lifecycle.Revision.equal
                     request.expected_lifecycle_revision
                     current_lifecycle_revision))
      then Ok ()
      else conflict "recurrence session, source, lifecycle or catalog expectation changed"
    in
    let id = C.Mutation.id request.mutation in
    let old = Recurrence_catalog.find catalog id in
    let%bind () =
      match request.mutation, old with
      | Create _, None -> Ok ()
      | Create _, Some _ -> conflict "recurrence identity already retained"
      | (Edit _ | Enable _ | Pause _ | Retire _), None ->
        conflict "recurrence identity is missing"
      | (Edit _ | Enable _ | Pause _ | Retire _), Some old ->
        if
          P.Session_ref.equal session (E.controller old)
          && Option.equal
               Int64.equal
               request.expected_control_revision
               (Some (E.control_revision old))
        then Ok ()
        else conflict "recurrence control expectation changed"
    in
    let%bind () =
      if needs_source
      then
        List.fold_result (Recurrence_catalog.entries catalog) ~init:() ~f:(fun () entry ->
          match E.control entry with
          | Retired _ -> Ok ()
          | Enabled | Paused _ ->
            if
              Option.equal
                P.Session_lifecycle.Revision.equal
                (Some (F.birth_lifecycle_revision (E.current entry)))
                current_lifecycle_revision
            then Ok ()
            else conflict "old recurrence lifecycle must be durably retired first")
      else Ok ()
    in
    let capture_definition capture ~revision ~creator ~birth =
      let%bind source =
        Result.of_option
          current_source
          ~error:(P.Error.invalid_request "recurrence capture lacks current source")
      in
      let%bind key =
        I.Definition.create ~controller:session ~recurrence_id:id ~revision
      in
      F.create
        ~key
        ~creator
        ~source
        ~birth_lifecycle_revision:birth
        ~schedule:capture.C.Capture.schedule
        ~target:capture.target
        ~policy:capture.policy
        ~created_at:now
    in
    let%bind entry, kind =
      match request.mutation, old with
      | Create (_, capture), None ->
        let%bind birth =
          Result.of_option
            current_lifecycle_revision
            ~error:(P.Error.invalid_request "recurrence capture lacks current lifecycle")
        in
        let%bind definition =
          capture_definition capture ~revision:0L ~creator:principal_id ~birth
        in
        let%map entry = E.create definition in
        entry, R.Create
      | Edit (_, capture), Some old ->
        let current = E.current old in
        let revision = I.Definition.revision (F.key current) in
        if Int64.equal revision Int64.max_value
        then conflict "recurrence definition revision exhausted"
        else (
          let%bind definition =
            capture_definition
              capture
              ~revision:(Int64.succ revision)
              ~creator:(F.creator current)
              ~birth:(F.birth_lifecycle_revision current)
          in
          let%map entry = E.edit old definition in
          entry, R.Edit)
      | Enable _, Some old ->
        if
          Option.equal P.Run_source.equal (Some (F.source (E.current old))) current_source
        then (
          let%map entry = E.enable old in
          entry, R.Enable)
        else conflict "recurrence definition requires explicit current-source edit"
      | Pause _, Some old ->
        let%map entry = E.pause old ~reason:Operator_pause in
        entry, R.Pause
      | Retire _, Some old ->
        let%map entry = E.retire old ~reason:Operator_retire in
        entry, R.Retire
      | Create _, Some _ | (Edit _ | Enable _ | Pause _ | Retire _), None ->
        conflict "recurrence candidate no longer matches inventory"
    in
    if Int64.equal (Recurrence_catalog.revision catalog) Int64.max_value
    then conflict "recurrence catalog revision exhausted"
    else (
      let%bind receipt =
        R.create
          ~principal_id
          ~key:request.key
          ~request_sha256
          ~recurrence_id:id
          ~definition_revision:(I.Definition.revision (F.key (E.current entry)))
          ~control_revision:(E.control_revision entry)
          ~catalog_revision:(Int64.succ (Recurrence_catalog.revision catalog))
          ~kind
          ~source:(if needs_source then current_source else None)
          ~birth_lifecycle_revision:
            (if needs_source then current_lifecycle_revision else None)
          ~accepted_at:now
      in
      let%map catalog = Recurrence_catalog.commit catalog ~entry ~receipt in
      Commit { catalog; receipt })
;;
