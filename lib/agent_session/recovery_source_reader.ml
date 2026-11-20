open! Core
module Store = Agent_store
module Sessions = Store.Session_store
module Reader = Store.Retention_reader
module P = Agent_protocol
module Ledger = Recovery_window_ledger
module Member = Ledger.Member
module Archive = Session_state.Compaction_archive
module Effective = Recovery_effective_record
module Effective_capture = Recovery_effective_capture

type error =
  | Denied
  | Unavailable
  | Withdrawn
  | Expired
  | Stale
  | Missing
  | Corrupt
  | Unsupported
  | Limit
[@@deriving equal, sexp_of]

let lookup state member =
  match
    List.nth
      state.Session_state.conversation.canonical_history
      (Member.source_ordinal member)
  with
  | Some row when Member.matches member row ->
    if row.redacted then Error Withdrawn else Ok row
  | Some _ | None -> Error Corrupt
;;

let archive_reference state ~kind ~operation ~revision ~digest =
  let sha256 = Chatml_source.Digest.to_hex digest in
  List.find state.Session_state.conversation.compaction_archives ~f:(fun reference ->
    Archive.equal_kind reference.kind (Archive.of_recovery_source kind)
    && P.Id.Operation.equal reference.operation_id operation
    && Int64.equal reference.revision revision
    && String.equal reference.sha256 sha256)
  |> Result.of_option ~error:Corrupt
;;

let read_archived_state store handle reference ~max_frame_bytes =
  let open Result.Let_syntax in
  (* The frame header and SHA-256 trailer add at most 52 bytes. *)
  let max_bytes = max_frame_bytes + 52 in
  let%bind reader =
    Sessions.retained_reader store ~handle ~max_entries:4 ~max_bytes
    |> Result.map_error ~f:(fun _ -> Denied)
  in
  let path = Filename.concat "archive" (Compaction_archive.filename reference) in
  let%bind bytes =
    Reader.read_promised_file reader ~path ~max_bytes
    |> Result.map_error ~f:(function
      | Store.Store_error.Missing _ -> Missing
      | Admission_capacity _ -> Limit
      | Io _ | Locked _ -> Unavailable
      | Corrupt _ | Document _ | Framing _ | Schema_too_new _ | Migration_required _ ->
        Corrupt)
  in
  Compaction_archive.decode_file
    ~handle
    ~max_payload_length:max_frame_bytes
    reference
    bytes
  |> Result.map_error ~f:(fun _ -> Corrupt)
;;

let archive_row store handle state member (root : Ledger.Source.archive) ~max_frame_bytes =
  let open Result.Let_syntax in
  let%bind reference =
    archive_reference
      state
      ~kind:root.kind
      ~operation:root.operation
      ~revision:root.basis.revision
      ~digest:root.digest
  in
  let%bind archived = read_archived_state store handle reference ~max_frame_bytes in
  if Int.equal archived.identity.generation root.basis.generation
  then lookup archived member
  else Error Corrupt
;;

let current_visibility store handle ~current ~visible =
  let open Result.Let_syntax in
  let owner =
    P.Session_ref.create
      ~server_id:(Sessions.server_id store)
      ~session_id:(Sessions.Handle.session_id handle)
  in
  if not (Sessions.retained_lifecycle_available store handle)
  then Error Denied
  else (
    let%bind state = current () |> Result.map_error ~f:(fun _ -> Denied) in
    if not (Sessions.retained_lifecycle_available store handle)
    then Error Denied
    else if not (visible state)
    then Error Unavailable
    else (
      let%map view =
        Recovery_visibility.create state ~owner |> Result.map_error ~f:(fun _ -> Corrupt)
      in
      state, view))
;;

let with_entry
      ?(visible = fun _ -> true)
      store
      ~handle
      ~current
      ~max_frame_bytes
      citation
      ~f
  =
  let open Result.Let_syntax in
  let select () =
    let%bind state, view = current_visibility store handle ~current ~visible in
    match Recovery_visibility.select view citation with
    | Unavailable -> Error Unavailable
    | Withdrawn -> Error Withdrawn
    | Expired -> Error Expired
    | Eligible member -> Ok (state, member)
  in
  let%bind state, member = select () in
  if max_frame_bytes < 1
  then Error Limit
  else (
    (* A single archive read leaves room within the 64 MiB source-work budget
       for checksumming, typed decoding and the selected output. *)
    let max_frame_bytes = Int.min max_frame_bytes (8 * 1024 * 1024) in
    let row =
      match Member.source member with
      | Live _ -> lookup state member
      | Archive root -> archive_row store handle state member root ~max_frame_bytes
    in
    let recheck () =
      let%bind _, fresh = select () in
      if
        Ledger.Source.equal (Member.source member) (Member.source fresh)
        && Int.equal (Member.source_ordinal member) (Member.source_ordinal fresh)
      then Ok ()
      else Error Stale
    in
    let%bind () = recheck () in
    let%bind row = row in
    let output = f row ~check:recheck in
    let%bind () = recheck () in
    output)
;;

let read ?visible store ~handle ~current ~max_frame_bytes citation ~project =
  with_entry
    ?visible
    store
    ~handle
    ~current
    ~max_frame_bytes
    citation
    ~f:(fun row ~check:_ -> project row)
;;

let read_effective
      ?(visible = fun _ -> true)
      ?check_replaced_source
      store
      ~handle
      ~current
      ~max_frame_bytes
      ~source_revision
      citation
      ~project
  =
  let open Result.Let_syntax in
  let select () =
    let%bind state, view = current_visibility store handle ~current ~visible in
    let%bind record =
      Effective_capture.resolve_record state ~source_revision
      |> Result.of_option ~error:Unavailable
    in
    let%bind member =
      Effective.find_member record citation |> Result.of_option ~error:Unavailable
    in
    let%map () =
      Recovery_visibility.check_effective view citation
      |> Result.map_error ~f:(function
        | `Unavailable -> Unavailable
        | `Withdrawn -> Withdrawn
        | `Expired -> Expired)
    in
    state, record, member
  in
  let%bind state, record, member = select () in
  if max_frame_bytes < 1
  then Error Limit
  else (
    let check_origin (state : Session_state.t) ordinal (row : P.History.entry) =
      match check_replaced_source, row.provenance with
      | Some check, Moderator_replaced id ->
        (match Option.bind ordinal ~f:(List.nth state.conversation.canonical_history) with
         | Some original when P.History.Id.equal original.id id ->
           Result.map (check original) ~f:(fun () -> row)
         | Some _ | None -> Error Corrupt)
      | _ -> Ok row
    in
    let live_row state =
      let%bind row =
        Effective_capture.resolve_live state ~source_revision citation
        |> Result.map_error ~f:(fun _ -> Corrupt)
      in
      let%bind row = Result.of_option row ~error:Unavailable in
      check_origin state (Effective.Member.canonical_ordinal member) row
    in
    let custody = Effective.custody record in
    let row =
      match custody with
      | Live -> live_row state
      | Lost -> Error Unavailable
      | Archived location ->
        let%bind position =
          Effective.Archive.find
            location
            ~request_ordinal:(Effective.Member.request_ordinal member)
          |> Result.of_option ~error:Unavailable
        in
        let%bind reference =
          let module Source = Effective.Archive in
          archive_reference
            state
            ~kind:(Source.kind location)
            ~operation:(Source.operation location)
            ~revision:(Source.revision location)
            ~digest:(Source.digest location)
        in
        let max_frame_bytes = Int.min max_frame_bytes (8 * 1024 * 1024) in
        let%bind archived = read_archived_state store handle reference ~max_frame_bytes in
        let%bind row =
          Effective_capture.resolve_archived archived record citation
          |> Result.map_error ~f:(fun _ -> Corrupt)
        in
        let%bind row = Result.of_option row ~error:Corrupt in
        check_origin archived (Effective.Archive.Position.canonical_ordinal position) row
    in
    let recheck () =
      let%bind fresh_state, fresh_record, _ = select () in
      match custody, Effective.custody fresh_record with
      | Live, Live ->
        if Int64.equal state.counters.revision fresh_state.counters.revision
        then Ok ()
        else live_row fresh_state |> Result.map ~f:ignore
      | Lost, Lost -> Ok ()
      | Archived previous, Archived current when Effective.Archive.equal previous current
        -> Ok ()
      | _ -> Error Stale
    in
    let%bind () = recheck () in
    let%bind row = row in
    let output = project row in
    let%bind () = recheck () in
    output)
;;
