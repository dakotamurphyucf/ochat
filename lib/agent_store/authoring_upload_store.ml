open! Core
module P = Agent_protocol
module B = P.Authoring_bundle.Bundle
module Bundle = B
module Doc = Authoring_upload_document
module Files = Authoring_upload_files
module Publication = Authoring_upload_publication
module Id = P.Id.Authoring_upload

module Limits = struct
  type t =
    { max_active_uploads : int
    ; max_active_per_principal : int
    ; max_members : int
    ; max_manifest_bytes : int
    ; max_member_bytes : int
    ; max_bundle_bytes : int
    ; max_chunk_bytes : int
    ; max_reserved_bytes : int
    ; max_reserved_per_principal : int
    ; max_published_records : int
    ; max_installed_sources : int
    ; max_installed_bytes_per_bundle : int
    ; max_installed_bytes : int
    ; max_installed_per_principal : int
    ; max_publication_metadata_bytes : int
    ; max_intent_bytes : int
    ; max_marker_bytes : int
    ; max_work_steps : int
    ; max_journal_records : int
    ; max_journal_bytes : int
    ; max_idempotency_records : int
    ; max_tombstones : int
    ; max_tombstone_bytes : int
    ; max_recovery_entries : int
    ; max_recovery_metadata_bytes : int
    ; max_recovery_source_bytes : int
    ; staging_ttl_ms : int
    ; receipt_ttl_ms : int
    ; expired_tombstone_ttl_ms : int
    }

  let span_ms span =
    let nanoseconds = Core.Time_ns.Span.to_int63_ns span in
    let maximum = 30 * 24 * 60 * 60 * 1000 * 1000000 in
    if
      Int63.(nanoseconds <= zero || nanoseconds > of_int maximum)
      || not (Int63.equal (Int63.rem nanoseconds (Int63.of_int 1000000)) Int63.zero)
    then Or_error.error_string "upload TTL must be whole milliseconds within 30 days"
    else Or_error.try_with (fun () -> Int63.to_int_exn nanoseconds / 1000000)
  ;;

  let create
        ~max_active_uploads
        ~max_active_per_principal
        ~max_members
        ~max_manifest_bytes
        ~max_member_bytes
        ~max_bundle_bytes
        ~max_chunk_bytes
        ~max_reserved_bytes
        ~max_reserved_per_principal
        ~max_published_records
        ~max_installed_sources
        ~max_installed_bytes_per_bundle
        ~max_installed_bytes
        ~max_installed_per_principal
        ~max_publication_metadata_bytes
        ~max_intent_bytes
        ~max_marker_bytes
        ~max_work_steps
        ~max_journal_records
        ~max_journal_bytes
        ~max_idempotency_records
        ~max_tombstones
        ~max_tombstone_bytes
        ~max_recovery_entries
        ~max_recovery_metadata_bytes
        ~max_recovery_source_bytes
        ~staging_ttl
        ~receipt_ttl
        ~expired_tombstone_ttl
    =
    let open Or_error.Let_syntax in
    let%bind staging_ttl_ms = span_ms staging_ttl in
    let%bind receipt_ttl_ms = span_ms receipt_ttl in
    let%bind expired_tombstone_ttl_ms = span_ms expired_tombstone_ttl in
    let positive values = List.for_all values ~f:(fun value -> value > 0) in
    let safe_per_record extra =
      max_active_uploads > 0
      && max_active_uploads <= 4096
      && max_bundle_bytes > 0
      &&
      let worst_stage =
        max_active_uploads
        * Int.min max_bundle_bytes B.Wire_limits.max_total_declared_bytes
      in
      extra <= (Int.max_value - worst_stage) / max_active_uploads
    in
    if
      (not
         (positive
            [ max_active_uploads
            ; max_active_per_principal
            ; max_members
            ; max_manifest_bytes
            ; max_member_bytes
            ; max_bundle_bytes
            ; max_chunk_bytes
            ; max_reserved_bytes
            ; max_reserved_per_principal
            ; max_published_records
            ; max_installed_sources
            ; max_installed_bytes_per_bundle
            ; max_installed_bytes
            ; max_installed_per_principal
            ; max_publication_metadata_bytes
            ; max_intent_bytes
            ; max_marker_bytes
            ; max_work_steps
            ; max_journal_records
            ; max_journal_bytes
            ; max_idempotency_records
            ; max_tombstones
            ; max_tombstone_bytes
            ; max_recovery_entries
            ; max_recovery_metadata_bytes
            ; max_recovery_source_bytes
            ]))
      || max_members > B.Wire_limits.max_members
      || max_manifest_bytes > B.Wire_limits.max_manifest_bytes
      || max_member_bytes > B.Wire_limits.max_member_bytes
      || max_bundle_bytes > B.Wire_limits.max_total_declared_bytes
      || max_chunk_bytes > B.Wire_limits.max_chunk_bytes
      || max_chunk_bytes > max_member_bytes
      || max_member_bytes > max_bundle_bytes
      || max_bundle_bytes > max_reserved_bytes
      || max_bundle_bytes > max_reserved_per_principal
      || max_installed_bytes_per_bundle > max_installed_bytes
      || max_installed_bytes_per_bundle > max_installed_per_principal
      || (not (safe_per_record max_installed_bytes_per_bundle))
      || (not (safe_per_record max_publication_metadata_bytes))
      || max_intent_bytes > max_publication_metadata_bytes
      || max_marker_bytes > max_publication_metadata_bytes
      || max_marker_bytes > max_publication_metadata_bytes - max_intent_bytes
      || max_published_records > max_active_uploads
      || max_installed_sources > 65536
      || max_recovery_source_bytes - max_reserved_bytes < max_installed_bytes
      || max_recovery_metadata_bytes - max_manifest_bytes < max_publication_metadata_bytes
      || max_active_per_principal > max_active_uploads
      || max_recovery_entries < max_active_uploads
      || max_recovery_source_bytes < max_reserved_bytes
      || max_recovery_metadata_bytes < max_manifest_bytes
      || max_recovery_metadata_bytes > Int.max_value - max_recovery_source_bytes
      || max_active_uploads > 4096
      || max_idempotency_records > 4096
      || max_tombstones > 4096
    then Or_error.error_string "invalid authoring upload store limits"
    else
      Ok
        { max_active_uploads
        ; max_active_per_principal
        ; max_members
        ; max_manifest_bytes
        ; max_member_bytes
        ; max_bundle_bytes
        ; max_chunk_bytes
        ; max_reserved_bytes
        ; max_reserved_per_principal
        ; max_published_records
        ; max_installed_sources
        ; max_installed_bytes_per_bundle
        ; max_installed_bytes
        ; max_installed_per_principal
        ; max_publication_metadata_bytes
        ; max_intent_bytes
        ; max_marker_bytes
        ; max_work_steps
        ; max_journal_records
        ; max_journal_bytes
        ; max_idempotency_records
        ; max_tombstones
        ; max_tombstone_bytes
        ; max_recovery_entries
        ; max_recovery_metadata_bytes
        ; max_recovery_source_bytes
        ; staging_ttl_ms
        ; receipt_ttl_ms
        ; expired_tombstone_ttl_ms
        }
  ;;
end

module Error = struct
  type t =
    | Invalid_manifest
    | Limit_exceeded
    | Idempotency_conflict
    | Invalid_offset
    | Incomplete
    | Sealed
    | Expired
    | Missing_or_foreign
    | Busy
    | Closed
    | Storage of Store_error.t
  [@@deriving sexp_of]
end

type sealed =
  { upload_id : Id.t
  ; token : unit ref
  ; commit_key : P.Idempotency_key.t
  ; parent_context : P.Authoring.Context.Expected.t option
  }

let sealed_parent_context t = t.parent_context

module Snapshot = struct
  type t =
    { manifest : B.Manifest.t
    ; sources : Chatml_source.t list
    ; upload_id : Id.t
    ; principal : P.Id.Principal.t
    ; commit_key : P.Idempotency_key.t
    ; payload_digest : Chatml_source.Digest.t
    ; token : unit ref
    }

  let manifest t = t.manifest
  let sources t = t.sources
end

module Proof = Publication.Proof

let same_source_bytes left right ~checkpoint =
  if
    (not (Chatml_source.Id.equal (Chatml_source.id left) (Chatml_source.id right)))
    || (not
          (Chatml_source.Digest.equal
             (Chatml_source.digest left)
             (Chatml_source.digest right)))
    || Chatml_source.byte_length left <> Chatml_source.byte_length right
  then false
  else (
    let left = Chatml_source.contents left in
    let right = Chatml_source.contents right in
    let same = ref true in
    let offset = ref 0 in
    while !same && !offset < String.length left do
      if !offset mod 4096 = 0 then checkpoint ();
      same := Char.equal left.[!offset] right.[!offset];
      incr offset
    done;
    !same)
;;

let same_source_list left right ~checkpoint =
  List.length left = List.length right
  && List.for_all2_exn left right ~f:(fun left right ->
    same_source_bytes left right ~checkpoint)
;;

module Published_pin = struct
  type t =
    { store_token : unit ref
    ; upload_id : Id.t
    ; principal : P.Id.Principal.t
    ; bundle_id : P.Id.Authoring_bundle.t
    ; receipt : B.Receipt.t
    ; manifest_fingerprint : Chatml_source.Digest.t
    ; deadline : P.Timestamp.t
    ; marker_intent_digest : Chatml_source.Digest.t
    ; marker_inventory_digest : Chatml_source.Digest.t
    }
end

module Published_capture = struct
  module Graph = struct
    type t =
      { selector : B.Manifest.Script.Selector.t option
      ; lock_source : Chatml_source.t
      ; sources : Chatml_source.t list
      }

    let selector t = t.selector
    let lock_source t = t.lock_source
    let sources t = t.sources
  end

  type t =
    { receipt : B.Receipt.t
    ; manifest : B.Manifest.t
    ; uploaded_sources : Chatml_source.t list
    ; graphs : Graph.t list
    ; pin : Published_pin.t
    }

  let receipt t = t.receipt
  let manifest t = t.manifest
  let uploaded_sources t = t.uploaded_sources
  let graphs t = t.graphs
  let pin t = t.pin

  let agrees_with_proof t proof ~checkpoint =
    match Publication.of_proof t.manifest proof ~checkpoint with
    | Error () -> false
    | Ok candidate ->
      B.Receipt.equal_basis (B.Receipt.basis t.receipt) (Publication.basis candidate)
      && List.length t.graphs = List.length (Publication.graphs candidate)
      && List.for_all2_exn
           t.graphs
           (Publication.graphs candidate)
           ~f:(fun saved restored ->
             checkpoint ();
             Option.equal
               B.Manifest.Script.Selector.equal
               saved.Graph.selector
               (Publication.Graph.selector restored)
             && same_source_bytes
                  saved.lock_source
                  (Publication.Graph.lock_source restored)
                  ~checkpoint
             && same_source_list
                  saved.sources
                  (Publication.Graph.sources restored)
                  ~checkpoint)
  ;;
end

type publication =
  | Not_published
  | Intent of
      { value : Doc.Intent.t
      ; intent_bytes : int
      ; marker : Doc.Marker.t option
      ; marker_bytes : int
      ; supplementary_bytes : int
      }

module Claim_result = struct
  type t =
    | Ready of sealed
    | Published of
        { receipt : B.Receipt.t
        ; parent_context : P.Authoring.Context.Expected.t option
        }
    | Rejected of
        { reason : B.Rejection.t
        ; parent_context : P.Authoring.Context.Expected.t option
        }
end

module Status = struct
  type t =
    | Open of B.Status.Progress.t list
    | Ready
    | Validating
    | Published of B.Receipt.t
    | Rejected of B.Rejection.t
    | Expired
end

type upload =
  { manifest_doc : Doc.Manifest.t
  ; manifest_bytes : int
  ; mutable phase : Doc.Phase.t
  ; mutable phase_bytes : int
  ; offsets : int array
  ; progress_bytes : int array
  ; mutable data_bytes : int
  ; mutable publication : publication
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; root_path : string
  ; directory : Eio.Fs.dir_ty Eio.Path.t
  ; daemon_lock : Lock.t
  ; daemon_lock_path : string
  ; host_id : P.Id.Server.t
  ; limits : Limits.t
  ; gate : Eio.Mutex.t
  ; readers_idle : Eio.Condition.t
  ; uploads : (Id.t, upload) Hashtbl.t
  ; read_leases : (Id.t, int) Hashtbl.t
  ; begin_keys : (string, Id.t) Hashtbl.t
  ; token : unit ref
  ; mutable closed : bool
  ; mutable fully_closed : bool
  ; mutable uncertain : bool
  }

let host_id t = t.host_id
let fully_closed t = t.fully_closed
let registry = Atomic.make String.Set.empty

let rec reserve_root path =
  let current = Atomic.get registry in
  if Set.mem current path
  then false
  else if Atomic.compare_and_set registry current (Set.add current path)
  then true
  else reserve_root path
;;

let rec release_root path =
  let current = Atomic.get registry in
  let next = Set.remove current path in
  if not (Atomic.compare_and_set registry current next) then release_root path
;;

let complete_close t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true t.gate (fun () ->
      t.closed <- true;
      while Hashtbl.length t.read_leases > 0 do
        Eio.Condition.await t.readers_idle t.gate
      done;
      if not t.fully_closed
      then (
        release_root t.root_path;
        t.fully_closed <- true);
      Ok ()))
;;

let ( let* ) result f = Result.bind result ~f
let storage result = Result.map_error result ~f:(fun error -> Error.Storage error)
let corrupt text = Error (Error.Storage (Store_error.Corrupt text))
let upload_name id = Id.to_string id
let manifest_max limits = limits.Limits.max_manifest_bytes + 4096
let phase_max = 2048
let progress_max = 512

let check_lock t =
  if
    Lock.is_live t.daemon_lock
    && String.equal (Lock.path t.daemon_lock) t.daemon_lock_path
    && P.Id.Server.equal (Lock.owner t.daemon_lock).server_id t.host_id
  then Ok ()
  else Error Error.Closed
;;

exception Work_limit

let use_gate t f =
  let outcome =
    Eio.Mutex.use_rw ~protect:false t.gate (fun () ->
      match
        if t.closed
        then Error Error.Closed
        else
          let* () = check_lock t in
          f ()
      with
      | result -> `Returned result
      | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | `Returned result -> result
  | `Raised (Work_limit, _) -> Error Error.Limit_exceeded
  | `Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let deadline now milliseconds =
  P.Timestamp.add_ms now milliseconds
  |> Result.map_error ~f:(fun _ -> Error.Limit_exceeded)
;;

let expired now deadline = P.Timestamp.compare now deadline >= 0

let key principal key =
  P.Id.Principal.to_string principal ^ "\000" ^ P.Idempotency_key.to_string key
;;

let with_upload_directory_raw t upload_id f =
  let path = Eio.Path.(t.directory / upload_name upload_id) in
  try
    match Eio.Path.kind ~follow:false path with
    | `Directory -> Eio.Path.with_open_dir path f
    | _ -> Error (Store_error.Corrupt "upload directory is missing or not owned")
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error
      (Store_error.of_exn
         ~operation:"open upload directory"
         ~path:(upload_name upload_id)
         exn)
;;

let with_upload_directory t upload_id f =
  let path = Eio.Path.(t.directory / upload_name upload_id) in
  try
    match Eio.Path.kind ~follow:false path with
    | `Directory -> Eio.Path.with_open_dir path f
    | _ -> corrupt "upload directory is missing or not owned"
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    storage
      (Error
         (Store_error.of_exn
            ~operation:"open upload directory"
            ~path:(upload_name upload_id)
            exn))
;;

let save_in ?(before_rename = Fn.id) directory name bytes =
  Durable_file.replace_in
    ~directory
    ~durability:Flush_file_and_directory
    ~basename:name
    ~before_rename
    bytes
  |> storage
;;

let load_in directory name max_bytes =
  Durable_file.load_bounded_in ~directory ~basename:name ~max_bytes |> storage
;;

let persist_phase t upload_id phase =
  let* bytes = Doc.Phase.to_bytes phase ~max_bytes:phase_max |> storage in
  let* () =
    with_upload_directory t upload_id (fun directory ->
      save_in directory Files.phase_name bytes)
  in
  Ok (String.length bytes)
;;

let member_source upload index =
  let member = List.nth_exn (B.Manifest.members upload.manifest_doc.manifest) index in
  B.Member.source member
;;

let member_count upload = List.length (B.Manifest.members upload.manifest_doc.manifest)

let declared_bytes upload =
  List.fold
    (B.Manifest.members upload.manifest_doc.manifest)
    ~init:0
    ~f:(fun sum member -> sum + P.Authoring.Source.byte_length (B.Member.source member))
;;

let phase_deadline upload =
  match upload.phase.outcome with
  | Doc.Phase.Open -> upload.manifest_doc.stage_deadline
  | Ready { receipt_deadline; _ } | Rejected { receipt_deadline; _ } -> receipt_deadline
  | Expired { tombstone_deadline } -> tombstone_deadline
;;

let active upload =
  match upload.phase.outcome with
  | Doc.Phase.Open | Ready _ -> true
  | Rejected _ | Expired _ -> false
;;

let find_owned t principal upload_id =
  match Hashtbl.find t.uploads upload_id with
  | Some upload when P.Id.Principal.equal upload.manifest_doc.owner.principal principal ->
    Ok upload
  | Some _ | None -> Error Error.Missing_or_foreign
;;

let work limits checkpoint =
  let steps = ref 0 in
  fun () ->
    if !steps >= limits.Limits.max_work_steps then raise Work_limit;
    incr steps;
    checkpoint ()
;;

let installed_bytes upload =
  match upload.publication with
  | Not_published -> 0
  | Intent { supplementary_bytes; _ } -> supplementary_bytes
;;

let publication_metadata upload =
  match upload.publication with
  | Not_published -> 0
  | Intent { intent_bytes; marker_bytes; _ } -> intent_bytes + marker_bytes
;;

let reserved_publication_metadata limits upload =
  match upload.publication with
  | Not_published -> 0
  | Intent { intent_bytes; marker; marker_bytes; _ } ->
    intent_bytes
    +
      (match marker with
      | None -> limits.Limits.max_marker_bytes
      | Some _ -> marker_bytes)
;;

let is_published upload =
  match upload.publication with
  | Intent { marker = Some _; _ } -> true
  | Not_published | Intent { marker = None; _ } -> false
;;

let read_lease_count t upload_id =
  Hashtbl.find t.read_leases upload_id |> Option.value ~default:0
;;

let metadata_bytes upload =
  upload.manifest_bytes
  + upload.phase_bytes
  + Array.fold upload.progress_bytes ~init:0 ~f:( + )
  + publication_metadata upload
;;

let journal_records upload =
  1 + Array.count upload.progress_bytes ~f:(fun length -> length > 0)
;;

let journal_bytes upload =
  upload.phase_bytes + Array.fold upload.progress_bytes ~init:0 ~f:( + )
;;

let reserved_journal_records upload =
  if active upload then 1 + member_count upload else journal_records upload
;;

let reserved_journal_bytes upload =
  phase_max
  +
  if active upload
  then member_count upload * progress_max
  else Array.fold upload.progress_bytes ~init:0 ~f:( + )
;;

let potential_metadata_bytes limits upload =
  upload.manifest_bytes
  + reserved_journal_bytes upload
  + reserved_publication_metadata limits upload
;;

let manifest_within_limits limits manifest =
  let members = B.Manifest.members manifest in
  List.length members <= limits.Limits.max_members
  && List.fold members ~init:0 ~f:(fun total member ->
       total + P.Authoring.Source.byte_length (B.Member.source member))
     <= limits.max_bundle_bytes
  && List.for_all members ~f:(fun member ->
    P.Authoring.Source.byte_length (B.Member.source member) <= limits.max_member_bytes)
;;

let counts t =
  Hashtbl.fold
    t.uploads
    ~init:(0, 0, 0, 0, 0, 0)
    ~f:
      (fun
        ~key:_
        ~data:upload
        ( active_count
        , reserved_bytes
        , journal_count
        , journal_bytes_total
        , tombstones
        , tombstone_bytes )
      ->
      let is_active = active upload in
      let reserved =
        (if is_active then declared_bytes upload else upload.data_bytes)
        + installed_bytes upload
      in
      let is_tombstone =
        match upload.phase.outcome with
        | Doc.Phase.Expired _ -> true
        | Open | Ready _ | Rejected _ -> false
      in
      ( (active_count + if is_active then 1 else 0)
      , reserved_bytes + reserved
      , journal_count + journal_records upload
      , journal_bytes_total + journal_bytes upload
      , (tombstones + if is_tombstone then 1 else 0)
      , tombstone_bytes + if is_tombstone then metadata_bytes upload else 0 ))
;;

let count_principal t principal =
  Hashtbl.fold
    t.uploads
    ~init:(0, 0)
    ~f:(fun ~key:_ ~data:upload (active_count, reserved) ->
      if P.Id.Principal.equal upload.manifest_doc.owner.principal principal
      then (
        let is_active = active upload in
        ( (active_count + if is_active then 1 else 0)
        , reserved
          + (if is_active then declared_bytes upload else upload.data_bytes)
          + installed_bytes upload ))
      else active_count, reserved)
;;

let ensure_begin_capacity t principal manifest_bytes declared member_count =
  let active_count, reserved, journals, _, _, _ = counts t in
  let future_records =
    Hashtbl.fold t.uploads ~init:0 ~f:(fun ~key:_ ~data:upload total ->
      total + reserved_journal_records upload)
  in
  let retained_metadata =
    Hashtbl.fold t.uploads ~init:0 ~f:(fun ~key:_ ~data:upload total ->
      total + potential_metadata_bytes t.limits upload)
  in
  let reserved_journal =
    Hashtbl.fold t.uploads ~init:0 ~f:(fun ~key:_ ~data:upload total ->
      total + reserved_journal_bytes upload)
  in
  let principal_active, principal_reserved = count_principal t principal in
  let within used addition max = addition <= max - used in
  if
    active_count >= t.limits.max_active_uploads
    || principal_active >= t.limits.max_active_per_principal
    || Hashtbl.length t.uploads >= t.limits.max_idempotency_records
    || Hashtbl.length t.uploads >= t.limits.max_tombstones
    || (not
          (within
             retained_metadata
             (manifest_bytes + phase_max + (member_count * progress_max))
             t.limits.max_tombstone_bytes))
    || (not (within reserved declared t.limits.max_reserved_bytes))
    || (not (within principal_reserved declared t.limits.max_reserved_per_principal))
    || journals >= t.limits.max_journal_records
    || (not (within future_records (1 + member_count) t.limits.max_journal_records))
    || (not
          (within
             reserved_journal
             (phase_max + (member_count * progress_max))
             t.limits.max_journal_bytes))
    || manifest_bytes > t.limits.max_manifest_bytes
  then Error Error.Limit_exceeded
  else Ok ()
;;

let progress_document upload_id index offset =
  Doc.Progress.{ upload_id; member_index = index; accepted_until_byte = offset }
;;

let verify_members t upload ~materialize ~checkpoint =
  let digest_context = ref Digestif.SHA256.empty in
  let sources = ref [] in
  let* () =
    with_upload_directory t upload.manifest_doc.upload_id (fun directory ->
      List.foldi
        (B.Manifest.members upload.manifest_doc.manifest)
        ~init:(Ok ())
        ~f:(fun index result member ->
          let* () = result in
          checkpoint ();
          let source = B.Member.source member in
          let expected = P.Authoring.Source.byte_length source in
          if upload.offsets.(index) <> expected
          then Error Error.Incomplete
          else
            let* bytes =
              Files.file_bytes directory index ~max_bytes:expected ~checkpoint |> storage
            in
            let actual = Chatml_source.Digest.of_contents bytes ~checkpoint in
            if not (Chatml_source.Digest.equal actual (P.Authoring.Source.digest source))
            then Error Error.Invalid_manifest
            else (
              digest_context := Digestif.SHA256.feed_string !digest_context bytes;
              if materialize
              then (
                let* captured =
                  Chatml_source.create
                    (P.Authoring.Source.id source)
                    ~contents:bytes
                    ~expected_digest:(Some actual)
                    ~max_bytes:(Some t.limits.max_member_bytes)
                    ~checkpoint
                  |> Result.map_error ~f:(fun _ -> Error.Invalid_manifest)
                in
                sources := captured :: !sources;
                Ok ())
              else Ok ())))
  in
  let digest =
    Digestif.SHA256.(get !digest_context |> to_hex)
    |> Chatml_source.Digest.of_hex
    |> Or_error.ok_exn
  in
  Ok (digest, List.rev !sources)
;;

let source_matches_metadata source metadata =
  Chatml_source.Id.equal (Chatml_source.id source) (P.Authoring.Source.id metadata)
  && Chatml_source.Digest.equal
       (Chatml_source.digest source)
       (P.Authoring.Source.digest metadata)
  && Int.equal
       (Chatml_source.byte_length source)
       (P.Authoring.Source.byte_length metadata)
;;

let digest_text bytes ~checkpoint = Chatml_source.Digest.of_contents bytes ~checkpoint

let digest_receipt receipt ~checkpoint =
  B.Receipt.to_json receipt
  |> Jsonaf.to_string
  |> fun bytes -> digest_text bytes ~checkpoint
;;

let feed_checked context text ~checkpoint =
  let context = ref context in
  let offset = ref 0 in
  while !offset < String.length text do
    checkpoint ();
    let length = Int.min 4096 (String.length text - !offset) in
    context
    := Digestif.SHA256.feed_string !context (String.sub text ~pos:!offset ~len:length);
    offset := !offset + length
  done;
  !context
;;

let digest_context context =
  Digestif.SHA256.(get context |> to_hex)
  |> Chatml_source.Digest.of_hex
  |> Or_error.ok_exn
;;

let verify_installed t upload intent sources ~checkpoint =
  let uploaded = Array.of_list sources in
  let member_count = Array.length uploaded in
  let supplementary = Doc.Intent.supplementary intent in
  let* () =
    if List.length supplementary > t.limits.max_installed_sources
    then Error Error.Limit_exceeded
    else Ok ()
  in
  let* installed =
    with_upload_directory t upload.manifest_doc.upload_id (fun directory ->
      List.fold
        supplementary
        ~init:(Ok (Int.Map.empty, -1, 0))
        ~f:(fun result (ordinal, metadata) ->
          let* map, prior, bytes_total = result in
          checkpoint ();
          if ordinal <> prior + 1
          then Error Error.Invalid_manifest
          else (
            let length = P.Authoring.Source.byte_length metadata in
            if length > t.limits.max_installed_bytes_per_bundle - bytes_total
            then Error Error.Limit_exceeded
            else
              let* bytes =
                Files.read_supplemental
                  directory
                  ~ordinal
                  ~expected_bytes:length
                  ~checkpoint
                |> storage
              in
              let digest = digest_text bytes ~checkpoint in
              if
                not
                  (Chatml_source.Digest.equal digest (P.Authoring.Source.digest metadata))
              then Error Error.Invalid_manifest
              else Ok (Map.set map ~key:ordinal ~data:bytes, ordinal, bytes_total + length))))
  in
  let installed, _, installed_bytes = installed in
  let check_reference source_ref =
    let metadata = Doc.Intent.Source_ref.source source_ref in
    match Doc.Intent.Source_ref.location source_ref with
    | Uploaded_member ordinal ->
      ordinal >= 0
      && ordinal < member_count
      && source_matches_metadata uploaded.(ordinal) metadata
    | Supplemental_file ordinal ->
      (match Map.find installed ordinal with
       | None -> false
       | Some bytes ->
         Int.equal (String.length bytes) (P.Authoring.Source.byte_length metadata)
         && Chatml_source.Digest.equal
              (digest_text bytes ~checkpoint)
              (P.Authoring.Source.digest metadata))
  in
  let* () =
    if
      List.for_all (Doc.Intent.graphs intent) ~f:(fun graph ->
        checkpoint ();
        check_reference (Doc.Intent.Graph.lock graph)
        && List.for_all (Doc.Intent.Graph.sources graph) ~f:(fun source ->
          checkpoint ();
          check_reference source))
    then Ok ()
    else Error Error.Invalid_manifest
  in
  let context =
    feed_checked
      Digestif.SHA256.empty
      (Chatml_source.Digest.to_hex
         (match upload.phase.outcome with
          | Doc.Phase.Ready { payload_digest; _ } -> payload_digest
          | Open | Rejected _ | Expired _ -> assert false))
      ~checkpoint
  in
  let context =
    Map.fold installed ~init:context ~f:(fun ~key:ordinal ~data:bytes context ->
      let context =
        feed_checked
          context
          (Int.to_string ordinal ^ ":" ^ Int.to_string (String.length bytes) ^ "\000")
          ~checkpoint
      in
      feed_checked context bytes ~checkpoint)
  in
  Ok (digest_context context, installed_bytes)
;;

let intent_matches_upload upload intent =
  match upload.phase.outcome with
  | Doc.Phase.Ready { commit_key; payload_digest; receipt_deadline } ->
    B.Owner.equal (Doc.Intent.owner intent) upload.manifest_doc.owner
    && Id.equal (Doc.Intent.upload_id intent) upload.manifest_doc.upload_id
    && P.Idempotency_key.equal (Doc.Intent.commit_key intent) commit_key
    && Chatml_source.Digest.equal (Doc.Intent.payload_digest intent) payload_digest
    && Chatml_source.Digest.equal
         (Doc.Intent.manifest_fingerprint intent)
         (B.Manifest.fingerprint upload.manifest_doc.manifest)
    && P.Timestamp.equal
         (B.Receipt.expires_at (Doc.Intent.receipt intent))
         receipt_deadline
  | Open | Rejected _ | Expired _ -> false
;;

let verify_publication_metadata t upload intent marker ~checkpoint =
  let* encoded_intent, encoded_marker =
    with_upload_directory t upload.manifest_doc.upload_id (fun directory ->
      let* encoded_intent =
        load_in directory Files.intent_name t.limits.max_intent_bytes
      in
      let* encoded_marker =
        load_in directory Files.marker_name t.limits.max_marker_bytes
      in
      Ok (encoded_intent, encoded_marker))
  in
  let* canonical_intent =
    Doc.Intent.to_bytes intent ~max_bytes:t.limits.max_intent_bytes |> storage
  in
  let* canonical_marker =
    Doc.Marker.to_bytes marker ~max_bytes:t.limits.max_marker_bytes |> storage
  in
  let receipt = Doc.Intent.receipt intent in
  if
    (not (String.equal encoded_intent canonical_intent))
    || (not (String.equal encoded_marker canonical_marker))
    || (not
          (Chatml_source.Digest.equal
             (Doc.Marker.intent_digest marker)
             (digest_text encoded_intent ~checkpoint)))
    || not
         (Chatml_source.Digest.equal
            (Doc.Marker.receipt_digest marker)
            (digest_receipt receipt ~checkpoint))
  then Error Error.Invalid_manifest
  else Ok ()
;;

let verify_publication t upload ~checkpoint =
  match upload.publication with
  | Not_published | Intent { marker = None; _ } -> Error Error.Sealed
  | Intent { value = intent; marker = Some marker; supplementary_bytes; _ } ->
    if not (intent_matches_upload upload intent)
    then Error Error.Invalid_manifest
    else
      let* payload, sources = verify_members t upload ~materialize:true ~checkpoint in
      if not (Chatml_source.Digest.equal payload (Doc.Intent.payload_digest intent))
      then Error Error.Invalid_manifest
      else
        let* inventory_digest, actual_bytes =
          verify_installed t upload intent sources ~checkpoint
        in
        if actual_bytes <> supplementary_bytes
        then Error Error.Invalid_manifest
        else
          let* () = verify_publication_metadata t upload intent marker ~checkpoint in
          let receipt = Doc.Intent.receipt intent in
          if
            not
              (Chatml_source.Digest.equal
                 (Doc.Marker.inventory_digest marker)
                 inventory_digest)
          then Error Error.Invalid_manifest
          else Ok receipt
;;

let materialize_published t upload published_receipt ~checkpoint =
  let* _, uploaded_captures = verify_members t upload ~materialize:true ~checkpoint in
  let uploaded = Array.of_list uploaded_captures in
  match upload.publication with
  | Not_published | Intent { marker = None; _ } -> Error Error.Sealed
  | Intent { value = intent; marker = Some marker; _ } ->
    let* published_graphs =
      with_upload_directory t upload.manifest_doc.upload_id (fun directory ->
        let* supplemental =
          List.fold
            (Doc.Intent.supplementary intent)
            ~init:(Ok Int.Map.empty)
            ~f:(fun result (ordinal, metadata) ->
              let* values = result in
              checkpoint ();
              let* bytes =
                Files.read_supplemental
                  directory
                  ~ordinal
                  ~expected_bytes:(P.Authoring.Source.byte_length metadata)
                  ~checkpoint
                |> storage
              in
              let* source =
                Chatml_source.create
                  (P.Authoring.Source.id metadata)
                  ~contents:bytes
                  ~expected_digest:(Some (P.Authoring.Source.digest metadata))
                  ~max_bytes:(Some t.limits.max_installed_bytes_per_bundle)
                  ~checkpoint
                |> Result.map_error ~f:(fun _ -> Error.Invalid_manifest)
              in
              Ok (Map.set values ~key:ordinal ~data:source))
        in
        let source_of_ref reference =
          let metadata = Doc.Intent.Source_ref.source reference in
          let found =
            match Doc.Intent.Source_ref.location reference with
            | Uploaded_member ordinal ->
              if ordinal < 0 || ordinal >= Array.length uploaded
              then None
              else Some uploaded.(ordinal)
            | Supplemental_file ordinal -> Map.find supplemental ordinal
          in
          match found with
          | Some source when source_matches_metadata source metadata -> Ok source
          | Some _ | None -> Error Error.Invalid_manifest
        in
        List.fold (Doc.Intent.graphs intent) ~init:(Ok []) ~f:(fun result graph ->
          let* graphs = result in
          checkpoint ();
          let* lock_capture = source_of_ref (Doc.Intent.Graph.lock graph) in
          let* graph_sources =
            List.map (Doc.Intent.Graph.sources graph) ~f:source_of_ref |> Result.all
          in
          Ok
            (Published_capture.Graph.
               { selector = Doc.Intent.Graph.selector graph
               ; lock_source = lock_capture
               ; sources = graph_sources
               }
             :: graphs)))
    in
    Ok
      Published_capture.
        { receipt = published_receipt
        ; manifest = upload.manifest_doc.manifest
        ; uploaded_sources = uploaded_captures
        ; graphs = List.rev published_graphs
        ; pin =
            { Published_pin.store_token = t.token
            ; upload_id = upload.manifest_doc.upload_id
            ; principal = upload.manifest_doc.owner.principal
            ; bundle_id = B.Receipt.bundle_id published_receipt
            ; receipt = published_receipt
            ; manifest_fingerprint = B.Manifest.fingerprint upload.manifest_doc.manifest
            ; deadline = B.Receipt.expires_at published_receipt
            ; marker_intent_digest = Doc.Marker.intent_digest marker
            ; marker_inventory_digest = Doc.Marker.inventory_digest marker
            }
        }
;;

let verify_partial_install t upload intent names ~checkpoint =
  with_upload_directory t upload.manifest_doc.upload_id (fun directory ->
    List.fold
      (Doc.Intent.supplementary intent)
      ~init:(Ok ())
      ~f:(fun result (ordinal, metadata) ->
        let* () = result in
        checkpoint ();
        let filename = Files.supplemental_name ordinal in
        if not (List.mem names filename ~equal:String.equal)
        then Ok ()
        else
          let* bytes =
            Files.read_supplemental
              directory
              ~ordinal
              ~expected_bytes:(P.Authoring.Source.byte_length metadata)
              ~checkpoint
            |> storage
          in
          let actual = digest_text bytes ~checkpoint in
          if Chatml_source.Digest.equal actual (P.Authoring.Source.digest metadata)
          then Ok ()
          else Error Error.Invalid_manifest))
;;

let pruning_name upload_id = ".prune-" ^ upload_name upload_id

let cleanup_pruning_directory t reader name ~checkpoint =
  let* () =
    match String.chop_prefix name ~prefix:".prune-" with
    | Some suffix ->
      (match Id.of_string suffix with
       | Ok upload_id when String.equal suffix (Id.to_string upload_id) -> Ok ()
       | _ -> corrupt "invalid upload cleanup identity")
    | None -> corrupt "unknown upload cleanup directory"
  in
  let* kind = Retention_reader.kind reader ~path:name |> storage in
  let* () =
    match kind with
    | `Directory -> Ok ()
    | `File -> corrupt "upload cleanup marker is not a directory"
  in
  let* files =
    Retention_reader.list_with_checkpoint reader ~directory:name ~checkpoint |> storage
  in
  let valid_file file =
    let target = Option.value (Durable_file.temporary_target file) ~default:file in
    String.equal target Files.manifest_name
    || String.equal target Files.phase_name
    || Files.is_member_filename target
    || Files.is_publication_filename target
  in
  if not (List.for_all files ~f:valid_file)
  then corrupt "unknown file in upload cleanup directory"
  else (
    let directory_path = Eio.Path.(t.directory / name) in
    let* () =
      try
        Eio.Path.with_open_dir directory_path (fun directory ->
          let* () =
            List.fold files ~init:(Ok ()) ~f:(fun result file ->
              let* () = result in
              checkpoint ();
              let* kind =
                Retention_reader.kind reader ~path:(name ^ "/" ^ file) |> storage
              in
              match kind with
              | `Directory -> corrupt "nested upload cleanup entry"
              | `File ->
                Eio.Path.unlink Eio.Path.(directory / file);
                Ok ())
          in
          Durable_file.sync_directory_in ~directory |> storage)
      with
      | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
        storage
          (Error (Store_error.of_exn ~operation:"cleanup pruned upload" ~path:name exn))
    in
    try
      Eio.Path.rmdir directory_path;
      Durable_file.sync_directory_in ~directory:t.directory |> storage
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      storage
        (Error (Store_error.of_exn ~operation:"finish pruned upload" ~path:name exn)))
;;

let charge_recovery_source remaining bytes =
  if bytes < 0 || bytes > !remaining
  then Error (Store_error.Corrupt "recovered source inventory exceeds configured bounds")
  else (
    remaining := !remaining - bytes;
    Ok ())
;;

let recover_publication t reader name upload names ~remaining_source ~checkpoint =
  let has file = List.mem names file ~equal:String.equal in
  let relative file = name ^ "/" ^ file in
  let members = B.Manifest.members upload.manifest_doc.manifest in
  match upload.phase.outcome with
  | Doc.Phase.Expired _ ->
    let valid name =
      let target = Option.value (Durable_file.temporary_target name) ~default:name in
      String.equal target Files.manifest_name
      || String.equal target Files.phase_name
      || Files.is_member_filename target
      || Files.is_publication_filename target
    in
    if List.for_all names ~f:valid
    then Ok Not_published
    else Error (Store_error.Corrupt "unknown file in expired upload")
  | Open | Rejected _ ->
    if
      has Files.intent_name
      || has Files.marker_name
      || List.exists names ~f:Files.is_publication_filename
      || not (Files.validate_entry_names members names)
    then Error (Store_error.Corrupt "unsealed upload has publication state")
    else Ok Not_published
  | Ready _ ->
    let* intent =
      if not (has Files.intent_name)
      then Ok None
      else
        let* encoded =
          Retention_reader.read_with_checkpoint
            reader
            ~path:(relative Files.intent_name)
            ~max_bytes:t.limits.max_intent_bytes
            ~checkpoint
        in
        let* value =
          Doc.Intent.of_bytes
            encoded
            ~max_bytes:t.limits.max_intent_bytes
            ~max_graphs:(t.limits.max_members + 1)
            ~max_sources:t.limits.max_installed_sources
        in
        let* canonical = Doc.Intent.to_bytes value ~max_bytes:t.limits.max_intent_bytes in
        if String.equal encoded canonical && intent_matches_upload upload value
        then Ok (Some (value, String.length encoded))
        else Error (Store_error.Corrupt "published intent differs from sealed upload")
    in
    let* marker =
      if not (has Files.marker_name)
      then Ok None
      else
        let* encoded =
          Retention_reader.read_with_checkpoint
            reader
            ~path:(relative Files.marker_name)
            ~max_bytes:t.limits.max_marker_bytes
            ~checkpoint
        in
        let* value = Doc.Marker.of_bytes encoded ~max_bytes:t.limits.max_marker_bytes in
        let* canonical = Doc.Marker.to_bytes value ~max_bytes:t.limits.max_marker_bytes in
        if String.equal encoded canonical
        then Ok (Some (value, String.length encoded))
        else Error (Store_error.Corrupt "noncanonical publication marker")
    in
    let intent_value = Option.map intent ~f:fst in
    let marker_value = Option.map marker ~f:fst in
    if
      not
        (Files.validate_entry_names
           ?intent:intent_value
           ?marker:marker_value
           members
           names)
    then Error (Store_error.Corrupt "unknown published upload file")
    else (
      match intent, marker with
      | None, None -> Ok Not_published
      | None, Some _ -> Error (Store_error.Corrupt "publication marker lacks intent")
      | Some (value, intent_bytes), marker ->
        let supplementary_bytes = Doc.Intent.supplementary_bytes value in
        if
          Doc.Intent.supplementary_count value > t.limits.max_installed_sources
          || supplementary_bytes > t.limits.max_installed_bytes_per_bundle
        then Error (Store_error.Corrupt "publication exceeds configured source bounds")
        else
          let* () = charge_recovery_source remaining_source supplementary_bytes in
          let marker_value = Option.map marker ~f:fst in
          let marker_bytes = Option.value_map marker ~default:0 ~f:snd in
          let state =
            Intent
              { value
              ; intent_bytes
              ; marker = marker_value
              ; marker_bytes
              ; supplementary_bytes
              }
          in
          upload.publication <- state;
          let result =
            match marker_value with
            | None -> verify_partial_install t upload value names ~checkpoint
            | Some _ -> verify_publication t upload ~checkpoint |> Result.map ~f:ignore
          in
          Result.map_error result ~f:(fun _ ->
            Store_error.Corrupt "published upload capture verification failed")
          |> Result.map ~f:(fun () -> state))
;;

let recover_upload t reader name ~remaining_source ~checkpoint =
  let upload_id =
    match Id.of_string name with
    | Ok id when String.equal name (Id.to_string id) -> Ok id
    | _ -> Error (Store_error.Corrupt "unknown upload directory name")
  in
  let* upload_id = upload_id in
  let* kind = Retention_reader.kind reader ~path:name in
  let* () =
    match kind with
    | `Directory -> Ok ()
    | `File -> Error (Store_error.Corrupt "upload entry is not a directory")
  in
  let* names = Retention_reader.list_with_checkpoint reader ~directory:name ~checkpoint in
  let relative file = name ^ "/" ^ file in
  let has file = List.mem names file ~equal:String.equal in
  if not (has Files.manifest_name)
  then
    if
      List.for_all names ~f:(fun name ->
        Option.value_map
          (Durable_file.temporary_target name)
          ~default:false
          ~f:(String.equal Files.manifest_name))
    then
      with_upload_directory_raw t upload_id (fun directory ->
        let* () = Files.remove_temporary_files directory names in
        try
          Eio.Path.rmdir Eio.Path.(t.directory / name);
          Durable_file.sync_directory_in ~directory:t.directory
        with
        | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
          Error
            (Store_error.of_exn ~operation:"discard unacknowledged upload" ~path:name exn))
      |> Result.map ~f:(fun () -> None)
    else Error (Store_error.Corrupt "missing authoritative upload manifest")
  else
    let* bytes =
      Retention_reader.read_with_checkpoint
        reader
        ~path:(relative Files.manifest_name)
        ~max_bytes:(manifest_max t.limits)
        ~checkpoint
    in
    let* manifest_doc = Doc.Manifest.of_bytes bytes ~max_bytes:(manifest_max t.limits) in
    if
      (not (Id.equal upload_id manifest_doc.upload_id))
      || not (P.Id.Server.equal t.host_id manifest_doc.owner.host)
    then Error (Store_error.Corrupt "upload manifest owner or identity differs")
    else if
      String.length bytes > t.limits.max_manifest_bytes
      || not (manifest_within_limits t.limits manifest_doc.manifest)
    then Error (Store_error.Corrupt "recovered upload exceeds configured manifest bounds")
    else (
      let members = B.Manifest.members manifest_doc.manifest in
      if (not (has Files.phase_name)) && not (Files.validate_entry_names members names)
      then Error (Store_error.Corrupt "unknown upload member or record")
      else if not (has Files.phase_name)
      then (
        let allowed =
          List.for_all names ~f:(fun entry ->
            String.equal entry Files.manifest_name
            || Option.value_map
                 (Durable_file.temporary_target entry)
                 ~default:false
                 ~f:(String.equal Files.phase_name))
        in
        if not allowed
        then Error (Store_error.Corrupt "incomplete begin has unexpected member data")
        else (
          let phase = Doc.Phase.{ upload_id; outcome = Open } in
          let* phase_bytes = Doc.Phase.to_bytes phase ~max_bytes:phase_max in
          let* () =
            with_upload_directory_raw t upload_id (fun directory ->
              let* () = Files.remove_temporary_files directory names in
              Durable_file.replace_in
                ~directory
                ~durability:Flush_file_and_directory
                ~basename:Files.phase_name
                phase_bytes)
          in
          Ok
            (Some
               { manifest_doc
               ; manifest_bytes = String.length bytes
               ; phase
               ; phase_bytes = String.length phase_bytes
               ; offsets = Array.create ~len:(List.length members) 0
               ; progress_bytes = Array.create ~len:(List.length members) 0
               ; data_bytes = 0
               ; publication = Not_published
               })))
      else
        let* phase_encoded =
          Retention_reader.read_with_checkpoint
            reader
            ~path:(relative Files.phase_name)
            ~max_bytes:phase_max
            ~checkpoint
        in
        let* phase = Doc.Phase.of_bytes phase_encoded ~max_bytes:phase_max in
        if not (Id.equal phase.upload_id upload_id)
        then Error (Store_error.Corrupt "upload phase identity differs")
        else
          let* () =
            match phase.outcome with
            | Ready _ ->
              charge_recovery_source
                remaining_source
                (List.fold members ~init:0 ~f:(fun total member ->
                   total + P.Authoring.Source.byte_length (B.Member.source member)))
            | Open | Rejected _ | Expired _ -> Ok ()
          in
          let offsets = Array.create ~len:(List.length members) 0 in
          let progress_bytes = Array.create ~len:(List.length members) 0 in
          let* () =
            List.foldi members ~init:(Ok ()) ~f:(fun index result member ->
              let* () = result in
              checkpoint ();
              let expected = P.Authoring.Source.byte_length (B.Member.source member) in
              if has (Files.progress_name index)
              then
                let* encoded =
                  Retention_reader.read_with_checkpoint
                    reader
                    ~path:(relative (Files.progress_name index))
                    ~max_bytes:progress_max
                    ~checkpoint
                in
                let* progress = Doc.Progress.of_bytes encoded ~max_bytes:progress_max in
                if
                  (not (Id.equal upload_id progress.upload_id))
                  || progress.member_index <> index
                  || progress.accepted_until_byte > expected
                then Error (Store_error.Corrupt "upload progress differs from member")
                else (
                  offsets.(index) <- progress.accepted_until_byte;
                  progress_bytes.(index) <- String.length encoded;
                  Ok ())
              else Ok ())
          in
          let upload =
            { manifest_doc
            ; manifest_bytes = String.length bytes
            ; phase
            ; phase_bytes = String.length phase_encoded
            ; offsets
            ; progress_bytes
            ; data_bytes = Array.fold offsets ~init:0 ~f:( + )
            ; publication = Not_published
            }
          in
          let* () =
            with_upload_directory_raw t upload_id (fun directory ->
              match phase.outcome with
              | Doc.Phase.Expired _ -> Files.cleanup_member_files directory names
              | Open | Ready _ | Rejected _ ->
                let* () =
                  Array.foldi offsets ~init:(Ok ()) ~f:(fun index result accepted ->
                    let* () = result in
                    checkpoint ();
                    Files.truncate_suffix directory index accepted)
                in
                Files.remove_temporary_files directory names)
          in
          (match phase.outcome with
           | Doc.Phase.Expired _ ->
             Array.fill offsets ~pos:0 ~len:(Array.length offsets) 0;
             Array.fill progress_bytes ~pos:0 ~len:(Array.length progress_bytes) 0;
             upload.data_bytes <- 0
           | Open | Ready _ | Rejected _ -> ());
          let* publication =
            recover_publication t reader name upload names ~remaining_source ~checkpoint
          in
          upload.publication <- publication;
          Ok (Some upload))
;;

let check_recovered_limits t ~checkpoint =
  let active_count, reserved, journals, journal_bytes, tombstones, tombstone_bytes =
    counts t
  in
  let retained_metadata =
    Hashtbl.fold t.uploads ~init:0 ~f:(fun ~key:_ ~data:upload total ->
      total + potential_metadata_bytes t.limits upload)
  in
  let reserved_journal =
    Hashtbl.fold t.uploads ~init:0 ~f:(fun ~key:_ ~data:upload total ->
      total + reserved_journal_bytes upload)
  in
  let future_records =
    Hashtbl.fold t.uploads ~init:0 ~f:(fun ~key:_ ~data:upload total ->
      total + reserved_journal_records upload)
  in
  let within used added limit =
    used >= 0 && added >= 0 && used <= limit && added <= limit - used
  in
  let published_ids = Hash_set.create (module P.Id.Authoring_bundle) in
  let* published, installed, publication_metadata =
    Hashtbl.fold
      t.uploads
      ~init:(Ok (0, 0, 0))
      ~f:(fun ~key:_ ~data:upload result ->
        let* published, installed, metadata = result in
        checkpoint ();
        let occupied =
          match upload.publication with
          | Not_published -> 0
          | Intent { value; _ } ->
            let id = B.Receipt.bundle_id (Doc.Intent.receipt value) in
            if Hash_set.mem published_ids id
            then -1
            else (
              Hash_set.add published_ids id;
              1)
        in
        let supplement = installed_bytes upload in
        let record_metadata = reserved_publication_metadata t.limits upload in
        if
          occupied < 0
          || (not (within published occupied t.limits.max_published_records))
          || (not (within installed supplement t.limits.max_installed_bytes))
          || not (within metadata record_metadata t.limits.max_publication_metadata_bytes)
        then corrupt "recovered publication exceeds configured bounds"
        else Ok (published + occupied, installed + supplement, metadata + record_metadata))
  in
  ignore (published, installed, publication_metadata);
  if
    active_count > t.limits.max_active_uploads
    || reserved > t.limits.max_reserved_bytes
    || journals > t.limits.max_journal_records
    || future_records > t.limits.max_journal_records
    || journal_bytes > t.limits.max_journal_bytes
    || reserved_journal > t.limits.max_journal_bytes
    || tombstones > t.limits.max_tombstones
    || tombstone_bytes > t.limits.max_tombstone_bytes
    || retained_metadata > t.limits.max_tombstone_bytes
    || Hashtbl.length t.uploads > t.limits.max_tombstones
    || Hashtbl.length t.uploads > t.limits.max_idempotency_records
  then corrupt "recovered upload quota exceeds configured bounds"
  else (
    let per_principal = Hashtbl.create (module P.Id.Principal) in
    Hashtbl.fold t.uploads ~init:(Ok ()) ~f:(fun ~key:_ ~data:upload result ->
      let* () = result in
      checkpoint ();
      let principal = upload.manifest_doc.owner.principal in
      let previous_active, previous_reserved, previous_installed =
        Hashtbl.find per_principal principal |> Option.value ~default:(0, 0, 0)
      in
      let is_active = if active upload then 1 else 0 in
      let supplement = installed_bytes upload in
      let stage = if active upload then declared_bytes upload else upload.data_bytes in
      if
        (not (within previous_active is_active t.limits.max_active_per_principal))
        || (not
              (within previous_installed supplement t.limits.max_installed_per_principal))
        || (not (within previous_reserved stage t.limits.max_reserved_per_principal))
        || not
             (within
                (previous_reserved + stage)
                supplement
                t.limits.max_reserved_per_principal)
      then corrupt "recovered principal upload quota exceeds configured bounds"
      else (
        Hashtbl.set
          per_principal
          ~key:principal
          ~data:
            ( previous_active + is_active
            , previous_reserved + stage + supplement
            , previous_installed + supplement );
        Ok ())))
;;

let recover t ~checkpoint =
  Hashtbl.clear t.uploads;
  Hashtbl.clear t.begin_keys;
  let remaining_source = ref t.limits.max_recovery_source_bytes in
  let max_bytes =
    t.limits.max_recovery_metadata_bytes + t.limits.max_recovery_source_bytes
  in
  let* reader =
    Retention_reader.create_at
      ~root:t.directory
      ~max_entries:t.limits.max_recovery_entries
      ~max_bytes
    |> storage
  in
  let* names =
    Retention_reader.list_with_checkpoint reader ~directory:"." ~checkpoint |> storage
  in
  let* () =
    List.fold names ~init:(Ok ()) ~f:(fun result name ->
      let* () = result in
      checkpoint ();
      if String.is_prefix name ~prefix:".prune-"
      then cleanup_pruning_directory t reader name ~checkpoint
      else
        let* recovered =
          recover_upload t reader name ~remaining_source ~checkpoint |> storage
        in
        match recovered with
        | None -> Ok ()
        | Some upload ->
          let id = upload.manifest_doc.upload_id in
          let identity =
            key upload.manifest_doc.owner.principal upload.manifest_doc.begin_key
          in
          if Hashtbl.mem t.begin_keys identity
          then corrupt "duplicate durable upload begin key"
          else (
            Hashtbl.set t.uploads ~key:id ~data:upload;
            Hashtbl.set t.begin_keys ~key:identity ~data:id;
            Ok ()))
  in
  let metadata =
    Hashtbl.fold t.uploads ~init:0 ~f:(fun ~key:_ ~data:upload total ->
      total + metadata_bytes upload)
  in
  if metadata > t.limits.max_recovery_metadata_bytes
  then Error Error.Limit_exceeded
  else
    let* () = check_recovered_limits t ~checkpoint in
    let source_bytes = ref 0 in
    Hashtbl.fold t.uploads ~init:(Ok ()) ~f:(fun ~key:_ ~data:upload result ->
      let* () = result in
      match upload.phase.outcome with
      | Doc.Phase.Ready { payload_digest; _ } ->
        let needed = declared_bytes upload + installed_bytes upload in
        if needed > t.limits.max_recovery_source_bytes - !source_bytes
        then Error Error.Limit_exceeded
        else (
          source_bytes := !source_bytes + needed;
          let* actual, _ = verify_members t upload ~materialize:false ~checkpoint in
          if Chatml_source.Digest.equal actual payload_digest
          then Ok ()
          else corrupt "sealed upload payload changed on recovery")
      | Open | Rejected _ | Expired _ -> Ok ())
;;

let open_or_create ~env ~sw ~data_root ~daemon_lock ~host_id ~limits ~checkpoint =
  let path = Filename.concat (Data_root.path data_root) "authoring-uploads" in
  let lock_path = Data_root.daemon_lock_path data_root in
  if
    (not (Lock.is_live daemon_lock))
    || (not (String.equal (Lock.path daemon_lock) lock_path))
    || not (P.Id.Server.equal (Lock.owner daemon_lock).server_id host_id)
  then Error Error.Closed
  else if not (reserve_root path)
  then Error Error.Busy
  else (
    let keep = ref false in
    Fun.protect
      (fun () ->
         let result =
           let* encoded =
             Durable_file.load_bounded
               ~env
               ~path:(Data_root.server_id_path data_root)
               ~max_bytes:256
             |> storage
           in
           let* stored =
             P.Id.Server.of_string (String.strip encoded)
             |> Result.map_error ~f:(fun _ -> Error.Invalid_manifest)
           in
           if not (P.Id.Server.equal stored host_id)
           then corrupt "data root host differs from upload owner"
           else (
             let native = Eio.Path.(Eio.Stdenv.fs env / path) in
             let* () =
               try
                 match Eio.Path.kind ~follow:false native with
                 | `Not_found ->
                   Eio.Path.mkdir ~perm:0o700 native;
                   Durable_file.sync_directory ~env ~path:(Data_root.path data_root)
                   |> storage
                 | `Directory -> Ok ()
                 | _ -> corrupt "upload root is not an owned directory"
               with
               | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
                 storage
                   (Error (Store_error.of_exn ~operation:"open upload root" ~path exn))
             in
             let directory = Eio.Path.open_dir ~sw native in
             let t =
               { env
               ; root_path = path
               ; directory
               ; daemon_lock
               ; daemon_lock_path = lock_path
               ; host_id
               ; limits
               ; gate = Eio.Mutex.create ()
               ; readers_idle = Eio.Condition.create ()
               ; uploads = Hashtbl.create (module Id)
               ; read_leases = Hashtbl.create (module Id)
               ; begin_keys = String.Table.create ()
               ; token = ref ()
               ; closed = false
               ; fully_closed = false
               ; uncertain = false
               }
             in
             let recovered =
               try recover t ~checkpoint:(work limits checkpoint) with
               | Work_limit -> Error Error.Limit_exceeded
             in
             let* () = recovered in
             if not (Lock.is_live daemon_lock)
             then Error Error.Closed
             else (
               Eio.Switch.on_release sw (fun () ->
                 ignore (complete_close t : (unit, Error.t) result));
               keep := true;
               Ok t))
         in
         result)
      ~finally:(fun () -> if not !keep then release_root path))
;;

let ensure_consistent t ~checkpoint =
  if not t.uncertain
  then Ok ()
  else
    let* () = recover t ~checkpoint in
    t.uncertain <- false;
    Ok ()
;;

let begin_response t upload =
  B.Begin.create_response
    ~owner:upload.manifest_doc.owner
    ~upload_id:upload.manifest_doc.upload_id
    ~idempotency_key:upload.manifest_doc.begin_key
    ~manifest_fingerprint:(B.Manifest.fingerprint upload.manifest_doc.manifest)
    ~parent_context:upload.manifest_doc.parent_context
    ~max_chunk_bytes:t.limits.max_chunk_bytes
    ~expires_at:upload.manifest_doc.stage_deadline
  |> Result.map_error ~f:(fun _ -> Error.Invalid_manifest)
;;

let begin_upload t ~principal ~request ~now ~checkpoint =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let now = now () in
    let request : B.Begin.request = request in
    let identity = key principal request.idempotency_key in
    match Hashtbl.find t.begin_keys identity with
    | Some upload_id ->
      let* previous = find_owned t principal upload_id in
      if
        B.Manifest.equal previous.manifest_doc.manifest request.manifest
        && Option.equal
             P.Authoring_context.Context.Expected.equal
             previous.manifest_doc.parent_context
             request.parent_context
      then begin_response t previous
      else Error Error.Idempotency_conflict
    | None ->
      let members = B.Manifest.members request.manifest in
      let declared =
        List.fold members ~init:0 ~f:(fun total member ->
          total + P.Authoring.Source.byte_length (B.Member.source member))
      in
      if not (manifest_within_limits t.limits request.manifest)
      then Error Error.Limit_exceeded
      else
        let* stage_deadline = deadline now t.limits.staging_ttl_ms in
        let upload_id = Id.create () in
        let owner = B.Owner.create ~host:t.host_id ~principal in
        let manifest_doc =
          Doc.Manifest.
            { owner
            ; upload_id
            ; begin_key = request.idempotency_key
            ; manifest = request.manifest
            ; parent_context = request.parent_context
            ; created_at = now
            ; stage_deadline
            }
        in
        let* manifest_bytes =
          Doc.Manifest.to_bytes manifest_doc ~max_bytes:(manifest_max t.limits) |> storage
        in
        let phase = Doc.Phase.{ upload_id; outcome = Open } in
        let* phase_bytes = Doc.Phase.to_bytes phase ~max_bytes:phase_max |> storage in
        let* () =
          ensure_begin_capacity
            t
            principal
            (String.length manifest_bytes)
            declared
            (List.length members)
        in
        let name = upload_name upload_id in
        let path = Eio.Path.(t.directory / name) in
        t.uncertain <- true;
        let* () =
          try
            Eio.Path.mkdir ~perm:0o700 path;
            Durable_file.sync_directory_in ~directory:t.directory |> storage
          with
          | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
            storage
              (Error
                 (Store_error.of_exn
                    ~operation:"create private upload stage"
                    ~path:name
                    exn))
        in
        let* () =
          with_upload_directory t upload_id (fun directory ->
            let* () = save_in directory Files.manifest_name manifest_bytes in
            save_in directory Files.phase_name phase_bytes)
        in
        let upload =
          { manifest_doc
          ; manifest_bytes = String.length manifest_bytes
          ; phase
          ; phase_bytes = String.length phase_bytes
          ; offsets = Array.create ~len:(List.length members) 0
          ; progress_bytes = Array.create ~len:(List.length members) 0
          ; data_bytes = 0
          ; publication = Not_published
          }
        in
        Hashtbl.set t.uploads ~key:upload_id ~data:upload;
        Hashtbl.set t.begin_keys ~key:identity ~data:upload_id;
        t.uncertain <- false;
        begin_response t upload)
;;

let find_member_index upload source =
  List.find_mapi (B.Manifest.members upload.manifest_doc.manifest) ~f:(fun index member ->
    if Chatml_source.Id.equal (P.Authoring.Source.id (B.Member.source member)) source
    then Some index
    else None)
;;

let chunk t ~principal ~request ~now ~checkpoint =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let now = now () in
    let request : B.Chunk.request = request in
    let* upload = find_owned t principal request.upload_id in
    if expired now (phase_deadline upload)
    then Error Error.Expired
    else (
      match upload.phase.outcome with
      | Doc.Phase.Open ->
        (match find_member_index upload request.source with
         | None -> Error Error.Invalid_manifest
         | Some index ->
           let expected_length =
             member_source upload index |> P.Authoring.Source.byte_length
           in
           let accepted = upload.offsets.(index) in
           let bytes_length = String.length request.bytes in
           if
             bytes_length > t.limits.max_chunk_bytes
             || request.byte_offset > expected_length - bytes_length
           then Error Error.Limit_exceeded
           else if request.byte_offset < accepted
           then
             if bytes_length > accepted - request.byte_offset
             then Error Error.Invalid_offset
             else
               let* same =
                 with_upload_directory t request.upload_id (fun directory ->
                   Files.read_repeat
                     directory
                     index
                     ~offset:request.byte_offset
                     ~bytes:request.bytes
                     ~checkpoint
                   |> storage)
               in
               if not same
               then Error Error.Idempotency_conflict
               else
                 B.Chunk.create_response
                   ~owner:upload.manifest_doc.owner
                   ~upload_id:request.upload_id
                   ~source:request.source
                   ~accepted_until_byte:accepted
                 |> Result.map_error ~f:(fun _ -> Error.Invalid_manifest)
           else if request.byte_offset <> accepted
           then Error Error.Invalid_offset
           else (
             let new_offset = accepted + bytes_length in
             let progress = progress_document request.upload_id index new_offset in
             let* progress_bytes =
               Doc.Progress.to_bytes progress ~max_bytes:progress_max |> storage
             in
             let _, _, records, current_journal_bytes, _, _ = counts t in
             let current_metadata =
               Hashtbl.fold t.uploads ~init:0 ~f:(fun ~key:_ ~data:upload total ->
                 total + metadata_bytes upload)
             in
             let added_record = if upload.progress_bytes.(index) = 0 then 1 else 0 in
             let added_bytes =
               String.length progress_bytes - upload.progress_bytes.(index)
             in
             if
               records + added_record > t.limits.max_journal_records
               || added_bytes > t.limits.max_journal_bytes - current_journal_bytes
               || added_bytes > t.limits.max_tombstone_bytes - current_metadata
             then Error Error.Limit_exceeded
             else (
               t.uncertain <- true;
               let* () =
                 with_upload_directory t request.upload_id (fun directory ->
                   let* () =
                     Files.append_member directory index ~accepted ~bytes:request.bytes
                     |> storage
                   in
                   save_in directory (Files.progress_name index) progress_bytes)
               in
               upload.offsets.(index) <- new_offset;
               upload.progress_bytes.(index) <- String.length progress_bytes;
               upload.data_bytes <- upload.data_bytes + bytes_length;
               t.uncertain <- false;
               B.Chunk.create_response
                 ~owner:upload.manifest_doc.owner
                 ~upload_id:request.upload_id
                 ~source:request.source
                 ~accepted_until_byte:new_offset
               |> Result.map_error ~f:(fun _ -> Error.Invalid_manifest))))
      | Ready _ | Rejected _ -> Error Error.Sealed
      | Expired _ -> Error Error.Expired))
;;

let phase_admission phase =
  let* bytes = Doc.Phase.to_bytes phase ~max_bytes:phase_max |> storage in
  Ok bytes
;;

let save_phase t upload phase bytes =
  t.uncertain <- true;
  let* () =
    with_upload_directory t upload.manifest_doc.upload_id (fun directory ->
      save_in directory Files.phase_name bytes)
  in
  upload.phase <- phase;
  upload.phase_bytes <- String.length bytes;
  t.uncertain <- false;
  Ok ()
;;

let claim_commit t ~principal ~request ~now ~checkpoint =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let now = now () in
    let request : B.Commit.request = request in
    let* upload = find_owned t principal request.upload_id in
    if expired now (phase_deadline upload)
    then Error Error.Expired
    else (
      match upload.phase.outcome with
      | Doc.Phase.Expired _ -> Error Error.Expired
      | Doc.Phase.Ready { commit_key; payload_digest; _ } ->
        if not (P.Idempotency_key.equal commit_key request.idempotency_key)
        then Error Error.Idempotency_conflict
        else
          let* actual, _ = verify_members t upload ~materialize:false ~checkpoint in
          if not (Chatml_source.Digest.equal actual payload_digest)
          then Error Error.Invalid_manifest
          else (
            match upload.publication with
            | Intent { marker = Some _; _ } ->
              let* receipt = verify_publication t upload ~checkpoint in
              Ok
                (Claim_result.Published
                   { receipt; parent_context = upload.manifest_doc.parent_context })
            | Not_published | Intent { marker = None; _ } ->
              Ok
                (Claim_result.Ready
                   { upload_id = request.upload_id
                   ; token = t.token
                   ; commit_key = request.idempotency_key
                   ; parent_context = upload.manifest_doc.parent_context
                   }))
      | Doc.Phase.Rejected { commit_key; reason; _ } ->
        if P.Idempotency_key.equal commit_key request.idempotency_key
        then
          Ok
            (Claim_result.Rejected
               { reason; parent_context = upload.manifest_doc.parent_context })
        else Error Error.Idempotency_conflict
      | Doc.Phase.Open ->
        if
          List.existsi
            (B.Manifest.members upload.manifest_doc.manifest)
            ~f:(fun index member ->
              upload.offsets.(index)
              <> P.Authoring.Source.byte_length (B.Member.source member))
        then Error Error.Incomplete
        else
          let* receipt_deadline = deadline now t.limits.receipt_ttl_ms in
          (match verify_members t upload ~materialize:false ~checkpoint with
           | Error Error.Invalid_manifest ->
             let reason = B.Rejection.Invalid_closure in
             let phase =
               Doc.Phase.
                 { upload_id = request.upload_id
                 ; outcome =
                     Rejected
                       { commit_key = request.idempotency_key; reason; receipt_deadline }
                 }
             in
             let* bytes = phase_admission phase in
             let* () = save_phase t upload phase bytes in
             Ok
               (Claim_result.Rejected
                  { reason; parent_context = upload.manifest_doc.parent_context })
           | Error error -> Error error
           | Ok (payload_digest, _) ->
             let phase =
               Doc.Phase.
                 { upload_id = request.upload_id
                 ; outcome =
                     Ready
                       { commit_key = request.idempotency_key
                       ; payload_digest
                       ; receipt_deadline
                       }
                 }
             in
             let* bytes = phase_admission phase in
             let* () = save_phase t upload phase bytes in
             Ok
               (Claim_result.Ready
                  { upload_id = request.upload_id
                  ; token = t.token
                  ; commit_key = request.idempotency_key
                  ; parent_context = upload.manifest_doc.parent_context
                  }))))
;;

let snapshot t ~principal (sealed : sealed) ~now ~checkpoint =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let now = now () in
    if not (phys_equal sealed.token t.token)
    then Error Error.Closed
    else (
      match find_owned t principal sealed.upload_id with
      | Error error -> Error error
      | Ok upload ->
        (match upload.phase.outcome with
         | Doc.Phase.Ready { commit_key; payload_digest; receipt_deadline }
           when P.Idempotency_key.equal commit_key sealed.commit_key ->
           if expired now receipt_deadline
           then Error Error.Expired
           else
             let* actual, sources =
               verify_members t upload ~materialize:true ~checkpoint
             in
             if Chatml_source.Digest.equal actual payload_digest
             then
               Ok
                 { Snapshot.manifest = upload.manifest_doc.manifest
                 ; sources
                 ; upload_id = sealed.upload_id
                 ; principal
                 ; commit_key
                 ; payload_digest
                 ; token = t.token
                 }
             else Error Error.Invalid_manifest
         | Open | Rejected _ | Expired _ | Ready _ -> Error Error.Sealed)))
;;

let ensure_publish_capacity t principal installed ~intent_bytes =
  let _, reserved, _, _, _, _ = counts t in
  let _, principal_reserved = count_principal t principal in
  let records, installed_total, publication_metadata_total, principal_installed =
    Hashtbl.fold
      t.uploads
      ~init:(0, 0, 0, 0)
      ~f:(fun ~key:_ ~data:upload (records, bytes, metadata, principal_bytes) ->
        let occupied =
          match upload.publication with
          | Not_published -> 0
          | Intent _ -> 1
        in
        let installed = installed_bytes upload in
        ( records + occupied
        , bytes + installed
        , metadata + reserved_publication_metadata t.limits upload
        , principal_bytes
          +
          if P.Id.Principal.equal upload.manifest_doc.owner.principal principal
          then installed
          else 0 ))
  in
  let supplement_bytes =
    Doc.Intent.supplementary_bytes (Publication.Installed.intent installed)
  in
  let metadata = intent_bytes + t.limits.max_marker_bytes in
  let within used added limit =
    used >= 0 && added >= 0 && used <= limit && added <= limit - used
  in
  if
    records >= t.limits.max_published_records
    || (not (within installed_total supplement_bytes t.limits.max_installed_bytes))
    || (not
          (within
             principal_installed
             supplement_bytes
             t.limits.max_installed_per_principal))
    || (not
          (within
             publication_metadata_total
             metadata
             t.limits.max_publication_metadata_bytes))
    || (not (within reserved supplement_bytes t.limits.max_reserved_bytes))
    || not
         (within principal_reserved supplement_bytes t.limits.max_reserved_per_principal)
  then Error Error.Limit_exceeded
  else Ok ()
;;

let compare_published_pin_identity t ~principal (pin : Published_pin.t) ~at =
  if
    (not (phys_equal t.token pin.store_token))
    || not (P.Id.Principal.equal principal pin.principal)
  then Error Error.Missing_or_foreign
  else (
    match Hashtbl.find t.uploads pin.upload_id with
    | None -> Error Error.Missing_or_foreign
    | Some upload ->
      if
        (not (P.Id.Principal.equal upload.manifest_doc.owner.principal principal))
        || not
             (Chatml_source.Digest.equal
                (B.Manifest.fingerprint upload.manifest_doc.manifest)
                pin.manifest_fingerprint)
      then Error Error.Missing_or_foreign
      else if expired at pin.deadline || expired at (phase_deadline upload)
      then Error Error.Expired
      else (
        match upload.publication with
        | Not_published | Intent { marker = None; _ } -> Error Error.Sealed
        | Intent { value = intent; marker = Some marker; _ } ->
          let receipt = Doc.Intent.receipt intent in
          if
            (not (intent_matches_upload upload intent))
            || (not (B.Receipt.equal receipt pin.receipt))
            || (not
                  (P.Id.Authoring_bundle.equal
                     (B.Receipt.bundle_id receipt)
                     pin.bundle_id))
            || (not (P.Timestamp.equal (B.Receipt.expires_at receipt) pin.deadline))
            || (not
                  (Chatml_source.Digest.equal
                     (Doc.Marker.intent_digest marker)
                     pin.marker_intent_digest))
            || not
                 (Chatml_source.Digest.equal
                    (Doc.Marker.inventory_digest marker)
                    pin.marker_inventory_digest)
          then Error Error.Invalid_manifest
          else Ok (upload, intent, marker)))
;;

exception Publication_expired
exception Publication_closed
exception Publication_parent_invalid of Error.t

let publish
      t
      ~principal
      (sealed : sealed)
      ~(snapshot : Snapshot.t)
      ~proof
      ~parent_pin
      ~now
      ~checkpoint
      ~final_admission
  =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let admitted_at = now () in
    if
      (not (phys_equal sealed.token t.token))
      || (not (phys_equal snapshot.token t.token))
      || (not (Id.equal snapshot.upload_id sealed.upload_id))
      || (not (P.Id.Principal.equal snapshot.principal principal))
      || not (P.Idempotency_key.equal snapshot.commit_key sealed.commit_key)
    then Error Error.Closed
    else
      let* upload = find_owned t principal sealed.upload_id in
      let* () =
        if
          Option.equal
            P.Authoring.Context.Expected.equal
            sealed.parent_context
            upload.manifest_doc.parent_context
        then Ok ()
        else Error Error.Idempotency_conflict
      in
      match upload.phase.outcome with
      | Doc.Phase.Ready { commit_key; payload_digest; receipt_deadline } ->
        if expired admitted_at receipt_deadline
        then Error Error.Expired
        else if
          (not (P.Idempotency_key.equal sealed.commit_key commit_key))
          || (not (Chatml_source.Digest.equal snapshot.payload_digest payload_digest))
          || not (B.Manifest.equal snapshot.manifest upload.manifest_doc.manifest)
        then Error Error.Idempotency_conflict
        else
          let* actual_payload, actual_sources =
            verify_members t upload ~materialize:true ~checkpoint
          in
          if
            (not (Chatml_source.Digest.equal actual_payload payload_digest))
            || not (same_source_list snapshot.sources actual_sources ~checkpoint)
          then Error Error.Invalid_manifest
          else
            let* candidate =
              Publication.of_proof upload.manifest_doc.manifest proof ~checkpoint
              |> Result.map_error ~f:(fun () -> Error.Invalid_manifest)
            in
            let bundle_id =
              match upload.publication with
              | Not_published -> P.Id.Authoring_bundle.create ()
              | Intent { value; _ } -> B.Receipt.bundle_id (Doc.Intent.receipt value)
            in
            let duplicate_bundle_id =
              Hashtbl.fold
                t.uploads
                ~init:false
                ~f:(fun ~key:other_id ~data:other found ->
                  checkpoint ();
                  found
                  || ((not (Id.equal other_id sealed.upload_id))
                      &&
                      match other.publication with
                      | Not_published -> false
                      | Intent { value; _ } ->
                        P.Id.Authoring_bundle.equal
                          bundle_id
                          (B.Receipt.bundle_id (Doc.Intent.receipt value))))
            in
            let* () =
              if duplicate_bundle_id then Error Error.Idempotency_conflict else Ok ()
            in
            let* plan =
              Publication.install_plan
                candidate
                ~manifest:upload.manifest_doc.manifest
                ~uploaded:actual_sources
                ~owner:upload.manifest_doc.owner
                ~upload_id:sealed.upload_id
                ~commit_key
                ~payload_digest
                ~bundle_id
                ~expires_at:receipt_deadline
                ~max_graphs:(t.limits.max_members + 1)
                ~max_sources:t.limits.max_installed_sources
                ~max_supplemental_bytes:t.limits.max_installed_bytes_per_bundle
                ~max_intent_bytes:t.limits.max_intent_bytes
                ~checkpoint
              |> Result.map_error ~f:(fun () -> Error.Limit_exceeded)
            in
            let intent = Publication.Installed.intent plan in
            let intent_bytes = Publication.Installed.intent_bytes plan in
            let* () =
              match upload.publication with
              | Not_published ->
                ensure_publish_capacity
                  t
                  principal
                  plan
                  ~intent_bytes:(String.length intent_bytes)
              | Intent { value; marker; _ } ->
                let* previous =
                  Doc.Intent.to_bytes value ~max_bytes:t.limits.max_intent_bytes
                  |> storage
                in
                if not (String.equal previous intent_bytes)
                then Error Error.Idempotency_conflict
                else (
                  match marker with
                  | None -> Ok ()
                  | Some _ ->
                    let* receipt = verify_publication t upload ~checkpoint in
                    if B.Receipt.equal receipt (Publication.Installed.receipt plan)
                    then Ok ()
                    else Error Error.Invalid_manifest)
            in
            (match upload.publication with
             | Intent { marker = Some _; _ } -> Ok (Publication.Installed.receipt plan)
             | Not_published | Intent { marker = None; _ } ->
               t.uncertain <- true;
               let* () =
                 match upload.publication with
                 | Not_published ->
                   with_upload_directory t sealed.upload_id (fun directory ->
                     save_in directory Files.intent_name intent_bytes)
                 | Intent _ -> Ok ()
               in
               upload.publication
               <- Intent
                    { value = intent
                    ; intent_bytes = String.length intent_bytes
                    ; marker = None
                    ; marker_bytes = 0
                    ; supplementary_bytes = Doc.Intent.supplementary_bytes intent
                    };
               let* () =
                 with_upload_directory t sealed.upload_id (fun directory ->
                   List.fold
                     (Publication.Installed.supplemental plan)
                     ~init:(Ok ())
                     ~f:(fun result (ordinal, source) ->
                       let* () = result in
                       checkpoint ();
                       Files.install_supplemental
                         directory
                         ~ordinal
                         ~bytes:(Chatml_source.contents source)
                         ~max_bytes:t.limits.max_installed_bytes_per_bundle
                         ~checkpoint
                       |> storage))
               in
               let* inventory_digest, installed_bytes =
                 verify_installed t upload intent actual_sources ~checkpoint
               in
               if installed_bytes <> Doc.Intent.supplementary_bytes intent
               then Error Error.Invalid_manifest
               else (
                 let marker =
                   Doc.Marker.create
                     ~intent_digest:(digest_text intent_bytes ~checkpoint)
                     ~inventory_digest
                     ~receipt_digest:
                       (digest_receipt (Publication.Installed.receipt plan) ~checkpoint)
                 in
                 let* marker_bytes =
                   Doc.Marker.to_bytes marker ~max_bytes:t.limits.max_marker_bytes
                   |> storage
                 in
                 let* () =
                   try
                     with_upload_directory t sealed.upload_id (fun directory ->
                       save_in
                         ~before_rename:(fun () ->
                           if expired (now ()) receipt_deadline
                           then raise Publication_expired;
                           (match parent_pin with
                            | None -> ()
                            | Some pin ->
                              (match check_lock t with
                               | Error error -> raise (Publication_parent_invalid error)
                               | Ok () ->
                                 (match
                                    compare_published_pin_identity
                                      t
                                      ~principal
                                      pin
                                      ~at:(now ())
                                  with
                                  | Ok _ -> ()
                                  | Error error ->
                                    raise (Publication_parent_invalid error))));
                           final_admission ();
                           match check_lock t with
                           | Ok () -> ()
                           | Error _ -> raise Publication_closed)
                         directory
                         Files.marker_name
                         marker_bytes)
                   with
                   | Publication_expired -> Error Error.Expired
                   | Publication_closed -> Error Error.Closed
                   | Publication_parent_invalid error -> Error error
                 in
                 upload.publication
                 <- Intent
                      { value = intent
                      ; intent_bytes = String.length intent_bytes
                      ; marker = Some marker
                      ; marker_bytes = String.length marker_bytes
                      ; supplementary_bytes = Doc.Intent.supplementary_bytes intent
                      };
                 t.uncertain <- false;
                 verify_publication t upload ~checkpoint))
      | Open | Rejected _ -> Error Error.Sealed
      | Expired _ -> Error Error.Expired)
;;

let reject t ~principal (sealed : sealed) ~reason ~now ~checkpoint =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let now = now () in
    if not (phys_equal sealed.token t.token)
    then Error Error.Closed
    else
      let* upload = find_owned t principal sealed.upload_id in
      match upload.phase.outcome, upload.publication with
      | Doc.Phase.Ready { commit_key; receipt_deadline; _ }, Not_published ->
        if expired now receipt_deadline
        then Error Error.Expired
        else if not (P.Idempotency_key.equal sealed.commit_key commit_key)
        then Error Error.Idempotency_conflict
        else (
          let phase =
            Doc.Phase.
              { upload_id = sealed.upload_id
              ; outcome = Rejected { commit_key; reason; receipt_deadline }
              }
          in
          let* bytes = phase_admission phase in
          save_phase t upload phase bytes)
      | Doc.Phase.Rejected { commit_key; reason = prior; _ }, Not_published ->
        if
          P.Idempotency_key.equal sealed.commit_key commit_key
          && B.Rejection.equal prior reason
        then Ok ()
        else Error Error.Idempotency_conflict
      | Doc.Phase.Expired _, _ -> Error Error.Expired
      | (Doc.Phase.Open | Doc.Phase.Ready _ | Doc.Phase.Rejected _), _ ->
        Error Error.Sealed)
;;

let status t ~principal ~upload_id ~now ~checkpoint =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let now = now () in
    let* upload = find_owned t principal upload_id in
    if expired now (phase_deadline upload)
    then Ok Status.Expired
    else (
      match upload.phase.outcome with
      | Doc.Phase.Open ->
        let* progress =
          List.mapi
            (B.Manifest.members upload.manifest_doc.manifest)
            ~f:(fun index member ->
              B.Status.Progress.create
                ~source:(P.Authoring.Source.id (B.Member.source member))
                ~accepted_until_byte:upload.offsets.(index)
              |> Result.map_error ~f:(fun _ -> Error.Invalid_manifest))
          |> Result.all
        in
        Ok (Status.Open progress)
      | Ready _ ->
        (match upload.publication with
         | Not_published -> Ok Status.Ready
         | Intent { marker = None; _ } -> Ok Status.Validating
         | Intent { marker = Some _; _ } ->
           let* receipt = verify_publication t upload ~checkpoint in
           Ok (Status.Published receipt))
      | Rejected { reason; _ } -> Ok (Status.Rejected reason)
      | Expired _ -> Ok Status.Expired))
;;

let active_reads : t list Eio.Fiber.key = Eio.Fiber.create_key ()

let with_published t ~principal ~bundle_id ~now ~checkpoint ~admit_metadata ~f =
  let admitted =
    use_gate t (fun () ->
      let checkpoint = work t.limits checkpoint in
      checkpoint ();
      let* () = ensure_consistent t ~checkpoint in
      checkpoint ();
      let admitted_at = now () in
      if Hashtbl.length t.read_leases > 0
      then Error Error.Busy
      else
        let* upload =
          Hashtbl.fold
            t.uploads
            ~init:(Error Error.Missing_or_foreign)
            ~f:(fun ~key:_ ~data:upload found ->
              checkpoint ();
              match upload.publication with
              | Intent { value; marker = Some _; _ }
                when P.Id.Principal.equal upload.manifest_doc.owner.principal principal
                     && P.Id.Authoring_bundle.equal
                          (B.Receipt.bundle_id (Doc.Intent.receipt value))
                          bundle_id -> Ok upload
              | Not_published | Intent _ -> found)
        in
        if expired admitted_at (phase_deadline upload)
        then Error Error.Expired
        else
          let* () = check_lock t in
          (match upload.publication with
           | Intent { value; marker = Some _; _ } ->
             admit_metadata (Doc.Intent.receipt value) upload.manifest_doc.manifest
           | Not_published | Intent _ -> assert false);
          let* receipt = verify_publication t upload ~checkpoint in
          let* capture = materialize_published t upload receipt ~checkpoint in
          let* () = check_lock t in
          if expired (now ()) (phase_deadline upload)
          then Error Error.Expired
          else (
            let upload_id = upload.manifest_doc.upload_id in
            Hashtbl.set
              t.read_leases
              ~key:upload_id
              ~data:(read_lease_count t upload_id + 1);
            Ok (upload, capture)))
  in
  match admitted with
  | Error _ as error -> error
  | Ok (upload, capture) ->
    let active = Option.value (Eio.Fiber.get active_reads) ~default:[] in
    let release () =
      Eio.Cancel.protect (fun () ->
        Eio.Mutex.use_rw ~protect:true t.gate (fun () ->
          let upload_id = upload.manifest_doc.upload_id in
          let count = read_lease_count t upload_id in
          if count <= 1
          then Hashtbl.remove t.read_leases upload_id
          else Hashtbl.set t.read_leases ~key:upload_id ~data:(count - 1);
          Eio.Condition.broadcast t.readers_idle))
    in
    Eio.Fiber.with_binding active_reads (t :: active) (fun () ->
      Fun.protect ~finally:release (fun () -> Ok (f capture)))
;;

let check_published_pin_now t ~principal pin ~now =
  if not (Eio.Mutex.try_lock t.gate)
  then Error Error.Busy
  else
    Exn.protect
      ~finally:(fun () -> Eio.Mutex.unlock t.gate)
      ~f:(fun () ->
        let* () = if t.closed then Error Error.Closed else check_lock t in
        if t.uncertain
        then Error Error.Busy
        else
          let* _, _, _ = compare_published_pin_identity t ~principal pin ~at:(now ()) in
          Ok ())
;;

let check_published_pin t ~principal pin ~now ~checkpoint =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let* upload, intent, marker =
      compare_published_pin_identity t ~principal pin ~at:(now ())
    in
    let* () = verify_publication_metadata t upload intent marker ~checkpoint in
    let* () = check_lock t in
    if expired (now ()) pin.deadline then Error Error.Expired else Ok ())
;;

let cleanup_expired_data t upload ~checkpoint =
  let upload_id = upload.manifest_doc.upload_id in
  t.uncertain <- true;
  let* () =
    with_upload_directory t upload_id (fun directory ->
      let* reader =
        Retention_reader.create_at
          ~root:directory
          ~max_entries:t.limits.max_recovery_entries
          ~max_bytes:0
        |> storage
      in
      let* names =
        Retention_reader.list_with_checkpoint reader ~directory:"." ~checkpoint |> storage
      in
      let intent, marker =
        match upload.publication with
        | Not_published -> None, None
        | Intent { value; marker; _ } -> Some value, marker
      in
      if
        (not
           (Files.validate_entry_names
              ?intent
              ?marker
              (B.Manifest.members upload.manifest_doc.manifest)
              names))
        || not
             (List.for_all names ~f:(fun name ->
                String.equal name Files.manifest_name
                || String.equal name Files.phase_name
                || Files.is_member_filename name
                || Files.is_publication_filename name))
      then corrupt "unexpected file in expired upload"
      else Files.cleanup_member_files directory names |> storage)
  in
  Array.fill upload.offsets ~pos:0 ~len:(Array.length upload.offsets) 0;
  Array.fill upload.progress_bytes ~pos:0 ~len:(Array.length upload.progress_bytes) 0;
  upload.data_bytes <- 0;
  upload.publication <- Not_published;
  t.uncertain <- false;
  Ok ()
;;

let tombstone t upload ~now ~checkpoint =
  match upload.phase.outcome with
  | Doc.Phase.Expired _ -> cleanup_expired_data t upload ~checkpoint
  | Open | Ready _ | Rejected _ ->
    let* tombstone_deadline = deadline now t.limits.expired_tombstone_ttl_ms in
    let phase =
      Doc.Phase.
        { upload_id = upload.manifest_doc.upload_id
        ; outcome = Expired { tombstone_deadline }
        }
    in
    let* bytes = phase_admission phase in
    let* () = save_phase t upload phase bytes in
    cleanup_expired_data t upload ~checkpoint
;;

let cancel t ~principal ~upload_id ~now ~checkpoint =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let now = now () in
    let* upload = find_owned t principal upload_id in
    if is_published upload
    then Error Error.Sealed
    else tombstone t upload ~now ~checkpoint)
;;

let prune_one t upload ~checkpoint =
  let upload_id = upload.manifest_doc.upload_id in
  let old_name = upload_name upload_id in
  let new_name = pruning_name upload_id in
  let source = Eio.Path.(t.directory / old_name) in
  let target = Eio.Path.(t.directory / new_name) in
  t.uncertain <- true;
  let* () =
    try
      match Eio.Path.kind ~follow:false target with
      | `Not_found ->
        Eio.Path.rename source target;
        Durable_file.sync_directory_in ~directory:t.directory |> storage
      | _ -> corrupt "conflicting upload cleanup marker"
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      storage
        (Error (Store_error.of_exn ~operation:"mark pruned upload" ~path:old_name exn))
  in
  let* reader =
    Retention_reader.create_at
      ~root:t.directory
      ~max_entries:t.limits.max_recovery_entries
      ~max_bytes:0
    |> storage
  in
  let* () = cleanup_pruning_directory t reader new_name ~checkpoint in
  Hashtbl.remove t.uploads upload_id;
  Hashtbl.remove
    t.begin_keys
    (key upload.manifest_doc.owner.principal upload.manifest_doc.begin_key);
  t.uncertain <- false;
  Ok ()
;;

let prune t ~now ~protected_uploads ~checkpoint =
  use_gate t (fun () ->
    let checkpoint = work t.limits checkpoint in
    checkpoint ();
    let* () = ensure_consistent t ~checkpoint in
    checkpoint ();
    let now = now () in
    if
      List.length (List.take protected_uploads (t.limits.max_idempotency_records + 1))
      > t.limits.max_idempotency_records
    then Error Error.Limit_exceeded
    else (
      let protected = Hash_set.create (module Id) in
      List.iter protected_uploads ~f:(Hash_set.add protected);
      let ids = Hashtbl.keys t.uploads |> List.sort ~compare:Id.compare in
      let* () =
        List.fold ids ~init:(Ok ()) ~f:(fun result upload_id ->
          let* () = result in
          checkpoint ();
          match Hashtbl.find t.uploads upload_id with
          | None -> Ok ()
          | Some upload ->
            if Hash_set.mem protected upload_id || read_lease_count t upload_id > 0
            then Ok ()
            else (
              match upload.phase.outcome with
              | Doc.Phase.Expired _ -> Ok ()
              | Open | Ready _ | Rejected _ ->
                if expired now (phase_deadline upload)
                then tombstone t upload ~now ~checkpoint
                else Ok ()))
      in
      let* removed =
        List.fold ids ~init:(Ok 0) ~f:(fun result upload_id ->
          let* count = result in
          checkpoint ();
          match Hashtbl.find t.uploads upload_id with
          | None -> Ok count
          | Some upload ->
            (match upload.phase.outcome with
             | Doc.Phase.Expired { tombstone_deadline }
               when expired now tombstone_deadline
                    && (not (Hash_set.mem protected upload_id))
                    && read_lease_count t upload_id = 0 ->
               let* () = prune_one t upload ~checkpoint in
               Ok (count + 1)
             | Open | Ready _ | Rejected _ | Expired _ -> Ok count))
      in
      Ok removed))
;;

let close_and_wait t =
  let active = Option.value (Eio.Fiber.get active_reads) ~default:[] in
  if List.exists active ~f:(phys_equal t) then Error Error.Busy else complete_close t
;;
