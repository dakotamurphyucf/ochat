open! Core
module R = Session_archive_record
module C = Session_archive_document
module P = Agent_protocol
module D = Document_schema

module Marker = struct
  type t =
    | Root
    | Payload
end

module Location = struct
  type t =
    | Source of C.t
    | Container of
        { document : C.t
        ; marker : Marker.t
        }
    | Empty
    | Finished
end

type t =
  { env : Eio_unix.Stdenv.base
  ; data_root : Data_root.t
  ; session_id : P.Id.Session.t
  ; source : string
  ; container : string
  ; mutable location : Location.t
  }

let session_id t = t.session_id
let path t name = Eio.Path.(Eio.Stdenv.fs t.env / name)
let payload t = Filename.concat t.container "payload"

let marker_path t = function
  | Marker.Root -> Filename.concat t.container "ARCHIVED"
  | Payload -> Filename.concat (payload t) "ARCHIVED"
;;

let io ~operation ~path f =
  try Ok (f ()) with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation ~path exn)
;;

let kind ~env name =
  io ~operation:"inspect removal path" ~path:name (fun () ->
    Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / name))
;;

let require_directory ~env name =
  let open Result.Let_syntax in
  let%bind kind = kind ~env name in
  match kind with
  | `Directory -> Ok ()
  | _ -> Error (Store_error.Corrupt "removal namespace is not a directory")
;;

let load_marker ~env ~session_id name =
  let open Result.Let_syntax in
  let%bind kind = kind ~env name in
  match kind with
  | `Regular_file ->
    let%bind bytes =
      Durable_file.load_bounded ~env ~path:name ~max_bytes:(D.Limits.max_bytes C.limits)
    in
    let%bind document =
      D.Document.decode ~limits:C.limits bytes |> Document_fields.store
    in
    let%bind owner = C.stored_session_id document |> Document_fields.store in
    if not (P.Id.Session.equal owner session_id)
    then Error (Store_error.Corrupt "removal marker identity differs from its namespace")
    else (
      let%bind document = C.of_document document |> Document_fields.store in
      if R.Status.equal (R.status (C.value document)) Removed
      then Ok document
      else Error (Store_error.Corrupt "removal namespace lacks terminal authority"))
  | _ -> Error (Store_error.Corrupt "removal marker is absent or not a regular file")
;;

let inspect_container t =
  let open Result.Let_syntax in
  let%bind () = require_directory ~env:t.env t.container in
  let%bind names =
    io ~operation:"list removal container" ~path:t.container (fun () ->
      Eio.Path.read_dir (path t t.container))
  in
  if List.is_empty names
  then Ok Location.Empty
  else if
    not
      (List.for_all names ~f:(fun name ->
         String.equal name "ARCHIVED" || String.equal name "payload"))
  then Error (Store_error.Corrupt "removal container has unknown entries")
  else (
    let%bind root_kind = kind ~env:t.env (marker_path t Root) in
    let%bind payload_kind = kind ~env:t.env (payload t) in
    let%bind () =
      match payload_kind with
      | `Directory | `Not_found -> Ok ()
      | _ -> Error (Store_error.Corrupt "removal payload is not an owned directory")
    in
    let%bind payload_marker_kind =
      match payload_kind with
      | `Directory -> kind ~env:t.env (marker_path t Payload)
      | `Not_found -> Ok `Not_found
      | _ -> Error (Store_error.Corrupt "invalid removal payload kind")
    in
    match root_kind, payload_marker_kind with
    | `Regular_file, `Not_found ->
      let%map document =
        load_marker ~env:t.env ~session_id:t.session_id (marker_path t Root)
      in
      Location.Container { document; marker = Root }
    | `Not_found, `Regular_file ->
      let%map document =
        load_marker ~env:t.env ~session_id:t.session_id (marker_path t Payload)
      in
      Location.Container { document; marker = Payload }
    | _ -> Error (Store_error.Corrupt "removal marker locations are ambiguous or missing"))
;;

let create ~env ~data_root session_id =
  let open Result.Let_syntax in
  let source = Data_root.session_path data_root session_id in
  let%bind () = require_directory ~env source in
  let%map document = load_marker ~env ~session_id (Filename.concat source "ARCHIVED") in
  let name =
    "deleted-"
    ^ P.Id.Session.to_string session_id
    ^ "-"
    ^ P.Id.Transaction.to_string (P.Id.Transaction.create ())
  in
  { env
  ; data_root
  ; session_id
  ; source
  ; container = Filename.concat (Data_root.lost_and_found_path data_root) name
  ; location = Source document
  }
;;

let parse_name ~session_id name =
  let prefix = "deleted-" ^ P.Id.Session.to_string session_id ^ "-" in
  match String.chop_prefix name ~prefix with
  | None ->
    Error (Store_error.Corrupt "removal namespace differs from terminal marker owner")
  | Some transaction ->
    P.Id.Transaction.of_string transaction
    |> Result.map ~f:(fun _ -> ())
    |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
;;

let marker_owner ~env container =
  let open Result.Let_syntax in
  let root_marker = Filename.concat container "ARCHIVED" in
  let payload_marker = Filename.concat (Filename.concat container "payload") "ARCHIVED" in
  let%bind root_kind = kind ~env root_marker in
  let payload = Filename.concat container "payload" in
  let%bind payload_directory_kind = kind ~env payload in
  let%bind payload_kind =
    match payload_directory_kind with
    | `Directory -> kind ~env payload_marker
    | `Not_found -> Ok `Not_found
    | _ -> Error (Store_error.Corrupt "removal payload is not an owned directory")
  in
  let%bind marker =
    match root_kind, payload_kind with
    | `Regular_file, `Not_found -> Ok root_marker
    | `Not_found, `Regular_file -> Ok payload_marker
    | _ -> Error (Store_error.Corrupt "removal marker locations are ambiguous or missing")
  in
  let%bind bytes =
    Durable_file.load_bounded ~env ~path:marker ~max_bytes:(D.Limits.max_bytes C.limits)
  in
  let%bind document = D.Document.decode ~limits:C.limits bytes |> Document_fields.store in
  C.stored_session_id document |> Document_fields.store
;;

let is_recognized_empty_name name =
  match String.chop_prefix name ~prefix:"deleted-" with
  | None -> false
  | Some suffix ->
    (* Both protocol IDs are bounded to 96 bytes and allow hyphens. Recognize
       the bounded grammar without guessing one split or deriving authority. *)
    String.length suffix <= 193
    && String.to_list suffix
       |> List.existsi ~f:(fun index ch ->
         if not (Char.equal ch '-')
         then false
         else (
           let session = String.prefix suffix index in
           let transaction = String.drop_prefix suffix (index + 1) in
           Result.is_ok (P.Id.Session.of_string session)
           && Result.is_ok (P.Id.Transaction.of_string transaction)))
;;

let discover ~env ~data_root =
  let open Result.Let_syntax in
  let directory = Data_root.lost_and_found_path data_root in
  let%bind () = require_directory ~env directory in
  let%bind names =
    io ~operation:"discover removal containers" ~path:directory (fun () ->
      Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory))
  in
  let discover_one name =
    let container = Filename.concat directory name in
    let%bind () = require_directory ~env container in
    let%bind children =
      io ~operation:"inspect removal container contents" ~path:container (fun () ->
        Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / container))
    in
    if List.is_empty children && not (is_recognized_empty_name name)
    then Ok None
    else if List.is_empty children
    then (
      (* Empty private namespace retirement has no receipt or session authority.
         It cannot mutate or conceal an independently existing active source. *)
      let%bind () =
        io ~operation:"retire empty removal container" ~path:container (fun () ->
          Eio.Path.rmdir Eio.Path.(Eio.Stdenv.fs env / container))
      in
      let%map () = Durable_file.sync_directory ~env ~path:directory in
      None)
    else (
      let%bind session_id = marker_owner ~env container in
      let%bind () = parse_name ~session_id name in
      let t =
        { env
        ; data_root
        ; session_id
        ; source = Data_root.session_path data_root session_id
        ; container
        ; location = Empty
        }
      in
      let%map location = inspect_container t in
      t.location <- location;
      Some t)
  in
  names
  |> List.filter ~f:(String.is_prefix ~prefix:"deleted-")
  |> List.sort ~compare:String.compare
  |> List.fold_result ~init:[] ~f:(fun containers name ->
    Result.map (discover_one name) ~f:(fun container -> container :: containers))
  |> Result.map ~f:(fun containers -> List.filter_opt (List.rev containers))
;;

let document t =
  match t.location with
  | Source document | Container { document; marker = _ } -> Some document
  | Empty | Finished -> None
;;

let receipts t =
  Option.value_map (document t) ~default:[] ~f:(fun document ->
    R.receipts (C.value document))
;;

let terminal_outcome t =
  Option.bind (document t) ~f:(fun document ->
    let revision = R.revision (C.value document) in
    List.find_map (receipts t) ~f:(fun receipt ->
      if
        R.Outcome.equal_action receipt.R.Receipt.outcome.action Remove
        && R.Outcome.equal_disposition receipt.outcome.disposition Applied
        && R.Revision.equal receipt.outcome.lifecycle_revision revision
      then Some receipt.outcome
      else None))
;;

let sync t directory = Durable_file.sync_directory ~env:t.env ~path:directory

let move_marker_to_root t =
  let open Result.Let_syntax in
  match t.location with
  | Container { marker = Root; document = _ } ->
    let%bind payload_kind = kind ~env:t.env (payload t) in
    let%bind () =
      match payload_kind with
      | `Directory -> sync t (payload t)
      | `Not_found -> Ok ()
      | _ -> Error (Store_error.Corrupt "invalid removal payload")
    in
    sync t t.container
  | Container { marker = Payload; document } ->
    let%bind () =
      io ~operation:"move terminal marker to removal root" ~path:t.container (fun () ->
        Eio.Path.rename (path t (marker_path t Payload)) (path t (marker_path t Root)))
    in
    t.location <- Container { marker = Root; document };
    let%bind () = sync t (payload t) in
    sync t t.container
  | Source _ -> Error (Store_error.Corrupt "removal source has not entered its container")
  | Empty | Finished -> Ok ()
;;

let stage t =
  let open Result.Let_syntax in
  let%bind () =
    if
      List.exists (receipts t) ~f:(fun receipt ->
        not receipt.R.Receipt.completion_acknowledged)
    then Error (Store_error.Corrupt "unacknowledged proof prevents namespace transfer")
    else Ok ()
  in
  match t.location with
  | Empty | Finished -> Ok ()
  | Container _ ->
    let%bind location = inspect_container t in
    t.location <- location;
    let%bind () = sync t (Data_root.sessions_path t.data_root) in
    let%bind () = sync t t.container in
    move_marker_to_root t
  | Source original ->
    let%bind container_kind = kind ~env:t.env t.container in
    let%bind () =
      match container_kind with
      | `Not_found ->
        io ~operation:"create removal container" ~path:t.container (fun () ->
          Eio.Path.mkdir ~perm:0o700 (path t t.container))
      | `Directory -> Ok ()
      | _ -> Error (Store_error.Corrupt "removal container is not an owned directory")
    in
    let%bind () = sync t (Data_root.lost_and_found_path t.data_root) in
    let%bind location = inspect_container t in
    (match location with
     | Empty ->
       let%bind current =
         load_marker
           ~env:t.env
           ~session_id:t.session_id
           (Filename.concat t.source "ARCHIVED")
       in
       let%bind observed =
         Session_lifecycle_documents.create ~session_id:t.session_id (Some original)
       in
       let%bind current_observed =
         Session_lifecycle_documents.create ~session_id:t.session_id (Some current)
       in
       if not (Session_lifecycle_documents.equal observed current_observed)
       then
         Error
           (Store_error.Corrupt "removal source proof changed before namespace transfer")
       else (
         let%bind () =
           io ~operation:"move removed source into payload" ~path:t.source (fun () ->
             Eio.Path.rename (path t t.source) (path t (payload t)))
         in
         t.location <- Container { marker = Payload; document = current };
         let%bind () = sync t (Data_root.sessions_path t.data_root) in
         let%bind () = sync t t.container in
         move_marker_to_root t)
     | Container _ ->
       let%bind source_kind = kind ~env:t.env t.source in
       (match source_kind with
        | `Not_found ->
          t.location <- location;
          let%bind () = sync t (Data_root.sessions_path t.data_root) in
          let%bind () = sync t t.container in
          move_marker_to_root t
        | _ -> Error (Store_error.Corrupt "removal source and payload both remain"))
     | Source _ | Finished ->
       Error (Store_error.Corrupt "invalid discovered removal location"))
;;

let complete_receipts t ~complete =
  let open Result.Let_syntax in
  let pending =
    List.filter (receipts t) ~f:(fun receipt ->
      not receipt.R.Receipt.completion_acknowledged)
  in
  List.fold_result pending ~init:() ~f:(fun () receipt ->
    let%bind () = complete receipt in
    let%bind document =
      document t
      |> Result.of_option
           ~error:(Store_error.Corrupt "removal proof vanished during completion")
    in
    let%bind acknowledged =
      C.acknowledge document ~key:receipt.key ~request_digest:receipt.request_digest
      |> Document_fields.store
    in
    let%bind bytes = Session_lifecycle_documents.encoded_document acknowledged in
    let marker =
      match t.location with
      | Source _ -> Filename.concat t.source "ARCHIVED"
      | Container { marker; document = _ } -> marker_path t marker
      | Empty | Finished -> Filename.concat t.container "ARCHIVED"
    in
    let%bind () =
      Durable_file.replace
        ~env:t.env
        ~durability:Flush_file_and_directory
        ~path:marker
        bytes
    in
    (match t.location with
     | Source _ -> t.location <- Source acknowledged
     | Container { marker; document = _ } ->
       t.location <- Container { marker; document = acknowledged }
     | Empty | Finished -> ());
    Ok ())
  |> Result.bind ~f:(fun () ->
    match document t with
    | None -> Ok ()
    | Some document ->
      let%bind bytes = Session_lifecycle_documents.encoded_document document in
      let marker =
        match t.location with
        | Source _ -> Filename.concat t.source "ARCHIVED"
        | Container { marker; document = _ } -> marker_path t marker
        | Empty | Finished -> Filename.concat t.container "ARCHIVED"
      in
      Durable_file.replace
        ~env:t.env
        ~durability:Flush_file_and_directory
        ~path:marker
        bytes)
;;

let cleanup t =
  let open Result.Let_syntax in
  if
    List.exists (receipts t) ~f:(fun receipt ->
      not receipt.R.Receipt.completion_acknowledged)
  then
    Error (Store_error.Corrupt "unacknowledged lifecycle proof prevents physical cleanup")
  else (
    let%bind () = stage t in
    let%bind () =
      if
        List.exists (receipts t) ~f:(fun receipt ->
          not receipt.R.Receipt.completion_acknowledged)
      then Error (Store_error.Corrupt "refreshed removal proof is unacknowledged")
      else Ok ()
    in
    match t.location with
    | Source _ | Container { marker = Payload; document = _ } ->
      Error (Store_error.Corrupt "terminal proof is not in stable removal root")
    | Finished -> sync t (Data_root.lost_and_found_path t.data_root)
    | Empty ->
      let%bind () =
        io ~operation:"remove empty removal container" ~path:t.container (fun () ->
          Eio.Path.rmdir (path t t.container))
      in
      t.location <- Finished;
      sync t (Data_root.lost_and_found_path t.data_root)
    | Container { marker = Root; document = _ } ->
      let%bind () =
        io ~operation:"remove terminal session payload" ~path:(payload t) (fun () ->
          Eio.Path.rmtree ~missing_ok:true (path t (payload t)))
      in
      let%bind payload_kind = kind ~env:t.env (payload t) in
      let%bind () =
        match payload_kind with
        | `Not_found -> sync t t.container
        | _ -> Error (Store_error.Corrupt "removed payload still exists")
      in
      Eio.Cancel.protect (fun () ->
        let%bind () =
          io ~operation:"retire terminal removal marker" ~path:t.container (fun () ->
            Eio.Path.unlink (path t (marker_path t Root)))
        in
        t.location <- Empty;
        let%bind () =
          io ~operation:"retire terminal removal container" ~path:t.container (fun () ->
            Eio.Path.rmdir (path t t.container))
        in
        t.location <- Finished;
        sync t (Data_root.lost_and_found_path t.data_root)))
;;
