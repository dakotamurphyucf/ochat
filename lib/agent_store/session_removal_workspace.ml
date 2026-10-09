open! Core
module P = Agent_protocol
module R = Session_archive_record

type t =
  { env : Eio_unix.Stdenv.base
  ; container : string
  ; payload : string
  ; source : string
  ; retained : string
  }

module Disposition = struct
  type t =
    | Retained
    | Absent
end

let io ~operation ~path f =
  try Ok (f ()) with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation ~path exn)
;;

let path t name = Eio.Path.(Eio.Stdenv.fs t.env / name)

let kind t name =
  io ~operation:"inspect removal workspace" ~path:name (fun () ->
    Eio.Path.kind ~follow:false (path t name))
;;

let sync t name = Durable_file.sync_directory ~env:t.env ~path:name
let corrupt message = Error (Store_error.Corrupt message)

let create ~env ~data_root ~session_id ~container ~document =
  let open Result.Let_syntax in
  let%bind container_kind =
    io ~operation:"admit workspace removal container" ~path:container (fun () ->
      Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / container))
  in
  let%bind () =
    match container_kind with
    | `Directory -> Ok ()
    | _ -> corrupt "workspace removal container is not a non-symlink directory"
  in
  let prefix = "deleted-" ^ P.Id.Session.to_string session_id ^ "-" in
  let record = Session_archive_document.value document in
  if
    (not
       (String.equal
          (Filename.dirname container)
          (Data_root.lost_and_found_path data_root)))
    || (not (P.Id.Session.equal (R.session_id record) session_id))
    || not (R.Status.equal (R.status record) Removed)
  then corrupt "workspace retention lacks owned terminal removal authority"
  else (
    match String.chop_prefix (Filename.basename container) ~prefix with
    | None -> corrupt "workspace retention namespace identity differs"
    | Some suffix ->
      (match P.Id.Transaction.of_string suffix with
       | Error _ -> corrupt "workspace retention namespace transaction is invalid"
       | Ok _ ->
         let payload = Filename.concat container "payload" in
         Ok
           { env
           ; container
           ; payload
           ; source = Filename.concat payload "workspace"
           ; retained = Filename.concat container "workspace"
           }))
;;

let sync_rename t =
  let open Result.Let_syntax in
  let%bind () = sync t t.payload in
  sync t t.container
;;

let preserve t =
  let open Result.Let_syntax in
  let%bind payload_kind = kind t t.payload in
  let%bind retained_kind = kind t t.retained in
  match payload_kind with
  | `Not_found ->
    (match retained_kind with
     | `Directory | `Not_found -> sync t t.container
     | _ -> corrupt "retained workspace is not a non-symlink directory")
  | `Directory ->
    let%bind source_kind = kind t t.source in
    (match source_kind, retained_kind with
     | `Directory, `Not_found ->
       let%bind () =
         io ~operation:"retain removed session workspace" ~path:t.source (fun () ->
           Eio.Path.rename (path t t.source) (path t t.retained))
       in
       sync_rename t
     | `Not_found, `Directory -> sync_rename t
     | `Directory, `Directory -> corrupt "both removal workspace locations exist"
     | `Not_found, `Not_found -> Ok ()
     | _ -> corrupt "removal workspace is not a non-symlink directory")
  | _ -> corrupt "removal payload is not a non-symlink directory"
;;

let disposition t =
  let open Result.Let_syntax in
  let%bind payload_kind = kind t t.payload in
  match payload_kind with
  | `Not_found ->
    let%bind retained_kind = kind t t.retained in
    (match retained_kind with
     | `Not_found -> Ok Disposition.Absent
     | `Directory -> Ok Disposition.Retained
     | _ -> corrupt "retained workspace is not a non-symlink directory")
  | _ -> corrupt "workspace disposition requires durable payload absence"
;;
