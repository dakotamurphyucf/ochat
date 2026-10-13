open! Core
module B = Agent_protocol.Authoring_bundle.Bundle

let ( let* ) result f = Result.bind result ~f
let progress_name index = sprintf "member-%04d.offset.json" index
let data_name index = sprintf "member-%04d.data" index
let manifest_name = "manifest.json"
let phase_name = "phase.json"

let file_kind path =
  try Ok (Eio.Path.kind ~follow:false path) with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation:"inspect upload file" ~path:"private upload" exn)
;;

let truncate_suffix directory index accepted =
  let name = data_name index in
  let path = Eio.Path.(directory / name) in
  let* kind = file_kind path in
  match kind with
  | `Not_found when accepted = 0 -> Ok ()
  | `Not_found -> Error (Store_error.Corrupt "accepted upload member is absent")
  | `Regular_file ->
    (try
       Eio.Path.with_open_out ~create:`Never path (fun flow ->
         let size = (Eio.File.stat flow).size |> Optint.Int63.to_int in
         if size < accepted
         then Error (Store_error.Corrupt "accepted upload member is truncated")
         else if size = accepted
         then Ok ()
         else (
           Eio.File.truncate flow (Optint.Int63.of_int accepted);
           Eio.File.sync flow;
           Ok ()))
     with
     | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
       Error (Store_error.of_exn ~operation:"truncate upload suffix" ~path:name exn))
  | _ -> Error (Store_error.Corrupt "upload member is not a regular file")
;;

let file_bytes directory index ~max_bytes ~checkpoint =
  let name = data_name index in
  let path = Eio.Path.(directory / name) in
  let* kind = file_kind path in
  match kind with
  | `Not_found when max_bytes = 0 -> Ok ""
  | `Regular_file ->
    (try
       Eio.Path.with_open_in path (fun input ->
         let size = (Eio.File.stat input).size |> Optint.Int63.to_int in
         if size <> max_bytes
         then Error (Store_error.Corrupt "upload member length differs from claim")
         else (
           let buffer = Buffer.create (Int.min size 65536) in
           let chunk = Cstruct.create 65536 in
           let rec loop remaining =
             checkpoint ();
             if remaining = 0
             then Ok (Buffer.contents buffer)
             else (
               let count =
                 try
                   Eio.Flow.single_read
                     input
                     (Cstruct.sub chunk 0 (Int.min remaining 65536))
                 with
                 | End_of_file -> 0
               in
               if count = 0
               then Error (Store_error.Corrupt "upload member truncated while reading")
               else (
                 Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 count));
                 loop (remaining - count)))
           in
           loop size))
     with
     | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
       Error (Store_error.of_exn ~operation:"read upload member" ~path:name exn))
  | `Not_found -> Error (Store_error.Corrupt "claimed upload member is absent")
  | _ -> Error (Store_error.Corrupt "upload member is not a regular file")
;;

let read_repeat directory index ~offset ~bytes ~checkpoint =
  let path = Eio.Path.(directory / data_name index) in
  let* kind = file_kind path in
  match kind with
  | `Regular_file ->
    (try
       Eio.Path.with_open_in path (fun input ->
         ignore (Eio.File.seek input (Optint.Int63.of_int offset) `Set : Optint.Int63.t);
         let chunk = Cstruct.create (Int.min 65536 (String.length bytes)) in
         let rec loop position =
           checkpoint ();
           if position = String.length bytes
           then Ok true
           else (
             let wanted =
               Int.min (String.length bytes - position) (Cstruct.length chunk)
             in
             let got =
               try Eio.Flow.single_read input (Cstruct.sub chunk 0 wanted) with
               | End_of_file -> 0
             in
             if got = 0
             then Ok false
             else if
               not
                 (String.equal
                    (Cstruct.to_string (Cstruct.sub chunk 0 got))
                    (String.sub bytes ~pos:position ~len:got))
             then Ok false
             else loop (position + got))
         in
         loop 0)
     with
     | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
       Error
         (Store_error.of_exn
            ~operation:"compare upload retry"
            ~path:(data_name index)
            exn))
  | _ -> Error (Store_error.Corrupt "accepted upload member is absent")
;;

let validate_entry_names members names =
  let allowed =
    let initial = String.Set.of_list [ manifest_name; phase_name ] in
    List.foldi members ~init:initial ~f:(fun index allowed _ ->
      Set.add (Set.add allowed (data_name index)) (progress_name index))
  in
  List.for_all names ~f:(fun name ->
    Set.mem allowed name
    || Option.value_map
         (Durable_file.temporary_target name)
         ~default:false
         ~f:(Set.mem allowed))
;;

let remove_temporary_files directory names =
  let temporaries =
    List.filter names ~f:(fun name -> Option.is_some (Durable_file.temporary_target name))
  in
  try
    List.iter temporaries ~f:(fun name -> Eio.Path.unlink Eio.Path.(directory / name));
    if List.is_empty temporaries then Ok () else Durable_file.sync_directory_in ~directory
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error
      (Store_error.of_exn
         ~operation:"remove interrupted upload record"
         ~path:"private upload"
         exn)
;;

let is_member_filename name =
  let target = Option.value (Durable_file.temporary_target name) ~default:name in
  let suffix =
    if String.is_suffix target ~suffix:".data"
    then Some ".data"
    else if String.is_suffix target ~suffix:".offset.json"
    then Some ".offset.json"
    else None
  in
  match suffix with
  | None -> false
  | Some suffix ->
    (match
       String.chop_prefix target ~prefix:"member-"
       |> Option.bind ~f:(fun rest -> String.chop_suffix rest ~suffix)
     with
     | Some number ->
       (match Int.of_string_opt number with
        | Some index when index >= 0 && index < B.Wire_limits.max_members ->
          String.equal
            target
            (if String.equal suffix ".data" then data_name index else progress_name index)
        | _ -> false)
     | None -> false)
;;

let cleanup_member_files directory names =
  try
    let removed = ref false in
    let* () =
      List.fold names ~init:(Ok ()) ~f:(fun result name ->
        let* () = result in
        if not (is_member_filename name)
        then Ok ()
        else (
          let path = Eio.Path.(directory / name) in
          let* kind = file_kind path in
          match kind with
          | `Regular_file ->
            Eio.Path.unlink path;
            removed := true;
            Ok ()
          | _ -> Error (Store_error.Corrupt "upload cleanup found nonregular member")))
    in
    if !removed then Durable_file.sync_directory_in ~directory else Ok ()
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error
      (Store_error.of_exn ~operation:"cleanup upload member" ~path:"private upload" exn)
;;

let append_member directory index ~accepted ~bytes =
  let name = data_name index in
  let path = Eio.Path.(directory / name) in
  let* kind = file_kind path in
  let* () =
    match kind with
    | `Not_found when accepted = 0 -> Ok ()
    | `Regular_file -> truncate_suffix directory index accepted
    | `Not_found -> Error (Store_error.Corrupt "accepted upload member is absent")
    | _ -> Error (Store_error.Corrupt "upload member is not a regular file")
  in
  try
    Eio.Path.with_open_out ~append:true ~create:(`If_missing 0o600) path (fun flow ->
      let size = (Eio.File.stat flow).size |> Optint.Int63.to_int in
      if size <> accepted
      then Error (Store_error.Corrupt "upload append offset differs from durable record")
      else (
        Eio.Flow.copy_string bytes flow;
        Eio.File.sync flow;
        Ok ()))
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation:"append upload bytes" ~path:name exn)
;;
