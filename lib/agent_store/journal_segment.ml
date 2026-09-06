open Core

module Id = struct
  type t = int64 [@@deriving compare, equal, sexp]

  let first = Int64.one
  let to_int64 t = t

  let of_int64 value =
    if Int64.(value <= zero)
    then Error (Store_error.Corrupt "journal segment ID must be positive")
    else Ok value
  ;;

  let next t =
    if Int64.equal t Int64.max_value
    then Error (Store_error.Corrupt "journal segment ID overflow")
    else Ok Int64.(t + one)
  ;;

  let filename t = sprintf "%016Ld.log" t

  let of_filename filename =
    match String.chop_suffix filename ~suffix:".log" with
    | None ->
      Error (Store_error.Corrupt ("invalid journal segment filename: " ^ filename))
    | Some digits ->
      (match Int64.of_string digits with
       | value -> of_int64 value
       | exception _ ->
         Error (Store_error.Corrupt ("invalid journal segment filename: " ^ filename)))
  ;;
end

type t =
  { id : Id.t
  ; path : string
  }

type durability =
  | Buffered
  | Flush
[@@deriving compare, equal, sexp]

type entry =
  { offset : int64
  ; next_offset : int64
  ; frame : Frame.t
  }

type scan =
  { entries : entry list
  ; valid_length : int64
  ; crash_tail : bool
  }

let id t = t.id
let path t = t.path
let segment_path directory id = Filename.concat directory (Id.filename id)
let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)

let validate_directory directory =
  if Filename.is_absolute directory
  then Ok ()
  else
    Error
      (Store_error.Io
         { operation = "validate journal directory"
         ; path = directory
         ; message = "path must be absolute"
         })
;;

let create_exclusive ~env ~directory ~id =
  Result.bind (validate_directory directory) ~f:(fun () ->
    let path = segment_path directory id in
    try
      Eio.Path.with_open_out ~create:(`Exclusive 0o600) (eio_path env path) Eio.File.sync;
      Ok { id; path }
    with
    | exn -> Error (Store_error.of_exn ~operation:"create journal segment" ~path exn))
;;

let open_existing ~env ~directory ~id =
  Result.bind (validate_directory directory) ~f:(fun () ->
    let path = segment_path directory id in
    try
      if Eio.Path.is_file (eio_path env path)
      then Ok { id; path }
      else Error (Store_error.Missing path)
    with
    | exn -> Error (Store_error.of_exn ~operation:"open journal segment" ~path exn))
;;

let append_eio ~env ~durability path frame =
  Eio.Path.with_open_out
    ~append:true
    ~create:(`If_missing 0o600)
    (eio_path env path)
    (fun flow ->
       let offset = Eio.File.seek flow Optint.Int63.zero `End in
       Eio.Flow.copy_string frame flow;
       (match durability with
        | Buffered -> ()
        | Flush -> Eio.File.sync flow);
       let next_offset =
         Optint.Int63.add offset (Optint.Int63.of_int (String.length frame))
       in
       Optint.Int63.to_int64 offset, Optint.Int63.to_int64 next_offset)
;;

let append ~env ~durability t ~frame =
  try Ok (append_eio ~env ~durability t.path frame) with
  | exn -> Error (Store_error.of_exn ~operation:"append journal segment" ~path:t.path exn)
;;

let frame_error offset error =
  Store_error.Corrupt
    (sprintf
       "journal frame at byte %d is invalid: %s"
       offset
       (Sexp.to_string_hum ([%sexp_of: Frame.error] error)))
;;

let scan_contents ~max_payload_length contents =
  let rec loop offset entries =
    if offset = String.length contents
    then
      Ok
        { entries = List.rev entries
        ; valid_length = Int64.of_int offset
        ; crash_tail = false
        }
    else (
      match Frame.decode ~max_payload_length ~contents ~offset with
      | Error error -> Error (frame_error offset error)
      | Ok (Incomplete_tail _) ->
        Ok
          { entries = List.rev entries
          ; valid_length = Int64.of_int offset
          ; crash_tail = true
          }
      | Ok (Complete { frame; next_offset }) ->
        let entry =
          { offset = Int64.of_int offset; next_offset = Int64.of_int next_offset; frame }
        in
        loop next_offset (entry :: entries))
  in
  loop 0 []
;;

let scan ~env ~max_payload_length t =
  try Eio.Path.load (eio_path env t.path) |> scan_contents ~max_payload_length with
  | exn -> Error (Store_error.of_exn ~operation:"scan journal segment" ~path:t.path exn)
;;

let truncate_crash_tail ~env t scan =
  if not scan.crash_tail
  then Error (Store_error.Corrupt "refusing to truncate a segment without a crash tail")
  else (
    try
      Eio.Path.with_open_out
        ~create:(`If_missing 0o600)
        (eio_path env t.path)
        (fun flow ->
           Eio.File.truncate flow (Optint.Int63.of_int64 scan.valid_length);
           Eio.File.sync flow);
      Ok ()
    with
    | exn ->
      Error (Store_error.of_exn ~operation:"truncate journal tail" ~path:t.path exn))
;;
