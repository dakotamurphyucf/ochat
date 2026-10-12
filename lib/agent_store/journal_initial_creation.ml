open! Core

type seal =
  | Absent
  | Partial
  | Complete

type t =
  { reader : Retention_reader.t
  ; env : Eio_unix.Stdenv.base
  ; max_payload_length : int
  ; names : string list
  ; current_contents : string
  ; current_id : Journal_segment.Id.t
  ; first_contents : string
  ; first_entry : Journal_segment.entry
  ; seal : seal
  ; successor_present : bool
  }

let entry t = t.first_entry
let env t = t.env
let directory t = Retention_reader.root t.reader
let max_payload_length t = t.max_payload_length
let corrupt reason = Error (Store_error.Corrupt reason)
let first = Journal_segment.Id.first
let first_name = Journal_segment.Id.filename first

let frame_error error =
  Store_error.Corrupt
    ("initial journal frame is invalid: " ^ Sexp.to_string_hum (Frame.sexp_of_error error))
;;

let io ~operation ~path f =
  try Ok (f ()) with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation ~path exn)
;;

let read_bytes reader input count ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    if count < 0 || count > Retention_reader.remaining_bytes reader
    then corrupt "initial journal frame exceeds the remaining byte budget"
    else Ok ()
  in
  let buffer = Buffer.create (Int.min 8192 count) in
  let chunk = Cstruct.create (Int.min 8192 count) in
  let rec loop remaining =
    if remaining = 0
    then Ok (Buffer.contents buffer)
    else (
      checkpoint ();
      let%bind read =
        io ~operation:"read initial journal frame" ~path:first_name (fun () ->
          try
            Eio.Flow.single_read input (Cstruct.sub chunk 0 (Int.min 8192 remaining))
          with
          | End_of_file -> 0)
      in
      let%bind () = Retention_reader.charge_bytes reader read in
      checkpoint ();
      if read = 0
      then Ok (Buffer.contents buffer)
      else (
        Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 read));
        loop (remaining - read)))
  in
  loop count
;;

let initial_entry reader ~env ~max_payload_length ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    if max_payload_length <= 0
    then corrupt "initial journal payload bound must be positive"
    else Ok ()
  in
  let path = Eio.Path.(Eio.Stdenv.fs env / Retention_reader.root reader / first_name) in
  checkpoint ();
  let%bind kind =
    io ~operation:"inspect initial journal frame" ~path:first_name (fun () ->
      Eio.Path.kind ~follow:false path)
  in
  match kind with
  | `Not_found -> Ok None
  | `Unknown
  | `Directory
  | `Fifo
  | `Character_special
  | `Block_device
  | `Symbolic_link
  | `Socket -> corrupt "initial journal segment is not a regular nonlinked file"
  | `Regular_file ->
    let%bind _ = Retention_reader.kind reader ~path:first_name in
    Eio.Switch.run (fun sw ->
      let%bind input =
        io ~operation:"open initial journal frame" ~path:first_name (fun () ->
          Eio.Path.open_in ~sw path)
      in
      let%bind header = read_bytes reader input Frame.header_length ~checkpoint in
      checkpoint ();
      let%bind length =
        Frame.encoded_length_of_header ~max_payload_length header
        |> Result.map_error ~f:frame_error
      in
      match length with
      | None -> Ok None
      | Some length ->
        let%bind suffix =
          read_bytes reader input (length - Frame.header_length) ~checkpoint
        in
        let contents = header ^ suffix in
        checkpoint ();
        let%bind decoded =
          Frame.decode ~max_payload_length ~contents ~offset:0
          |> Result.map_error ~f:frame_error
        in
        checkpoint ();
        (match decoded with
         | Incomplete_tail _ -> Ok None
         | Complete { frame; next_offset } ->
           Ok
             (Some
                Journal_segment.
                  { offset = 0L; next_offset = Int64.of_int next_offset; frame })))
;;

let seal_bytes ~max_payload_length =
  Frame.encode ~max_payload_length ~flags:1 "segment sealed"
  |> Result.map_error ~f:frame_error
;;

let inspect_frames contents ~max_payload_length ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind decoded =
    Frame.decode ~max_payload_length ~contents ~offset:0
    |> Result.map_error ~f:frame_error
  in
  checkpoint ();
  match decoded with
  | Incomplete_tail _ -> Ok None
  | Complete { frame; next_offset } ->
    if Frame.flags frame <> 0
    then corrupt "initial journal frame is not a transaction"
    else (
      let entry =
        Journal_segment.{ offset = 0L; next_offset = Int64.of_int next_offset; frame }
      in
      if next_offset = String.length contents
      then Ok (Some (entry, Absent))
      else (
        let%bind seal = seal_bytes ~max_payload_length in
        let suffix_length = String.length contents - next_offset in
        let%bind () =
          if suffix_length > String.length seal
          then corrupt "Pending creation journal has later frames"
          else Ok ()
        in
        let rec compare_suffix index =
          if index = suffix_length
          then Ok ()
          else (
            if index mod 4096 = 0 then checkpoint ();
            if Char.equal contents.[next_offset + index] seal.[index]
            then compare_suffix (index + 1)
            else corrupt "initial creation has an unrelated tail")
        in
        let%bind () = compare_suffix 0 in
        checkpoint ();
        let seal = if suffix_length = String.length seal then Complete else Partial in
        Ok (Some (entry, seal))))
;;

let inspect reader ~env ~max_payload_length ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    if max_payload_length <= 0
    then corrupt "initial journal payload bound must be positive"
    else Ok ()
  in
  let%bind successor = Journal_segment.Id.next first in
  let successor_name = Journal_segment.Id.filename successor in
  checkpoint ();
  let%bind names = Retention_reader.list reader ~directory:"." in
  checkpoint ();
  let%bind () =
    List.fold_result names ~init:() ~f:(fun () name ->
      checkpoint ();
      if
        String.is_suffix name ~suffix:".log"
        && not (String.equal name first_name || String.equal name successor_name)
      then corrupt "Pending creation journal has another segment"
      else Ok ())
  in
  if not (List.mem names first_name ~equal:String.equal)
  then Ok None
  else (
    let%bind current_contents =
      Retention_reader.read reader ~path:"CURRENT" ~max_bytes:64
    in
    checkpoint ();
    let%bind current_id =
      Journal_segment.Id.of_filename (String.strip current_contents)
    in
    let%bind () =
      if
        Journal_segment.Id.equal current_id first
        || Journal_segment.Id.equal current_id successor
      then Ok ()
      else corrupt "Pending creation journal has another current segment"
    in
    let%bind first_contents =
      Retention_reader.read
        reader
        ~path:first_name
        ~max_bytes:(Retention_reader.remaining_bytes reader)
    in
    let%bind decoded = inspect_frames first_contents ~max_payload_length ~checkpoint in
    match decoded with
    | None -> Ok None
    | Some (first_entry, seal) ->
      let successor_present = List.mem names successor_name ~equal:String.equal in
      let%bind () =
        if successor_present
        then (
          let%bind contents =
            Retention_reader.read reader ~path:successor_name ~max_bytes:0
          in
          if String.is_empty contents
          then Ok ()
          else corrupt "initial successor is not empty")
        else Ok ()
      in
      let%bind () =
        match seal with
        | Absent | Partial ->
          if Journal_segment.Id.equal current_id first && not successor_present
          then Ok ()
          else corrupt "unsealed initial transaction has a successor"
        | Complete ->
          if Journal_segment.Id.equal current_id successor && not successor_present
          then corrupt "CURRENT initial successor is absent"
          else Ok ()
      in
      checkpoint ();
      Ok
        (Some
           { reader
           ; env
           ; max_payload_length
           ; names
           ; current_contents
           ; current_id
           ; first_contents
           ; first_entry
           ; seal
           ; successor_present
           }))
;;

let equal_observation left right ~checkpoint =
  let compare_string left right =
    let rec loop index =
      if index = String.length left
      then true
      else (
        if index mod 4096 = 0 then checkpoint ();
        Char.equal left.[index] right.[index] && loop (index + 1))
    in
    String.length left = String.length right && loop 0
  in
  List.equal
    (fun left right ->
       checkpoint ();
       compare_string left right)
    left.names
    right.names
  && compare_string left.current_contents right.current_contents
  && compare_string left.first_contents right.first_contents
  && Bool.equal left.successor_present right.successor_present
;;

let write_current ~env path id =
  Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path
    (Journal_segment.Id.filename id ^ "\n")
;;

let repair t ~checkpoint =
  let open Result.Let_syntax in
  let%bind observed =
    inspect t.reader ~env:t.env ~max_payload_length:t.max_payload_length ~checkpoint
  in
  let%bind () =
    match observed with
    | Some observed when equal_observation t observed ~checkpoint -> Ok ()
    | Some _ | None -> corrupt "initial journal observation changed before repair"
  in
  let directory = directory t in
  let first_scan crash_tail =
    Journal_segment.
      { entries = [ t.first_entry ]
      ; valid_length = t.first_entry.next_offset
      ; crash_tail
      }
  in
  let%bind initial = Journal_segment.open_existing ~env:t.env ~directory ~id:first in
  checkpoint ();
  match t.seal with
  | Absent -> Ok (initial, first_scan false)
  | Partial ->
    let%map () =
      Journal_segment.truncate_crash_tail ~env:t.env initial (first_scan true)
    in
    initial, first_scan false
  | Complete ->
    let%bind successor = Journal_segment.Id.next first in
    let%bind current =
      if t.successor_present
      then Journal_segment.open_existing ~env:t.env ~directory ~id:successor
      else Journal_segment.create_exclusive ~env:t.env ~directory ~id:successor
    in
    let%map () =
      if Journal_segment.Id.equal t.current_id successor
      then Ok ()
      else write_current ~env:t.env (Filename.concat directory "CURRENT") successor
    in
    current, Journal_segment.{ entries = []; valid_length = 0L; crash_tail = false }
;;
