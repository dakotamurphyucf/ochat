open Core
module Native = Git_host_native

type t = Native.Descriptor.t

let joined f = Eio.Cancel.protect (fun () -> Eio_unix.run_in_systhread f)

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ()
;;

let close t = joined (fun () -> Native.Descriptor.close t)
let borrowed_fd = Native.Descriptor.borrowed_fd
let validate_root t = joined (fun () -> Native.Descriptor.validate_root t)

let owned ~acquire =
  let transferred = ref false in
  let t = Native.Descriptor.create () in
  let success = Ok t in
  Fun.protect
    ~finally:(fun () -> if not !transferred then close t)
    (fun () ->
       match acquire t with
       | Error error -> Error error
       | Ok () ->
         transferred := true;
         success)
;;

let borrow root ~checkpoint ~private_ =
  let _, path = root in
  if not (String.is_empty path)
  then Error Native.Code.Unsupported
  else (
    poll checkpoint;
    owned ~acquire:(fun output ->
      Eio.Switch.run (fun sw ->
        let input =
          try Ok (Eio.Path.open_in ~sw Eio.Path.(root / ".")) with
          | Eio.Io _ -> Error Native.Code.Unavailable
        in
        match input with
        | Error error -> Error error
        | Ok input ->
          (match Eio_unix.Resource.fd_opt input with
           | None -> Error Native.Code.Unsupported
           | Some fd ->
             Eio_unix.Fd.use_exn "Git host root" fd (fun fd ->
               joined (fun () ->
                 if private_
                 then Native.Descriptor.root output fd
                 else Native.Descriptor.trusted_directory output fd))))))
;;

let borrow_root root ~checkpoint = borrow root ~checkpoint ~private_:true
let borrow_deployment root ~checkpoint = borrow root ~checkpoint ~private_:false

let with_cwd t ~f =
  Eio.Switch.run (fun sw ->
    let fd =
      Eio_unix.Fd.of_unix
        ~sw
        ~close_unix:false
        ~blocking:true
        ~seekable:false
        (borrowed_fd t)
    in
    f fd)
;;

let directory t ~name ~create ~exclusive =
  owned ~acquire:(fun output ->
    joined (fun () ->
      Native.Descriptor.directory output ~parent:t ~name ~create ~exclusive))
;;

let with_directory t ~name ~create ~exclusive ~f =
  match directory t ~name ~create ~exclusive with
  | Error error -> Error error
  | Ok child -> Fun.protect ~finally:(fun () -> close child) (fun () -> f child)
;;

let with_file t ~name ~create ~f =
  match
    owned ~acquire:(fun output ->
      joined (fun () -> Native.Descriptor.file output ~parent:t ~name ~create))
  with
  | Error error -> Error error
  | Ok file -> Fun.protect ~finally:(fun () -> close file) (fun () -> f file)
;;

let lease t ~name =
  let acquire create =
    owned ~acquire:(fun output ->
      let open Result.Let_syntax in
      let%bind () =
        joined (fun () -> Native.Descriptor.file output ~parent:t ~name ~create)
      in
      joined (fun () -> Native.Descriptor.lock output))
  in
  match acquire true with
  | Error Native.Code.Exists -> acquire false
  | result -> result
;;

let entry t ~name = joined (fun () -> Native.entry t ~name)

let names t ~max_entries ~checkpoint =
  if max_entries < 0
  then Error Native.Code.Too_large
  else (
    let cursor = Native.Cursor.create () in
    let buffer = Bigstring.create 1024 in
    Fun.protect
      ~finally:(fun () -> joined (fun () -> Native.Cursor.close cursor))
      (fun () ->
         let open Result.Let_syntax in
         let%bind () = joined (fun () -> Native.Cursor.open_ cursor t) in
         let rec loop count acc =
           poll checkpoint;
           match joined (fun () -> Native.Cursor.next cursor buffer) with
           | Error error -> Error error
           | Ok None -> Ok (List.rev acc)
           | Ok (Some _) when count >= max_entries -> Error Native.Code.Too_large
           | Ok (Some name) -> loop (count + 1) (name :: acc)
         in
         loop 0 []))
;;

let read_file t ~name ~max_bytes ~checkpoint =
  if max_bytes < 0
  then Error Native.Code.Too_large
  else
    with_file t ~name ~create:false ~f:(fun file ->
      let buffer = Bigstring.create 4096 in
      let contents = Buffer.create (Int.min 4096 max_bytes) in
      let rec loop bytes =
        poll checkpoint;
        (* One extra byte detects excess without allocating an input-sized buffer. *)
        let wanted = if max_bytes - bytes >= 4096 then 4096 else max_bytes - bytes + 1 in
        match joined (fun () -> Native.read file buffer ~max_bytes:wanted) with
        | Error error -> Error error
        | Ok 0 -> Ok (Buffer.contents contents)
        | Ok n when n > max_bytes - bytes -> Error Native.Code.Too_large
        | Ok n ->
          Buffer.add_string contents (Bigstring.to_string buffer ~pos:0 ~len:n);
          loop (bytes + n)
      in
      loop 0)
;;

let write_buffer file buffer ~bytes ~checkpoint =
  let rec loop remaining =
    if remaining = 0
    then Ok ()
    else (
      poll checkpoint;
      match joined (fun () -> Native.write file buffer ~bytes:remaining) with
      | Error error -> Error error
      | Ok 0 -> Error Native.Code.Unavailable
      | Ok written ->
        let remaining = remaining - written in
        if remaining > 0
        then
          Bigstring.blit
            ~src:buffer
            ~src_pos:written
            ~dst:buffer
            ~dst_pos:0
            ~len:remaining;
        loop remaining)
  in
  loop bytes
;;

let write_chunks file contents ~checkpoint =
  let buffer = Bigstring.create 4096 in
  let rec chunks pos =
    if pos = String.length contents
    then joined (fun () -> Native.sync file)
    else (
      poll checkpoint;
      let count = Int.min 4096 (String.length contents - pos) in
      Bigstring.From_string.blit
        ~src:contents
        ~src_pos:pos
        ~dst:buffer
        ~dst_pos:0
        ~len:count;
      let open Result.Let_syntax in
      let%bind () = write_buffer file buffer ~bytes:count ~checkpoint in
      chunks (pos + count))
  in
  chunks 0
;;

let create_file t ~name ~contents ~checkpoint =
  with_file t ~name ~create:true ~f:(fun file -> write_chunks file contents ~checkpoint)
;;

module Inventory = struct
  type file =
    { path : string list
    ; payload_bytes : int
    }

  type t =
    { files : file list
    ; directories : string list list
    ; entries : int
    ; payload_bytes : int
    ; work_steps : int
    }

  let files t = t.files
  let directories t = t.directories
  let entries t = t.entries
  let payload_bytes t = t.payload_bytes
  let work_steps t = t.work_steps
end

let inventory root ~max_entries ~max_depth ~max_payload_bytes ~max_work_steps ~checkpoint =
  if max_entries < 0 || max_depth < 0 || max_payload_bytes < 0 || max_work_steps < 0
  then Error Native.Code.Too_large
  else (
    let work = ref 0 in
    let reserve units =
      if units > max_work_steps - !work
      then Error Native.Code.Too_large
      else (
        work := !work + units;
        Ok ())
    in
    let rec walk t path state =
      let open Result.Let_syntax in
      let remaining = max_work_steps - !work in
      if remaining < 4
      then Error Native.Code.Too_large
      else (
        let admitted_names =
          Int.min (max_entries - state.Inventory.entries) ((remaining - 4) / 2)
        in
        let%bind entries = names t ~max_entries:admitted_names ~checkpoint in
        let%bind () = reserve ((2 * List.length entries) + 4) in
        List.fold_result entries ~init:state ~f:(fun state name ->
          poll checkpoint;
          if state.entries >= max_entries
          then Error Native.Code.Too_large
          else (
            let state = { state with entries = state.entries + 1 } in
            let%bind () = reserve 8 in
            let%bind fact = entry t ~name in
            let path = path @ [ name ] in
            match fact.kind with
            | Native.Entry.Symlink -> Error Native.Code.Corrupt
            | Directory ->
              if List.length path > max_depth
              then Error Native.Code.Too_large
              else (
                let%bind () = reserve 6 in
                with_directory t ~name ~create:false ~exclusive:false ~f:(fun child ->
                  walk child path { state with directories = path :: state.directories }))
            | Regular ->
              let remaining = Int64.of_int (max_payload_bytes - state.payload_bytes) in
              if Int64.(fact.payload_bytes < 0L || fact.payload_bytes > remaining)
              then Error Native.Code.Too_large
              else (
                let bytes = Int64.to_int_exn fact.payload_bytes in
                Ok
                  { state with
                    files = { path; payload_bytes = bytes } :: state.files
                  ; payload_bytes = state.payload_bytes + bytes
                  }))))
    in
    let empty =
      { Inventory.files = []
      ; directories = []
      ; entries = 0
      ; payload_bytes = 0
      ; work_steps = 0
      }
    in
    Result.map (walk root [] empty) ~f:(fun inventory ->
      { inventory with work_steps = !work }))
;;

let remove_tree parent ~name ~max_entries ~max_depth ~checkpoint =
  let count = ref 0 in
  let rec remove parent name depth =
    poll checkpoint;
    if !count >= max_entries || depth > max_depth
    then Error Native.Code.Too_large
    else (
      incr count;
      let open Result.Let_syntax in
      let%bind fact = entry parent ~name in
      let%bind () =
        match fact.kind with
        | Regular | Symlink -> Ok ()
        | Directory ->
          with_directory parent ~name ~create:false ~exclusive:false ~f:(fun child ->
            let%bind entries =
              names child ~max_entries:(max_entries - !count) ~checkpoint
            in
            List.fold_result entries ~init:() ~f:(fun () name ->
              remove child name (depth + 1)))
      in
      joined (fun () ->
        Native.unlink
          parent
          ~name
          ~directory:(Native.Entry.equal_kind fact.kind Directory)))
  in
  if max_entries < 0 || max_depth < 0
  then Error Native.Code.Too_large
  else remove parent name 0
;;

let rec with_path root path ~create ~f =
  match path with
  | [] -> f root
  | name :: rest ->
    with_directory root ~name ~create ~exclusive:false ~f:(fun child ->
      with_path child rest ~create ~f)
;;

let copy_inventory source ~destination inventory ~checkpoint =
  let open Result.Let_syntax in
  let directories =
    List.sort inventory.Inventory.directories ~compare:(fun a b ->
      let by_depth = Int.compare (List.length a) (List.length b) in
      if by_depth = 0 then List.compare String.compare a b else by_depth)
  in
  let%bind () =
    List.fold_result directories ~init:() ~f:(fun () path ->
      poll checkpoint;
      with_path destination path ~create:true ~f:(fun _ -> Ok ()))
  in
  let%bind () =
    List.fold_result inventory.files ~init:() ~f:(fun () file ->
      match List.rev file.path with
      | [] -> Error Native.Code.Corrupt
      | name :: reversed_parent ->
        let parent = List.rev reversed_parent in
        with_path source parent ~create:false ~f:(fun input ->
          with_path destination parent ~create:false ~f:(fun output ->
            with_file input ~name ~create:false ~f:(fun input ->
              with_file output ~name ~create:true ~f:(fun output ->
                let buffer = Bigstring.create 4096 in
                let rec copy remaining =
                  poll checkpoint;
                  let wanted = if remaining >= 4096 then 4096 else remaining + 1 in
                  let%bind count =
                    joined (fun () -> Native.read input buffer ~max_bytes:wanted)
                  in
                  if count = 0
                  then
                    if remaining <> 0
                    then Error Native.Code.Corrupt
                    else joined (fun () -> Native.sync output)
                  else if count > remaining
                  then Error Native.Code.Corrupt
                  else (
                    let%bind () = write_buffer output buffer ~bytes:count ~checkpoint in
                    copy (remaining - count))
                in
                copy file.payload_bytes)))))
  in
  let%bind () =
    List.fold_result (List.rev directories) ~init:() ~f:(fun () path ->
      poll checkpoint;
      with_path destination path ~create:false ~f:(fun directory ->
        joined (fun () -> Native.sync directory)))
  in
  joined (fun () -> Native.sync destination)
;;

let publish t ~name ~destination ~destination_name =
  joined (fun () -> Native.publish t ~name ~destination ~destination_name)
;;
