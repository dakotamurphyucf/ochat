open Core

type boundary =
  | After_create
  | After_bytes of int
  | Before_sync
  | After_sync
  | Before_rename
  | After_rename
  | Before_directory_sync

let wrap_file (Eio.Resource.T (file, handler)) ~boundary ~reached ~path =
  let module Original = (val Eio.Resource.get handler Eio.File.Pi.Write) in
  let remaining =
    ref
      (match boundary with
       | After_bytes count -> count
       | _ -> 0)
  in
  let module File = struct
    include Original

    let single_write file buffers =
      match boundary with
      | After_bytes _ ->
        let buffer = Cstruct.concat buffers in
        let length = Int.min !remaining (Cstruct.length buffer) in
        let written = Original.single_write file [ Cstruct.sub buffer 0 length ] in
        remaining := !remaining - written;
        if !remaining = 0 then reached path;
        written
      | _ -> Original.single_write file buffers
    ;;

    let copy file ~src = Eio.Flow.Pi.simple_copy ~single_write file ~src

    let sync file =
      (match boundary with
       | Before_sync -> reached path
       | _ -> ());
      Original.sync file;
      match boundary with
      | After_sync -> reached path
      | _ -> ()
    ;;
  end
  in
  Eio.Resource.T (file, Eio.File.Pi.rw (module File))
;;

let wrap_directory
      (Eio.Resource.T (directory, handler) as native_directory)
      ~matches
      ~boundary
      ~reached
  =
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let module Directory = struct
    include Original

    let open_out directory ~sw ~append ~create path =
      let file = Original.open_out directory ~sw ~append ~create path in
      if matches path
      then (
        (match boundary with
         | After_create -> reached path
         | _ -> ());
        wrap_file file ~boundary ~reached ~path)
      else file
    ;;

    let open_in directory ~sw path =
      let opened = Original.open_in directory ~sw path in
      if matches path
      then (
        match boundary with
        | Before_directory_sync ->
          (match (Eio.File.stat opened).kind with
           | `Directory -> ()
           | _ -> failwith "directory sync crash boundary requires a directory");
          if Option.is_none (Eio_unix.Resource.fd_opt opened)
          then failwith "directory sync crash boundary requires a native directory FD";
          reached path
        | _ -> ());
      opened
    ;;

    let rename directory source _destination target =
      if matches source
      then (
        match boundary with
        | Before_rename -> reached source
        | _ -> ());
      Original.rename directory source native_directory target;
      if matches source
      then (
        match boundary with
        | After_rename -> reached target
        | _ -> ())
    ;;
  end
  in
  Eio.Resource.T
    (directory, Eio.Resource.handler [ H (Eio.Fs.Pi.Dir, (module Directory)) ])
;;

let wrap env ~matches ~boundary ~reached =
  let directory, path = Eio.Stdenv.fs env in
  let fs = wrap_directory directory ~matches ~boundary ~reached, path in
  object
    method fs = fs
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = env#domain_mgr
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;
