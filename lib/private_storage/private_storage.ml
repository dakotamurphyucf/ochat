module Native_unix = Unix
open! Core

module Error = struct
  type operation =
    | Validate
    | Open_directory
    | Open_lock
    | Read
    | Create
    | Replace
    | Delete
  [@@deriving sexp_of]

  type code =
    | Missing
    | Exists
    | Denied
    | Unavailable
    | Unsupported_filesystem
    | Corrupt
    | Too_large
    | Invalid_name
    | Busy
    | Closed
  [@@deriving equal, sexp_of]

  type publication =
    | Not_published
    | Published_durability_unknown
  [@@deriving equal, sexp_of]

  type t =
    { operation : operation
    ; code : code
    ; publication : publication option
    }
  [@@deriving sexp_of]

  let code t = t.code
  let operation t = t.operation
  let publication t = t.publication
  let make ?publication operation code = { operation; code; publication }

  let native_code = function
    | 1 -> Missing
    | 2 -> Exists
    | 3 -> Denied
    | 4 -> Unavailable
    | 5 -> Unsupported_filesystem
    | 6 -> Corrupt
    | 7 -> Too_large
    | 8 -> Busy
    | _ -> Unavailable
  ;;

  let mutation operation code published =
    make
      operation
      (native_code code)
      ~publication:(if published then Published_durability_unknown else Not_published)
  ;;
end

module Name = struct
  type t = string

  let create name =
    if
      String.length name = 0
      || String.length name > 128
      || String.equal name "."
      || String.equal name ".."
      || not
           (String.for_all name ~f:(function
              | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
              | _ -> false))
    then Error (Error.make Validate Invalid_name)
    else Ok name
  ;;
end

external native_open
  :  Native_unix.file_descr
  -> string array
  -> int * bool * Native_unix.file_descr
  = "ochat_private_open_directory"

external native_read
  :  Native_unix.file_descr
  -> string
  -> int
  -> int * string
  = "ochat_private_read"

external native_write
  :  Native_unix.file_descr
  -> string
  -> string
  -> bool
  -> int
  -> int * bool * Native_unix.file_descr
  = "ochat_private_write"

external native_delete
  :  Native_unix.file_descr
  -> string
  -> Native_unix.file_descr
  -> bool
  -> int * bool
  = "ochat_private_delete"

external native_lock
  :  Native_unix.file_descr
  -> string
  -> bool
  -> int * bool * Native_unix.file_descr
  = "ochat_private_lock"

external native_close : Native_unix.file_descr -> unit = "ochat_private_close"

let joined f = Eio.Cancel.protect (fun () -> Eio_unix.run_in_systhread f)
let close_native descriptor = joined (fun () -> native_close descriptor)

module Descriptor = struct
  type t =
    { fd : Eio_unix.Fd.t
    ; native : Native_unix.file_descr
    ; mutex : Eio.Mutex.t
    ; mutable closed : bool
    }

  let close t =
    Eio.Cancel.protect (fun () ->
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if not t.closed
        then (
          ignore (Eio_unix.Fd.remove t.fd : Native_unix.file_descr option);
          close_native t.native;
          t.closed <- true)))
  ;;

  let adopt ~sw native =
    Eio.Cancel.protect (fun () ->
      let fd =
        try Eio_unix.Fd.of_unix ~sw ~close_unix:false native with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          close_native native;
          Stdlib.Printexc.raise_with_backtrace exn backtrace
      in
      let t = { fd; native; mutex = Eio.Mutex.create (); closed = false } in
      try
        Eio.Switch.on_release sw (fun () -> close t);
        t
      with
      | exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        close t;
        Stdlib.Printexc.raise_with_backtrace exn backtrace)
  ;;
end

module Directory = struct
  type t =
    { fd : Descriptor.t
    ; mutex : Eio.Mutex.t
    ; mutable closed : bool
    }

  let maximum_bytes = 1024 * 1024

  let close t =
    Eio.Cancel.protect (fun () ->
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if not t.closed
        then (
          t.closed <- true;
          Descriptor.close t.fd)))
  ;;

  let open_anchor ~sw anchor =
    try Ok (Eio.Path.open_dir ~sw anchor) with
    | Eio.Io (error, _) ->
      let code =
        match error with
        | Eio.Fs.E (Not_found _) -> Error.Missing
        | Eio.Fs.E (Permission_denied _) -> Denied
        | Eio.Fs.E (Not_native _) -> Unsupported_filesystem
        | _ -> Unavailable
      in
      Error (Error.make Open_directory code ~publication:Not_published)
  ;;

  let open_anchor_flow ~sw anchor =
    try Ok (Eio.Path.open_in ~sw Eio.Path.(anchor / ".")) with
    | Eio.Io (error, _) ->
      let code =
        match error with
        | Eio.Fs.E (Not_found _) -> Error.Missing
        | Eio.Fs.E (Permission_denied _) -> Denied
        | Eio.Fs.E (Not_native _) -> Unsupported_filesystem
        | _ -> Unavailable
      in
      Error (Error.make Open_directory code ~publication:Not_published)
  ;;

  let open_or_create ~sw ~anchor ~components =
    match components with
    | [] -> Error (Error.make Open_directory Invalid_name ~publication:Not_published)
    | _ ->
      Eio.Switch.run (fun anchor_sw ->
        match open_anchor ~sw:anchor_sw anchor with
        | Error error -> Error error
        | Ok opened ->
          (match open_anchor_flow ~sw:anchor_sw opened with
           | Error error -> Error error
           | Ok resource ->
             (match Eio_unix.Resource.fd_opt resource with
              | None ->
                Error
                  (Error.make
                     Open_directory
                     Unsupported_filesystem
                     ~publication:Not_published)
              | Some anchor_fd ->
                let result =
                  Eio.Cancel.protect (fun () ->
                    let code, published, descriptor =
                      Eio_unix.Fd.use_exn "private storage anchor" anchor_fd (fun fd ->
                        joined (fun () -> native_open fd (Array.of_list components)))
                    in
                    if code <> 0
                    then Error (Error.mutation Open_directory code published)
                    else (
                      let fd = Descriptor.adopt ~sw descriptor in
                      let t = { fd; mutex = Eio.Mutex.create (); closed = false } in
                      (try Eio.Switch.on_release sw (fun () -> close t) with
                       | exn ->
                         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                         close t;
                         Stdlib.Printexc.raise_with_backtrace exn backtrace);
                      Ok t))
                in
                (try
                   Eio.Fiber.check ();
                   result
                 with
                 | exn ->
                   let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                   (match result with
                    | Ok t -> close t
                    | Error _ -> ());
                   Stdlib.Printexc.raise_with_backtrace exn backtrace))))
  ;;

  let with_directory t operation ~mutation f =
    (* Operations borrow the descriptor without changing mutex-owned lifetime
       state. Expected cancellation must unlock without poisoning that state. *)
    Eio.Mutex.use_ro t.mutex (fun () ->
      if t.closed
      then
        Error
          (Error.make
             operation
             Closed
             ?publication:(if mutation then Some Error.Not_published else None))
      else Eio_unix.Fd.use_exn "private storage" t.fd.fd f)
  ;;

  let bounds operation ~mutation length =
    if length <= 0 || length > maximum_bytes
    then
      Some
        (Error.make
           operation
           Too_large
           ?publication:(if mutation then Some Error.Not_published else None))
    else None
  ;;

  let read_bounded t name ~max_bytes =
    match bounds Read ~mutation:false max_bytes with
    | Some error -> Error error
    | None ->
      with_directory t Read ~mutation:false (fun fd ->
        let code, data = joined (fun () -> native_read fd name max_bytes) in
        let bytes = Bytes.of_string data in
        (try Eio.Fiber.check () with
         | exn ->
           let backtrace = Stdlib.Printexc.get_raw_backtrace () in
           Bytes.fill bytes ~pos:0 ~len:(Bytes.length bytes) '\000';
           Stdlib.Printexc.raise_with_backtrace exn backtrace);
        if code = 0 then Ok bytes else Error (Error.make Read (Error.native_code code)))
  ;;

  let write t name bytes ~replace ~fault ~after_native =
    let operation = if replace then Error.Replace else Error.Create in
    match bounds operation ~mutation:true (Bytes.length bytes) with
    | Some error -> Error error
    | None ->
      let data = Bytes.to_string bytes in
      with_directory t operation ~mutation:true (fun fd ->
        let code, published, owned =
          joined (fun () -> native_write fd name data replace fault)
        in
        Fun.protect
          ~finally:(fun () -> if published then close_native owned)
          (fun () ->
             (try
                Eio.Cancel.protect after_native;
                Eio.Fiber.check ()
              with
              | exn ->
                let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                if published && not replace
                then
                  ignore
                    (joined (fun () -> native_delete fd name owned true) : int * bool);
                Stdlib.Printexc.raise_with_backtrace exn backtrace);
             if code = 0 then Ok () else Error (Error.mutation operation code published)))
  ;;

  let create_immutable t name bytes =
    write t name bytes ~replace:false ~fault:0 ~after_native:Fn.ignore
  ;;

  let replace_metadata t name bytes =
    write t name bytes ~replace:true ~fault:0 ~after_native:Fn.ignore
  ;;

  let delete t name =
    with_directory t Delete ~mutation:true (fun fd ->
      let code, published = joined (fun () -> native_delete fd name fd false) in
      Eio.Fiber.check ();
      if code = 0 then Ok () else Error (Error.mutation Delete code published))
  ;;

  module For_testing = struct
    type fault =
      | Before_publication
      | Before_directory_sync

    let create_with_fault t name bytes ~fault =
      write
        t
        name
        bytes
        ~replace:false
        ~fault:
          (match fault with
           | Before_publication -> 1
           | Before_directory_sync -> 2)
        ~after_native:Fn.ignore
    ;;

    let create_with_completion_hook t name bytes ~after_native =
      write t name bytes ~replace:false ~fault:0 ~after_native
    ;;
  end
end

module Lock = struct
  type mode =
    | Shared
    | Exclusive
  [@@deriving sexp_of]

  type t = Descriptor.t

  let release t = Descriptor.close t

  let acquire directory name ~sw ~mode =
    Directory.with_directory directory Open_lock ~mutation:true (fun fd ->
      let code, created, descriptor =
        joined (fun () ->
          native_lock
            fd
            name
            (match mode with
             | Exclusive -> true
             | Shared -> false))
      in
      if code <> 0
      then Error (Error.mutation Open_lock code created)
      else (
        let lease = Descriptor.adopt ~sw descriptor in
        try
          Eio.Fiber.check ();
          Ok lease
        with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          release lease;
          Stdlib.Printexc.raise_with_backtrace exn backtrace))
  ;;
end
