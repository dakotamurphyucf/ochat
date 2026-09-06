let create ~request ~notifications ~close =
  let closed, close_resolver = Eio.Promise.create () in
  let closed_flag = ref false in
  let mutex = Eio.Mutex.create () in
  let close_once () =
    let should_close =
      Eio.Mutex.use_rw ~protect:true mutex (fun () ->
        if !closed_flag
        then false
        else (
          closed_flag := true;
          Eio.Promise.resolve close_resolver ();
          true))
    in
    if should_close then close ()
  in
  Transport.create
    ~request
    ~next_notification:(fun () ->
      Eio.Fiber.first
        (fun () -> Some (Eio.Stream.take notifications))
        (fun () ->
           Eio.Promise.await closed;
           None))
    ~close:close_once
  |> Connection.create
;;
