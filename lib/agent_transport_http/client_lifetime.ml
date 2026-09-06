open! Core

type t =
  { sw : Eio.Switch.t
  ; stopped : unit Eio.Promise.t
  }

exception Closed

let switch t = t.sw

let close t =
  Eio.Cancel.protect (fun () ->
    if not (Eio.Promise.is_resolved t.stopped) then Eio.Switch.fail t.sw Closed;
    Eio.Promise.await t.stopped)
;;

let initialize construct ready stopped owner stopping =
  Eio.Switch.run (fun sw ->
    let lifetime = { sw; stopped } in
    owner := Some lifetime;
    if !stopping then raise Closed;
    let result = construct lifetime in
    Eio.Promise.resolve ready result;
    match result with
    | Error _ -> Eio.Switch.fail sw Closed
    | Ok _ -> Eio.Fiber.await_cancel ())
;;

let run_owner construct (ready, resolve_ready) (stopped, resolve_stopped) owner stopping =
  Exn.protect
    ~finally:(fun () -> Eio.Promise.resolve resolve_stopped ())
    ~f:(fun () ->
      try initialize construct resolve_ready stopped owner stopping with
      | Closed -> ()
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
        if Eio.Promise.is_resolved ready
        then raise exn
        else
          Eio.Promise.resolve
            resolve_ready
            (Error
               (Agent_protocol.Error.create
                  Interrupted
                  ~message:"HTTP connection initialization failed"
                  ~retryable:true
                  ())))
;;

let start ~sw construct =
  Eio.Switch.check sw;
  let ready = Eio.Promise.create () in
  let stopped = Eio.Promise.create () in
  let owner = ref None in
  let stopping = ref false in
  let transferred = ref false in
  Eio.Fiber.fork ~sw (fun () -> run_owner construct ready stopped owner stopping);
  Exn.protect
    ~finally:(fun () ->
      if not !transferred
      then
        Eio.Cancel.protect (fun () ->
          stopping := true;
          Option.iter !owner ~f:close;
          Eio.Promise.await (fst stopped)))
    ~f:(fun () ->
      let result = Eio.Promise.await (fst ready) in
      transferred := Result.is_ok result;
      result)
;;
