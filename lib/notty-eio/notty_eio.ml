open Eio.Std
open Notty

module Term = struct
  type t =
    { trm : Tmachine.t
    ; input : Eio_unix.source_ty r
    ; ibuf : Cstruct.t (* Scratch buffer for [event] *)
    ; iparse : Unescape.t (* Input parser *)
    ; output : Eio_unix.sink_ty r
    ; obuf : Buffer.t (* Pending output *)
    ; cleanup : (unit -> unit) Queue.t (* Actions to perform on release *)
    ; mutable need_resize : bool
    ; mutable hook : Eio.Switch.hook
    }

  let write t =
    Tmachine.output t.trm t.obuf;
    let out = Buffer.contents t.obuf in
    (* XXX There goes 0copy. :/ *)
    Buffer.clear t.obuf;
    Eio.Flow.copy_string out t.output
  ;;

  let release t =
    Eio.Switch.remove_hook t.hook;
    Queue.iter (fun f -> f ()) t.cleanup;
    if Tmachine.release t.trm then write t
  ;;

  let cap_for_fd x =
    Eio_unix.Fd.use_exn "cap_for_fd" (Eio_unix.Resource.fd x)
    @@ fun fd -> Notty_unix.Private.cap_for_fd fd
  ;;

  let refresh t =
    Tmachine.refresh t.trm;
    write t
  ;;

  let image t image =
    Tmachine.image t.trm image;
    write t
  ;;

  let cursor t curs =
    Tmachine.cursor t.trm curs;
    write t
  ;;

  let set_size t dim = Tmachine.set_size t.trm dim
  let size t = Tmachine.size t.trm

  let on_resize t () =
    (* todo: send a notification to the main event loop too *)
    t.need_resize <- true
  ;;

  let with_cleanup t (`Revert fn) = Queue.add fn t.cleanup

  let rec input_events ~on_event t =
    if t.need_resize
    then (
      t.need_resize <- false;
      Buffer.reset t.obuf;
      Eio_unix.Fd.use_exn "winsize" (Eio_unix.Resource.fd t.output) (fun output ->
        Notty_unix.winsize output |> Option.iter (set_size t));
      on_event `Resize)
    else (
      match Unescape.next t.iparse with
      | #Unescape.event as r -> on_event r
      | `End -> raise End_of_file
      | `Await ->
        let len = Eio.Flow.single_read t.input t.ibuf in
        let ibuf = Cstruct.to_bytes t.ibuf ~len in
        Unescape.input t.iparse ibuf 0 len);
    input_events ~on_event t
  ;;

  let run ?(nosig = true) ?(mouse = true) ?(bpaste = true) ~input ~output ~on_event fn =
    (* Note: Notty uses [input_fd] during shutdown, so must hold it open until release: *)
    Eio_unix.Fd.use_exn "notty" (Eio_unix.Resource.fd input)
    @@ fun input_fd ->
    Switch.run
    @@ fun sw ->
    let input = (input :> Eio_unix.source_ty r) in
    let output = (output :> Eio_unix.sink_ty r) in
    let t =
      { trm = Tmachine.create ~mouse ~bpaste (cap_for_fd output)
      ; input
      ; ibuf = Cstruct.create 1024
      ; iparse = Unescape.create ()
      ; output
      ; obuf = Buffer.create 4096
      ; hook = Eio.Switch.null_hook
      ; cleanup = Queue.create ()
      ; need_resize = false
      }
    in
    t.hook <- Switch.on_release_cancellable sw (fun () -> release t);
    with_cleanup t @@ Notty_unix.Private.setup_tcattr ~nosig input_fd;
    with_cleanup t @@ Notty_unix.Private.set_winch_handler (on_resize t);
    Eio_unix.Fd.use_exn "winsize" (Eio_unix.Resource.fd output) (fun output_fd ->
      Notty_unix.winsize output_fd |> Option.iter (set_size t));
    Fiber.fork_daemon ~sw (fun () -> input_events ~on_event t);
    fn t
  ;;
end
