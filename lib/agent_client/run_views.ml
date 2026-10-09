open! Core
module P = Agent_protocol

type t =
  { connection : Connection.t
  ; server_id : P.Id.Server.t
  }

let create connection ~server_id = { connection; server_id }

let check_host t reference =
  if not (P.Id.Server.equal t.server_id (P.Session_ref.server_id reference))
  then Error (P.Error.invalid_request "run query belongs to another host")
  else (
    match Connection.initialization t.connection with
    | Some initialized when P.Id.Server.equal initialized.server_id t.server_id -> Ok ()
    | Some _ -> Error (P.Error.invalid_request "run connection names another host")
    | None ->
      Error (P.Error.invalid_request "run query requires an initialized connection"))
;;

let page t (request : P.Run_query.Request.t) =
  let%bind.Result () = check_host t request.session in
  match Connection.request_without_history t.connection (Session_runs request) with
  | Ok (Session_runs page) ->
    if
      List.for_all page.items ~f:(fun view ->
        P.Session_ref.equal view.P.Run_query.View.run.session request.session)
    then Ok page
    else Error (P.Error.invalid_request "run page response names another session host")
  | Ok _ -> Error (P.Error.invalid_request "unexpected session.runs response")
  | Error _ as failure -> failure
;;

let lookup t (request : P.Run_query.Lookup_request.t) =
  let%bind.Result () = check_host t request.session in
  match Connection.request_without_history t.connection (Session_run request) with
  | Ok (Session_run outcome) ->
    let matches =
      match outcome with
      | P.Run_query.Outcome.Available view ->
        P.Session_ref.equal view.run.session request.session
        && P.Id.Run.equal view.run.id request.run_id
      | Unavailable id -> P.Id.Run.equal id request.run_id
    in
    if matches
    then Ok outcome
    else Error (P.Error.invalid_request "run lookup response names another occurrence")
  | Ok _ -> Error (P.Error.invalid_request "unexpected session.run response")
  | Error _ as failure -> failure
;;

let watch t ~sw ~(request : P.Run_query.Lookup_request.t) ~on_result ~on_error =
  let closed = ref false in
  let pending = ref false in
  let synchronization_error = ref None in
  let signal = ref (Eio.Promise.create ()) in
  let last_revision = ref None in
  let request_refresh () =
    if not !pending
    then (
      pending := true;
      Eio.Promise.resolve (snd !signal) ())
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Exn.protect
      ~finally:(fun () -> closed := true)
      ~f:(fun () ->
        let rec refresh () =
          Eio.Promise.await (fst !signal);
          pending := false;
          signal := Eio.Promise.create ();
          let result =
            match !synchronization_error with
            | Some failure ->
              synchronization_error := None;
              Error failure
            | None -> lookup t request
          in
          (match result with
           | Ok outcome -> on_result outcome
           | Error failure -> on_error failure);
          refresh ()
        in
        refresh ()));
  fun projection ->
    let snapshot = Projection.snapshot projection |> P.Public.Snapshot.fields in
    if
      (not !closed)
      && P.Id.Session.equal snapshot.session.id (P.Session_ref.session_id request.session)
    then (
      match Projection.synchronization projection with
      | Snapshot_required failure ->
        last_revision := None;
        synchronization_error := Some failure;
        request_refresh ()
      | Current ->
        synchronization_error := None;
        if
          P.Id.Session.equal
            snapshot.session.id
            (P.Session_ref.session_id request.session)
          && not (Option.equal Int64.equal !last_revision (Some snapshot.revision))
        then (
          last_revision := Some snapshot.revision;
          request_refresh ()))
;;
