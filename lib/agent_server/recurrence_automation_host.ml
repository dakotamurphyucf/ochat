open! Core
module P = Agent_protocol
module A = Agent_session
module Local = Chatml_local_source
module Configuration = Recurrence_automation_configuration

module Zone_status = struct
  type t =
    | Not_configured
    | Directory_unavailable
    | Available
  [@@deriving equal, sexp_of]
end

type t =
  { configuration : Configuration.t
  ; server_id : P.Id.Server.t
  ; principal : P.Principal.t
  ; now : unit -> P.Timestamp.t
  ; checkpoint : unit -> unit
  ; root : Local.t option
  ; zone_status : Zone_status.t
  ; mutable closed : bool
  ; mutable active : int
  ; idle : Eio.Condition.t
  ; domain : int
  ; sw : Eio.Switch.t
  }

let domain () = (Stdlib.Domain.self () :> int)

let check_domain t =
  if not (Int.equal t.domain (domain ()))
  then invalid_arg "Recurrence_automation_host: foreign domain"
;;

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ();
  Eio.Fiber.check ()
;;

let open_directory zone ~sw ~fs ~checkpoint =
  let open Result.Let_syntax in
  let components =
    if String.equal zone.Configuration.Zone_directory.directory "/"
    then []
    else String.drop_prefix zone.directory 1 |> String.split ~on:'/'
  in
  let%bind directory =
    Local.Directory_path.create
      components
      ~max_bytes:4096
      ~max_components:64
      ~checkpoint:(fun () -> poll checkpoint)
    |> Result.map_error ~f:(fun _ -> ())
  in
  Eio.Switch.run (fun temporary ->
    poll checkpoint;
    (* Only the explicit filesystem root is opened through Eio. The arbitrary
         configured path is traversed exclusively by the qualified Local owner. *)
    let%bind root =
      try Ok (Eio.Path.open_dir ~sw:temporary Eio.Path.(fs / "/")) with
      | Eio.Io _ -> Error ()
    in
    let%bind base =
      Local.open_root root ~sw:temporary ~max_path_components:64 ~checkpoint:(fun () ->
        poll checkpoint)
      |> Result.map_error ~f:(fun _ -> ())
    in
    Local.open_subroot base ~sw ~directory ~checkpoint:(fun () -> poll checkpoint)
    |> Result.map_error ~f:(fun _ -> ()))
;;

let open_owned configuration ~server_id ~sw ~fs ~now ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = Configuration.validate configuration in
  let%bind principal =
    P.Principal.create
      ~id:configuration.principal_id
      ~authentication_kind:"daemon.recurrence"
      ~scopes:(P.Scope.Set.singleton Send_messages)
      ~attributes:[]
  in
  let root, zone_status =
    match configuration.zone_directory with
    | None -> None, Zone_status.Not_configured
    | Some zone ->
      (match open_directory zone ~sw ~fs ~checkpoint with
       | Ok root -> Some root, Zone_status.Available
       | Error () -> None, Zone_status.Directory_unavailable)
  in
  Ok
    { configuration
    ; server_id
    ; principal
    ; now
    ; checkpoint
    ; root
    ; zone_status
    ; closed = false
    ; active = 0
    ; idle = Eio.Condition.create ()
    ; domain = domain ()
    ; sw
    }
;;

let reason t ~controller =
  check_domain t;
  if t.closed || Option.is_some (Eio.Switch.get_error t.sw)
  then Some A.Recurrence_planning_status.Reason.Closing
  else if
    (not (P.Id.Server.equal t.server_id (P.Session_ref.server_id controller)))
    || not
         (List.mem
            t.configuration.controllers
            (P.Session_ref.session_id controller)
            ~equal:P.Id.Session.equal)
  then Some A.Recurrence_planning_status.Reason.Not_granted
  else if
    Option.exists t.configuration.expires_at ~f:(fun expiry ->
      P.Timestamp.compare (t.now ()) expiry >= 0)
  then Some A.Recurrence_planning_status.Reason.Expired
  else None
;;

let authorize t ~controller =
  match reason t ~controller with
  | Some reason -> Error reason
  | None ->
    Ok
      (Operator_authorization.guarded ~principal:t.principal ~is_current:(fun () ->
         Option.is_none (reason t ~controller)))
;;

let load_zone t ~name =
  check_domain t;
  match t.closed, t.root, t.configuration.zone_directory with
  | false, Some root, Some zone ->
    if Int.equal t.active Int.max_value
    then
      Error
        (A.Recurrence_zone_loader.Error.Invalid
           (Recurrence_rules.Timing_error.Invalid "planning load count overflow"))
    else (
      t.active <- t.active + 1;
      Exn.protect
        ~f:(fun () ->
          A.Recurrence_zone_loader.load_local
            root
            ~source:zone.source
            ~name
            ~from:zone.from
            ~until:zone.until
            ~checkpoint:t.checkpoint
          |> Result.map ~f:Option.some)
        ~finally:(fun () ->
          t.active <- t.active - 1;
          if t.active = 0 then Eio.Condition.broadcast t.idle))
  | true, _, _ | false, None, _ | false, Some _, None -> Ok None
;;

let planning_host t =
  check_domain t;
  A.Recurrence_planning_host.create ~authorize:(authorize t) ~load_zone:(load_zone t)
;;

let zone_status t =
  check_domain t;
  t.zone_status
;;

let close t =
  check_domain t;
  t.closed <- true
;;

let close_and_wait t =
  close t;
  while t.active > 0 do
    Eio.Condition.await_no_mutex t.idle
  done;
  Option.iter t.root ~f:Local.close
;;
