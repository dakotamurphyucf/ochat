open! Core
module P = Agent_protocol
module C = Agent_server.Recurrence_automation_configuration
module H = Agent_server.Recurrence_automation_host
module Port = Agent_session.Recurrence_planning_host
module Reason = Agent_session.Recurrence_planning_status.Reason

let utc = [%blob "../recurrence_zone_data/fixtures/Etc_UTC.tzif"]

let ok = function
  | Ok value -> value
  | Error _ -> failwith "recurrence host fixture admission failed"
;;

let principal_id = P.Id.Principal.of_string "pri_recurrence_host" |> ok
let server_id = P.Id.Server.of_string "srv_recurrence_host" |> ok
let session_id = P.Id.Session.of_string "ses_recurrence_host" |> ok
let controller = P.Session_ref.create ~server_id ~session_id
let timestamp value = P.Timestamp.of_string value |> ok
let initial_time = timestamp "2050-01-01T00:00:00Z"
let expiry = timestamp "2050-01-02T00:00:00Z"

let with_workspace f =
  Eio_main.run (fun env ->
    let random = Cstruct.create 16 in
    Eio.Flow.read_exact (Eio.Stdenv.secure_random env) random;
    let suffix =
      String.concat_map (Cstruct.to_string random) ~f:(fun byte ->
        sprintf "%02x" (Char.to_int byte))
    in
    (* macOS /tmp is itself an alias. Exercise genuinely canonical configured
       components here; the separate alias case must be the one that rejects. *)
    let temporary =
      let private_tmp = Eio.Path.(Eio.Stdenv.fs env / "/private/tmp") in
      match Eio.Path.kind ~follow:false private_tmp with
      | `Directory -> "/private/tmp"
      | _ -> "/tmp"
    in
    let root =
      Eio.Path.(Eio.Stdenv.fs env / temporary / ("ochat-recurrence-host-" ^ suffix))
    in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~f:(fun () -> Eio.Switch.run (fun sw -> f env sw root))
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root)))
;;

let admitted_config env root extra =
  let source_file = Eio.Path.native_exn Eio.Path.(root / "server.sexp") in
  let text =
    sprintf
      {|(version 1)
(server ((data_dir "./data") (unix_socket "./agent.sock") %s))
(workspaces ()) (prompts ()) (permission_profiles ())|}
      extra
  in
  Agent_server.Config_parser.parse_string ~source_file text
  |> ok
  |> Agent_server.Config_validator.validate ~env
;;

let automation zone =
  sprintf
    {|(recurrence_automation ((principal_id pri_recurrence_host)
      (controllers (ses_recurrence_host)) (expires_at 2050-01-02T00:00:00Z) %s))|}
    zone
;;

let%expect_test "real administrative grammar and validated decoders precede IO" =
  with_workspace (fun env _ root ->
    let config =
      admitted_config
        env
        root
        (automation
           {|(zone_directory ((directory absent) (source pinned-2025b)
              (from 2049-01-01T00:00:00Z) (until 2051-01-01T00:00:00Z)))|})
      |> ok
    in
    let admitted = Option.value_exn config.server.recurrence_automation in
    let zone = Option.value_exn admitted.zone_directory in
    assert (String.equal zone.directory (Eio.Path.native_exn Eio.Path.(root / "absent")));
    assert (C.equal admitted (C.t_of_sexp (C.sexp_of_t admitted)));
    assert (
      Option.is_none (admitted_config env root "" |> ok).server.recurrence_automation);
    let invalid extra = assert (Result.is_error (admitted_config env root extra)) in
    List.iter
      [ "(recurrence_automation ((principal_id pri_x) (controllers ())))"
      ; "(recurrence_automation ((principal_id pri_x) (controllers (ses_x ses_x))))"
      ; "(recurrence_automation ((principal_id ses_wrong) (controllers (ses_x))))"
      ; automation
          {|(zone_directory ((directory ../zones) (source pinned)
              (from 2051-01-01T00:00:00Z) (until 2050-01-01T00:00:00Z)))|}
      ]
      ~f:invalid;
    let malformed =
      Sexp.List
        [ List [ Atom "principal_id"; Atom "ses_wrong" ]
        ; List [ Atom "controllers"; List [ Atom "ses_x" ] ]
        ; List [ Atom "expires_at"; List [] ]
        ; List [ Atom "zone_directory"; List [] ]
        ]
    in
    assert (Result.is_error (Or_error.try_with (fun () -> C.t_of_sexp malformed)));
    print_endline
      "real config; canonical source-relative path; bounded typed decode; absent stays \
       disabled");
  [%expect
    {| real config; canonical source-relative path; bounded typed decode; absent stays disabled |}]
;;

let%expect_test "actual no-follow directory host, original expiry and instance revocation"
  =
  with_workspace (fun env sw root ->
    let zones = Eio.Path.(root / "zones") in
    Eio.Path.mkdir ~perm:0o700 zones;
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(zones / "UTC") utc;
    Eio.Path.symlink ~link_to:"zones" Eio.Path.(root / "alias");
    let now = ref initial_time in
    let make directory =
      let zone_directory =
        C.Zone_directory.create
          ~directory:(Eio.Path.native_exn Eio.Path.(root / directory))
          ~source:"pinned-2025b"
          ~from:(P.Timestamp.to_time_ns (timestamp "2049-01-01T00:00:00Z"))
          ~until:(P.Timestamp.to_time_ns (timestamp "2051-01-01T00:00:00Z"))
        |> ok
      in
      C.create
        ~principal_id
        ~controllers:[ session_id ]
        ~expires_at:(Some expiry)
        ~zone_directory:(Some zone_directory)
      |> ok
    in
    let open_host directory =
      H.open_owned
        (make directory)
        ~server_id
        ~sw
        ~fs:(Eio.Stdenv.fs env)
        ~now:(fun () -> !now)
        ~checkpoint:ignore
      |> ok
    in
    let host = open_host "zones" in
    let port = H.planning_host host in
    let proof = Port.authorize port ~controller |> ok in
    assert (Operator_authorization.is_current proof);
    let principal = Operator_authorization.principal proof in
    assert (P.Scope.Set.equal principal.scopes (P.Scope.Set.singleton Send_messages));
    assert (String.equal principal.authentication_kind "daemon.recurrence");
    let selected = Port.load_zone port ~name:"UTC" |> ok |> Option.value_exn in
    assert (
      String.equal
        (Recurrence_rules.Zone.Provenance.sha256
           (Recurrence_rules.Zone.provenance selected))
        "fddce1e648a1732ac29afd9a16151b2973cdf082e7ec0c690f7e42be6b598b93");
    let foreign =
      P.Session_ref.create
        ~server_id:(P.Id.Server.of_string "srv_foreign" |> ok)
        ~session_id
    in
    assert (Result.is_error (Port.authorize port ~controller:foreign));
    let alias = open_host "alias" in
    assert (H.Zone_status.equal (H.zone_status alias) Directory_unavailable);
    assert (
      Operator_authorization.is_current
        (Port.authorize (H.planning_host alias) ~controller |> ok));
    assert (Option.is_none (Port.load_zone (H.planning_host alias) ~name:"UTC" |> ok));
    now := expiry;
    assert (not (Operator_authorization.is_current proof));
    (match Port.authorize port ~controller with
     | Error reason -> assert (Reason.equal reason Expired)
     | Ok _ -> failwith "original expiry was extended");
    now := initial_time;
    H.close host;
    assert (not (Operator_authorization.is_current proof));
    H.close_and_wait host;
    H.close_and_wait alias;
    assert (String.equal (Eio.Path.load Eio.Path.(zones / "UTC")) utc);
    print_endline
      "canonical pinned bytes; directory alias unavailable; exact server/grant; expiry \
       and close fence original proof");
  [%expect
    {| canonical pinned bytes; directory alias unavailable; exact server/grant; expiry and close fence original proof |}]
;;
