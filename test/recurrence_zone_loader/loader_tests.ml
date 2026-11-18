open Core
module Loader = Agent_session.Recurrence_zone_loader
module Local = Chatml_local_source
module Zone = Recurrence_rules.Zone

let lord_howe = [%blob "../recurrence_zone_data/fixtures/Australia_Lord_Howe.tzif"]
let utc = [%blob "../recurrence_zone_data/fixtures/Etc_UTC.tzif"]
let time = Time_ns.of_string_with_utc_offset
let from = time "2049-12-30 00:00:00Z"
let until = time "2051-01-02 00:00:00Z"

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Loader.Error.t)]
;;

let offset zone at =
  match Zone.offset_seconds zone (time at) with
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Recurrence_rules.Timing_error.t)]
;;

let save path bytes = Eio.Path.save ~create:(`Or_truncate 0o600) path bytes

let with_root f =
  Eio_main.run (fun env ->
    let random = Cstruct.create 16 in
    Eio.Flow.read_exact (Eio.Stdenv.secure_random env) random;
    let suffix =
      Cstruct.to_string random
      |> String.to_list
      |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
      |> String.concat
    in
    let anchor =
      Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("ochat-zone-loader-" ^ suffix))
    in
    Eio.Path.mkdir ~perm:0o700 anchor;
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree anchor))
      (fun () ->
         let workspace = Eio.Path.(anchor / "zones") in
         Eio.Path.mkdir ~perm:0o700 workspace;
         Eio.Path.with_open_dir workspace (fun root ->
           Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "Australia");
           save Eio.Path.(root / "Australia/Lord_Howe") lord_howe;
           f anchor root)))
;;

let load
      ?(name = "Australia/Lord_Howe")
      ?(source = "IANA2025b-controlled")
      ?(from = from)
      ?(until = until)
      ?(checkpoint = ignore)
      root
  =
  Loader.load root ~source ~name ~from ~until ~checkpoint
;;

let filesystem result operation code =
  match result with
  | Error (Loader.Error.Filesystem error) ->
    assert (Local.Error.equal_operation (Local.Error.operation error) operation);
    assert (Local.Error.equal_code (Local.Error.code error) code)
  | Ok _ | Error (Invalid _ | Data _) ->
    failwith "expected original filesystem classification"
;;

let%expect_test "retained canonical file yields exact immutable finite zone" =
  let retained =
    with_root (fun _ root ->
      let selected = load root |> ok in
      assert (String.equal (Zone.name selected) "Australia/Lord_Howe");
      let lower, upper = Zone.coverage selected in
      assert (Time_ns.equal lower from && Time_ns.equal upper until);
      let provenance = Zone.provenance selected in
      assert (String.equal (Zone.Provenance.source provenance) "IANA2025b-controlled");
      assert (
        String.equal
          (Zone.Provenance.sha256 provenance)
          "f368bd25659c0293d02bb79ec7dac7d5b73a92dffafce14b4dd2ffb8ba11aada");
      assert (offset selected "2050-04-02 14:59:59Z" = 39600);
      assert (offset selected "2050-04-02 15:00:00Z" = 37800);
      (* Controlled source replacement, not a claimed real IANA rule update. *)
      save Eio.Path.(root / "Australia/Lord_Howe") utc;
      let updated = load ~source:"controlled-replacement" root |> ok in
      assert (
        String.equal
          (Zone.Provenance.sha256 (Zone.provenance updated))
          "fddce1e648a1732ac29afd9a16151b2973cdf082e7ec0c690f7e42be6b598b93");
      assert (offset updated "2050-04-02 15:00:00Z" = 0);
      assert (offset selected "2050-04-02 15:00:00Z" = 37800);
      Eio.Path.unlink Eio.Path.(root / "Australia/Lord_Howe");
      filesystem (load root) Open_file Missing;
      assert (List.mem (Eio.Path.read_dir root) "Australia" ~equal:String.equal);
      selected)
  in
  (* Both the original directory and its switch have now closed; immutable data
     remains usable without any descriptor, current path or timezone lookup. *)
  assert (offset retained "2050-10-01 15:30:00Z" = 39600);
  print_endline
    "exact hash/name/coverage; explicit reread; old zone survives root and file \
     retirement";
  [%expect
    {| exact hash/name/coverage; explicit reread; old zone survives root and file retirement |}]
;;

let%expect_test
    "preflight precedes IO and canonical-only errors retain reader classification"
  =
  with_root (fun anchor root ->
    let invalid result =
      match result with
      | Error (Loader.Error.Invalid _) -> ()
      | Ok _ | Error (Filesystem _ | Data _) ->
        failwith "input validation did not run first"
    in
    let checkpoint () = failwith "preflight attempted acquisition" in
    let nonempty = Eio.Path.(root / "not-an-open-root") in
    List.iter [ "../UTC"; "/UTC"; "America//UTC"; "UTC\000" ] ~f:(fun name ->
      invalid (load ~name ~checkpoint nonempty));
    List.iter
      [ ""; String.make 257 's' ]
      ~f:(fun source -> invalid (load ~source ~checkpoint nonempty));
    invalid (load ~from:until ~until:from ~checkpoint nonempty);
    filesystem (load nonempty) Borrow_root Not_retained_directory;
    filesystem (load ~name:"Missing" root) Open_file Missing;
    filesystem (load ~name:"Missing/UTC" root) Open_parent Missing;
    save
      Eio.Path.(root / "Oversized")
      (String.make (Recurrence_zone_data.maximum_bytes + 1) 'x');
    filesystem (load ~name:"Oversized" root) Read Too_large;
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "Directory");
    filesystem (load ~name:"Directory" root) Open_file Unsupported_kind;
    save Eio.Path.(anchor / "outside") utc;
    Eio.Path.symlink ~link_to:"Australia/Lord_Howe" Eio.Path.(root / "Internal");
    Eio.Path.symlink ~link_to:"../outside" Eio.Path.(root / "Escape");
    Eio.Path.symlink ~link_to:".." Eio.Path.(root / "Parent");
    List.iter [ "Internal"; "Escape" ] ~f:(fun name ->
      filesystem (load ~name root) Open_file Symlink_or_non_directory);
    filesystem (load ~name:"Parent/outside" root) Open_parent Symlink_or_non_directory;
    save Eio.Path.(root / "Malformed") "TZif";
    (match load ~name:"Malformed" root with
     | Error (Loader.Error.Data (Malformed _)) -> ()
     | Ok _ | Error _ -> failwith "decoder error was reclassified");
    assert (offset (load root |> ok) "2050-04-02 15:00:00Z" = 37800);
    print_endline
      "invalid inputs before acquisition; bounded regular files; all aliases rejected; \
       typed errors");
  [%expect
    {| invalid inputs before acquisition; bounded regular files; all aliases rejected; typed errors |}]
;;

let%expect_test
    "checkpoint failure and final cancellation join cleanup without publishing a zone"
  =
  with_root (fun _ root ->
    let polls = ref 0 in
    ignore (load ~checkpoint:(fun () -> incr polls) root |> ok : Zone.t);
    assert (!polls > 4);
    let visited = ref 0 in
    let marker = Failure "timezone acquisition checkpoint" in
    let checkpoint () =
      incr visited;
      if !visited = !polls / 2 then raise marker
    in
    let published = ref false in
    (try
       ignore (load ~checkpoint root : (Zone.t, Loader.Error.t) Result.t);
       published := true
     with
     | error -> assert (phys_equal marker error));
    assert (not !published);
    assert (List.mem (Eio.Path.read_dir root) "Australia" ~equal:String.equal);
    ignore (load root |> ok : Zone.t);
    let visited = ref 0 in
    (try
       Eio.Switch.run (fun sw ->
         let checkpoint () =
           incr visited;
           if !visited = !polls then Eio.Switch.fail sw Exit
         in
         match load ~checkpoint root with
         | Ok _ -> published := true
         | Error error -> raise_s [%sexp (error : Loader.Error.t)])
     with
     | Exit | Eio.Cancel.Cancelled Exit -> ());
    assert (!visited = !polls);
    assert (not !published);
    assert (offset (load root |> ok) "2050-04-02 15:00:00Z" = 37800);
    print_endline
      "checkpoint exception unchanged; cancellation at final checkpoint; borrowed root \
       reusable");
  [%expect
    {| checkpoint exception unchanged; cancellation at final checkpoint; borrowed root reusable |}]
;;
