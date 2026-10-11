open Core
open Support

let%expect_test "retained original directory and captured code have separate lifetimes" =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>original</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let captured =
        Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
      in
      let workspace = Eio.Path.(parent / "workspace") in
      Eio.Path.rename workspace Eio.Path.(parent / "old-workspace");
      Eio.Path.mkdir ~perm:0o700 workspace;
      save workspace "root.chatmd" "<user>replacement</user>";
      let retained =
        Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
      in
      assert (String.equal (root_text retained) "<user>original</user>");
      save root "root.chatmd" "<user>edited original</user>";
      let edited =
        Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
      in
      assert (String.equal (root_text edited) "<user>edited original</user>");
      assert (String.equal (root_text captured) "<user>original</user>");
      Owner.Policy.revoke (Owner.policy owner);
      error
        (Owner.with_scope owner ~f:(fun scope -> capture env scope definition))
        Revoked;
      assert (String.equal (root_text captured) "<user>original</user>")));
  print_endline
    "Fresh reads use the admitted directory; earlier captures keep their bytes. \
     Revocation stops reads.";
  [%expect
    {| Fresh reads use the admitted directory; earlier captures keep their bytes. Revocation stops reads. |}]
;;

let%test_unit "runtime lease limit precedes duplication and is released once" =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>original</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let policy =
        Owner.Policy.create (receipt definition) ~limits:(limits ~leases:1 ())
      in
      let owner = Owner.open_owned root ~sw ~policy ~checkpoint:ignore |> ok in
      let lease = Owner.lease owner ~sw ~checkpoint:ignore |> ok in
      let before = descriptors env in
      error (Owner.lease owner ~sw ~checkpoint:(fun () -> assert false)) Lease_limit;
      assert (descriptors env = before);
      Owner.close lease;
      Owner.close lease;
      let next = Owner.lease owner ~sw ~checkpoint:ignore |> ok in
      Owner.close owner;
      assert (
        String.equal
          (root_text
             (Owner.with_scope next ~f:(fun scope -> capture env scope definition) |> ok))
          "<user>original</user>");
      Owner.Policy.revoke policy;
      error (Owner.check next) Revoked))
;;

let%test_unit "actual parser inputs provide declaration routes, foreign inputs do not" =
  with_workspace (fun env _ root ->
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "nested");
    save root "root.chatmd" {|<import src="nested/import.chatmd"/>|};
    save root "nested/import.chatmd" "<user>import</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let captured =
        Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
      in
      let inputs =
        Owner.Capture.value captured
        |> Builder.Capture.root
        |> Prompt.Chat_markdown.Parsed.inputs
      in
      let input =
        List.find_exn inputs ~f:(fun input ->
          String.equal (Chatmd_parser_input.path input) "nested/import.chatmd")
      in
      let route = Owner.Capture.route captured input ~checkpoint:ignore |> ok in
      let project = Source.Portable_path.create "custom.project.json" |> ok in
      (match
         Owner.Capture.Route.local_root
           route
           ~locator:"package"
           ~project
           ~max_locator_bytes:4096
           ~checkpoint:ignore
         |> ok
       with
       | Chatml_project_prepare.Root.Local { locator; project = actual } ->
         assert (
           String.equal
             locator
             (Filename.concat
                (Filename.concat (Filename.dirname definition.root_file) "nested")
                "package"));
         assert (Source.Portable_path.equal actual project)
       | Git _ -> assert false);
      let absolute = "/package" in
      ignore
        (Owner.Capture.Route.local_root
           route
           ~locator:absolute
           ~project
           ~max_locator_bytes:(String.length absolute)
           ~checkpoint:ignore
         |> ok
         : Chatml_project_prepare.Root.t);
      let another =
        Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
      in
      assert (Result.is_error (Owner.Capture.route another input ~checkpoint:ignore))))
;;

let%test_unit
    "independent Local lease survives original Eio root switch and source owner close"
  =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>retained descriptor</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner =
        Eio.Switch.run (fun original_sw ->
          let original = Eio.Path.open_dir ~sw:original_sw Eio.Path.(root / ".") in
          open_owner sw original definition)
      in
      let lease = Owner.lease owner ~sw ~checkpoint:ignore |> ok in
      Owner.close owner;
      let captured =
        Owner.with_scope lease ~f:(fun scope -> capture env scope definition) |> ok
      in
      assert (String.equal (root_text captured) "<user>retained descriptor</user>")))
;;

let%test_unit
    "explicit close unregisters long-lived switch owners instead of retaining closed \
     metadata"
  =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>bounded lifetime</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let before = descriptors env in
      let remember_closed () =
        let owner = open_owner sw root definition in
        let remembered = Stdlib.Weak.create 1 in
        Stdlib.Weak.set remembered 0 (Some owner);
        Owner.close owner;
        remembered
      in
      let owners = List.init 20 ~f:(fun _ -> remember_closed ()) in
      let remember_local () =
        let owner =
          Chatml_local_source.open_root
            root
            ~sw
            ~max_path_components:32
            ~checkpoint:ignore
          |> ok
        in
        let remembered = Stdlib.Weak.create 1 in
        Stdlib.Weak.set remembered 0 (Some owner);
        Chatml_local_source.close owner;
        remembered
      in
      let locals = List.init 20 ~f:(fun _ -> remember_local ()) in
      assert (descriptors env = before);
      Stdlib.Gc.full_major ();
      List.iter owners ~f:(fun owner -> assert (not (Stdlib.Weak.check owner 0)));
      List.iter locals ~f:(fun owner -> assert (not (Stdlib.Weak.check owner 0)))))
;;
