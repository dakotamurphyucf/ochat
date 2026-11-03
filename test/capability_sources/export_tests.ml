open! Core
open Support

let%test_unit "exclusive destination rejects files directories and both symlink forms" =
  with_workspace (fun _ root parent ->
    let selected = resource () in
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(root / "existing-file") "preserve";
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "existing-directory");
    Eio.Path.symlink ~link_to:"existing-file" Eio.Path.(root / "existing-link");
    Eio.Path.symlink ~link_to:"missing" Eio.Path.(root / "dangling-link");
    List.iter
      [ "existing-file"; "existing-directory"; "existing-link"; "dangling-link" ]
      ~f:(fun name ->
        require_error (export selected parent name) ~code:Exists ~progress:Not_created);
    [%test_eq: string] (Eio.Path.load Eio.Path.(root / "existing-file")) "preserve")
;;

let first_file =
  "examples/learning/lifecycle-composition/components/counter/src/counter.chatml"
;;

let%test_unit "exclusive member creation never follows existing or dangling symlinks" =
  List.iter [ false; true ] ~f:(fun dangling ->
    with_workspace (fun _ root parent ->
      let selected = resource () in
      let victim = Eio.Path.(root / "victim") in
      if not dangling then Eio.Path.save ~create:(`Exclusive 0o600) victim "preserve";
      let leaf = Eio.Path.(root / "tree" / first_file) in
      let injected = ref false in
      let checkpoint () =
        Eio.Fiber.check ();
        if
          (not !injected)
          && Eio.Path.is_directory Eio.Path.(root / "tree" / Filename.dirname first_file)
        then (
          injected := true;
          Eio.Path.symlink ~link_to:(Eio.Path.native_exn victim) leaf)
      in
      require_error
        (export ~checkpoint selected parent "tree")
        ~code:Exists
        ~progress:Incomplete;
      assert !injected;
      if dangling
      then assert (missing victim)
      else [%test_eq: string] (Eio.Path.load victim) "preserve"))
;;

let%test_unit
    "directory admission rejects replacement before writes and retains caller tree"
  =
  with_workspace (fun _ root parent ->
    let selected = resource () in
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "victim");
    let injected = ref false in
    let checkpoint () =
      Eio.Fiber.check ();
      if (not !injected) && Eio.Path.is_directory Eio.Path.(root / "tree")
      then (
        injected := true;
        Eio.Path.rename Eio.Path.(root / "tree") Eio.Path.(root / "moved");
        Eio.Path.symlink ~link_to:"victim" Eio.Path.(root / "tree"))
    in
    require_error
      (export ~checkpoint selected parent "tree")
      ~code:Changed
      ~progress:Incomplete;
    assert !injected;
    [%test_eq: string list] (Eio.Path.read_dir Eio.Path.(root / "victim")) [];
    assert (Eio.Path.is_directory Eio.Path.(root / "moved")))
;;

let%test_unit
    "actual cancellation joins export and never reports a successful partial tree"
  =
  with_workspace (fun _ root parent ->
    let selected = resource () in
    let cancelled = ref false in
    (try
       Eio.Cancel.sub (fun cancel ->
         let checkpoint () =
           if Eio.Path.is_directory Eio.Path.(root / "tree")
           then Eio.Cancel.cancel cancel Exit;
           Eio.Fiber.check ()
         in
         ignore (export ~checkpoint selected parent "tree" : (_, _) Result.t))
     with
     | Eio.Cancel.Cancelled _ -> cancelled := true);
    assert !cancelled;
    assert (
      missing
        Eio.Path.(root / "tree" / "examples/learning/lifecycle-composition/agent.chatmd"));
    require_error (export selected parent "tree") ~code:Exists ~progress:Not_created)
;;
