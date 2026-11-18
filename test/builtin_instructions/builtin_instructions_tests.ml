open Core
open Expect_test_helpers_core
module B = Builtin_instructions

let%test_module "embedded instruction catalog" =
  (module struct
    let chatml () =
      match B.find (B.Id.of_string "chatml" |> Or_error.ok_exn) with
      | Ok asset -> asset
      | Error _ -> failwith "reviewed starter missing"
    ;;

    let%expect_test "path-independent reviewed starter" =
      let asset = chatml () in
      print_s
        [%sexp
          (B.Id.to_string (B.Asset.id asset) : string), (B.Asset.title asset : string)];
      (match B.find (B.Id.of_string "not-installed" |> Or_error.ok_exn) with
       | Ok _ -> failwith "unknown builtin resolved"
       | Error error -> print_s [%sexp (error : B.Error.t)]);
      [%expect
        {|
        (chatml "ChatML starter")
        (Unknown_id not-installed)
        |}]
    ;;

    let%test_unit "canonical asset bytes and bounded catalog identity" =
      let canonical =
        [ ( "chatml"
          , "ChatML starter"
          , [%blob "../../lib/builtin_instructions/assets/chatml.md"] )
        ; ( "goals"
          , "Persistent goals"
          , [%blob "../../lib/builtin_instructions/assets/goals.md"] )
        ; ( "recurrence"
          , "Recurring work"
          , [%blob "../../lib/builtin_instructions/assets/recurrence.md"] )
        ; ( "skills"
          , "Admitted skills"
          , [%blob "../../lib/builtin_instructions/assets/skills.md"] )
        ; ( "project-instructions"
          , "Project guidance"
          , [%blob "../../lib/builtin_instructions/assets/project-instructions.md"] )
        ; ( "context-recovery"
          , "Context recovery policy"
          , [%blob "../../lib/builtin_instructions/assets/context-recovery.md"] )
        ; ( "packages"
          , "Reusable capability packages"
          , [%blob "../../lib/builtin_instructions/assets/packages.md"] )
        ; ( "lifecycle"
          , "Composed lifecycle decisions"
          , [%blob "../../lib/builtin_instructions/assets/lifecycle.md"] )
        ]
      in
      let ids =
        B.installed_ids ~max_items:8 ~checkpoint:ignore
        |> Result.map_error ~f:(fun _ -> "enumeration")
        |> Result.ok_or_failwith
      in
      [%test_eq: string list]
        (List.map ids ~f:B.Id.to_string)
        (List.map canonical ~f:(fun (id, _, _) -> id));
      List.iter canonical ~f:(fun (id, title, contents) ->
        let asset =
          B.find (B.Id.of_string id |> Or_error.ok_exn)
          |> Result.map_error ~f:(fun _ -> "missing asset")
          |> Result.ok_or_failwith
        in
        [%test_eq: string] (B.Asset.title asset) title;
        [%test_eq: string] (B.Asset.contents asset) contents;
        [%test_eq: int] (B.Asset.byte_length asset) (String.length contents);
        [%test_eq: string]
          (B.Asset.sha256 asset)
          Digestif.SHA256.(digest_string contents |> to_hex));
      assert (
        match B.installed_ids ~max_items:7 ~checkpoint:ignore with
        | Error Too_many -> true
        | Ok _ | Error Invalid_limit -> false);
      assert (
        match B.installed_ids ~max_items:0 ~checkpoint:ignore with
        | Error Invalid_limit -> true
        | Ok _ | Error Too_many -> false);
      let marker = Failure "catalog checkpoint" in
      match B.installed_ids ~max_items:8 ~checkpoint:(fun () -> raise marker) with
      | exception exn -> assert (phys_equal marker exn)
      | _ -> failwith "checkpoint exception lost"
    ;;

    let%test_unit "actual embedded examples compile with current one-off contract" =
      let lines = String.split_lines (B.Asset.contents (chatml ())) in
      let rec collect active body examples = function
        | [] ->
          require [%here] (not active);
          List.rev examples
        | line :: rest when String.equal line "```ocaml" ->
          require [%here] (not active);
          collect true [] examples rest
        | line :: rest when active && String.equal line "```" ->
          collect false [] (String.concat ~sep:"\n" (List.rev body) :: examples) rest
        | line :: rest ->
          collect active (if active then line :: body else body) examples rest
      in
      let examples = collect false [] [] lines in
      [%test_eq: int] (List.length examples) 2;
      List.iter examples ~f:(fun source ->
        match
          Chatml_host_runtime.compile_script
            ~surface:Chatml.Chatml_extension_surface.one_off_v1
            ~required_bindings:Chatml.Chatml_extension_surface.one_off_entrypoints
            ~source
            ()
        with
        | Ok _ -> ()
        | Error error -> failwith error);
      List.iter
        [ ""; "Chatml"; "../chatml"; "a:b"; "goals@2"; String.make 65 'a' ]
        ~f:(fun value -> require [%here] (Result.is_error (B.Id.of_string value)))
    ;;
  end)
;;
