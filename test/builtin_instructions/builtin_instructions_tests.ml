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
      (match B.find (B.Id.of_string "skills" |> Or_error.ok_exn) with
       | Ok _ -> failwith "unsupported skill instructions"
       | Error error -> print_s [%sexp (error : B.Error.t)]);
      [%expect
        {|
        (chatml "ChatML starter")
        (Unknown_id skills)
        |}]
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
        [ ""; "Chatml"; "../chatml"; "a:b"; String.make 65 'a' ]
        ~f:(fun value -> require [%here] (Result.is_error (B.Id.of_string value)))
    ;;
  end)
;;
