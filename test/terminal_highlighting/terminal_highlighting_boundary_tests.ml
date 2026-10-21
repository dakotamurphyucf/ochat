open Core
module Shared = Terminal_highlighting
module Legacy = Chat_tui

let%test_unit "legacy and shared public types are identical" =
  let theme : Legacy.Highlight_theme.t = Shared.Highlight_theme.github_dark in
  let (_ : Shared.Highlight_theme.t) = theme in
  let registry : Legacy.Highlight_tm_loader.registry =
    Shared.Highlight_tm_loader.create_registry ()
  in
  let (_ : Shared.Highlight_tm_loader.registry) = registry in
  let engine : Legacy.Highlight_tm_engine.t =
    Shared.Highlight_tm_engine.create ~theme
    |> Shared.Highlight_tm_engine.with_registry ~registry
  in
  let (_ : Shared.Highlight_tm_engine.t) = engine in
  let span : Legacy.Highlight_tm_engine.span = Notty.A.empty, "text" in
  let (_ : Shared.Highlight_tm_engine.span) = span in
  let scoped : Legacy.Highlight_tm_engine.scoped_span =
    { attr = Notty.A.empty; text = "text"; scopes = [ "source.ocaml" ] }
  in
  let (_ : Shared.Highlight_tm_engine.scoped_span) = scoped in
  let fallback : Legacy.Highlight_tm_engine.fallback_reason =
    Legacy.Highlight_tm_engine.Unknown_language "none"
  in
  let (_ : Shared.Highlight_tm_engine.fallback_reason) = fallback in
  let info : Legacy.Highlight_tm_engine.info = { fallback = Some fallback } in
  let (_ : Shared.Highlight_tm_engine.info) = info in
  ()
;;

let%test_unit "private registries and shared/legacy rendering agree" =
  let registered = Shared.Highlight_tm_loader.create_registry () in
  let empty = Shared.Highlight_tm_loader.create_registry () in
  Shared.Highlight_grammars.add_ocaml registered |> Or_error.ok_exn;
  let make registry =
    Shared.Highlight_tm_engine.create ~theme:Shared.Highlight_theme.github_dark
    |> Shared.Highlight_tm_engine.with_registry ~registry
  in
  let source = "let greeting = \"hello\"" in
  let shared, shared_info =
    Shared.Highlight_tm_engine.highlight_text_with_info
      (make registered)
      ~lang:(Some "ocaml")
      ~text:source
  in
  let legacy, legacy_info =
    Legacy.Highlight_tm_engine.highlight_text_with_info
      (make registered)
      ~lang:(Some "ocaml")
      ~text:source
  in
  let same_span (left_attr, left_text) (right_attr, right_text) =
    Notty.A.equal left_attr right_attr && String.equal left_text right_text
  in
  assert (List.equal (List.equal same_span) shared legacy);
  assert (Option.is_none shared_info.fallback);
  assert (Option.is_none legacy_info.fallback);
  let _, empty_info =
    Shared.Highlight_tm_engine.highlight_text_with_info
      (make empty)
      ~lang:(Some "ocaml")
      ~text:source
  in
  match empty_info.fallback with
  | Some (Shared.Highlight_tm_engine.Unknown_language "ocaml") -> ()
  | None
  | Some Shared.Highlight_tm_engine.No_registry
  | Some (Shared.Highlight_tm_engine.Unknown_language _)
  | Some Shared.Highlight_tm_engine.Tokenize_error ->
    failwith "private registry unexpectedly resolved OCaml"
;;

let%expect_test "hex alpha forms preserve existing RGB behavior" =
  [ "#abc"; "#abcd"; "aabbccdd"; "#12xz" ]
  |> List.iter ~f:(fun value ->
    let rgb = Shared.Highlight_styles.hex_to_rgb value in
    print_s [%sexp (value : string), (rgb : (int * int * int) option)]);
  [%expect
    {|
    (#abc ((170 187 204)))
    (#abcd ((170 187 204)))
    (aabbccdd ((170 187 204)))
    (#12xz ())
    |}]
;;
