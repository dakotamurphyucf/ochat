open! Core
module Sources = Authoring_sources

let require condition message =
  match condition with
  | true -> ()
  | false -> failwith message
;;

let () =
  (* Retrieve outside the checkout. Dune actions cannot chdir outside the build
     tree, so this isolated executable changes directory before loading sources. *)
  Core_unix.chdir "/";
  let sources = Sources.installed () |> Result.ok_or_failwith in
  let expected =
    [ "agent-server/extensibility-foundations.md"
    ; "guide/authoring-context-tool.md"
    ; "guide/chatmd-authoring-capabilities.md"
    ; "guide/chatmd-authoring-definitions.md"
    ; "guide/chatmd-shell-extensions.md"
    ; "guide/chatml-authoring-background.md"
    ; "guide/chatml-authoring-children.md"
    ; "guide/chatml-authoring-language.md"
    ; "guide/chatml-authoring-primer.md"
    ; "guide/chatml-authoring-runtime.md"
    ; "guide/chatml-background-values.md"
    ; "guide/chatml-collections.md"
    ; "guide/chatml-evaluation.md"
    ; "guide/chatml-global-helpers.md"
    ; "guide/chatml-host-effects.md"
    ; "guide/chatml-inference.md"
    ; "guide/chatml-invocation-context.md"
    ; "guide/chatml-json.md"
    ; "guide/chatml-language-spec.md"
    ; "guide/chatml-match-semantics.md"
    ; "guide/chatml-moderator-data.md"
    ; "guide/chatml-moderator-runtime.md"
    ; "guide/chatml-native-requests.md"
    ; "guide/chatml-ocaml-differences.md"
    ; "guide/chatml-runtime-control.md"
    ; "guide/chatml-strings.md"
    ; "guide/chatml-surface-inventory.md"
    ; "guide/chatml-tables.md"
    ; "guide/chatml-task-effects.md"
    ; "overview/chatmd-language.md"
    ]
  in
  require
    (List.equal
       String.equal
       (List.map (Sources.documents sources) ~f:(fun d -> d.path))
       expected)
    "installed reference source set changed; review the corpus coverage";
  List.iter expected ~f:(fun path ->
    let document = Sources.document sources ~path |> Result.ok_or_failwith in
    require
      (String.is_prefix document.text ~prefix:"#")
      ("missing reference body: " ^ path);
    require
      (String.equal document.sha256 (Chatmd_shell_spec.Source_ref.digest document.text))
      ("corrupt embedded source: " ^ path));
  let differences =
    Sources.document sources ~path:"guide/chatml-ocaml-differences.md"
    |> Result.ok_or_failwith
  in
  require
    (String.is_substring differences.text ~substring:"tasks.deferred-failure-catch")
    "checked authoring examples were not embedded";
  let signatures =
    Sources.signatures sources ~surface_id:"one_off_v1" |> Result.ok_or_failwith
  in
  require
    (List.exists signatures.items ~f:(fun item ->
       match item.kind, item.name, item.scheme with
       | Entrypoint, "main", TFun ([ _ ], _) -> true
       | _ -> false))
    "installed one-off entrypoint signature is missing";
  require
    (not (List.mem signatures.modules "Process" ~equal:String.equal))
    "offline lookup merged unrelated compiler surfaces";
  let grammar = Sources.grammar sources in
  require
    (List.exists grammar ~f:(fun production ->
       String.equal
         production.id
         "expr -> LETSTAR task_let_binder EQ expr_sequence IN expr_sequence"))
    "compiled grammar is unavailable outside the checkout";
  printf
    "Offline authoring sources: %d documents, %d separate compiler surfaces; no checkout \
     required PASS\n"
    (List.length expected)
    (List.length (Sources.surface_ids sources))
;;
