open Core
open Authoring_context_tests
module Corpus = Authoring_corpus
module M = Chatmd_shell_spec.Authoring_metadata

let package name text =
  let id = "custom." ^ name ^ ".rules" in
  Corpus.
    { help =
        M.
          { version = 1
          ; package = name
          ; tasks = [ One_off_script ]
          ; topics = [ id ]
          ; required_helpers = []
          }
    ; topics =
        [ { id
          ; title = "Local conventions"
          ; prerequisites = [ "chatml.syntax.calls" ]
          ; surfaces = [ "one_off_v1" ]
          ; source_name = name ^ ".md"
          ; text
          }
        ]
    }
;;

let%expect_test
    "captured authored references are capability-scoped across prepare search paging and \
     revisions"
  =
  Mirage_crypto_rng_unix.use_default ();
  let reports =
    package "reports" "# Report conventions\nKeep source file names in every result.\n"
  in
  let private_ = package "private" "PRIVATE-AUTHORING-PROSE-SENTINEL" in
  let service reports =
    Q.create
      ~secret:"authored-query-test-secret"
      ~authored_packages:[ reports; private_ ]
      ()
    |> Result.ok_or_failwith
  in
  let context = service reports in
  let module Definition = struct
    type input = string

    let name = "report_author"
    let description = Some "Write a report workflow."
    let type_ = "function"
    let parameters = `Object []
    let input_of_string input = input
  end
  in
  let capabilities =
    C.create
      ~owner:"author"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "reference-resource")
      ~metadata:[ (Definition.name, M.{ authoring = Some reports.help; helper = None }) ]
      [ ( Chatmd_shell_spec.Source_ref.digest "author"
        , Ochat_function.create_function
            (module Definition)
            (fun _ -> failwith "reference lookup executed a tool") )
      ]
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith
  in
  let empty =
    C.select capabilities ~names:[]
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith
  in
  let query ?(context = context) ?(capabilities = capabilities) request =
    let result =
      Q.query context ~host:(host ()) ~capabilities ~scope:"custom-parent:1" request
    in
    assert (
      not
        (String.is_substring
           (Jsonaf.to_string result)
           ~substring:"PRIVATE-AUTHORING-PROSE-SENTINEL"));
    result
  in
  let topic =
    request
      ~task:"one_off_script"
      ~topic_id:"custom.reports.rules"
      ~max_tokens:32000
      "topic"
  in
  let response = query topic in
  require_json `True (field response "complete");
  let custom = List.last_exn (items response) in
  require_json (`String "authored_conventions") (field custom "source_kind");
  require_json (`String "reports") (field custom "author_package");
  require_json (`String "reports.md") (field custom "source");
  require_json (`String (List.hd_exn reports.topics).text) (field custom "text");
  assert (
    String.is_substring
      (Jsonaf.string_exn (field custom "authority"))
      ~substring:"not authoritative");
  require_json (`String "installed") (field (List.hd_exn (items response)) "source_kind");
  assert (has_error (query ~capabilities:empty topic));
  assert (
    has_error
      (query (request ~task:"one_off_script" ~topic_id:"custom.private.rules" "topic")));
  let search = request ~task:"one_off_script" ~query:"conventions" "search" in
  let found = query search |> items in
  assert (
    List.exists found ~f:(fun item ->
      Jsonaf.exactly_equal (field item "topic_id") (`String "custom.reports.rules")));
  assert (
    not
      (String.is_substring
         (Jsonaf.to_string (query ~capabilities:empty search))
         ~substring:"custom.reports.rules"));
  let prepared = query (request ~task:"one_off_script" ~max_tokens:32000 "prepare") in
  let rec collect remaining response =
    assert (remaining > 0);
    let current = items response in
    match field response "next_cursor" with
    | `Null -> current
    | `String cursor ->
      current
      @ collect (remaining - 1) (query (request ~cursor ~max_tokens:32000 "continue"))
    | _ -> failwith "invalid authored continuation"
  in
  let prepared = collect 50 prepared in
  assert (
    List.exists prepared ~f:(fun item ->
      match Jsonaf.member "topic_id" item with
      | Some (`String "custom.reports.rules") -> true
      | _ -> false));
  let first =
    query
      (request
         ~task:"one_off_script"
         ~topic_id:"custom.reports.rules"
         ~max_tokens:1000
         "topic")
  in
  require_json `False (field first "complete");
  let cursor = field first "next_cursor" |> Jsonaf.string_exn in
  let continue = request ~cursor ~max_tokens:32000 "continue" in
  assert (not (has_error (query continue)));
  assert (has_error (query ~capabilities:empty continue));
  let changed = package "reports" "Changed conventions." |> service in
  assert (has_error (query ~context:changed continue));
  print_endline
    "authored source labelled; selected roots prepared; private packages absent; \
     stale/narrowed cursors rejected";
  [%expect
    {| authored source labelled; selected roots prepared; private packages absent; stale/narrowed cursors rejected |}]
;;
