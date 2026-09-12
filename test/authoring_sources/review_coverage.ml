open Core

(* Maintainer aid only: emit candidate pins after reviewing a module's semantics
   and checking its reference examples. CI never regenerates maintained pins. *)
let () =
  let name, topic =
    match Array.to_list (Sys.get_argv ()) with
    | [ _; name; topic ] -> name, topic
    | _ -> failwith "usage: review_coverage MODULE TOPIC_ID"
  in
  let sources = Authoring_sources.installed () |> Result.ok_or_failwith in
  let corpus = Authoring_corpus.runtime_foundation ~sources |> Result.ok_or_failwith in
  let candidates =
    List.map
      [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
      ~f:(fun surface ->
        let targets =
          Authoring_corpus.Coverage.compiler_targets ~sources ~surface_ids:[ surface ]
          |> Result.ok_or_failwith
        in
        let selected =
          List.filter targets ~f:(fun target ->
            String.equal target.Authoring_corpus.Coverage.id (surface ^ "/module/" ^ name)
            || String.is_prefix
                 target.id
                 ~prefix:(surface ^ "/module_export/" ^ name ^ "."))
        in
        if List.is_empty selected
        then failwith ("module unavailable: " ^ surface ^ "/" ^ name);
        let closure =
          Authoring_corpus.Coverage.topic_contract
            corpus
            ~surface_id:surface
            ~topic_id:topic
          |> Result.ok_or_failwith
        in
        `Object
          [ "surface", `String surface
          ; "topic_sha256", `String closure
          ; ( "bindings"
            , `Array
                (List.map selected ~f:(fun target ->
                   `Object
                     [ ( "target"
                       , `String
                           (String.chop_prefix_exn target.id ~prefix:(surface ^ "/")) )
                     ; "contract_sha256", `String target.contract_sha256
                     ])) )
          ])
  in
  print_endline
    (Jsonaf.to_string
       (`Object [ "review_required", `True; "candidates", `Array candidates ]))
;;
