open Core

(* Maintainer aid only: emit candidate pins after reviewing the selected semantics
   and checking its reference examples. CI never regenerates maintained pins. *)
type selection =
  | Module of string
  | Globals
  | Alias of string

let () =
  let selection, topic, requested_surfaces =
    match Array.to_list (Sys.get_argv ()) with
    | _ :: "--globals" :: topic :: surfaces -> Globals, topic, surfaces
    | _ :: "--alias" :: name :: topic :: surfaces -> Alias name, topic, surfaces
    | _ :: name :: topic :: surfaces -> Module name, topic, surfaces
    | _ ->
      failwith
        "usage: review_coverage (MODULE | --globals | --alias NAME) TOPIC_ID [SURFACE \
         ...]"
  in
  let surfaces =
    match requested_surfaces with
    | [] -> [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
    | surfaces -> surfaces
  in
  (match List.find_a_dup surfaces ~compare:String.compare with
   | None -> ()
   | Some surface -> failwith ("duplicate surface: " ^ surface));
  let sources = Authoring_sources.installed () |> Result.ok_or_failwith in
  let corpus = Authoring_corpus.runtime_foundation ~sources |> Result.ok_or_failwith in
  let candidates =
    List.map surfaces ~f:(fun surface ->
      let targets =
        Authoring_corpus.Coverage.compiler_targets ~sources ~surface_ids:[ surface ]
        |> Result.ok_or_failwith
      in
      let selected =
        List.filter targets ~f:(fun target ->
          let id = target.Authoring_corpus.Coverage.id in
          match selection with
          | Globals -> String.is_prefix id ~prefix:(surface ^ "/global/")
          | Alias name -> String.equal id (surface ^ "/type_alias/" ^ name)
          | Module name ->
            String.equal id (surface ^ "/module/" ^ name)
            || String.is_prefix id ~prefix:(surface ^ "/module_export/" ^ name ^ "."))
      in
      if List.is_empty selected then failwith ("no selected targets on " ^ surface);
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
                     , `String (String.chop_prefix_exn target.id ~prefix:(surface ^ "/"))
                     )
                   ; "contract_sha256", `String target.contract_sha256
                   ])) )
        ])
  in
  print_endline
    (Jsonaf.to_string
       (`Object [ "review_required", `True; "candidates", `Array candidates ]))
;;
