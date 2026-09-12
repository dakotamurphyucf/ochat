open Core

(* Maintainer aid only: emit candidate pins after reviewing the selected semantics
   and checking its reference examples. CI never regenerates maintained pins. *)
type selection =
  | Module of string
  | Globals
  | Alias of string

type request =
  | Missing
  | Grammar
  | Semantics
  | Declarations
  | Native
  | Runtime
  | Changed_docs
  | Candidates of selection * string

let () =
  let request, requested_surfaces =
    match Array.to_list (Sys.get_argv ()) with
    | _ :: "--missing" :: surfaces -> Missing, surfaces
    | _ :: "--grammar" :: surfaces -> Grammar, surfaces
    | _ :: "--semantics" :: surfaces -> Semantics, surfaces
    | _ :: "--declarations" :: surfaces -> Declarations, surfaces
    | _ :: "--native" :: surfaces -> Native, surfaces
    | _ :: "--runtime" :: surfaces -> Runtime, surfaces
    | _ :: "--changed-docs" :: surfaces -> Changed_docs, surfaces
    | _ :: "--globals" :: topic :: surfaces -> Candidates (Globals, topic), surfaces
    | _ :: "--alias" :: name :: topic :: surfaces ->
      Candidates (Alias name, topic), surfaces
    | _ :: name :: topic :: surfaces -> Candidates (Module name, topic), surfaces
    | _ ->
      failwith
        "usage: review_coverage (MODULE | --globals | --alias NAME) TOPIC_ID [SURFACE \
         ...] | (--missing | --grammar | --semantics | --declarations | --native | \
         --runtime | --changed-docs) [SURFACE ...]"
  in
  let surfaces =
    match requested_surfaces with
    | [] ->
      (match request with
       | Declarations -> [ "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
       | _ -> [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ])
    | surfaces -> surfaces
  in
  (match List.find_a_dup surfaces ~compare:String.compare with
   | None -> ()
   | Some surface -> failwith ("duplicate surface: " ^ surface));
  let sources = Authoring_sources.installed () |> Result.ok_or_failwith in
  let corpus = Authoring_corpus.runtime_foundation ~sources |> Result.ok_or_failwith in
  let output =
    match request with
    | Missing ->
      let module C = Authoring_corpus.Coverage in
      let targets =
        C.compiler_targets ~sources ~surface_ids:surfaces |> Result.ok_or_failwith
      in
      let mappings =
        List.filter C.reviewed_mappings ~f:(fun mapping ->
          List.exists surfaces ~f:(fun surface ->
            String.is_prefix mapping.target_id ~prefix:(surface ^ "/")))
      in
      let report = C.audit corpus ~targets ~mappings |> Result.ok_or_failwith in
      `Object
        [ ( "scope"
          , `String "compiler bindings only; semantic and ChatMD coverage is separate" )
        ; "targets", `Number (Int.to_string (List.length targets))
        ; "mapped", `Number (Int.to_string (List.length report.mapped))
        ; "missing", `Array (List.map report.missing ~f:(fun target -> `String target.id))
        ]
    | Grammar ->
      let module C = Authoring_corpus.Coverage in
      let targets =
        C.grammar_targets ~sources ~surface_ids:surfaces |> Result.ok_or_failwith
      in
      `Object
        [ "review_required", `True
        ; ( "productions"
          , `Array
              (List.map (Authoring_sources.grammar sources) ~f:(fun production ->
                 `Object
                   [ "id", `String production.id
                   ; "contract_sha256", `String production.contract_sha256
                   ])) )
        ; ( "topic_contracts"
          , `Array
              (List.concat_map surfaces ~f:(fun surface ->
                 List.map [ "chatml.programs"; "chatml.task-effects" ] ~f:(fun topic ->
                   let sha256 =
                     C.topic_contract corpus ~surface_id:surface ~topic_id:topic
                     |> Result.ok_or_failwith
                   in
                   `Object
                     [ "surface", `String surface
                     ; "topic", `String topic
                     ; "sha256", `String sha256
                     ]))) )
        ; ( "scope"
          , `String "grammar productions only; no automatic semantic coverage claim" )
        ; ( "targets"
          , `Array
              (List.map targets ~f:(fun target ->
                 `Object
                   [ "id", `String target.id
                   ; "contract_sha256", `String target.contract_sha256
                   ])) )
        ]
    | Semantics | Declarations | Native | Runtime ->
      let module C = Authoring_corpus.Coverage in
      let features, inventory =
        match request with
        | Declarations -> C.declaration_features, C.declaration_targets
        | Native -> C.native_features, C.native_targets
        | Runtime -> C.runtime_features, C.runtime_targets
        | _ -> C.semantic_features, C.semantic_targets
      in
      let targets = inventory ~sources ~surface_ids:surfaces |> Result.ok_or_failwith in
      `Object
        [ "review_required", `True
        ; "scope", `String "maintained feature taxonomy; not automatic feature discovery"
        ; ( "implementation_sources"
          , `Array
              (List.map
                 (Authoring_sources.implementation_sources sources)
                 ~f:(fun source ->
                   `Object
                     [ "path", `String source.path; "sha256", `String source.sha256 ])) )
        ; ( "features"
          , `Array
              (List.map features ~f:(fun feature ->
                 `Object
                   [ "id", `String feature.id
                   ; "description", `String feature.description
                   ; "topic", `String feature.topic_id
                   ; ( "implementation_paths"
                     , `Array
                         (List.map feature.implementation_paths ~f:(fun path ->
                            `String path)) )
                   ; ( "evidence"
                     , `Array
                         (List.map feature.evidence ~f:(fun reference ->
                            `String reference)) )
                   ])) )
        ; ( "targets"
          , `Array
              (List.map targets ~f:(fun target ->
                 `Object
                   [ "id", `String target.id
                   ; "contract_sha256", `String target.contract_sha256
                   ])) )
        ; ( "topic_contracts"
          , `Array
              (List.concat_map surfaces ~f:(fun surface ->
                 features
                 |> List.filter ~f:(fun feature ->
                   match request with
                   | Runtime ->
                     List.exists targets ~f:(fun target ->
                       String.equal target.C.id (surface ^ "/runtime/" ^ feature.C.id))
                   | _ -> true)
                 |> List.map ~f:(fun feature -> feature.topic_id)
                 |> List.dedup_and_sort ~compare:String.compare
                 |> List.map ~f:(fun topic ->
                   let sha256 =
                     C.topic_contract corpus ~surface_id:surface ~topic_id:topic
                     |> Result.ok_or_failwith
                   in
                   `Object
                     [ "surface", `String surface
                     ; "topic", `String topic
                     ; "sha256", `String sha256
                     ]))) )
        ]
    | Changed_docs ->
      let module C = Authoring_corpus.Coverage in
      let declaration_targets =
        match
          List.filter surfaces ~f:(function
            | "tool_v1" | "moderator_v1" | "delegated_moderator_v1" -> true
            | _ -> false)
        with
        | [] -> []
        | surface_ids ->
          C.declaration_targets ~sources ~surface_ids |> Result.ok_or_failwith
      in
      let targets =
        (C.compiler_targets ~sources ~surface_ids:surfaces |> Result.ok_or_failwith)
        @ (C.grammar_targets ~sources ~surface_ids:surfaces |> Result.ok_or_failwith)
        @ (C.semantic_targets ~sources ~surface_ids:surfaces |> Result.ok_or_failwith)
        @ (C.native_targets ~sources ~surface_ids:surfaces |> Result.ok_or_failwith)
        @ (C.runtime_targets ~sources ~surface_ids:surfaces |> Result.ok_or_failwith)
        @ declaration_targets
        |> List.map ~f:(fun target -> target.C.id, target)
        |> String.Map.of_alist_exn
      in
      let changes =
        C.reviewed_mappings
        @ C.grammar_mappings
        @ C.semantic_mappings
        @ C.declaration_mappings
        @ C.native_mappings
        @ C.runtime_mappings
        |> List.filter_map ~f:(fun mapping ->
          match Map.find targets mapping.C.target_id with
          | None ->
            (match
               List.exists surfaces ~f:(fun surface ->
                 String.is_prefix mapping.target_id ~prefix:(surface ^ "/"))
             with
             | false -> None
             | true -> failwith ("compiler target removed: " ^ mapping.target_id))
          | Some target ->
            if not (String.equal target.contract_sha256 mapping.contract_sha256)
            then failwith ("compiler contract changed: " ^ target.id);
            let current =
              C.topic_contract
                corpus
                ~surface_id:target.surface_id
                ~topic_id:mapping.topic_id
              |> Result.ok_or_failwith
            in
            (match String.equal current mapping.topic_closure_sha256 with
             | true -> None
             | false ->
               Some
                 ( target.surface_id
                 , mapping.topic_id
                 , mapping.topic_closure_sha256
                 , current )))
        |> List.dedup_and_sort ~compare:(fun a b ->
          [%compare: string * string * string * string] a b)
        |> List.map ~f:(fun (surface, topic, previous, current) ->
          `Object
            [ "surface", `String surface
            ; "topic", `String topic
            ; "previous", `String previous
            ; "current", `String current
            ])
      in
      `Object
        [ "review_required", `True
        ; "unchanged_reviewed_compiler_contracts", `True
        ; "changed_topic_closures", `Array changes
        ]
    | Candidates (selection, topic) ->
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
                         , `String
                             (String.chop_prefix_exn target.id ~prefix:(surface ^ "/")) )
                       ; "contract_sha256", `String target.contract_sha256
                       ])) )
            ])
      in
      `Object [ "review_required", `True; "candidates", `Array candidates ]
  in
  print_endline (Jsonaf.to_string output)
;;
