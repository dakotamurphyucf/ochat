open! Core
module Metadata = Chatmd_shell_spec.Authoring_metadata

type excerpt =
  { path : string
  ; heading : string
  ; include_children : bool
  }
[@@deriving sexp]

type review =
  | Pending
  | Audited of
      { excerpt_sha256 : string list
      ; evidence : string list
      }
[@@deriving sexp]

type specification =
  { id : string
  ; title : string
  ; prerequisites : string list
  ; surfaces : string list
  ; excerpts : excerpt list
  ; review : review
  }
[@@deriving sexp]

type fragment =
  { source : excerpt
  ; document_sha256 : string
  ; sha256 : string
  ; text : string
  }

type origin =
  | Installed
  | Authored of
      { package : string
      ; package_sha256 : string
      }
[@@deriving sexp, equal]

type topic =
  { specification : specification
  ; fragments : fragment list
  ; sha256 : string
  ; origin : origin
  }

type t =
  { identity : string
  ; installed_identity : string
  ; topics : topic String.Map.t
  ; surface_ids : string list
  ; packages : Metadata.help String.Map.t
  }

type authored_topic =
  { id : string
  ; title : string
  ; prerequisites : string list
  ; surfaces : string list
  ; source_name : string
  ; text : string
  }
[@@deriving sexp]

type authored_package =
  { help : Metadata.help
  ; topics : authored_topic list
  }
[@@deriving sexp]

let digest = Chatmd_shell_spec.Source_ref.digest
let identity t = t.identity
let topics (t : t) = Map.data t.topics
let authored_packages t = Map.data t.packages
let unique strings = Option.is_none (List.find_a_dup strings ~compare:String.compare)

let heading_depth line =
  let hashes = String.take_while line ~f:(Char.equal '#') |> String.length in
  match hashes > 0 && hashes <= 6 && String.length line > hashes with
  | true when Char.equal line.[hashes] ' ' -> Some hashes
  | _ -> None
;;

let fence_start line =
  let line = String.strip line in
  match String.is_empty line with
  | true -> None
  | false ->
    let character = line.[0] in
    (match character with
     | '`' | '~' ->
       let count = String.take_while line ~f:(Char.equal character) |> String.length in
       if count >= 3 then Some (character, count) else None
     | _ -> None)
;;

let fence_end (character, minimum) line =
  let line = String.strip line in
  let count = String.take_while line ~f:(Char.equal character) |> String.length in
  count >= minimum && String.is_empty (String.strip (String.drop_prefix line count))
;;

let section ~text ~heading ~include_children =
  let open Result.Let_syntax in
  let%bind depth =
    match heading_depth heading with
    | Some depth -> Ok depth
    | None -> Error ("invalid exact topic heading: " ^ heading)
  in
  let rec scan lines offset fence headings =
    match lines with
    | [] ->
      (match fence with
       | Some _ -> Error "unclosed Markdown code fence in topic source"
       | None -> Ok (List.rev headings))
    | raw :: rest ->
      let line = String.rstrip raw in
      let next = offset + String.length raw + 1 in
      (match fence with
       | Some current ->
         let fence = if fence_end current line then None else fence in
         scan rest next fence headings
       | None ->
         (match fence_start line with
          | Some _ as fence -> scan rest next fence headings
          | None ->
            let headings =
              match heading_depth line with
              | Some level -> (offset, level, line) :: headings
              | None -> headings
            in
            scan rest next None headings))
  in
  let%bind headings = scan (String.split text ~on:'\n') 0 None [] in
  match List.filter headings ~f:(fun (_, _, title) -> String.equal title heading) with
  | [] -> Error ("topic heading not found: " ^ heading)
  | _ :: _ :: _ -> Error ("ambiguous topic heading: " ^ heading)
  | [ (start, _, _) ] ->
    let finish =
      List.find_map headings ~f:(fun (offset, level, _) ->
        match offset > start && ((not include_children) || level <= depth) with
        | true -> Some offset
        | false -> None)
      |> Option.value ~default:(String.length text)
    in
    Ok (String.sub text ~pos:start ~len:(finish - start))
;;

let topic (t : t) ~id =
  match Map.find t.topics id with
  | Some topic -> Ok topic
  | None -> Error ("authoring topic is not installed: " ^ id)
;;

let closure topics roots =
  let open Result.Let_syntax in
  let completed = Hash_set.create (module String) in
  let rec visit trail reversed id =
    match Hash_set.mem completed id with
    | true -> Ok reversed
    | false ->
      (match List.mem trail id ~equal:String.equal, Map.find topics id with
       | true, _ ->
         Error
           ("authoring topic dependency cycle: "
            ^ String.concat ~sep:" -> " (List.rev (id :: trail)))
       | _, None -> Error ("authoring topic is not installed: " ^ id)
       | false, Some topic ->
         let%map reversed =
           List.fold_result
             topic.specification.prerequisites
             ~init:reversed
             ~f:(visit (id :: trail))
         in
         Hash_set.add completed id;
         topic :: reversed)
  in
  List.fold_result roots ~init:[] ~f:(visit []) |> Result.map ~f:List.rev
;;

let validate_graph topics =
  let open Result.Let_syntax in
  let%bind _ = closure topics (Map.keys topics) in
  Map.data topics
  |> List.map ~f:(fun topic ->
    List.map topic.specification.prerequisites ~f:(fun id ->
      let dependency = Map.find_exn topics id in
      match
        List.for_all
          topic.specification.surfaces
          ~f:(List.mem dependency.specification.surfaces ~equal:String.equal)
      with
      | true -> Ok ()
      | false ->
        Error
          (topic.specification.id
           ^ ": prerequisite unavailable on a declared surface: "
           ^ id))
    |> Result.all_unit)
  |> Result.all_unit
;;

let create ~sources specifications =
  let open Result.Let_syntax in
  let surface_ids = Authoring_sources.surface_ids sources in
  let%bind resolved =
    List.map specifications ~f:(fun (specification : specification) ->
      let fail message = Error (specification.id ^ ": " ^ message) in
      let%bind () =
        match
          Metadata.valid_topic specification.id
          && (not (String.is_empty (String.strip specification.title)))
          && unique specification.prerequisites
          && List.for_all specification.prerequisites ~f:Metadata.valid_topic
          && (not (List.is_empty specification.surfaces))
          && unique specification.surfaces
          && List.for_all
               specification.surfaces
               ~f:(List.mem surface_ids ~equal:String.equal)
          && not (List.is_empty specification.excerpts)
        with
        | false -> fail "invalid topic specification"
        | true ->
          (match specification.review with
           | Pending -> Ok ()
           | Audited { evidence; _ }
             when (not (List.is_empty evidence))
                  && unique evidence
                  && List.for_all evidence ~f:(fun s ->
                    not (String.is_empty (String.strip s))) -> Ok ()
           | Audited _ -> fail "audited topic requires evidence references")
      in
      let%bind fragments =
        List.map specification.excerpts ~f:(fun source ->
          let%bind document = Authoring_sources.document sources ~path:source.path in
          let%map text =
            section
              ~text:document.text
              ~heading:source.heading
              ~include_children:source.include_children
          in
          { source; document_sha256 = document.sha256; sha256 = digest text; text })
        |> Result.all
        |> Result.map_error ~f:(fun message -> specification.id ^ ": " ^ message)
      in
      let%bind () =
        match specification.review with
        | Pending -> Ok ()
        | Audited { excerpt_sha256; _ } ->
          (match
             List.equal
               String.equal
               excerpt_sha256
               (List.map fragments ~f:(fun f -> f.sha256))
           with
           | true -> Ok ()
           | false -> fail "audited excerpt hashes changed; review the topic again")
      in
      let sha256 =
        [%sexp
          (specification : specification)
        , (List.map fragments ~f:(fun f -> f.document_sha256, f.sha256)
           : (string * string) list)]
        |> Sexp.to_string_mach
        |> digest
      in
      Ok (specification.id, { specification; fragments; sha256; origin = Installed }))
    |> Result.all
  in
  let%bind topics =
    match String.Map.of_alist resolved with
    | `Duplicate_key id -> Error ("duplicate authoring topic: " ^ id)
    | `Ok topics when Map.is_empty topics -> Error "empty authoring corpus"
    | `Ok topics -> Ok topics
  in
  let%bind () = validate_graph topics in
  let identity =
    [%sexp
      (Authoring_sources.identity sources : string)
    , (Map.to_alist (Map.map topics ~f:(fun topic -> topic.sha256))
       : (string * string) list)]
    |> Sexp.to_string_mach
    |> digest
  in
  Ok
    { identity
    ; installed_identity = identity
    ; topics
    ; surface_ids
    ; packages = String.Map.empty
    }
;;

let with_authored_topics t ~topics ~packages =
  let identity =
    match Map.is_empty packages with
    | true -> t.installed_identity
    | false ->
      [%sexp
        ("ochat.authored-reference.v1" : string)
      , (t.installed_identity : string)
      , (Map.to_alist (Map.map topics ~f:(fun topic -> topic.sha256))
         : (string * string) list)
      , (Map.data packages : Metadata.help list)]
      |> Sexp.to_string_mach
      |> digest
  in
  { t with identity; topics; packages }
;;

let extend_authored ?(max_bytes = 4_000_000) (t : t) additions =
  let open Result.Let_syntax in
  let%bind () =
    let existing =
      Map.data t.topics
      |> List.sum
           (module Int)
           ~f:(fun topic ->
             match topic.origin with
             | Installed -> 0
             | Authored _ ->
               List.sum
                 (module Int)
                 topic.fragments
                 ~f:(fun fragment -> String.length fragment.text))
    in
    let bytes =
      List.sum
        (module Int)
        additions
        ~f:(fun (package : authored_package) ->
          List.sum (module Int) package.topics ~f:(fun topic -> String.length topic.text))
    in
    let count =
      List.sum (module Int) additions ~f:(fun package -> List.length package.topics)
    in
    match
      max_bytes > 0
      && existing <= max_bytes
      && bytes <= max_bytes - existing
      && Map.length t.packages + List.length additions <= 128
      && Map.length t.topics + count <= 512
    with
    | true -> Ok ()
    | false -> Error "authored reference package budget exceeded"
  in
  let%bind packages, topics =
    List.fold_result
      additions
      ~init:(t.packages, t.topics)
      ~f:(fun (packages, topics) (addition : authored_package) ->
        let%bind () = Metadata.validate_help addition.help in
        let package = addition.help.package in
        let%bind () =
          match Map.mem packages package, addition.topics with
          | true, _ -> Error ("duplicate authored package: " ^ package)
          | _, [] -> Error ("empty authored package: " ^ package)
          | false, _ -> Ok ()
        in
        let package_sha256 =
          let sorted =
            { addition with
              topics =
                List.sort addition.topics ~compare:(fun a b -> String.compare a.id b.id)
            }
          in
          [%sexp (sorted : authored_package)] |> Sexp.to_string_mach |> digest
        in
        let%bind topics =
          List.fold_result addition.topics ~init:topics ~f:(fun topics authored ->
            let%bind () =
              match
                Metadata.valid_topic authored.id
                && String.is_prefix authored.id ~prefix:("custom." ^ package ^ ".")
                && (not (String.is_empty (String.strip authored.title)))
                && Stdlib.String.is_valid_utf_8 authored.title
                && (not (String.is_empty (String.strip authored.text)))
                && Stdlib.String.is_valid_utf_8 authored.text
                && (not (String.is_empty authored.source_name))
                && Stdlib.String.is_valid_utf_8 authored.source_name
                && (not
                      (String.exists authored.source_name ~f:(function
                         | '\000' .. '\031' | '\127' -> true
                         | _ -> false)))
                && unique authored.prerequisites
                && List.for_all authored.prerequisites ~f:Metadata.valid_topic
                && (not (List.is_empty authored.surfaces))
                && unique authored.surfaces
                && List.for_all
                     authored.surfaces
                     ~f:(List.mem t.surface_ids ~equal:String.equal)
              with
              | true -> Ok ()
              | false -> Error ("invalid authored topic: " ^ authored.id)
            in
            let source =
              { path = authored.source_name
              ; heading = authored.title
              ; include_children = true
              }
            in
            let text_sha256 = digest authored.text in
            let specification =
              { id = authored.id
              ; title = authored.title
              ; prerequisites = authored.prerequisites
              ; surfaces = authored.surfaces
              ; excerpts = [ source ]
              ; review = Pending
              }
            in
            let topic =
              { specification
              ; fragments =
                  [ { source
                    ; document_sha256 = text_sha256
                    ; sha256 = text_sha256
                    ; text = authored.text
                    }
                  ]
              ; sha256 = digest (package_sha256 ^ ":" ^ authored.id)
              ; origin = Authored { package; package_sha256 }
              }
            in
            match Map.add topics ~key:authored.id ~data:topic with
            | `Duplicate -> Error ("duplicate authoring topic: " ^ authored.id)
            | `Ok topics -> Ok topics)
        in
        let%map () =
          List.map addition.help.topics ~f:(fun id ->
            match Map.find topics id with
            | Some { origin = Authored owner; _ } when String.equal owner.package package
              -> Ok ()
            | _ -> Error ("authored package root is not owned by its package: " ^ id))
          |> Result.all_unit
        in
        Map.set packages ~key:package ~data:addition.help, topics)
  in
  let%map () = validate_graph topics in
  with_authored_topics t ~topics ~packages
;;

let scope_authored t ~packages:selected =
  let open Result.Let_syntax in
  let%bind () =
    match unique selected && List.for_all selected ~f:(Map.mem t.packages) with
    | true -> Ok ()
    | false -> Error "invalid authored package selection"
  in
  let packages = Map.filter_keys t.packages ~f:(List.mem selected ~equal:String.equal) in
  let topics =
    Map.filter t.topics ~f:(fun topic ->
      match topic.origin with
      | Installed -> true
      | Authored owner -> Map.mem packages owner.package)
  in
  let%map () = validate_graph topics in
  with_authored_topics t ~topics ~packages
;;

let assemble t ~surface_id ~roots =
  let open Result.Let_syntax in
  let%bind () =
    match List.mem t.surface_ids surface_id ~equal:String.equal, roots with
    | false, _ -> Error ("unknown authoring compiler surface: " ^ surface_id)
    | _, [] -> Error "authoring topic roots must not be empty"
    | true, _ when unique roots -> Ok ()
    | _ -> Error "duplicate authoring topic root"
  in
  let%bind () =
    List.map roots ~f:(fun id ->
      let%bind requested = topic t ~id in
      match List.mem requested.specification.surfaces surface_id ~equal:String.equal with
      | true -> Ok ()
      | false -> Error ("authoring topic unavailable on " ^ surface_id ^ ": " ^ id))
    |> Result.all_unit
  in
  (* Construction already proved every dependency supports its dependent's
     surfaces. Report an incompatible requested root before traversing its graph. *)
  closure t.topics roots
;;

let pending t =
  topics t
  |> List.filter_map ~f:(fun topic ->
    match topic.specification.review with
    | Pending -> Some topic.specification.id
    | Audited _ -> None)
;;

module Coverage = struct
  type target =
    { id : string
    ; surface_id : string
    ; contract_sha256 : string
    }
  [@@deriving sexp]

  type mapping =
    { target_id : string
    ; contract_sha256 : string
    ; topic_id : string
    ; topic_closure_sha256 : string
    ; evidence : string list
    }
  [@@deriving sexp]

  type report =
    { mapped : string list
    ; missing : target list
    }
  [@@deriving sexp]

  type semantic_feature =
    { id : string
    ; description : string
    ; implementation_paths : string list
    ; topic_id : string
    ; evidence : string list
    }
  [@@deriving sexp]

  let features entries =
    List.map
      entries
      ~f:(fun (id, description, implementation_paths, topic_id, evidence) ->
        { id
        ; description
        ; implementation_paths
        ; topic_id
        ; evidence = List.dedup_and_sort evidence ~compare:String.compare
        })
  ;;

  let semantic_features = features Semantic_coverage_data.features
  let declaration_features = features Declaration_coverage_data.features
  let native_features = features Native_coverage_data.features
  let runtime_shared_features = features Runtime_coverage_data.shared_features
  let runtime_moderator_features = features Runtime_coverage_data.moderator_features
  let runtime_features = runtime_shared_features @ runtime_moderator_features

  let semantic_contract ~surface_id ~implementation_sources feature =
    let open Result.Let_syntax in
    let%map implementations =
      List.map feature.implementation_paths ~f:(fun path ->
        List.Assoc.find implementation_sources path ~equal:String.equal
        |> Result.of_option ~error:("missing semantic implementation source: " ^ path)
        |> Result.map ~f:(fun sha256 -> path, sha256))
      |> Result.all
    in
    [%sexp
      ("ochat.language-semantics.v1" : string)
    , (surface_id : string)
    , (feature : semantic_feature)
    , (implementations : (string * string) list)]
    |> Sexp.to_string_mach
    |> digest
  ;;

  let compiler_targets ~sources ~surface_ids =
    let open Result.Let_syntax in
    let%bind () =
      match surface_ids with
      | [] -> Error "coverage requires explicit compiler surfaces"
      | _ when not (unique surface_ids) -> Error "duplicate coverage surface"
      | _ -> Ok ()
    in
    let%map inventories =
      List.map surface_ids ~f:(fun surface_id ->
        Authoring_sources.signatures sources ~surface_id)
      |> Result.all
    in
    List.concat_map inventories ~f:(fun inventory ->
      let module I = Chatml.Chatml_surface_inventory in
      let surface_id = inventory.I.surface_id in
      let make kind name contract =
        let id = surface_id ^ "/" ^ kind ^ "/" ^ name in
        { id; surface_id; contract_sha256 = digest (id ^ "\n" ^ contract) }
      in
      List.map inventory.modules ~f:(fun name -> make "module" name name)
      @ List.map inventory.items ~f:(fun item ->
        let kind =
          match item.I.kind with
          | Global -> "global"
          | Module_export -> "module_export"
          | Type_alias -> "type_alias"
          | Entrypoint -> "entrypoint"
        in
        make
          kind
          item.name
          (Chatml.Chatml_builtin_spec.sexp_of_ty item.scheme |> Sexp.to_string_mach)))
    |> List.sort ~compare:(fun a b -> String.compare a.id b.id)
  ;;

  let feature_targets ~namespace ~features ~sources ~surface_ids =
    let open Result.Let_syntax in
    let%bind _ = compiler_targets ~sources ~surface_ids in
    let implementation_sources =
      Authoring_sources.implementation_sources sources
      |> List.map ~f:(fun source -> source.Authoring_sources.path, source.sha256)
    in
    List.concat_map surface_ids ~f:(fun surface_id ->
      List.map features ~f:(fun feature ->
        let%map contract_sha256 =
          semantic_contract ~surface_id ~implementation_sources feature
        in
        { id = surface_id ^ "/" ^ namespace ^ "/" ^ feature.id
        ; surface_id
        ; contract_sha256
        }))
    |> Result.all
  ;;

  let semantic_targets =
    feature_targets ~namespace:"semantics" ~features:semantic_features
  ;;

  let declaration_targets ~sources ~surface_ids =
    match
      List.for_all surface_ids ~f:(function
        | "tool_v1" | "moderator_v1" | "delegated_moderator_v1" -> true
        | _ -> false)
    with
    | false -> Error "ChatMD coverage requires a declaration-authoring surface"
    | true ->
      feature_targets
        ~namespace:"declarations"
        ~features:declaration_features
        ~sources
        ~surface_ids
  ;;

  let native_targets = feature_targets ~namespace:"native" ~features:native_features

  let runtime_targets ~sources ~surface_ids =
    let open Result.Let_syntax in
    let%bind _ = compiler_targets ~sources ~surface_ids in
    List.map surface_ids ~f:(fun surface_id ->
      let%bind features =
        match surface_id with
        | "one_off_v1" | "tool_v1" -> Ok runtime_shared_features
        | "moderator_v1" | "delegated_moderator_v1" -> Ok runtime_features
        | _ -> Error "runtime coverage requires an extensibility surface"
      in
      feature_targets ~namespace:"runtime" ~features ~sources ~surface_ids:[ surface_id ])
    |> Result.all
    |> Result.map ~f:List.concat
  ;;

  let feature_mappings
        ~namespace
        ~features
        ~implementation_sources
        ~topic_contracts
        ~surface_ids
    =
    List.concat_map surface_ids ~f:(fun surface_id ->
      List.map features ~f:(fun feature ->
        let contract_sha256 =
          semantic_contract ~surface_id ~implementation_sources feature
          |> Result.ok_or_failwith
        in
        let _, _, topic_closure_sha256 =
          List.find_exn topic_contracts ~f:(fun (surface, topic, _) ->
            String.equal surface surface_id && String.equal topic feature.topic_id)
        in
        { target_id = surface_id ^ "/" ^ namespace ^ "/" ^ feature.id
        ; contract_sha256
        ; topic_id = feature.topic_id
        ; topic_closure_sha256
        ; evidence = feature.evidence
        }))
  ;;

  let semantic_mappings =
    feature_mappings
      ~namespace:"semantics"
      ~features:semantic_features
      ~implementation_sources:Semantic_coverage_data.implementation_sources
      ~topic_contracts:Semantic_coverage_data.topic_contracts
      ~surface_ids:[ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
  ;;

  let declaration_mappings =
    feature_mappings
      ~namespace:"declarations"
      ~features:declaration_features
      ~implementation_sources:Declaration_coverage_data.implementation_sources
      ~topic_contracts:Declaration_coverage_data.topic_contracts
      ~surface_ids:[ "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
  ;;

  let native_mappings =
    feature_mappings
      ~namespace:"native"
      ~features:native_features
      ~implementation_sources:Native_coverage_data.implementation_sources
      ~topic_contracts:Native_coverage_data.topic_contracts
      ~surface_ids:[ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
  ;;

  let runtime_mappings =
    let map features surface_ids =
      feature_mappings
        ~namespace:"runtime"
        ~features
        ~implementation_sources:Runtime_coverage_data.implementation_sources
        ~topic_contracts:Runtime_coverage_data.topic_contracts
        ~surface_ids
    in
    map
      runtime_shared_features
      [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
    @ map runtime_moderator_features [ "moderator_v1"; "delegated_moderator_v1" ]
  ;;

  let grammar_targets ~sources ~surface_ids =
    let open Result.Let_syntax in
    (* Reuse exact surface validation; grammar is shared, target grants are not. *)
    let%map _ = compiler_targets ~sources ~surface_ids in
    List.concat_map surface_ids ~f:(fun surface_id ->
      List.map (Authoring_sources.grammar sources) ~f:(fun production ->
        let id = surface_id ^ "/grammar/" ^ production.Authoring_sources.id in
        { id
        ; surface_id
        ; contract_sha256 = digest (id ^ "\n" ^ production.contract_sha256)
        }))
    |> List.sort ~compare:(fun a b -> String.compare a.id b.id)
  ;;

  let grammar_mappings =
    List.concat_map
      [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
      ~f:(fun surface_id ->
        List.map
          Grammar_coverage_data.productions
          ~f:(fun (production, contract, topic_id) ->
            let target_id = surface_id ^ "/grammar/" ^ production in
            let _, _, topic_closure_sha256 =
              List.find_exn
                Grammar_coverage_data.topic_contracts
                ~f:(fun (surface, topic, _) ->
                  String.equal surface surface_id && String.equal topic topic_id)
            in
            { target_id
            ; contract_sha256 = digest (target_id ^ "\n" ^ contract)
            ; topic_id
            ; topic_closure_sha256
            ; evidence =
                [ "test/agent_docs/docs_chatml_authoring.ml"
                ; "test/chatml_typechecker_test.ml"
                ; "lib/chatml/chatml_parser.mly"
                ]
            }))
  ;;

  let topic_contract corpus ~surface_id ~topic_id =
    let open Result.Let_syntax in
    let%bind closure = assemble corpus ~surface_id ~roots:[ topic_id ] in
    let%map reviewed =
      List.map closure ~f:(fun topic ->
        match topic.specification.review with
        | Pending ->
          Error ("coverage topic has not been audited: " ^ topic.specification.id)
        | Audited _ -> Ok (topic.specification.id, topic.sha256))
      |> Result.all
    in
    [%sexp (surface_id : string), (reviewed : (string * string) list)]
    |> Sexp.to_string_mach
    |> digest
  ;;

  let audit corpus ~(targets : target list) ~(mappings : mapping list) =
    let open Result.Let_syntax in
    let%bind () =
      match targets with
      | [] -> Error "coverage requires a nonempty feature inventory"
      | _ -> Ok ()
    in
    let%bind targets_by_id =
      match
        String.Map.of_alist (List.map targets ~f:(fun target -> target.id, target))
      with
      | `Duplicate_key id -> Error ("duplicate coverage target: " ^ id)
      | `Ok targets -> Ok targets
    in
    let%bind () =
      match
        List.find_a_dup
          (List.map mappings ~f:(fun mapping -> mapping.target_id))
          ~compare:String.compare
      with
      | Some id -> Error ("duplicate coverage mapping: " ^ id)
      | None -> Ok ()
    in
    let%map mapped =
      List.map mappings ~f:(fun mapping ->
        let fail message = Error (mapping.target_id ^ ": " ^ message) in
        let%bind target =
          match Map.find targets_by_id mapping.target_id with
          | Some target -> Ok target
          | None -> fail "mapping has no inventory target"
        in
        let%bind () =
          match String.equal mapping.contract_sha256 target.contract_sha256 with
          | true -> Ok ()
          | false -> fail "compiler contract changed; review documentation coverage"
        in
        let%bind topic_closure_sha256 =
          topic_contract corpus ~surface_id:target.surface_id ~topic_id:mapping.topic_id
        in
        let%bind () =
          match String.equal mapping.topic_closure_sha256 topic_closure_sha256 with
          | true -> Ok ()
          | false -> fail "topic changed; review documentation coverage"
        in
        let%map () =
          match mapping.evidence with
          | [] -> fail "coverage requires example or behavioral test evidence"
          | references
            when List.exists references ~f:(fun reference ->
                   String.is_empty (String.strip reference)) ->
            fail "coverage contains empty evidence"
          | _ -> Ok ()
        in
        target.id)
      |> Result.all
    in
    let mapped = List.sort mapped ~compare:String.compare in
    let covered = String.Set.of_list mapped in
    { mapped
    ; missing =
        Map.data targets_by_id
        |> List.filter ~f:(fun target -> not (Set.mem covered target.id))
    }
  ;;

  let require_complete report =
    match report.missing with
    | [] -> Ok ()
    | missing ->
      Error
        ("unmapped authoring features: "
         ^ String.concat ~sep:", " (List.map missing ~f:(fun target -> target.id)))
  ;;

  let entrypoint_mappings =
    let make target_id contract_sha256 topic_id topic_closure_sha256 =
      { target_id
      ; contract_sha256
      ; topic_id
      ; topic_closure_sha256
      ; evidence =
          [ "test/agent_docs/docs_chatml_authoring.ml"
          ; "test/chatml_composition/authoring_context_tests.ml"
          ]
      }
    in
    [ make
        "one_off_v1/entrypoint/main"
        "64c4f50a9a1a6fac1ecc64fa075a61d56f9d884fe03e8d6d981094d9794ccc62"
        "runtime.invocations.one-off"
        "ee07cd1a51de018896436570b6c6195fde3f736df20f9cf96f655a68d5f17edb"
    ; make
        "tool_v1/entrypoint/run"
        "1edc5c34b159cedda05075fd0fa5abeb2ef464a37e93a57c36e5d11bdc6e4db3"
        "runtime.invocations.standalone"
        "c7f866a9e9d29c2ab77125b57aa22dd360eeeb556f3d466ae64307b7406c9d3b"
    ; make
        "moderator_v1/entrypoint/initial_state"
        "83c4550d994027c5d2347ec8fe8b1efe366e48105a2d8563b8b17293b534807b"
        "runtime.invocations.moderator"
        "b092943b64d89f61f8c63d9ecf17e6c1263746932fac188b306dacba22fb0605"
    ; make
        "moderator_v1/entrypoint/on_event"
        "57b6930208431728953597683fbb37e3cbcbfa49ee6e11ab88cabc03f135905e"
        "runtime.invocations.moderator"
        "b092943b64d89f61f8c63d9ecf17e6c1263746932fac188b306dacba22fb0605"
    ; make
        "delegated_moderator_v1/entrypoint/initial_state"
        "c179907ea7bf651414fa4400607c3f75beb87fa748e048e10abb1e8feaff7f8f"
        "runtime.invocations.moderator"
        "3aae7d8d502126fda8c0270a53c8b2a550144666900fb08f78cc20128916afb8"
    ; make
        "delegated_moderator_v1/entrypoint/on_event"
        "144a8646a312ca395b58114a6eb11f3c13a167f5f373db3a78222816e6acbbfd"
        "runtime.invocations.moderator"
        "3aae7d8d502126fda8c0270a53c8b2a550144666900fb08f78cc20128916afb8"
    ]
  ;;

  let task_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "873b9f154db650e5f77d473666fcb7da18a81da176549fd52eb2cde3a44d31f1"
        , [ ( "module/Task"
            , "a5057743d97256c6e123f089d84a21a57b42d89eed928a4d5183f527edf69a92" )
          ; ( "module_export/Task.bind"
            , "274e1738b20e0621475597d8e172be47420d32180c79c5f1f18f0dea4afe0555" )
          ; ( "module_export/Task.catch"
            , "ccf3b08ab8d322b5b1c1f85dda60a6e5143abac4845f91b25ce7d9bc25a314f5" )
          ; ( "module_export/Task.fail"
            , "87144726ec28b6b741dbe69314d4cf53d535c61844cd4a876bde87ef16b830cc" )
          ; ( "module_export/Task.iter_array"
            , "701bd85edbb8890bd877261017ba1ba7ed4536bcd0da2771fb1ba7b30914f942" )
          ; ( "module_export/Task.map"
            , "6ac2714682bf18c3899679e3c23ed8117a3bcce32b9e9c3f5043a510470d5f65" )
          ; ( "module_export/Task.map_array"
            , "e545158cbbea327024488856f60ec1a8f11dddb7639158fb12d91b96fedab39c" )
          ; ( "module_export/Task.pure"
            , "0bb01ffcc976325a794463b2a16cbbd97116d10adb1bec9a4e376afff93c8f9e" )
          ] )
      ; ( "tool_v1"
        , "b631c5850592a190e151230ca8d5f66ca9b3ba26e03133b773d76460e9460922"
        , [ ( "module/Task"
            , "2113974a76db0e60a5d9f9b676c78a2863ea283b282fa841c4e0c4f5d20da7df" )
          ; ( "module_export/Task.bind"
            , "051f4da2e8ed1ea6c1cb5198986da7659195191ddc5c6180d291989d294d9a66" )
          ; ( "module_export/Task.catch"
            , "5b7bd9fc5248785682c5cc974bafcf32b19c0c011f86373860a27471bc15a967" )
          ; ( "module_export/Task.fail"
            , "c94bcea233605c0e7bfffad449191798368d862d09fc56cdab6e61c9055b16a2" )
          ; ( "module_export/Task.iter_array"
            , "aa2fa97ee5c5a634684b863f2bb85193a4d21dc2adb467b9bcb60481be505e2a" )
          ; ( "module_export/Task.map"
            , "52871b04f55a9776e388b8354036e23f4c5fdce9c7d87d55fe3d41ae30b16c72" )
          ; ( "module_export/Task.map_array"
            , "e7d5098da2895eeb55c1bc11b8986181cae6c0f681c1b6a1d50c235a4c6168a6" )
          ; ( "module_export/Task.pure"
            , "d76749102b2bdee5923fea7df2847544ea3e2a5337b7487c8a5cd96eb4bd5721" )
          ] )
      ; ( "moderator_v1"
        , "bac7beee30b97ac2ad6f3e370019b195355525483055bdb17641ea5cda46886a"
        , [ ( "module/Task"
            , "6b60971e6b0bc872f863dcc3dc1b62660e028b863021ec7fc79f23ea824c32b8" )
          ; ( "module_export/Task.bind"
            , "d853de56e33e37aad546cb7e4ea6a026b8bae3681e40592fd327aacb13ca3000" )
          ; ( "module_export/Task.catch"
            , "15a4f3ef868ec6287a2e7898b869bc49bc6c542cf93c1ef3275987b249fb9270" )
          ; ( "module_export/Task.fail"
            , "3e54225f13fdd5444cfca2db2b81be6031cd16a6ec9e708879f3d842d81411e2" )
          ; ( "module_export/Task.iter_array"
            , "c1731dfbac7361af4666e3919a304c7e0c7fa38d2fb9e04aa9640181b59ce892" )
          ; ( "module_export/Task.map"
            , "1b25952904e5653c0bc27ec2396f01297e61fb5f9b1583246caefce41730d508" )
          ; ( "module_export/Task.map_array"
            , "b5d167a288138d85643982df30294d5f31b3363cefbbd527663c4425f7e1360c" )
          ; ( "module_export/Task.pure"
            , "99f009e6fe16865e89ada34f70e71376560ba382fa373456fbb75fc811363b08" )
          ] )
      ; ( "delegated_moderator_v1"
        , "3e31eeb00d477c0334d066301d9f028f48f27158629d0e6ddb3e563da9cc13cd"
        , [ ( "module/Task"
            , "f77899678f7eb95edec85f308d5c47bc51a4e2fd8e91ba9698ad8d857f652b9a" )
          ; ( "module_export/Task.bind"
            , "61efaf1e960bf73ec2c8fd58800a36428ef66074ce13dfedde6ba6758c29d4dc" )
          ; ( "module_export/Task.catch"
            , "dcdfcf90251c9ad6d68cb6f0d0c90027e420ee6f9d4a55665f63edde324b1123" )
          ; ( "module_export/Task.fail"
            , "414d7662fbe5be4c1f107294ad704a15ae03f236273275229a99ed68ca8edaff" )
          ; ( "module_export/Task.iter_array"
            , "0247ff78d4a06d4824e1c7ceed9f4b56f04cc1a6e799aa0103a52162ee3b9512" )
          ; ( "module_export/Task.map"
            , "7d1fea8b83f24c8b9423274b2219f22c1c9adb59e77cf74fc9fec24caad047b7" )
          ; ( "module_export/Task.map_array"
            , "65ff1a412c4d8edac40d827cc2a4009cb5a887971f9d936c76364941ee9f5d68" )
          ; ( "module_export/Task.pure"
            , "6046b0cbac68d8df0c00bec1c0d689f0bb910fc5f5d78f2ceb3018b7e5043b3d" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.task-effects"
          ; topic_closure_sha256
          ; evidence =
              [ "test/agent_docs/docs_chatml_authoring.ml"
              ; "test/chatml_composition/ingress_tests.ml"
              ]
          }))
  ;;

  let string_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "5c94df79a958ba879ed589c80ff51e2fe35763a892979fdf9f2256a745a592e2"
        , [ ( "module/String"
            , "f83f103698597607463a72b4760e05febc850b6e61f0784c78206b293f112b50" )
          ; ( "module_export/String.concat"
            , "3dbcf1fdea08e50428f1772c90186e7ea4e940b56729c5b68cbc515b22bcac1d" )
          ; ( "module_export/String.contains"
            , "afc3a21c399a56bc9866df76abcfa7ea3f4c9e630026364190c96dea802d335a" )
          ; ( "module_export/String.ends_with"
            , "7c0222a60e29ba6128e24258926be91b23b9fa58e49debe4db34d4d1f0b42099" )
          ; ( "module_export/String.equal"
            , "49277dc0e0bac283fc5a6b6a55103bcd343c49cedc547f74fbdae9ebd473c79a" )
          ; ( "module_export/String.find"
            , "c76fbe4a457a2ac35d0ffd4c24c1d0ec6a6a79a2d43e17dbe18679f1237b415d" )
          ; ( "module_export/String.is_empty"
            , "ac6ccfc3b741e5138e1914cbd0ad80da21714df80d139cadaa060d98dee459df" )
          ; ( "module_export/String.length"
            , "7834074271044259445e24822407a0f1e983b58e462583059261de7b9bfedfb7" )
          ; ( "module_export/String.replace_all"
            , "63d43bb435d1c7cd67d6dd4ac56f219f0bda96210a7b6e82e7ab68fa06c4a646" )
          ; ( "module_export/String.slice"
            , "5c16f67d2f96e07703f0de4e617a035906a164892d9ddd9a716917fe3780c93c" )
          ; ( "module_export/String.split"
            , "359439cb01e781c8e7b4921f37f90990494ad5bc1f551006ed345764f3b36dcb" )
          ; ( "module_export/String.starts_with"
            , "b249946b6ed22f55d000d2037ffa2e23aee30fc6c68264d37330d6fd518d9f86" )
          ; ( "module_export/String.to_lower"
            , "dde7bc2ccdab808c9c2a72cb8de746316d92f0a6038d417a6ce45e0dbe4d54bd" )
          ; ( "module_export/String.to_upper"
            , "46d748f77991005958a5827fc5c1b2d71103c817a84c715993f2728b94eea36c" )
          ; ( "module_export/String.trim"
            , "d3de248c6c57b40dc591609f2c150b7f252d4e4e1d16618cd50e4d5e8365224c" )
          ] )
      ; ( "tool_v1"
        , "20604f049b07ced310b19a7f4584d64e5e9114887d0b6939a2f657c3fb4b4f2a"
        , [ ( "module/String"
            , "83fe32513e2d6a2f499cf26764e89aa8664f4d9fe79e59842a41eba900d63027" )
          ; ( "module_export/String.concat"
            , "d48ced3d07737fad10f6fdf95c2d955a8abbb1c53d1bfadfb51cae0206d5aa0d" )
          ; ( "module_export/String.contains"
            , "838214df2c3baa91cb3be28cd2c8490951d789b3f15f026e27f23b45ce656aa2" )
          ; ( "module_export/String.ends_with"
            , "2c7361077b8aa48b6c68e2028376f0bd5d28e8e240c7eeb0419c0cf8bd5c85b4" )
          ; ( "module_export/String.equal"
            , "44ee63c4d91bdf88341384d9ae7a9bc140c62f29a5cce6a7075a91810bfde6f5" )
          ; ( "module_export/String.find"
            , "552cee68d321512e4f7ead0ac5765517367a4670537b91bb30b11a303385640d" )
          ; ( "module_export/String.is_empty"
            , "ab4213fda11ac67241b53039c4b4dea80f84b8c4769376d37c2e3c597337b363" )
          ; ( "module_export/String.length"
            , "c4423210f14f759453c34dd85a65ec28fe6999696ed909f95ed668dedab1bf43" )
          ; ( "module_export/String.replace_all"
            , "5c2bdbfb66db3b4ce397443c67339937f8015d8a742f71574d554bdb67244c1a" )
          ; ( "module_export/String.slice"
            , "7dae10253229a8e9e9a5b114c5b5c599f5da97f921daeeaeaaa87df503d40bd9" )
          ; ( "module_export/String.split"
            , "a4d6d0f21013ca54bbbd489e5fb7052b446b68ff0e91e2e5c48e99912f5cea17" )
          ; ( "module_export/String.starts_with"
            , "8af0d9c38ac7b1966a62d969daf486b43ddc60a35a47501d8efbf63689c4ea5e" )
          ; ( "module_export/String.to_lower"
            , "ecce4ceafe5a4e31c6f1a84918cf3d99489ee6a30a256c6ccd1bf865908e562c" )
          ; ( "module_export/String.to_upper"
            , "aa02e4c6b23eae1c3b4fe77fe6512292ae2c02e8b1056a20b7ee4a628ac16f81" )
          ; ( "module_export/String.trim"
            , "22dab6846df6dc37ac7e5079f113b6166ae4d5b980ef3e37b7adf99df0b2990a" )
          ] )
      ; ( "moderator_v1"
        , "2d98784cb55d01ca072645d240ffaab59fd848549191ecca8c00b26b4e7e0277"
        , [ ( "module/String"
            , "509fb6b69a90ce2af0f64b3bbc6a42cac7f6fec12163f7b6f03bbfdd41017989" )
          ; ( "module_export/String.concat"
            , "b6deab74ac6b53a96977bc1dec63ca862fc905b583c16961daafbc5a1a9b14a7" )
          ; ( "module_export/String.contains"
            , "56943a3d4799d2e422af3b5a8d53877de81d295c11c0ccc6f9a1185c1bf31463" )
          ; ( "module_export/String.ends_with"
            , "ce4abc624c6025f7e542fc474932bc7e5997c9e8fd43b44e47a901d0bda130c0" )
          ; ( "module_export/String.equal"
            , "196616f9bcf3cb1831fc7c71db80440b97b26a15127a1ba71a1de6712b8077b8" )
          ; ( "module_export/String.find"
            , "d2324b938cd039574fd70a8726e0e9480795129c0e653d9e763e9979cd0522e2" )
          ; ( "module_export/String.is_empty"
            , "191e44b9deb73f79c09eb423d5b2999b0871b8ba9b66604ad0fb5103305d7d27" )
          ; ( "module_export/String.length"
            , "06dce9731ebb9237913c186184a70acc8b5bb6ede681f64b2bcff742d51277f1" )
          ; ( "module_export/String.replace_all"
            , "cd9accc75a948ea5f72eecf799bc69bb7f06ed748bb25425707bcbab9020ba7d" )
          ; ( "module_export/String.slice"
            , "0a4e1909e70838b22d64533e9cd55815fbedacd86e7f2a01faf529681e7921f8" )
          ; ( "module_export/String.split"
            , "533c262a119ebfa480c5bcad5472dddbb45dedcba74e78e1f6dcb9ef48093746" )
          ; ( "module_export/String.starts_with"
            , "fe2f45543de0fd36f63cc9ed1da3bab161b7c960ee1cccd0548fc3695202fef8" )
          ; ( "module_export/String.to_lower"
            , "095c1d3b70a6b6f8aafc3a237070ba445d0622b94da3cc73f1a0b0e77391995b" )
          ; ( "module_export/String.to_upper"
            , "73e70dc1d7d7e77d3672f1cec9f0275d6b1fccfd87356459661c510c37a2edc4" )
          ; ( "module_export/String.trim"
            , "05ac7d5684a1fcc693b1d04afb9439137a0bb7e6e17eb5e5bf83e4ea0d8be80a" )
          ] )
      ; ( "delegated_moderator_v1"
        , "4371639dbb6b857d2a27ce48d1326eb5c5d08832d2fe7c8e8b8e11febf0939eb"
        , [ ( "module/String"
            , "a9aea91e0022011ed771f94ef74061eab1d235bf0b56c94d8135546424cada59" )
          ; ( "module_export/String.concat"
            , "591ea0dee3999ca8e64c501bb07c7cff2ceaba8c599e9e508dfbc2b5a25a82e4" )
          ; ( "module_export/String.contains"
            , "efec110cfd86815e080e3336ae448908eacdae38719bbdc761d4c14a06eb5f3d" )
          ; ( "module_export/String.ends_with"
            , "6e2569cca05ddd90b69c3e7a8e149ed3be13486d1064df92b2132e8d1fee7b38" )
          ; ( "module_export/String.equal"
            , "c5559b9d2acde927bf359894c23bcb0d7d054f5598d248c8611155116668c022" )
          ; ( "module_export/String.find"
            , "6846cfa2e29fc9dade2200c0bd711cedab57f3e1adcf531c6e2a4adf18c3c81c" )
          ; ( "module_export/String.is_empty"
            , "f71a0522a5623c607b3c04eaff069f72bb633b1c61eb4ecabb057730af422cdf" )
          ; ( "module_export/String.length"
            , "c8e0f28d32c6eed56055d7b7f5670a2c3046fa7918df23f018dd84c8d21e4dfd" )
          ; ( "module_export/String.replace_all"
            , "eb101975d47255d378bfd3da2a89521fd04b98cd37512614fb0bda491d6b9dd7" )
          ; ( "module_export/String.slice"
            , "bc01f0c5687682c796d43c4f63b9a023a22a2c67ec7b75e9cfacc3813be0079f" )
          ; ( "module_export/String.split"
            , "2ab2468cac6fc0d5d86104b2db1d58f75890393cde5887319943caea2c4ec2af" )
          ; ( "module_export/String.starts_with"
            , "ca5a0174363c4c5a0d14d92579724e298f108513e1301321e27d1118beed8761" )
          ; ( "module_export/String.to_lower"
            , "dc2781c8102b557928974a3407469c12a3b1795001f1a233597f66124820da4c" )
          ; ( "module_export/String.to_upper"
            , "332f1505bfd654f5ff1f954821d7efa2bd61e4b89b226e1eaf7918f24a2be586" )
          ; ( "module_export/String.trim"
            , "4fa4fb048f66dcc1f7c83f8756c6e126dc809f7f1e158a1b550f660c88c4a2a5" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, bindings) ->
        List.map bindings ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.strings"
          ; topic_closure_sha256
          ; evidence =
              [ "test/agent_docs/docs_chatml_authoring.ml"
              ; "lib/chatml/chatml_builtin_spec.ml"
              ]
          }))
  ;;

  let array_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "d11d093c2e89b9e808b3ad87ac56a385ad328a8dd907f4e0eb52d0eb5c9f49b2"
        , [ ( "module/Array"
            , "78131df6b3d97fc91f2ea988d48ea28aa0dd8cab44858ebb72cbae22f036e25b" )
          ; ( "module_export/Array.append"
            , "bd0b45a0676986702047fbed74e60a40ef5d24c6afdff21b22035acd0f80ff64" )
          ; ( "module_export/Array.copy"
            , "6be9b94172c620f4ba598b952bd30025d9ba5757e4f33352fa8a0334eff0971b" )
          ; ( "module_export/Array.exists"
            , "afc0f747c74eea1ffe5a7c7e2a24a6032b465ae2542e98e9cf2fa83f13b6c728" )
          ; ( "module_export/Array.fill"
            , "67229bf885b93abb7157df5c6cd52ecc4ffb0254533fd57a8c9894cffba9e012" )
          ; ( "module_export/Array.filter"
            , "c7b347e0ec2e10921af69b567f160c7b95644efd28dee1f352327603cf6e3647" )
          ; ( "module_export/Array.filter_map"
            , "639462d2c7ccc8327e4b760ff787f4f525a536670e48c12dc09434b212c78a69" )
          ; ( "module_export/Array.find"
            , "9693422e0175af4b721460e50464f203f75400196af95215ae7fef1872dae8ff" )
          ; ( "module_export/Array.find_map"
            , "8d193a5ea06babd414309818fba9b0d28b00adb17a7974b3c379706de231ca23" )
          ; ( "module_export/Array.fold"
            , "58bd9ac6be6d4c70f865771adac3a4ef071dae0b23c6a5668f9e838f48d6d981" )
          ; ( "module_export/Array.for_all"
            , "966e21cc8046bc8a2b9e4a458c284574174cb12b9a5f065018effde6ebaa0321" )
          ; ( "module_export/Array.get"
            , "9e81c29ec389e012e874a8eae10bb614743576c435533803792a285b65a4c000" )
          ; ( "module_export/Array.init"
            , "578d48b3730128a37fbdd74afd4a292bcc2edc6c60035b6048b5870d647736ec" )
          ; ( "module_export/Array.iter"
            , "ee508fda3c7c06f069f3e4ea31288026269cc8c9bfd246231496e3decd31f466" )
          ; ( "module_export/Array.iteri"
            , "3a5ed080b7b3bfd7a020c8256b1c0fab871a7f83136be47be2f2ecdf78c7a8b5" )
          ; ( "module_export/Array.length"
            , "fdd452df2ce0e9a6be960ad3668707a6ec126e7f83ba3880674828ae95fd7ccb" )
          ; ( "module_export/Array.make"
            , "f373bb52fa00b9ef21f4e642577d56e1d0aec3bfbd0341ad6299586993504b0a" )
          ; ( "module_export/Array.map"
            , "aa7601eeedc29f8c69330d66f9480c3982076389709938f456fff39ed454304c" )
          ; ( "module_export/Array.mapi"
            , "d2bfe5f6b6b3c87a18ed8463ad14d98db032b419f592e61e5c90f843b7b3efeb" )
          ; ( "module_export/Array.reverse"
            , "cf380ff0ea4bfcf979a4babac0f7af59e72b45a4323aedec4194ba72f486eb1b" )
          ; ( "module_export/Array.reverse_in_place"
            , "7bf98c3f54065c47d96402755291c04ab2387d36ddfb3bf6e99363b324cea52a" )
          ; ( "module_export/Array.set"
            , "8c89c8680caa44a78a0a8498f5679df76a8cb422cc39ba2fbba7b5628dc318db" )
          ; ( "module_export/Array.sub"
            , "574dc7bd64345b0935b84aff3da31c27e7491cadb800c4227560ec9d73058fb6" )
          ; ( "module_export/Array.swap"
            , "492ac0862c0a81e7a1a8fab69fbbdbd4e771eba1d3648ef8515e2d73b878a02c" )
          ] )
      ; ( "tool_v1"
        , "f6c8b63590438df79519ea81140658d8964267b20f734cd71c59c9d2630829e5"
        , [ ( "module/Array"
            , "d2f0bd5b3888593ea1aa60b15d5011c322d547ff9d289715209acd0cb200be6c" )
          ; ( "module_export/Array.append"
            , "1957e127f0b02a0a172a0c1da2ef6e06eb11c43fcd4302900c639d2a3f825e1d" )
          ; ( "module_export/Array.copy"
            , "740bd9457271bddbd32183dd40abfda816fc96abdae8fc0d4eaa66ebf9acea0c" )
          ; ( "module_export/Array.exists"
            , "dd2937f39d64e9b50e28369925e656ddda2eeda7c322af6958552fa4bf5e8672" )
          ; ( "module_export/Array.fill"
            , "1a01881b0e409aceb8f7d0204c121574055ecfbbb65d9da9b5b03e08b2a9c826" )
          ; ( "module_export/Array.filter"
            , "6a6fed4d1ec5c63dddfcc82af4d959c0ce30a676bf023d447efa0a3c46836d8f" )
          ; ( "module_export/Array.filter_map"
            , "8cf4ee1147db3a60b97576ebd4835aab88895d806c1698a64b60f7390a0f9d24" )
          ; ( "module_export/Array.find"
            , "b534f391fab3e10d497680f2ade281e8f355011c3bee149b862eb0bf6cac273f" )
          ; ( "module_export/Array.find_map"
            , "111c88ff469effb8c37779d34bfca5b7d18945c06c424ee56afc9dc10c14ab54" )
          ; ( "module_export/Array.fold"
            , "5cfe8ad969e475b3c2ccfaba712651d4f2904fdb70990e204bb2058a07316605" )
          ; ( "module_export/Array.for_all"
            , "17b688b80d83016ca9a36d52eaaa583c997a951250aca90bfc33a6ddafc67dde" )
          ; ( "module_export/Array.get"
            , "b56d1e79865417b92e84448f65ce236b6d486cf90bc06ecb0699efb9f8ea7f7a" )
          ; ( "module_export/Array.init"
            , "82356e7ccf3bfb5b750b5ff6b4d95fc1d1e551705edfaf110b38b61ac41177fc" )
          ; ( "module_export/Array.iter"
            , "5eebf8a6a828cc60982fb1d5c40964a25b9dd75656e7ab30c8c3222e1f86dcce" )
          ; ( "module_export/Array.iteri"
            , "f14ea79fda8655ec65fbe898fd176284ffdaa8b217d19fcfbdc36418fe99a7be" )
          ; ( "module_export/Array.length"
            , "a7601153d56b1245f8862216ba70065e5a7b78808e2652a6595a508bfac4d2e2" )
          ; ( "module_export/Array.make"
            , "76479efd2889b51063b0068779b2daca54852f0caf9dcf4ce7958fe8656e5263" )
          ; ( "module_export/Array.map"
            , "a227881af11bf7f2c4be1202e523f12bc7b4374b2ebd0d2a3c7343581b3d83ab" )
          ; ( "module_export/Array.mapi"
            , "af9d53b093396a585c7d2541b31aa2f6ed848cecfb917582f94c284012857260" )
          ; ( "module_export/Array.reverse"
            , "983f7f861faf15c7236dd18b7b77b81b5479367c3757db0fc7dc6694304571d0" )
          ; ( "module_export/Array.reverse_in_place"
            , "1967fe00a9d0122ddd38ceb2f33f4af5077130152d8713088c796ed040514a8f" )
          ; ( "module_export/Array.set"
            , "b7225617db17bd902eac59ab019638984179554cc601cd94c4d0b6fff372f2d2" )
          ; ( "module_export/Array.sub"
            , "e3747471b58f9ae92b55e9c2c27cce38852aac64be696667282d56d5736b8acd" )
          ; ( "module_export/Array.swap"
            , "7107c4e1ba79f9df4d33b816245b6821b9e6ccfc2cb9ae017ff1db85097415cc" )
          ] )
      ; ( "moderator_v1"
        , "f21acb3b65ed6060c079c36b354749f2a0feefedda02a1f88a1c5850fc0e0efc"
        , [ ( "module/Array"
            , "6c0cb819069d3ce215a3fadcbd2ef3b53e038b29ded5aef027ebe125ee3ff7b8" )
          ; ( "module_export/Array.append"
            , "8477fff23a1cb9e0ae5ca1e67cd319e7ab6ec45f98155b8a875786cd189d2db7" )
          ; ( "module_export/Array.copy"
            , "53ad39377bc50b9ac16b78589fece7aeaa93d340e51cd7a03d9c4e87421e3e5f" )
          ; ( "module_export/Array.exists"
            , "290a43b7750e6c6a0cbb21234eb11e8722d1772195736e8e739e6bc11e0e043b" )
          ; ( "module_export/Array.fill"
            , "832376a7f2b6b01e5a9014bacbe5750a6c360f074f99a74ea2d4907707760a02" )
          ; ( "module_export/Array.filter"
            , "4cc3317a7ef26f3b9809129356074e21354fbf4f57548c3f6b02128fb75505ae" )
          ; ( "module_export/Array.filter_map"
            , "ca5864da859a11d8cf98d8b5edbdc4bf665d634000e15a03dfc49223d87bcbf3" )
          ; ( "module_export/Array.find"
            , "712c108fe07142efc01235fe3c191be8b0c54530d3196d40f0e3a066a8cc8f0a" )
          ; ( "module_export/Array.find_map"
            , "6193b607583f1e8ad842be9f4a8027add334aad8581ca467eb1ce69c64d1aa20" )
          ; ( "module_export/Array.fold"
            , "e91e6f6ee1a707c30fe4075e04f504ed3222ab07f04b421a6e704d6082ff2aed" )
          ; ( "module_export/Array.for_all"
            , "454f70da2f58765eec3bed27c23b52109c3bb9e1d96537b2f50a2d07a5a58dc4" )
          ; ( "module_export/Array.get"
            , "f081e182dbaf3d859755fafefd227fce10724de1386a404cfe4d53fc2c0cce04" )
          ; ( "module_export/Array.init"
            , "952962a59ded4116ae164a5a2b40033470ef006916212fa13d67932753c3542e" )
          ; ( "module_export/Array.iter"
            , "0a8c691fb4db7e6de25f5388ffd14903c6cecb0de726fa85ae7d9374e3d532ac" )
          ; ( "module_export/Array.iteri"
            , "87d9a9f1cacfe5099bcb1372efdfab465973eb8cb871a9d786e416fbe7d1b548" )
          ; ( "module_export/Array.length"
            , "d8299f8b07b7d257e6ce4a54f87984fd89da53d38e5d7aeb53b15611be6dd972" )
          ; ( "module_export/Array.make"
            , "98f38dcc4da172f30454f78bd488ff5fe383ece13a1f9daced6cdacb7e960f78" )
          ; ( "module_export/Array.map"
            , "d03d127018b5e5abbca6818ccdc444a9b23333dc5e55be11cc112a9f3ea776b5" )
          ; ( "module_export/Array.mapi"
            , "5c7cb0f5f91595eee532a2871319c1c5926367322443ee9218b62bb92514c26f" )
          ; ( "module_export/Array.reverse"
            , "575bfc0b9158718ac4c4faadd4e76d5c41c26390ed72392aea59e716d8bf9aee" )
          ; ( "module_export/Array.reverse_in_place"
            , "35ce3486b365e477659b6d0e252a2ee4d740ea350fbd4a0e15bc5cccc4e19e91" )
          ; ( "module_export/Array.set"
            , "a539a5c51b95edbc9e8cb3ad27f381c6dd1fddf2a6c8a1ad4c91045254631c4d" )
          ; ( "module_export/Array.sub"
            , "2313346512abe782a0a059ed16af21c0d33ed697dbb258466ee69c94ad0cc862" )
          ; ( "module_export/Array.swap"
            , "4fafa016b2d0bfbb6567494be00b463f8cb76cf48953e0e99084d8003d146c5b" )
          ] )
      ; ( "delegated_moderator_v1"
        , "a9afef77ac5700af3adb08f1fd18f590e9673b09e558d5b231375adebccd9f31"
        , [ ( "module/Array"
            , "40b9b5d09333d558fc36cc99caf8bebcbc3e76f73726caf81d67a1ff1bb39d00" )
          ; ( "module_export/Array.append"
            , "18a2b7e3b60112ed2c1e443ed87e6ffba434f9b71a486d107bcc3ed583bb2e04" )
          ; ( "module_export/Array.copy"
            , "bc478e9ceeff1f1946100513186cf3e74572673199919e29d22eed1198e3aa27" )
          ; ( "module_export/Array.exists"
            , "c7920c821f16c0666ca4c77d2e74c98c579d84097892df4b7cbfe4c50286741f" )
          ; ( "module_export/Array.fill"
            , "dd8a0a0564bc07d55eadad1ab3aa3b3d93efbfa7f859a328a60cbcf922b48d3c" )
          ; ( "module_export/Array.filter"
            , "a66e13f2d212434c6ee2e6c0f97b18e1505edf8b508e19762dfe5d680182ed4a" )
          ; ( "module_export/Array.filter_map"
            , "db50cea7acd5e449f70affb2ca37aa4b1962f51fa2399f778d7fdbaf01b24e12" )
          ; ( "module_export/Array.find"
            , "a78ee183453454c4d002955b52bd33b4cf177835638b096fec5cf10e57ba1c44" )
          ; ( "module_export/Array.find_map"
            , "7ae022e5692f64f62777b2e33e313c270f1614934cf285192dd4e9e2121282a5" )
          ; ( "module_export/Array.fold"
            , "2c191ec8e80fb1f88ac8fa59560c59082a206ca748b66f596de9c7e9e27562ee" )
          ; ( "module_export/Array.for_all"
            , "adb5a403e411a883bf1db63929c7ed86ca10e8d12189db0e5bae9083675d983e" )
          ; ( "module_export/Array.get"
            , "5d24ec02d938812ea426a53c3d81e3aac3dd0d5778eebdd25979197512fa58d0" )
          ; ( "module_export/Array.init"
            , "64faf4a9839637dd6d28c0bc737033cc66de6a0a08169e20507afdd5ad8ab7a0" )
          ; ( "module_export/Array.iter"
            , "f434c7552d8a9752baecdd52d3ecfdeab5b5f13813489170050bd03913ff221d" )
          ; ( "module_export/Array.iteri"
            , "04420e675167477e88aa03d40953ed2849741d13e282d89a74bb68696c94b78f" )
          ; ( "module_export/Array.length"
            , "113bd0c26fe99dcf635daffe280b83fb686dc49c62af3759de4300baf79dfafe" )
          ; ( "module_export/Array.make"
            , "ffd56b96f1771c6f0e02efc89d1bb7ac71c39d01bed2084e46116346dc8fe0d9" )
          ; ( "module_export/Array.map"
            , "f8555375daab594b6ba19913268d3a52c3646a786893276f82596870cabda49d" )
          ; ( "module_export/Array.mapi"
            , "39423b894f51d85bf70050b15e32aca8576d2831757984eac9ea254179904c81" )
          ; ( "module_export/Array.reverse"
            , "4d55c58a1c40efa975823ebde6a9b0db8e27fa803044506cf2824a04dd6b6bc5" )
          ; ( "module_export/Array.reverse_in_place"
            , "7d09177b6c9656700cf77002103d2ac3ee1d664b9508d33d22f3f0d8d0b86178" )
          ; ( "module_export/Array.set"
            , "05a52ac46e8937292a4d5e289da790d10c6924afdbf2d7091116025880536f49" )
          ; ( "module_export/Array.sub"
            , "a8ec33311681b727e558e7b81e6046ceb6f706f12fb599a4ded2b5f3ff76bf2b" )
          ; ( "module_export/Array.swap"
            , "e7447cd1971cdec89625cc67a23d7144952623298578baee4ea4b206d854e35a" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.collections"
          ; topic_closure_sha256
          ; evidence =
              [ "lib/chatml/chatml_builtin_spec.ml"
              ; "lib/chatml/chatml_builtin_composition.ml"
              ; "test/chatml_stdlib_composition/composition_tests.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let option_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "d11d093c2e89b9e808b3ad87ac56a385ad328a8dd907f4e0eb52d0eb5c9f49b2"
        , [ ( "module/Option"
            , "35625c7916a5431fdcef89a05a6f24dfe4154b12e3076e6a38db17e8dc484b96" )
          ; ( "module_export/Option.bind"
            , "82ef5800cb6ceea1292e4ea755b920a37562934637dffc262ae168fa4ac63990" )
          ; ( "module_export/Option.get_or"
            , "c250f563b7a2f9bc12e0cf82ca57b1cc77fa0dfa41111f474650b5f8b82414ac" )
          ; ( "module_export/Option.is_none"
            , "6b04fc380bf31bf54108b5f5ac5b2b143cc79a472794d1c6a3b44c82d0a07869" )
          ; ( "module_export/Option.is_some"
            , "caa9d8ffa1aa1a119203078ca97b4fcb5bf804e2bf4b2d80125ec3345a3d5665" )
          ; ( "module_export/Option.map"
            , "ff175c9d6770401b5b214cc5ff4f70243140b6b86e6e1c9b52cde29cd8bc5937" )
          ; ( "module_export/Option.none"
            , "51b2b211332bacda225ab3746395a5a095f4b3b46be5acae3b528e70ce4b8aec" )
          ; ( "module_export/Option.some"
            , "2d06dcb286e0900c6c401a1607f94389f2c162ed5295284d6783a92c1ab496b1" )
          ] )
      ; ( "tool_v1"
        , "f6c8b63590438df79519ea81140658d8964267b20f734cd71c59c9d2630829e5"
        , [ ( "module/Option"
            , "8139706127f4fb46b6b7d2c8f8951efe78e58e59452de6cf2c5a78e552a489f8" )
          ; ( "module_export/Option.bind"
            , "2de38526c594aa63853db7c607bb6a8265856c0d4faae4b5712e6c960d32fd1e" )
          ; ( "module_export/Option.get_or"
            , "9c20bee21649c4a569d2d86599d18cf2165b742ea5e8ff5c7bf0c160f1ed2607" )
          ; ( "module_export/Option.is_none"
            , "f672252d23f7ef0ace900f2980dcd00379eda6996fb46e1928622b1e9d4c106e" )
          ; ( "module_export/Option.is_some"
            , "d8fa9d851e3bdb7ca6fced5b8dd2a8075dffa2552cc52d2558a82e3a43566e1a" )
          ; ( "module_export/Option.map"
            , "081465f387cd690d5728b38af9ce3e3f88e2fd8e8e7eff88e83fe67ce7f0f8d7" )
          ; ( "module_export/Option.none"
            , "8ca69d39a0ed409a3a612f4a05f2d14d3aeb5604d391e43af446df1f5b9d4710" )
          ; ( "module_export/Option.some"
            , "fdc12fc1fa7767081c1c2eced825aa42f25905ff9415fd9f1183ffa713659fe4" )
          ] )
      ; ( "moderator_v1"
        , "f21acb3b65ed6060c079c36b354749f2a0feefedda02a1f88a1c5850fc0e0efc"
        , [ ( "module/Option"
            , "eed8a04643315785f17a45f6ff41a87b0936217c1205317613c9aefbd8375690" )
          ; ( "module_export/Option.bind"
            , "7942f156a02348603e382a9e95d3a9062a619a0c6a9714ad9d0fe5268ae11de3" )
          ; ( "module_export/Option.get_or"
            , "ae60c603c28bf1760498ea80263f0d7aa32ccedf77d990fc31e5f7a35d9fe757" )
          ; ( "module_export/Option.is_none"
            , "3065f59760816b941033459b39c66ea7a21fe6ab23510462fd1ddabc30bbc3ae" )
          ; ( "module_export/Option.is_some"
            , "f40bd0b7ed039a9bef03cb171230b897a65222895d8aad0d70d910b5d5e79d72" )
          ; ( "module_export/Option.map"
            , "b13fde64f06d4aff318e810f70748ef3236e114c0fa47506e72501ced0192b22" )
          ; ( "module_export/Option.none"
            , "561cddbcb13b387a1b43803f89fab7583f3feb15e7c0dbdea7da0d7df6718097" )
          ; ( "module_export/Option.some"
            , "4a95c14987aa4682bb0ac2db2ebe0d80e2264f6091c8de55396d71c9ec750d17" )
          ] )
      ; ( "delegated_moderator_v1"
        , "a9afef77ac5700af3adb08f1fd18f590e9673b09e558d5b231375adebccd9f31"
        , [ ( "module/Option"
            , "eb0a6714a9d24b6c7e72a1c0e54a424080e40e700fd464aaa8e8bd672fab7d53" )
          ; ( "module_export/Option.bind"
            , "f608e82a8d21f1eb58bed8ca16cb36b91dbf040b09cc4547bccdc90f447cc7b9" )
          ; ( "module_export/Option.get_or"
            , "b31ef258cdd8bc2f320a54cd48f264842a4a7dbbd202d7d30a4fbe44f0109bb2" )
          ; ( "module_export/Option.is_none"
            , "ec3e9752098d1b7977de8352f37b6fffd78f0f4f3d96933c17ec8aace7232a20" )
          ; ( "module_export/Option.is_some"
            , "49c1691b5027232893bc96f84380c00baa44a5372efae6d41167a0ed944d2d06" )
          ; ( "module_export/Option.map"
            , "ea09465f404fa8c9cd14c1747d9ab83ca760a93d18a90db586c740811c0d11e6" )
          ; ( "module_export/Option.none"
            , "8f2156ab892384645ab21e76db703b9655e45f158fb127858876c3b7809a9aff" )
          ; ( "module_export/Option.some"
            , "07f7717f967646210831799b73c15b32dc1752cbcadd46200a68390729e065ae" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.collections"
          ; topic_closure_sha256
          ; evidence =
              [ "lib/chatml/chatml_builtin_spec.ml"
              ; "lib/chatml/chatml_builtin_composition.ml"
              ; "test/chatml_stdlib_composition/composition_tests.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let outcome_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "d11d093c2e89b9e808b3ad87ac56a385ad328a8dd907f4e0eb52d0eb5c9f49b2"
        , [ ( "module/Outcome"
            , "e435dfca077d10897f521785f7dafe0c86a01fe386752fbb2a0234114de59706" )
          ; ( "module_export/Outcome.bind"
            , "a95e5220c637422682bc54707bf9427d1500d2235d52dbe6b04bf3e737790765" )
          ; ( "module_export/Outcome.map"
            , "3036ac3b4f040b48d62924af22e5ebb791a069e5e2e848dc8db67e4ec79885a4" )
          ; ( "module_export/Outcome.map_error"
            , "9d73e4974185c1a820795eec910d65915dd4df98f187f2ba921f9a366452099e" )
          ] )
      ; ( "tool_v1"
        , "f6c8b63590438df79519ea81140658d8964267b20f734cd71c59c9d2630829e5"
        , [ ( "module/Outcome"
            , "70c228dd632095449a6156da6f58efe28c70a41f3321ab6f893d3dc2638505e2" )
          ; ( "module_export/Outcome.bind"
            , "4c667e1281e1b46cdddc31adeed8be5aeb8b0dd5250fbabf3da1c75cebb56e23" )
          ; ( "module_export/Outcome.map"
            , "e4d90d708f62d4d941e0bbe4816ed524b96ca206ae0612206198195712afa781" )
          ; ( "module_export/Outcome.map_error"
            , "7c19db4bebf811b275ce818c14694473b3f998c0f1cb67d1a43021e0d433f0b4" )
          ] )
      ; ( "moderator_v1"
        , "f21acb3b65ed6060c079c36b354749f2a0feefedda02a1f88a1c5850fc0e0efc"
        , [ ( "module/Outcome"
            , "17caeac929da390a3884b642721bc3bfadecfa1b63172721cababfe2350c4e2b" )
          ; ( "module_export/Outcome.bind"
            , "ed82688158eb95356acae10e8c1404734631f9831b3c407d0341fbb1c4b43035" )
          ; ( "module_export/Outcome.map"
            , "138ab97def1bc8c075122f189ea21d64d10c068ff72cc3cf9f2e387be727e477" )
          ; ( "module_export/Outcome.map_error"
            , "7608f3960f136289934fe6f637eaacf70ac11ecbe14cba2680b14d2cb4ac1786" )
          ] )
      ; ( "delegated_moderator_v1"
        , "a9afef77ac5700af3adb08f1fd18f590e9673b09e558d5b231375adebccd9f31"
        , [ ( "module/Outcome"
            , "1e62658f3122705d5b95f6ed2aaf951696b5e92081ff924f74b2c345a5022fa2" )
          ; ( "module_export/Outcome.bind"
            , "1a7feaf623a570697004391337817a3dd7413cd788cc277ea79319d23df4f6ba" )
          ; ( "module_export/Outcome.map"
            , "eb42e01a708804ec88a6a123bb7f12e94ae119a26acaa76a8274868fa29e9226" )
          ; ( "module_export/Outcome.map_error"
            , "5b2ec66dcb3c5cda5cb224f47f1f4b2413c6ec703d9941f48c891cabd5418fdf" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.collections"
          ; topic_closure_sha256
          ; evidence =
              [ "lib/chatml/chatml_builtin_spec.ml"
              ; "lib/chatml/chatml_builtin_composition.ml"
              ; "test/chatml_stdlib_composition/composition_tests.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let json_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "d3fb1c4f54b2bdadd040675bbe919fcd6bb84b149dc279e08e7463d8faa24111"
        , [ ( "module/Json"
            , "8276161ec095028a7648fb61044eca879e3255f69d1280b8ad20b13c350235c1" )
          ; ( "module_export/Json.as_array"
            , "bbdb7996f5aed56db44bb15bff469821bb99ce5972a3764c8f42a18b853868ae" )
          ; ( "module_export/Json.as_bool"
            , "e3d64b57755cdb4714f4eebe82766d8ffb0200050857f7dc0bff10cf433e875a" )
          ; ( "module_export/Json.as_number"
            , "d5427796dfabdf0c97682226c6e76ceb0ab51629368a5695f1cbe1c4027b29fc" )
          ; ( "module_export/Json.as_object"
            , "5247189707ad12473ac02dbf47085ab475d091c423c4f9a78047e017079297cf" )
          ; ( "module_export/Json.as_string"
            , "d64119406c25277e258baa568a43c7edadc8b679d438c56330f2df8965312058" )
          ; ( "module_export/Json.get_field"
            , "8dd2a18718c8ae22254e131ca5a709480e7eec00d4440a1f0f845354c15b9f73" )
          ; ( "module_export/Json.get_path"
            , "af6e7d8c42e5633b3e0e8d82e278bb0378ecad5d1be5ecd1c65e443d2a57ff5c" )
          ; ( "module_export/Json.object_keys"
            , "b59f939da470d0d3c5e8a3c597485a92943cd5b661359ec392f5496681957d50" )
          ; ( "module_export/Json.parse"
            , "8f836cd53e5d62617c3b09491bead196d9a68572e09924d04eb9b6cf60772a3b" )
          ; ( "module_export/Json.parse_opt"
            , "66e5331a69948fb1e8514f9f717ffe79fe0279d2e8287e40ec7782ee30429541" )
          ; ( "module_export/Json.parse_result"
            , "ed53ca0f8e2f92cf1388f0fd7f450a5e554e7616d07cae5368f2281981ca2278" )
          ; ( "module_export/Json.pretty"
            , "8e4554faabaf2f366e1673267f2543d87724d4f873e7a8793ddca4088459bdda" )
          ; ( "module_export/Json.remove_field"
            , "a55c2b6a8f4035396665ac167199ba5388c50dab727e3f3987512f52416acb76" )
          ; ( "module_export/Json.set_field"
            , "0be68e86dfb83917a8bee22f217dbdf13d7a124f47bd777c7621fc4ec6023236" )
          ; ( "module_export/Json.stringify"
            , "2de22a1a614f08f6248fd5943ff41a8ecb129f3302b9272533b74b9edd1b5e99" )
          ; ( "module_export/Json.tag"
            , "5048e3b84cc7c85ec099deb3663d16b77d827a1aa9e853035c7b87e0eeda66eb" )
          ; ( "module_export/Json.validate"
            , "775f180b4c48e9955bb980993420ca28ba4a832139e5905eb31edb35b940369b" )
          ] )
      ; ( "tool_v1"
        , "cd899dca5fb65aae8b6e1af52167234846ade66203eef61bfe4ab63ece988977"
        , [ ( "module/Json"
            , "f594a3d6bedb39aa513044c62ffa6eccab150fdc787b030a370c47b61b9038d0" )
          ; ( "module_export/Json.as_array"
            , "297c045f0281978de474232b69bf7465db7c792318944cb932d39632e4968998" )
          ; ( "module_export/Json.as_bool"
            , "0322593f8aa1d13b9818072f4707882da0b5c477b3bd0d6dde7f2da125a75233" )
          ; ( "module_export/Json.as_number"
            , "d6caf59af33c1ab9b2b364929f32acfbda5a07252d25fec697206cf2950ed64d" )
          ; ( "module_export/Json.as_object"
            , "67e29e5dd5938aa365008d88825d1add1897ea0dc3fb2d855239d0beadb3d506" )
          ; ( "module_export/Json.as_string"
            , "ad301b73001564b9fbd16c92e6dedd164ab2986104e89ecf75851de36fcf1f0d" )
          ; ( "module_export/Json.get_field"
            , "8a0f10388e5e1cfc91210117fe95f3926d04bc2cc7abdd7cdeebe38ee155fb70" )
          ; ( "module_export/Json.get_path"
            , "8c19da2d8d9b6df93169809b27ac29aaed6a0c064b93c216c4515231cdb281a5" )
          ; ( "module_export/Json.object_keys"
            , "00325c0381606fccef9235a80fdbd433135b00b447ae509c1cdf4bafe085317e" )
          ; ( "module_export/Json.parse"
            , "fb2412b4113f5fccd41b213adeea235c36614ffc968ab9883dade033809e168a" )
          ; ( "module_export/Json.parse_opt"
            , "24ee488c8de7f1ccfc7e11476ed0de8a297642393286c900a0a29e5b3223c036" )
          ; ( "module_export/Json.parse_result"
            , "3492325a76cc74d13e806c4838b41e70638b96775e09ada6eac47e1696c09e55" )
          ; ( "module_export/Json.pretty"
            , "3a8489bd8d3238b28eb32f506e3c0a63911688b2783a52da96fbafd1e7fee588" )
          ; ( "module_export/Json.remove_field"
            , "4e9ca8153aaca94722ec4143b1e3b9ee7163b56bea289846f5ec68d066a91b7d" )
          ; ( "module_export/Json.set_field"
            , "f64878aee175873af14020e5e412876dfdf191373f82504a23b2d1f9ee6dabe5" )
          ; ( "module_export/Json.stringify"
            , "352ed21c255e1e5942e435f56e4bc861600392295eef1ac6b13a123855b036d0" )
          ; ( "module_export/Json.tag"
            , "32f3eda894a10e4ad5cbd7c30dccfa0cb60c971a3235a6e497cbf53463b7d7fa" )
          ; ( "module_export/Json.validate"
            , "91a455c705c41b4b046e84ad03f0125533c4872d94745e1f0a67c9e334eeaf3d" )
          ] )
      ; ( "moderator_v1"
        , "dbab086520689b37e66243d721a00a1fd415d0086fbfb5c4e05cb6f5d1ae2458"
        , [ ( "module/Json"
            , "5693ccaf8653d32a31d1f478d215fbb447bac00435cfc10c3df8caee1ad85670" )
          ; ( "module_export/Json.as_array"
            , "5340743c970edc2f7e218817fa0e55f454a8546f90de8461383ae507a678a7e6" )
          ; ( "module_export/Json.as_bool"
            , "106d6ef531886dd487c4364a81c16824292abd38b93a37362e7be3cb7d9ad818" )
          ; ( "module_export/Json.as_number"
            , "56a4814f47e6030be7283bdbe0a3fd5c324b78c89905bde88a9898284c9939b5" )
          ; ( "module_export/Json.as_object"
            , "25d5a0681db995e3a99fe354df2f4ca0d9c60c41948b0caddeb3fe7ae97ce21c" )
          ; ( "module_export/Json.as_string"
            , "2525f1f00da31e4452cbc588095d3e755d35a64e676e38cbcb7085bfb393c1b1" )
          ; ( "module_export/Json.get_field"
            , "e2333be53f02a57b0bc5990b6108d56b8b9ed01a251b0be8ae34e7acdbdd233f" )
          ; ( "module_export/Json.get_path"
            , "66214780233dbd617a50eb45ceaba74cd641f55ae6e66155b183e1add73c53c9" )
          ; ( "module_export/Json.object_keys"
            , "89792a481321c86522a2db0211dc4b2a05f54f75dc10389f883e71314583ca84" )
          ; ( "module_export/Json.parse"
            , "26f918a33e82e7c4e0d90547e68e7cfdf2c4221f5c08d90f0e9aaf02325e5195" )
          ; ( "module_export/Json.parse_opt"
            , "134a47c74ab0f4e0697477eee39c01afdd985ce4e9158a016f84c4aa1356f788" )
          ; ( "module_export/Json.parse_result"
            , "41a0ebb6be93e498cbd98dc3eda119bd4bc987b01bd0ae58d52fe3d4ae7ffab4" )
          ; ( "module_export/Json.pretty"
            , "36f442baf3fe981e57b88b62f262d3ce863750c86d88d291432af9b691fac27c" )
          ; ( "module_export/Json.remove_field"
            , "bc77a1a835c95b8d4fca410cf1912d32b67242e27da21fb4c068104a0ffca37b" )
          ; ( "module_export/Json.set_field"
            , "cb482fd45f051fc1c8503a7e62ff10e4a03f70c0dcd47418404196542091ce1d" )
          ; ( "module_export/Json.stringify"
            , "e1f74c16ff77bd7588d35ee464ad88bec35cfee29d62d041f227324910828ea4" )
          ; ( "module_export/Json.tag"
            , "5db006b6182b67a574ab065f3fb1361fd1be762361852c202bbcb064f7289f30" )
          ; ( "module_export/Json.validate"
            , "fd94d4e57a65a4381f7c6c87a98cc99f5b1c080b4eea5f9a6f20da11bd20b988" )
          ] )
      ; ( "delegated_moderator_v1"
        , "f25cf5ac0e6cd37531824e5d1d36b77054cf037c0d87ccf2140f6a86eb6a0558"
        , [ ( "module/Json"
            , "dd36cafbd30b3b4396d4d7b02394ed09a988f3d818cf4626c2fe7da26cefc457" )
          ; ( "module_export/Json.as_array"
            , "93dd88523493b7eb9f075a194c003d241f971b429b27a7617a0d6f62eec26d34" )
          ; ( "module_export/Json.as_bool"
            , "039954974748ba8bc987b0e1cf07319445b58d966f75fd081e36687e477d3ef8" )
          ; ( "module_export/Json.as_number"
            , "169c4a446d6081401c10c904db2b83044401e42f819c284ab697e0e8e7add168" )
          ; ( "module_export/Json.as_object"
            , "b57cc88d9db0c37eff3a65e016cc24225bb0e5836159b82c14805a9ba7d71217" )
          ; ( "module_export/Json.as_string"
            , "6a621caf154c8ea6babf2df96b198b22af428c933de8600abd061e656f63b91e" )
          ; ( "module_export/Json.get_field"
            , "7a7b2d2efb8d3bf221c976e7016c205a4878e9bcdf447a349e39d2e969f64c4b" )
          ; ( "module_export/Json.get_path"
            , "eabd489d4e7145726b62b4707d1f7fff5078a66a33b98b9917846993af572663" )
          ; ( "module_export/Json.object_keys"
            , "64bdeeb3aee4a87176d1bec55b33d032ad6a3c6cacc687d233c7687453b9bb8c" )
          ; ( "module_export/Json.parse"
            , "9413edab5575b80f0021b9937708ce3739fe7646a921008b122960a60ce6633d" )
          ; ( "module_export/Json.parse_opt"
            , "84188ca33edee557fd8f1c9190f93ca587c90a34877e64550155a8948f5ca08d" )
          ; ( "module_export/Json.parse_result"
            , "2b11d19f3ebc14fa93c2c7f5c95bba47bb565c2963d922c901737cbdbbe3fc00" )
          ; ( "module_export/Json.pretty"
            , "af50d68b217c0f2feec6eb014c2952568d8f6c6692c5a36957ccabfaf66275b3" )
          ; ( "module_export/Json.remove_field"
            , "e40fa6d13f1adfdf229da5b8b981f5cb3ea207469cb05f957b6d655ba8f001bd" )
          ; ( "module_export/Json.set_field"
            , "8dedbdd5eb34731489c35f52934b9f21199b8b432b4eecef0eb6afd5bb7dd56a" )
          ; ( "module_export/Json.stringify"
            , "4e4904f65104484986b808d3c6b3814b9be156438b827944fac9ddaadaa14c66" )
          ; ( "module_export/Json.tag"
            , "62d8321fc6756d8ba1c0edd69e2f348fe96c04469093d9e6e3114bfa62f6213c" )
          ; ( "module_export/Json.validate"
            , "ceda17f4f76502ab6e64c33d8b307fae18a055da08ed5d3287c4d0e750ea8f11" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.json"
          ; topic_closure_sha256
          ; evidence =
              [ "lib/chatml/chatml_builtin_spec.ml"
              ; "test/chatml_stdlib_composition/composition_tests.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let hashtbl_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "0271d150988f9b2dc71a74d407a99f519573ab9b22e4a6732ca2ea31829e3eaa"
        , [ ( "module/Hashtbl"
            , "44d97e5d36cbf23bb3b2e234a045a2f976bbc486da1bdcb2cf65c69b37d188e7" )
          ; ( "module_export/Hashtbl.create"
            , "1213527868f4fe7ebdda853ed034372dd4179fd3e2036ffeafe34145d0af4152" )
          ; ( "module_export/Hashtbl.get"
            , "57d751e2d23d73f24373a98bc846932cdbb343e1c5d7f685d6365f8410c7dcce" )
          ; ( "module_export/Hashtbl.mem"
            , "776c3ae80fddf50e10db2835e2b8ee84a1b5e8869f0d061b1e3a1e72fdfa5917" )
          ; ( "module_export/Hashtbl.remove"
            , "d78dfb76b213b6520389c43d36adb73e2bf9587fe59b0f2de5e5a0d2f98ebd65" )
          ; ( "module_export/Hashtbl.set"
            , "b1c0b7ec1710258f0df03ca217b9ee7a0a4696aaeed773439f5c920d3555c11b" )
          ; ( "module_export/Hashtbl.to_array"
            , "24ce283fd2d4c4fa586859668d522f0f728e3d17de34c77785803c580bf50a87" )
          ] )
      ; ( "tool_v1"
        , "35e12f6a4d88bc638b3631eebcf8047b64a559ecee751ed0b411aa1f95f89978"
        , [ ( "module/Hashtbl"
            , "866a8d298a7e56dbdb498517254086e88873c396a4595d494a8a05a26e8d3414" )
          ; ( "module_export/Hashtbl.create"
            , "a4d73c58e8d5fbb11a8d152fd6ced4a181342cbeec5977f4e4e8338431660fa8" )
          ; ( "module_export/Hashtbl.get"
            , "3bb74810bda0ad1ef03e4ac7d8571d34e16b56b57ee71d5507e666babb12b1f5" )
          ; ( "module_export/Hashtbl.mem"
            , "287de86a7494590e25cb3d3ea473fc0012fa6996518b6cd074324970f7f1fc10" )
          ; ( "module_export/Hashtbl.remove"
            , "fef23aca6a955ca9f9cb7fccbfdfa909e4432a17ff351264a913d2a229ecaee4" )
          ; ( "module_export/Hashtbl.set"
            , "a8aed9d5f05a6de5b81289f27bf5b5828d13abb93e2dd1635ba4249c83774f27" )
          ; ( "module_export/Hashtbl.to_array"
            , "f7857cc5a0e7476d12fdb5119cc170d848992813d26932a038ea9c93727a12c2" )
          ] )
      ; ( "moderator_v1"
        , "bd24543081d503a9e32edf8f6de1c0f6fc591c06d42509caf7aa2738587e8f11"
        , [ ( "module/Hashtbl"
            , "d3de0228abd1f243c2d62e30fee6d770908c8db4724e639bd1cef1200bcbcfdc" )
          ; ( "module_export/Hashtbl.create"
            , "4918800b94fe6812967df0847b9efe2ba2bb69c94f4a056e9fb5094da3f8ac07" )
          ; ( "module_export/Hashtbl.get"
            , "443ea43fff13312dfffd9a176cc3f3b43c663ea6aa1927b0028ab90af5e2bb13" )
          ; ( "module_export/Hashtbl.mem"
            , "2783288d3fa1e31f7a884deefadd4559ab223e6edd1fb087a08d7150fc674f0f" )
          ; ( "module_export/Hashtbl.remove"
            , "2ec781e9883f73b471980084be795eadb0d15f117712134a565267b7cdb9eb97" )
          ; ( "module_export/Hashtbl.set"
            , "37a78e5e2cfd9c7b5258d1d360cf3450ef66cf4bb0040b5356b5d95c18b9d407" )
          ; ( "module_export/Hashtbl.to_array"
            , "792e47a1ff269fd246f8e35d44bda353608d8d88902ba3d9c07e09e32179d30e" )
          ] )
      ; ( "delegated_moderator_v1"
        , "38ac20f3b39a7fec380618d6c738dc707b6803f0fbd6e937370bf47c8c9fddb5"
        , [ ( "module/Hashtbl"
            , "8460da183ee7f58302eed019d338422946e327f2522208d193c50ee7c80dabe0" )
          ; ( "module_export/Hashtbl.create"
            , "b8ccd94c7f31132b76d8aad33fadf709b3214bf7eeb5b83285d3c96fa56f0259" )
          ; ( "module_export/Hashtbl.get"
            , "aec1d32e69c3b119aef5d44d7e03beb84cbf10fde367e0d0964c73e487f9201e" )
          ; ( "module_export/Hashtbl.mem"
            , "0334be1e766167ca50ac23386019a7199d4104b920eee4a2b3fb709d8e38a177" )
          ; ( "module_export/Hashtbl.remove"
            , "02cd071b617ccc445c91c470f618d105e5966d094edea323efc3a03134942e7d" )
          ; ( "module_export/Hashtbl.set"
            , "727b76361304424162a6d4429b81e4109b719ab0e00ba79735a4c91a32dbbf95" )
          ; ( "module_export/Hashtbl.to_array"
            , "34cebef1f8a02ea16849d4087e5ebef3cc9fa2965b1a6be2277326ccf28d2070" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.tables"
          ; topic_closure_sha256
          ; evidence =
              [ "lib/chatml/chatml_builtin_spec.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let global_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "7ffd27ded8f8722e63414b580308e71e81c6bfd938c5f71c3f90d26f34faa935"
        , [ ( "global/array_copy"
            , "326c36574ab0637860405f83de93ae1cc46de5d4773bb6c5ae899820b9847b98" )
          ; ( "global/fail"
            , "0cb59f2292447d4ef3be1a2ea4ddeed8b13982e43df7afd2b5469ce76a356c27" )
          ; ( "global/hash_md5"
            , "41da44cf55bae0214bfc1246fe0894cafd88b18528f1c09e7443542306d4c031" )
          ; ( "global/length"
            , "aeace1ff131b4ee17d25261203e769409faead1f8ceb016c5146c55c7ecada4a" )
          ; ( "global/record_keys"
            , "8a2ca71008c82b1a7f097e025bc6612acc637c3eb73baf6fabaf7d99f58b913f" )
          ; ( "global/string_is_empty"
            , "aa275489b18cef598b4a907ca70ba380ab894b1beb72c895bfe536b8149d2655" )
          ; ( "global/string_length"
            , "8fa44df51298bdbe02e5101299945fc7aaa66ff43c7d002ed99ee1aaf5dc21b7" )
          ; ( "global/swap_ref"
            , "49f6064a50ce43b7554c6bd7104703ef296ac78227ac420543349ec1db5a7c54" )
          ; ( "global/to_string"
            , "4edf84ecbeeb1233ed58c0c79bc6456e841d5a060fa8b56880b47de2f177909c" )
          ; ( "global/variant_tag"
            , "8bb9905e0682879b2c43d2493d37acf23bcbb47fedbf23fdbac2e398d17fa5c1" )
          ] )
      ; ( "tool_v1"
        , "13162a647606f6a3cde15bebe8efe6370edb6189b12224c431d48963d38a4401"
        , [ ( "global/array_copy"
            , "a853a83afa1c9f7446e785af3bc34d652573e62e51a7350bbf7b4a197cf11893" )
          ; ( "global/fail"
            , "82c4d09469ba872e37f953906236530f127e1debb245b2b4207cdaa4b041741d" )
          ; ( "global/hash_md5"
            , "c3eb0f36573d265dacd55ca075ff34ceff33d302a2b0b067cebb5ab520b33be2" )
          ; ( "global/length"
            , "8368904b15fc8a4b40f90b8e0e67cdbb0da831d38b651abc092820d21d84b93d" )
          ; ( "global/record_keys"
            , "7591b1feecc7dbbc44149de158075985b88d2ee8857a42ab8acfd274770ece48" )
          ; ( "global/string_is_empty"
            , "8f24aa783e6637561c7d6a617ef244388296781ba97e2ca605f77ba49b087253" )
          ; ( "global/string_length"
            , "7c98966606f5bd97aa5baeb1347dfed587e3eef3db267f81ed8c88ea86e448da" )
          ; ( "global/swap_ref"
            , "70a8a3eedcf4d82f7918170d70036bd7ee0a641aa00ce023ebbede032f7b1d08" )
          ; ( "global/to_string"
            , "d168492b867408fd316135febc077c75d6a27322f051eab516a85cf60919ab33" )
          ; ( "global/variant_tag"
            , "33d50d056896bd02e4ef0957ee23b80d555e9ad8a211743e228b322236995205" )
          ] )
      ; ( "moderator_v1"
        , "52675d0d6c7d14bcb0ee222e91775b2dc974017d233f4c395d62295255253905"
        , [ ( "global/array_copy"
            , "593a99eb4072c3be8aa8fa8485efc2fca4b4efe65740568b0454e222e55a4e20" )
          ; ( "global/fail"
            , "6bdd6583bbfdbf38e1cec2862613405f3599f8351c9aef5e24a1c55efdf23e25" )
          ; ( "global/hash_md5"
            , "a8b2476f2e41d243b677e73e73cb7453c9315855b6de49e03a977a5d1ee52afd" )
          ; ( "global/length"
            , "ee54b176bca408726a4f54814625ff46d1e453c26b3acee8b2f7de97fcdefdf4" )
          ; ( "global/print"
            , "4c92a831438876c9857cae474b51af4bb6804574d605f6f4b190f8d4f8a5a80b" )
          ; ( "global/record_keys"
            , "7fee73e71ae28d34fdea7c8fedff14587093f578019915ce8ec0cba628ac734b" )
          ; ( "global/string_is_empty"
            , "298e95885e51043d3fef9428f55e52fff5bff2aa0c7393d48081e89f5a801909" )
          ; ( "global/string_length"
            , "b17096c250edd2bbc0ebbfe84fbdd33df14e630f47f692be8647d6da26e8c733" )
          ; ( "global/swap_ref"
            , "676f3d3e98659e6c48288437d9865f7dcca9412d18b7c93d19e750ebfa537f89" )
          ; ( "global/to_string"
            , "f8c225211e90e3e883f1741f4734930ae23fc096c8ef823c4cce086d9d5392bc" )
          ; ( "global/variant_tag"
            , "205ff716c378f45f26e61da51d375cf6a42e283fd6d257e8955ffbb887b4de2c" )
          ] )
      ; ( "delegated_moderator_v1"
        , "ae0e18c0fbef3efd694955431302fd52fa4bac4f2a256cf0573699612bee0cad"
        , [ ( "global/array_copy"
            , "aa75d71c337e070bee24369a8d14eb038601e57c106751310299f9890875b5e4" )
          ; ( "global/fail"
            , "221b75a5f71e51114075d7ff0344bb96e711bdb4ff4848dd0cef501b9acb9605" )
          ; ( "global/hash_md5"
            , "aafec401a51ce3c607238bf3a3b8854f88db55dc6f9ff64cf585c27a47f87468" )
          ; ( "global/length"
            , "ddac7a36eb0b29756c08c9bbe22ce78df37ee3f3a53384057b309bd40cd10241" )
          ; ( "global/record_keys"
            , "f855eaa5b1acb221a86c2e624e34f0424be1a323ee6aa7f75e06e64c8a7d0de9" )
          ; ( "global/string_is_empty"
            , "0646ea2618ee94ed73cbafe60631d9404d6186bea2f2dc8b88017527e0c780f1" )
          ; ( "global/string_length"
            , "6b9a1bb68fc5e33064099d52c4e83fdb9896f973eebc2804cd8d6d45f1e10c86" )
          ; ( "global/swap_ref"
            , "28868319f208e4940446d8aa0e8053811247438e3f509f74b404b310184f5b24" )
          ; ( "global/to_string"
            , "20d56853e18b107288211b460fa40f234924a9045a27d49abafe27f563860247" )
          ; ( "global/variant_tag"
            , "5659d2a7b042a359404c0c61f5f5dda2bb58bf9a86f7f53f842f21dd263e5535" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.globals"
          ; topic_closure_sha256
          ; evidence =
              [ "lib/chatml/chatml_builtin_spec.ml"
              ; "lib/chatml/chatml_extension_surface.ml"
              ; "test/chatml_runtime_test.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let json_alias_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "d3fb1c4f54b2bdadd040675bbe919fcd6bb84b149dc279e08e7463d8faa24111"
        , [ ( "type_alias/json"
            , "fbc8c5ee393ba0b6d964f6bb75511c62695879973a5d79d05b827a287ac68ae1" )
          ] )
      ; ( "tool_v1"
        , "cd899dca5fb65aae8b6e1af52167234846ade66203eef61bfe4ab63ece988977"
        , [ ( "type_alias/json"
            , "8d3754a6b4fc2796d8af78faca9d3b61cd4fadf5b8a35b04dca4cdeb3ab8831f" )
          ] )
      ; ( "moderator_v1"
        , "dbab086520689b37e66243d721a00a1fd415d0086fbfb5c4e05cb6f5d1ae2458"
        , [ ( "type_alias/json"
            , "dd4fec9f78fc2918d9bda1eb49d75eeba661fde32476899e80fc25fcc1307a48" )
          ] )
      ; ( "delegated_moderator_v1"
        , "f25cf5ac0e6cd37531824e5d1d36b77054cf037c0d87ccf2140f6a86eb6a0558"
        , [ ( "type_alias/json"
            , "0f369802e412e80f3683ec684b0d9d5f891b735b17c1ca6b0a9dec6cf5dea435" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.json"
          ; topic_closure_sha256
          ; evidence =
              [ "lib/chatml/chatml_builtin_spec.ml"
              ; "lib/chatml/chatml_value_codec.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let moderator_data_mappings =
    List.concat_map
      [ ( "moderator_v1"
        , "ade6a43e81177173ec428759c80f69a7a5683583a6a3672f7cd0dd71a8e7f7ae"
        , [ ( "module/Item"
            , "30a1aec7224048beb927ac1cff733daaaebb03fc4144e90f3ec7319bb0694158" )
          ; ( "module_export/Item.assistant_text"
            , "606d8abe1340a93749bb1bb99838a68926efb3531783e86665f80d983e51d987" )
          ; ( "module_export/Item.create"
            , "a3b0da852487f8382776590c3251b5d374aa044a4892971ac0e20f6eab7d629c" )
          ; ( "module_export/Item.id"
            , "f85eda0b8ee5a3409b74469558ce30ec03e2898d0515a7b9d6280e6ad587b718" )
          ; ( "module_export/Item.input_text_message"
            , "8bed7b21b5fd2bc728bd0cf7106c54abeeef5000f2900d6843445a69bd96f4d1" )
          ; ( "module_export/Item.is_assistant"
            , "2246c8ca733b0647d0ed66279e91ac12643a6ed885517529c006af5573db9646" )
          ; ( "module_export/Item.is_system"
            , "60fba7eefec4bae1d219f9bd7559ba2a1763d4f48f8d483f1fada3feb77ebbd0" )
          ; ( "module_export/Item.is_tool_call"
            , "4538ef56c1a8ca487e60b527ea5a20e26108e4710b1d603939a547b51dd146ba" )
          ; ( "module_export/Item.is_tool_result"
            , "d2c67cb0e41fd88e7b7fad186cb84723ac9c40f9fbc8f2c887579cd7e770cf40" )
          ; ( "module_export/Item.is_user"
            , "ad513f35957ddb16ef9e20dfe01125ff14a213a5a8e355f3461afcf7d130b9db" )
          ; ( "module_export/Item.kind"
            , "a1955812d65fb8002bda7e71a960e5bd22dc44ba43ec1dc77e9efb0dfe7a4315" )
          ; ( "module_export/Item.notice"
            , "5a79f951d6ec7fd7bb733a2d731b7ef4bb1f99978178245590d1e10c4d0721d4" )
          ; ( "module_export/Item.output_text_message"
            , "52875b810391538f3704a00f1c581442acff53316f2b379d4ce65b2e7c9a6ab4" )
          ; ( "module_export/Item.role"
            , "f9a2ad0071f300e24d8242e3987b8489b13245a7682a45a4972df1a34d179f46" )
          ; ( "module_export/Item.system_text"
            , "e3ecb26f34a7edff9e95466abc57041b859051aab8c1ca4f72f1987c7d6e6f36" )
          ; ( "module_export/Item.text"
            , "86f85d116ee95c633718401715a12b56125237fc1b1c4a706ddd88e82ca2b181" )
          ; ( "module_export/Item.text_parts"
            , "b006cc48ef484b0d22903c150eb67d0828a29c028010b3525b6814792bb59afb" )
          ; ( "module_export/Item.user_text"
            , "cebec29ebf110fe3c993bc1a26a8f53463e6ced14f20125c34bff53127584a56" )
          ; ( "module_export/Item.value"
            , "f902426fc28dfcde7ef7e850494242a00a2b5f71a1562d94633ed2ef519c9631" )
          ; ( "module/Context"
            , "043cc243406f1aa4741526787e90f317d226bd14b92760e36ef8a9ba3eef2744" )
          ; ( "module_export/Context.find_item"
            , "5bc39261b85eb319a7e1750a59f6c044307144764df6c216c72f5baa64f5494c" )
          ; ( "module_export/Context.find_tool"
            , "a18488bf686c664376d72dade6f14eaea69bfc74aea78183ffbac2c4937b9c7a" )
          ; ( "module_export/Context.has_tool"
            , "9f851e5aa974a481a4623e32b41a81b2c8d4683b409cc8227448d3e6aa560730" )
          ; ( "module_export/Context.items_by_role"
            , "328e511650f8080cb05ad922e3547e101c2f72715507e86009778964a5f214c1" )
          ; ( "module_export/Context.items_since_last_assistant_turn"
            , "1c5a577ea86a2ab25edf747e36a0feccb549d4085614181f7f74154aece655ec" )
          ; ( "module_export/Context.items_since_last_user_turn"
            , "a69f978e88514bee87e51338f1cd72ce82fc6f1a6d34fed2ea5eac1a3cbd1541" )
          ; ( "module_export/Context.last_assistant_item"
            , "9effe34ce9e301b1197d880f4749d95c593f0f4a9527a6c95ba461a8d0abe580" )
          ; ( "module_export/Context.last_item"
            , "6b940803c645d1038577ef468e47f7d89ec3d6a449a633e51ea7c37b74dc8ab8" )
          ; ( "module_export/Context.last_system_item"
            , "180fea4cb024fa7e6e9601767159d9a793c4d2a8c433457c76c3671d97ffd27f" )
          ; ( "module_export/Context.last_tool_call"
            , "6fabb5fdc99fb9dcebdf194253096ec52daf5c7e55f00fc757c4838615765e61" )
          ; ( "module_export/Context.last_tool_result"
            , "53754a3622ee8cb4ef50c612051358bf1769aacef5bd5eb720797acd01583e3f" )
          ; ( "module_export/Context.last_user_item"
            , "8a9c0c8f674e9ba1428effb8678dc7811fd1659a08884c0c3f0985a0c2ea9c44" )
          ; ( "module/Tool_call"
            , "111edba6b295387104b0daccf838d3bc4a1bc1d28a33ed408f9e3602bb848389" )
          ; ( "module_export/Tool_call.arg"
            , "55edc67651c4aabb3c08b288deb96b9331157b5f4ed95f95271e10f276627d8d" )
          ; ( "module_export/Tool_call.arg_array"
            , "ca866932216d38b027e5eeb97a5e4c10da20f81509cfaaf154d40e51edb25638" )
          ; ( "module_export/Tool_call.arg_bool"
            , "309cffbadf52a7aec710e60a7fbf2737808b22e2cf75278d7b37dc81c5c0d8ae" )
          ; ( "module_export/Tool_call.arg_string"
            , "c6efdde9a6f7dc9494ca0a3bd66702990220c1389566ba12ae1fe236a6ac820f" )
          ; ( "module_export/Tool_call.is_named"
            , "d9e83d3884dffac6fa679e811affc81a186052424f1b0a9246a71d516dd7edec" )
          ; ( "module_export/Tool_call.is_one_of"
            , "807112ecbfb365162ce8529bb73f55ddc52f0c1b593aa57a8aabd55a5299cf59" )
          ; ( "type_alias/item"
            , "779c45cff30af282cadf210f3cb5ca22dd3d74746790166b2126aa3e8867f987" )
          ; ( "type_alias/tool_desc"
            , "20d180b4abfc6867ecd98501438c37a319d759080fce6ddd8c8c2c72678a2b22" )
          ; ( "type_alias/tool_call"
            , "1884b099cde804fb7506d277b48e327fa6f0ae4c35cb7850697cee91f7b719ca" )
          ; ( "type_alias/tool_result"
            , "2411fbab2de06d52f0feebf84fce8da9acdc81ff5453514f5819d38e64cbac1d" )
          ; ( "type_alias/context"
            , "ae2e3abe05eb8ef3dc578d255620b17b914e44e8dfcd2da6227e453b870e41a3" )
          ] )
      ; ( "delegated_moderator_v1"
        , "ef5e8e80ec137a784a84070c04ac2bc2141b149bbdabc4f686dd5a53625433b3"
        , [ ( "module/Item"
            , "0d8a88fe4ea744ffef1c1e2c8ae566e17f6f27503da74c9ca8072fbb85f1b612" )
          ; ( "module_export/Item.assistant_text"
            , "be12a4d9e334644ef22b3dd34236f6261a93fc8d923208f408e0133d5752817b" )
          ; ( "module_export/Item.create"
            , "736b45c85aeb4fee02f5d588dbeb6c38f1ede0b55b87bec6be7bd17a4e2a9876" )
          ; ( "module_export/Item.id"
            , "951bf73c02f1375addc38664afd64d9df6dc3e85a19bca820d604a0383ad5b54" )
          ; ( "module_export/Item.input_text_message"
            , "72497f3505c95f1b172564bbbd7032ddb0e6c56184658f382347d73c7beb29b9" )
          ; ( "module_export/Item.is_assistant"
            , "57e7be76d1550eb45703617dc470e21e108acd42611a47d7d4a63b089e8dec0c" )
          ; ( "module_export/Item.is_system"
            , "77d635951c317897dc75784e0fdbb9f614a7d9e3586152003d9dd5d10fad38f9" )
          ; ( "module_export/Item.is_tool_call"
            , "53d43b4b6b810ec1f589246f949f1f6fba87ab9617c8b2734ea3f50943cdaf6a" )
          ; ( "module_export/Item.is_tool_result"
            , "f80bc783425547f87d6505f0095ee55f6fb0d763ea8e8fe30a97acc36abc0feb" )
          ; ( "module_export/Item.is_user"
            , "6ea949e2400b8177c257fe28680d0bf8e2664389cec5ae237f70d394b57135a8" )
          ; ( "module_export/Item.kind"
            , "dbf37c7fee15b869898cb68921ef1d0fcee63ad57140521075c1bb1cd95047f2" )
          ; ( "module_export/Item.notice"
            , "f4c035dd0ef300995fade2e36c779d248750223d18ed31884e0222fdbcd2934f" )
          ; ( "module_export/Item.output_text_message"
            , "a2fbcbf9335b91f92e584b3f51ecda34dcfa68a0fc6b8fe7aac9da85fc6bc6b5" )
          ; ( "module_export/Item.role"
            , "52e5235e6b21a6cd294d6a830e2484b8a28d78b356debcc9c03f91643281723d" )
          ; ( "module_export/Item.system_text"
            , "f9011ddd32c28d59f405035dc3b051bbbeb200bb0870c504fd09122951dc10b3" )
          ; ( "module_export/Item.text"
            , "e280939ac455acd202aabbe95852584bb25725f11c31ed3b03a3352b5837f48e" )
          ; ( "module_export/Item.text_parts"
            , "fbc15ed7f31e521a241a2bf0002e5205488430a359023c454b93ab76301f1e3d" )
          ; ( "module_export/Item.user_text"
            , "3be8b8c09aea101cccd12f7e4c3c478ae056ece307517a0a333a78c26c3fbdce" )
          ; ( "module_export/Item.value"
            , "e93d1bc81641d43c747a1fe55102d748043c0b7afdc59723764b74beb70d58ec" )
          ; ( "module/Context"
            , "f504951196837027022c550e85b3f7955f6776d45fe26eb1798afee2014f8ad4" )
          ; ( "module_export/Context.find_item"
            , "daa34a75324370fb1d8ed6120d50f3582b8973ff321918e6d1b9aadc511adea0" )
          ; ( "module_export/Context.find_tool"
            , "a295f68e762ba26082b9205e1e30758b78130054510641aacedc37331f43e2fe" )
          ; ( "module_export/Context.has_tool"
            , "a547b19ea7b91a7271cd379293a4a1512be64b9ba66f7c2e176e75ea5bd88b4e" )
          ; ( "module_export/Context.items_by_role"
            , "2824bde31280c20d3cb1fffc08580e78bd5a0072f71ad36ab3a1849ee3764649" )
          ; ( "module_export/Context.items_since_last_assistant_turn"
            , "1ec4b37086808f831dcb0a9ff4d1fab66f9adc8c1f4afbb59782b85f4ea5d221" )
          ; ( "module_export/Context.items_since_last_user_turn"
            , "90ae76cebd01d1d70fb6b914b1f7e596248220cb5c86f0d45f84c019ae22c23d" )
          ; ( "module_export/Context.last_assistant_item"
            , "e7a49857d5cea8c59c93669e61d9b797cfb4a92f9df3f39e60063ab3a315adbf" )
          ; ( "module_export/Context.last_item"
            , "9e982456517755b03e8092ad1dda642ce229aa6703a9b560ed986d2a0a644f6f" )
          ; ( "module_export/Context.last_system_item"
            , "7037ccdbe6e384cdfb5ac1476450088380c8b111595fc0e478f1613fd4979f14" )
          ; ( "module_export/Context.last_tool_call"
            , "0f5942209547da6684c58cd2bed9fe93d34530e413f061834bb9884db8863d5c" )
          ; ( "module_export/Context.last_tool_result"
            , "dac85e6d60a845b616cb7f6aee2da97ffe73e137b70e79f25ba0ae448e48b6a7" )
          ; ( "module_export/Context.last_user_item"
            , "fcadba7c80d98c3a2b0395a17788c25857266a0a5b6ac171314000d10ec3707e" )
          ; ( "module/Tool_call"
            , "b499b0cb8501b221b1099eeac7c45a8a07f475ae341c7ceee839d74e574ce6a3" )
          ; ( "module_export/Tool_call.arg"
            , "134a4ce2771dbf90cc76dbea526861198b23824fc2015650865947a24b664776" )
          ; ( "module_export/Tool_call.arg_array"
            , "a6b351f74f98cd930accdbe5aa00b2cb3841d09030b5a7fc698432ae73a7dcb6" )
          ; ( "module_export/Tool_call.arg_bool"
            , "d5dc3fa5ec35e55e7fea671cd89d89896a2e3630fd99041f08d554ff26289379" )
          ; ( "module_export/Tool_call.arg_string"
            , "70baccc3bdd4217a25d7652ad70e358737eacabbf18ba1440636cdd92ca37b2c" )
          ; ( "module_export/Tool_call.is_named"
            , "d9cc8147300ed80c0b6c2dceb0c07ae33830528c681165702857e22f5467100e" )
          ; ( "module_export/Tool_call.is_one_of"
            , "082b5dc528c7933d11cec23a74875432f969b7287461a65319cbc8377c831cf9" )
          ; ( "type_alias/item"
            , "02f0f8fcba7f09b35a5392ed12eb97643be4244a318f2d89ce8cdb604f7d169a" )
          ; ( "type_alias/tool_desc"
            , "eb36eaaca293768035f3e0ca834a7507481b56913a6780584df0d607e0bc2a9b" )
          ; ( "type_alias/tool_call"
            , "7360df4a73d2bb80c73379f2cc15d7742aa1fbf07be324f6098d9ab55e31ed5f" )
          ; ( "type_alias/tool_result"
            , "4a68c6d881988160e45a0758cd38e07b7fa8702377b4239342f07e1496df80c5" )
          ; ( "type_alias/context"
            , "b122063df0521b2469989feb4a480a70b34bc137e63a0216652023598134cd19" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "chatml.moderator-data"
          ; topic_closure_sha256
          ; evidence =
              [ "lib/chatml/chatml_builtin_spec.ml"
              ; "lib/chatml/chatml_extension_surface.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let host_effect_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "74a30aec51743a257df226664c057aac4656b485389456f50578c42728bc3fbe"
        , [ ( "module/Log"
            , "ae0c2cefee5c6d21a40afcd7011a1c9a56e06e677743ac3b850ac468c122eb81" )
          ; ( "module_export/Log.debug"
            , "a610ab55f8fce0f6956a50e90152f265c9952599351a728eaae32cc884ebda58" )
          ; ( "module_export/Log.error"
            , "276ff8059e1fbc55e03ddb23edea47aa5ccb0a6c73ef0aa08aa74022adfba13c" )
          ; ( "module_export/Log.info"
            , "1260dcdfcc20e66d6a12591ceffb8e5448a85b52bbcb6bcb06d1910e5f2666dc" )
          ; ( "module_export/Log.warn"
            , "596fa893422c76269570d34c82fa7a2aed1f84220f9f6ab040c5b23dfd61d66e" )
          ; ( "module/Tool"
            , "6817970dd16c418885b75e649ad6d7df8cf71f073c4ff1fd677eabce77864fa0" )
          ; ( "module_export/Tool.call"
            , "303dfd9663b39058fd11d8e9e9da43c8f0fd3eb6f6e86ea4336c9213bcee6292" )
          ; ( "module_export/Tool.spawn"
            , "f3eefc265f65cba0bb6e2a2c2724170d4ad30a3e2c2a8c52b334bacade6c8b34" )
          ] )
      ; ( "tool_v1"
        , "fa765652aa067280c849248c44a9bbdaae7da92d1ef0db7f3354ea10b0255dfe"
        , [ ( "module/Log"
            , "86a23151273713e0f33cc4e8603c5a3134e37be245390e7dc6fdfbc6a779ee57" )
          ; ( "module_export/Log.debug"
            , "a985d003e8441992198cb628122aff73a27a0a139f968c0a7467c171777ad737" )
          ; ( "module_export/Log.error"
            , "0f4d60b7c478d3ca119afcf10e7c5a2e279611577e9976c2add1e822e5332131" )
          ; ( "module_export/Log.info"
            , "a7b60f8eb1da0a0462e4870c2dd0845c30cdd2725a95dd7775fd05983f046776" )
          ; ( "module_export/Log.warn"
            , "facd0904b796187f63dcb83aab21765078154e59c3be2cbe80bf1f8de60f9183" )
          ; ( "module/Tool"
            , "42b49f23f03690fbc49b42e32d6f1d586be4039833ec48292088294f85d064a7" )
          ; ( "module_export/Tool.call"
            , "2a753fd9dd38e3dfb3f17c5203a73e19c76a3a90927cfd8845010b852e373f0f" )
          ; ( "module_export/Tool.spawn"
            , "eb4541da89ff68813bd6bb1057871ed5b9f7e243863d82adc1bd0963aca93b45" )
          ] )
      ; ( "moderator_v1"
        , "650f8f1f90e5c5236ed46135f2f652467c1fd686eda709f558f4943f03f6e3a1"
        , [ ( "module/Log"
            , "d273c73d08a97532f5fe61b4796c631c0782d667279e79a3ec2b0fdf18bc61e2" )
          ; ( "module_export/Log.debug"
            , "c6bc08a709902b953042bcbee6bed85ab152922b47645f61cc2a84801613395d" )
          ; ( "module_export/Log.error"
            , "cb0a928173f6bc5a12a1990345cf0d787544390f5444444811feeaec91231c31" )
          ; ( "module_export/Log.info"
            , "b7321d2383ede11b20c2f2a0256c7fc0aa1ead57b599afda58f9e580ebf8c27c" )
          ; ( "module_export/Log.warn"
            , "efe84b9d8563c240e284c84b1b1f1a4f562b6470acda9a77b9d70f8336269d26" )
          ; ( "module/Tool"
            , "0e3cff5a3f10b65183e4f3c6aad28ef955645d4a6da02c66f38fd6717b2ebeaa" )
          ; ( "module_export/Tool.approve"
            , "25a858e9067c7bbdeac5faf9e1cbb07923d85d385ade44a8c3a121e06d316594" )
          ; ( "module_export/Tool.call"
            , "86043d9871a1e13a84bdf2b4ad87052ef6cfb4aa53803cc5364cc9b47ebefce0" )
          ; ( "module_export/Tool.redirect"
            , "d20322e3983c7d9643563611fc0eefac4f943bace011a268ac1ce89481450dcf" )
          ; ( "module_export/Tool.reject"
            , "0ca7d1cf1b0bef06206b900f5162bdf6d85396fdcff95558ce8b9ac931583405" )
          ; ( "module_export/Tool.rewrite_args"
            , "40e65cc8110f12c10e9c349f87d2b69679b50ea778030b0b7016a500d1679204" )
          ; ( "module_export/Tool.spawn"
            , "9bb72207e2d1a326e47052ac52272bcf2e5727cdf5345d708d3a19aae66247d9" )
          ; ( "module/Turn"
            , "1e28ee53d9b5bda92bb82bfd26e3dc56a67c111903f83c794f5c7339273a93e2" )
          ; ( "module_export/Turn.append_item"
            , "adc1baea0b4c9b7a0d860505ffc6c2e8ad7605342484eda2d2982dcd404e951b" )
          ; ( "module_export/Turn.append_message"
            , "651eb6ffbceb10cab16e39bf6a5f62f982db6b0758e54e8d38e835515cf0df91" )
          ; ( "module_export/Turn.append_notice"
            , "10174b9d538c71819dd0dc2c369d83575a62bd052b6385f583c9dc4049a97e91" )
          ; ( "module_export/Turn.delete_item"
            , "ada7c56b9c9e5de50f0e3cdbc89c6c69f7f7cb6e163e158cf090bade83f8ef17" )
          ; ( "module_export/Turn.delete_message"
            , "c2dcc209778d78839355168360063549e7025467f1a2cae2c587921350bbdd00" )
          ; ( "module_export/Turn.halt"
            , "56fe2d13829df37b4966ca1a7d2d83025c89b5171c522ff7f306822410275a52" )
          ; ( "module_export/Turn.prepend_system"
            , "023c26e1e66e0d14f762a10d0d14bcb2d13a7fb5a45c2e1fb39a0e8d20b675ec" )
          ; ( "module_export/Turn.replace_item"
            , "ad71e6cbe5f0e0087e8506110d1b2949c7389e54823bc9afe2d27a51d525041a" )
          ; ( "module_export/Turn.replace_message"
            , "5633886156490bf2730b683c0d1b84caf9b042eb2328f9951a7e28a161503c95" )
          ; ( "module_export/Turn.replace_or_append"
            , "651a79c090b634f976acac8c3254a1a2646e8232bb87cd2bbd57f774ff36f31d" )
          ] )
      ; ( "delegated_moderator_v1"
        , "9b3f9b1a58354d5b83fc679e1fc38f2e6e9f1a494574f2787e8c2a5c41115c2f"
        , [ ( "module/Log"
            , "8bf27bc541206797209a5b21e7249350da83a72476de50ee7478daac9df9acd4" )
          ; ( "module_export/Log.debug"
            , "97cd8c2451cc7ff7769ac1738e34bd96fe335d4f7bbef430dd8f22786e819e4a" )
          ; ( "module_export/Log.error"
            , "8069257dca202f0519e790fb172bbd4ee7bad8bb252da5b691e34f058c8d8c85" )
          ; ( "module_export/Log.info"
            , "eb5b144a9a214c179d1190ab040d7e2f3326da290fca264cee53fcc3b20e2d98" )
          ; ( "module_export/Log.warn"
            , "939dc46770f12bb4f2063b15fd2d33ae3b4c227584a1c1484588e824a8e9e695" )
          ; ( "module/Tool"
            , "5ec4ff4a683f610c500d9bf1f545c8b64bef440db73ac5c034518d1c75d81598" )
          ; ( "module_export/Tool.approve"
            , "3b167c46c3c5c2e29dd5ff4560c8772bfe39dfa8c104f305757fbf1a47bd81ec" )
          ; ( "module_export/Tool.call"
            , "c347e870e6d35855b3e42ae3b16ec66591f36d34429242685205c8aba4e104b1" )
          ; ( "module_export/Tool.redirect"
            , "6cc1741d5d023c8a30a1b0a3969636bedcf68f3856e89e69841bb9ef0758a132" )
          ; ( "module_export/Tool.reject"
            , "750429786a525fa4af39e96dfc60cd329fba459a051d92a306676ed72abee88d" )
          ; ( "module_export/Tool.rewrite_args"
            , "68dfa500efdcf1cb6472744135131dd2c1fbb52c799ff47cafc295d0b917e76e" )
          ; ( "module_export/Tool.spawn"
            , "621ed2316b5b0de8784c77df4b41e28fd2f965e57d06686b4eba2a91c31cb6fd" )
          ; ( "module/Turn"
            , "e0a0406931a6c5156b39ca4e43ab61584afc884027deec39c42002c3345b8d92" )
          ; ( "module_export/Turn.append_item"
            , "18d4572aab87f9e0feba71838765a23c5431bf458fb30079fc79c41fd9d69338" )
          ; ( "module_export/Turn.append_message"
            , "3cc870b69e2354930f1405b31c962906ee28c992a124e4169d63456231f0db4f" )
          ; ( "module_export/Turn.append_notice"
            , "cc60b629572b98834e1840b0b0c1ff804d6a5c369bb894cf37b8e4f508f3e521" )
          ; ( "module_export/Turn.delete_item"
            , "0ae6c222113ac17a28dd6ffb922d60ee099bf4b59540954be0ffdf5652c6cf56" )
          ; ( "module_export/Turn.delete_message"
            , "33b5980ac24671ed8579744a1182ab1ae283af8672d575891909dc4014238bee" )
          ; ( "module_export/Turn.halt"
            , "de729f2623e216695a698fea0824caf099c988a52609e0fd493048a8e42a386c" )
          ; ( "module_export/Turn.prepend_system"
            , "ec1f368da810fec03b8472c9f2072378693be1b8a14c4183b6e68395e12988a2" )
          ; ( "module_export/Turn.replace_item"
            , "312bb13705e54826e92893c00e4e0657e47f13cb42bcbdc4d4093b15022c0591" )
          ; ( "module_export/Turn.replace_message"
            , "85c8b59da80adad4afdea4342e9a2bd7522dc066f908160c09601fe540045501" )
          ; ( "module_export/Turn.replace_or_append"
            , "fb1571be72deaa741c6067efa802818da46fb907a685ddd55f4a9ed3ea1916b5" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "runtime.effects"
          ; topic_closure_sha256
          ; evidence =
              [ "test/agent_docs/docs_chatml_effects.ml"
              ; "test/chatml_composition/moderator_job_tests.ml"
              ; "test/moderation/chat_response_moderator_manager_test.ml"
              ; "lib/chatml/chatml_extension_surface.ml"
              ; "lib/chat_response/in_memory_stream.ml"
              ]
          }))
  ;;

  let runtime_control_mappings =
    List.concat_map
      [ ( "moderator_v1"
        , "7d4949c806d2d70eb45864625b67a1fd2b7415ae79101ee8aaeaa4a6aa2822ae"
        , [ ( "module/Runtime"
            , "6a03d3ede904c05bd4e6e423ca255efaff8116367688886a061f782e020c3ef2" )
          ; ( "module_export/Runtime.emit"
            , "daa2033d20e7d0534cf8d2b7d8b4dd10d0daa2e2adc7734e034d191b96aa9723" )
          ; ( "module_export/Runtime.end_session"
            , "694178f8d868f0ceb3737e6a096d1674cba3ada6cf75a906ec4a815e1876bc0d" )
          ; ( "module_export/Runtime.request_compaction"
            , "d45ae1e44df12e9fad4e5c9807ce98572332b02f1845bc1a7f2ed50667cc6ab2" )
          ; ( "module_export/Runtime.request_turn"
            , "a37589db5b7a7a48d588350931c83adcc61d5ee17a90f1a6bec06ff98bc6c236" )
          ; ( "module/Model"
            , "0f098eb624aee2eedf45c042ce387b1bd713878a2927601d3790cca80e47a5af" )
          ; ( "module_export/Model.call"
            , "5ed1650ec28411890a3f6bf50e98a6a256eb3113975bf13e46e125c72ab429ca" )
          ; ( "module_export/Model.call_json"
            , "9b995c20470a5044c324fc21eaa2ddb9d01312b7c9b14e315bba56116cac1ceb" )
          ; ( "module_export/Model.call_text"
            , "90f1bb3f301520961d4dbc49a68bbde2e78645d7c75090d9757a38606276fe59" )
          ; ( "module_export/Model.spawn"
            , "766ca8c7eb642bbfbd70d7ca91bccce6ed7d754634b067a9d1e8b9ef1e64e83a" )
          ; ( "module_export/Model.spawn_text"
            , "24c8d7a071c73bedee083896630ae2f66133b11038f4b51edcbbc2bd0475022b" )
          ; ( "module/Process"
            , "ee99fe4464f8a9f477b9fa1f9aaf14bf5ce34d385f48ef0caf5867534a48c147" )
          ; ( "module_export/Process.run"
            , "883ed8b59789ef2950d08c978b439005ba5546d52d64908cf9bff26af355233d" )
          ] )
      ; ( "delegated_moderator_v1"
        , "728d9784d6bb4ac1eaf1dfba8cb92e72d9ecb1ece3d8a5236ea7341ad6c802d7"
        , [ ( "module/Runtime"
            , "a7f003fdab4a960c0fe55073944b63aebe21775ba44e9be5fdf0412d3833715e" )
          ; ( "module_export/Runtime.emit"
            , "205cb7e9d897b93cdc31323f649dbec18379e51fc7f7f0b7bf40552b2c568b5c" )
          ; ( "module_export/Runtime.end_session"
            , "ceb2dc7028797b8085b304b2ae973b7a289cc1f4136d77cf46bdd6324c8293e3" )
          ; ( "module_export/Runtime.request_compaction"
            , "b9ae3ba4dabb42e0bafd70318872d1c715efa2ecf97102bc367675b98fc3d513" )
          ; ( "module_export/Runtime.request_turn"
            , "3be8ccc22985f8d9f3fb1890a523fa89c55553fffe5bea83c00839b180ecaabf" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "runtime.control"
          ; topic_closure_sha256
          ; evidence =
              [ "test/agent_docs/docs_chatml_control.ml"
              ; "test/moderation/moderator_native_requests_test.ml"
              ; "lib/chat_response/model_executor.ml"
              ; "lib/shell_runtime/moderator_process_adapter.ml"
              ; "lib/agent_session/runtime_builder.ml"
              ; "lib/agent_server/session_factory.ml"
              ]
          }))
  ;;

  let run_mappings =
    List.concat_map
      [ ( "moderator_v1"
        , "2b9c89350722462144c41eae573f49a0790e0c02d7585d7e653939cbe1893642"
        , [ ( "module/Run"
            , "6eb6e335f86f30019f0bf3f7ff05141fbe26e7c95312a31501ab52bc016ef0e0" )
          ; ( "module_export/Run.continue"
            , "6070b540e55194d30941ee63b640d5d4e97d7d2a5a4cd0b906bbb4549059d174" )
          ; ( "module_export/Run.finish"
            , "690ac081a282adc4864fae6f8dc255eee65c15403fe9921aba0868b7509a984e" )
          ; ( "module_export/Run.wait"
            , "79322e34f129f854d870a6f52dd729f64afabd8255eb83e9642ccd2ee8b1dc23" )
          ] )
      ; ( "delegated_moderator_v1"
        , "c442cbe30513372fff67ae9feac7bc9ed153407353c193bebbafcc0d6257c267"
        , [ ( "module/Run"
            , "845fbe661e06d288d0974a92ecb3b14cde3918f2d126dfb0f4b8cc7c49de919b" )
          ; ( "module_export/Run.continue"
            , "9f4043518eb71e5e299067468acc5f7b65d1e71420ee47af0f275804058d5b56" )
          ; ( "module_export/Run.finish"
            , "75be2d80ecfb25babf6b35a41490f0c1c5d0ceb704441893081a23c1cfd7994e" )
          ; ( "module_export/Run.wait"
            , "6640a2cf1e6e5c5391523b16d693ae7e4be7bfd3e36f32fd89291475b39b8690" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "runtime.runs"
          ; topic_closure_sha256
          ; evidence =
              [ "test/agent_session/run_action_scope_tests.ml"
              ; "test/run_lifecycle_protocol_test.ml"
              ; "lib/chat_response/run_operations.ml"
              ; "lib/agent_session/run_action_service.ml"
              ]
          }))
  ;;

  let background_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "runtime.jobs.owned"
        , "6a63223ee7ca630bb446d2bfbb8d546ee6e3cc7129b0ad45ae8c652a136312c9"
        , [ "test/chatml_composition/job_launch_tests.ml"
          ; "test/chatml_composition/moderator_job_tests.ml"
          ; "test/chatml_composition/background_artifact_tests.ml"
          ; "lib/chat_response/background_job_operations.ml"
          ; "lib/agent_session/script_job_service.ml"
          ]
        , [ ( "module/Job"
            , "2aa8df304b64f5a737a322b36174a138731209957b503feb185e26417c8e7757" )
          ; ( "module_export/Job.cancel"
            , "9f8be3ad789fea8ec33c1d8ea41c2e8322c4fdfef5d7110360020712c9cda402" )
          ; ( "module_export/Job.get"
            , "9108f1e26c12ad792614d4f740a91fbc4142fac71f18eefd367375a86e2c81e4" )
          ; ( "module_export/Job.read_result"
            , "c999ef32a4b192e98545693df59c6fa48c25833bb095dc2956db378b9fbb8b39" )
          ; ( "module_export/Job.start_script"
            , "cf9106a40b074c61667ff26a864c8e4133b8391bbd043aab50212a3e43860252" )
          ; ( "module_export/Job.start_tool"
            , "4d70acf85b7b6fae8ce3593698ef326e276de65e5f43124dcc39488a35b7ef79" )
          ] )
      ; ( "tool_v1"
        , "runtime.jobs.owned"
        , "18d5a331f97cb694a68817d7ab1f7e8abafeadcf12125fe3980fb0dbfdfc721c"
        , [ "test/chatml_composition/job_launch_tests.ml"
          ; "test/chatml_composition/moderator_job_tests.ml"
          ; "test/chatml_composition/background_artifact_tests.ml"
          ; "lib/chat_response/background_job_operations.ml"
          ; "lib/agent_session/script_job_service.ml"
          ]
        , [ ( "module/Job"
            , "f26d78ea646b5556803eedbbb06d68a529769d95fac4faee537bd0dba2206649" )
          ; ( "module_export/Job.cancel"
            , "882a3ca8d82cd0506fe3839d6e4234837da73cea1543ed21598570497d10a299" )
          ; ( "module_export/Job.get"
            , "cb582c7df655a266a0b5c9f3e1b7d7eedb27bf0d76cc0267bcc9558e8d699b27" )
          ; ( "module_export/Job.read_result"
            , "25fa68abbd9cc4ffcef2fc1fc2f6b8727d7e7267c3895d37d52e058f69b6fe87" )
          ; ( "module_export/Job.start_script"
            , "88dd6f2bc9fb2e18318d25316ccdea91c73885ddfd146eca04e870554132f229" )
          ; ( "module_export/Job.start_tool"
            , "e2065743ebf221494181a3767f19dd7f5da83c46a926ef2f6ed66feddb3cca4b" )
          ] )
      ; ( "moderator_v1"
        , "runtime.jobs.owned"
        , "8e881cf173793f560f97242317d8dd71013b4e36b005c7a165315eebdfa68a09"
        , [ "test/chatml_composition/job_launch_tests.ml"
          ; "test/chatml_composition/moderator_job_tests.ml"
          ; "test/chatml_composition/background_artifact_tests.ml"
          ; "lib/chat_response/background_job_operations.ml"
          ; "lib/agent_session/script_job_service.ml"
          ]
        , [ ( "module/Job"
            , "e608006da494140ea66266be3e97a46c945dd142304a339941d6052fe4d5d906" )
          ; ( "module_export/Job.cancel"
            , "12d176b2d5579055b15370555061bff6a6b8c6c7a8f62f903139d9c7d2548349" )
          ; ( "module_export/Job.get"
            , "bc33db6adb893700cd45fca0c4d7246fb59d2fdbfeef6b85659095fe051134f0" )
          ; ( "module_export/Job.read_result"
            , "37f2d7854e029a5a2d9a619e8931337b5661442c249c982e2122180ea9edc4cd" )
          ; ( "module_export/Job.start_script"
            , "3379d9ec6f41d83323b5ee9dadbc8daf4f47aa84a74aad59868c9548deac135f" )
          ; ( "module_export/Job.start_tool"
            , "0f7a6668a8807ed6524df525a9215ab58c2b3f323bdba5f59f6dac08beb908b2" )
          ] )
      ; ( "delegated_moderator_v1"
        , "runtime.jobs.owned"
        , "9808375f654f660d542bcf30bea4a774a6a059fdccc048ca16368bf651ed205d"
        , [ "test/chatml_composition/job_launch_tests.ml"
          ; "test/chatml_composition/moderator_job_tests.ml"
          ; "test/chatml_composition/background_artifact_tests.ml"
          ; "lib/chat_response/background_job_operations.ml"
          ; "lib/agent_session/script_job_service.ml"
          ]
        , [ ( "module/Job"
            , "da15259a7eb3cf0b0926c8e447d83e2c782253a7fa4b721222f5994e018f3b83" )
          ; ( "module_export/Job.cancel"
            , "98a6f5b4a549cb0393952d26cb52c5669105a88bc6791e89042f22994a94a3d3" )
          ; ( "module_export/Job.get"
            , "41ed0ee3d6b4c05eb1ea93c579aee8866a0f51117791aa86d6989ba358c1c336" )
          ; ( "module_export/Job.read_result"
            , "77d880fe092e31f9ed64da1317f1f9593a264836b1fa9c0936f6b08d3a766dc7" )
          ; ( "module_export/Job.start_script"
            , "2c8fbcbefe62ab8311e43bf22554b4daa2067081945c77599961daa8a625bc07" )
          ; ( "module_export/Job.start_tool"
            , "0de57f29a7e05d218d2ca16906f6032887af243e58ffaea4c6cf81486faf86cb" )
          ] )
      ; ( "moderator_v1"
        , "runtime.jobs.subscriptions"
        , "e148a7b0663855c25344a50f272c572fb0bc92281332e25d6e11fc9374e06ff3"
        , [ "test/chatml_composition/subscription_tests.ml"
          ; "test/chatml_composition/timer_tests.ml"
          ; "test/moderation/subscription_transaction_tests.ml"
          ; "lib/chat_response/subscription_operations.ml"
          ]
        , [ ( "module/Subscription"
            , "52316cd512ca3c9743d6a83013e7e26832c1e0e03825d4959db1160d84203db1" )
          ; ( "module_export/Subscription.arm"
            , "26fdc314a780f512c8309cc5e4156b380f8a89c00a318967e006919c339fd05d" )
          ; ( "module_export/Subscription.cancel"
            , "6bbf3cb608978def2b3da1ab1e92dd630c638e923da20108ed66618133d28711" )
          ; ( "module_export/Subscription.complete"
            , "e081db89d96d4defd330cbc59a748f60d6c7798132ebc7b3552a8a0f48868352" )
          ; ( "module_export/Subscription.create"
            , "b70822a0e8a079005b887ba6d22478740156e9b770471fd20f96e120173b42f5" )
          ; ( "module_export/Subscription.fail"
            , "b0eaeec54b68deee3339c2295c80c0389766d2985f1efaabb031679b487acd27" )
          ; ( "module_export/Subscription.get"
            , "75d766ce25282d36f8c414ac6c34252d06694518f3e099df86c3905bd486ef6b" )
          ] )
      ; ( "delegated_moderator_v1"
        , "runtime.jobs.subscriptions"
        , "72f36b078824174c7e1d26fbcc2fff7161a03760465f29745ee7245286746543"
        , [ "test/chatml_composition/subscription_tests.ml"
          ; "test/chatml_composition/timer_tests.ml"
          ; "test/moderation/subscription_transaction_tests.ml"
          ; "lib/chat_response/subscription_operations.ml"
          ]
        , [ ( "module/Subscription"
            , "6a9a5b13b50b1e046f2a112657fae92568ea320808b4f93a706a1c9e743fdd32" )
          ; ( "module_export/Subscription.arm"
            , "e56d0769e36b0522e8d6f48b1f39e16a9981691b5e105c700afee26488d59aaa" )
          ; ( "module_export/Subscription.cancel"
            , "3fe4f883492def1be2e156a55ce573c198b0a57b0d56a79a9b63a923a2410d44" )
          ; ( "module_export/Subscription.complete"
            , "f97a211295dbc777f8690f0623bb46db6cb1d44c6d3c3671ab51d15b909d41ee" )
          ; ( "module_export/Subscription.create"
            , "fe88cdbbd369dd31aa13d4378a6a6ceceffcdfe5a960fecc68903ea8a80088b3" )
          ; ( "module_export/Subscription.fail"
            , "5a995ab2f1f57df67f14031493ad3b7f171dcf95fba7fe4740f001ecc8c84139" )
          ; ( "module_export/Subscription.get"
            , "b60d55664bbd83ce14a3e1480d0e8e33343df55bed2df01c9f80c097dc0d0f28" )
          ] )
      ; ( "moderator_v1"
        , "runtime.jobs.timers"
        , "358ca8e869d14d96f7781486c087ec6a0a7642018698cfda1a2749e668699fef"
        , [ "test/chatml_composition/timer_tests.ml"
          ; "lib/chat_response/schedule_operations.ml"
          ; "lib/agent_session/script_schedule_service.ml"
          ]
        , [ ( "module/Schedule"
            , "80f3738fc56c9c565a090f9a923b7bd327a56ca40b2f6d96ff6dfaeb4e0e74ab" )
          ; ( "module_export/Schedule.after_ms"
            , "ac204e39264cd9e750afe6acbb1ac69c469cbaf665e3406b6158e4fcca3cd3a0" )
          ; ( "module_export/Schedule.after_ms_with_policy"
            , "d0e7d7146b9f3c2db5b857062e3ff3971545da767cd9076c5a90a61c58658ba4" )
          ; ( "module_export/Schedule.cancel"
            , "24885cdfb870bbe2d2b6434d5a9dff673beb1168e1f1ab3347c61245770acb26" )
          ; ( "module_export/Schedule.get"
            , "f70c8e74b3c7921786d80b1eac721896667c4b7df6552e84153cf99a4febf31d" )
          ] )
      ; ( "delegated_moderator_v1"
        , "runtime.jobs.timers"
        , "2d295c1416a8e300b697f3c475f74861572b481367c6fffb933e112682e6a11d"
        , [ "test/chatml_composition/timer_tests.ml"
          ; "lib/chat_response/schedule_operations.ml"
          ; "lib/agent_session/script_schedule_service.ml"
          ]
        , [ ( "module/Schedule"
            , "4840e3965fbd2a5f1ed97b91822fdff20e7ebc840326177e4349d44b6a706f4c" )
          ; ( "module_export/Schedule.after_ms"
            , "59feff961ac734b6147c9fb4b4274f937c68485cab66cdc78cffc1b8e7f2cf75" )
          ; ( "module_export/Schedule.after_ms_with_policy"
            , "0b5383fe04cc0e1a02119985bef96e6716db174f268581cd635bc6986aabb9b7" )
          ; ( "module_export/Schedule.cancel"
            , "aeb8f1d2083955b082943b5bb0ebde5780d368a13bf43c63cf53bd86d2a6b5f1" )
          ; ( "module_export/Schedule.get"
            , "01162e5dcdbf664a21d99278656d3db2690504ea7ea37f9bd5677680033e5f25" )
          ] )
      ; ( "moderator_v1"
        , "runtime.delivery.notifications"
        , "563221957cf4435a3e2ec32a969ab6b335767d89d10411eacbe7a6f3e9b829bc"
        , [ "test/chatml_composition/notification_admission_tests.ml"
          ; "test/chatml_composition/notification_wake_tests.ml"
          ; "test/moderation/notification_transaction_tests.ml"
          ; "lib/chat_response/notification_operations.ml"
          ]
        , [ ( "module/Notification"
            , "60777c4d4ba53251cbd7a70e4acad833945646e2df8fecd1e3a9496adb998a2a" )
          ; ( "module_export/Notification.get"
            , "4e255705325eb8d3bd337ed4655b215c29732b0f6353a6fc6520d3dff186fa43" )
          ; ( "module_export/Notification.publish"
            , "75816d11e03dc61780f663f75f3103c2cbc82bc3448a012a66d5a07666bbd001" )
          ] )
      ; ( "delegated_moderator_v1"
        , "runtime.delivery.notifications"
        , "9b13892a4dad1b69bbcebacbae85eb40ed439270ff34c00844f204da888f201b"
        , [ "test/chatml_composition/notification_admission_tests.ml"
          ; "test/chatml_composition/notification_wake_tests.ml"
          ; "test/moderation/notification_transaction_tests.ml"
          ; "lib/chat_response/notification_operations.ml"
          ]
        , [ ( "module/Notification"
            , "78ac3dc103b756aff41f21bcf62aacc067fa4d7f87bbb0569d72e28b3eb294a4" )
          ; ( "module_export/Notification.get"
            , "61898fffb86f244c0823ab959224a27ef62374f5a684fc98fcff249ecee22cd5" )
          ; ( "module_export/Notification.publish"
            , "cbaa39e372093f7dda18bdd638dd1e346a9207a8487a4422f21880222937f207" )
          ] )
      ; ( "moderator_v1"
        , "runtime.delivery.ingress"
        , "1d7dac24b1f14b518d3fdbb559830c0bb6ca5b1213adde2bd3e64fcfb6cc4c2a"
        , [ "test/chatml_composition/ingress_tests.ml"
          ; "test/chatml_composition/ingress_socket_tests.ml"
          ; "lib/chat_response/ingress_operations.ml"
          ; "lib/agent_session/script_ingress_service.ml"
          ]
        , [ ( "module/Ingress"
            , "1435e0e85966a47f7b8723f72a68dec378154ef1c9321b165ce79de5ab7447d2" )
          ; ( "module_export/Ingress.get"
            , "b3f59b1347da17e2e1963626375cef3bcf9abdde7f16d13e854ab7a243c3d0b6" )
          ; ( "module_export/Ingress.register"
            , "1e9f6523f5499be49ff6f3d9d5e5b0ab118b43e037cf2c9b828a3a5743345491" )
          ; ( "module_export/Ingress.revoke"
            , "df9988cdd69e804585bc5b213308fd6e394ef9881db6712c077950a15c21ac53" )
          ] )
      ; ( "delegated_moderator_v1"
        , "runtime.delivery.ingress"
        , "58eefe48d7131f17be5fb7fb5de68ae233c712d8c9bb7366f221f752151bc0d1"
        , [ "test/chatml_composition/ingress_tests.ml"
          ; "test/chatml_composition/ingress_socket_tests.ml"
          ; "lib/chat_response/ingress_operations.ml"
          ; "lib/agent_session/script_ingress_service.ml"
          ]
        , [ ( "module/Ingress"
            , "d2923f8b23db1aff9a7f9db802fb88da9628d6059e4d958c65af159c82bd4abd" )
          ; ( "module_export/Ingress.get"
            , "01af797d4adff493b49dd3df175f2aac531be225aeb45bc711f1fa11977fb860" )
          ; ( "module_export/Ingress.register"
            , "ef76fa3a3efa22e8b77a1b78aa35909739a51bcf1fc6dacb81fc750ad71c2d32" )
          ; ( "module_export/Ingress.revoke"
            , "6dd4f33a930d8705786a63235040520382cba5c4cc8277bdfa9f8443916beb37" )
          ] )
      ; ( "tool_v1"
        , "runtime.work-values"
        , "e487025eacee4350bd5950edb733cc5d66f48578fb418380a98ba339e5433ade"
        , [ "test/agent_docs/docs_chatml_authoring.ml"
          ; "test/chatml_composition/job_launch_tests.ml"
          ; "lib/chatml/chatml_extension_surface.ml"
          ]
        , [ ( "type_alias/work_ref"
            , "f63a75db469864cc86c2bd6959c8926e3422f7ecdd243bda7b5972203e4777bf" )
          ] )
      ; ( "moderator_v1"
        , "runtime.work-values"
        , "3d8e23c8979e76406882f01e5a6962ab547af2d565725652c3ce86e1abca30d2"
        , [ "test/agent_docs/docs_chatml_authoring.ml"
          ; "test/chatml_composition/job_launch_tests.ml"
          ; "lib/chatml/chatml_extension_surface.ml"
          ]
        , [ ( "type_alias/work_ref"
            , "4edc1aad1ed5984ccbfe3a72a859f362c2ad26d147947e16caaeb41291326495" )
          ; ( "type_alias/completion"
            , "25ceb0c00efe4af3f14c6a579ee7babe56009078724b4b14bec24f2e8a9cd3d3" )
          ; ( "type_alias/wake_policy"
            , "1078708e3baf49ac8de2361e82aed8e658f9c2b5f8a15637e13330b915c8ce88" )
          ; ( "type_alias/schedule_misfire"
            , "4aa25f3d514cf3e70d1de93e6687a12c51b4b9b0e7ca18beb463e1cb11a564d6" )
          ; ( "type_alias/notification_correlation"
            , "d4ecac5742f3659a5bd50bfd7f734dd298b57088bba81ad1b6eb3e6aec886f9f" )
          ; ( "type_alias/work_completion"
            , "174d445457b64033dd079c197ef50eb2acfa69eb93f27a17c4c438472722bba1" )
          ] )
      ; ( "delegated_moderator_v1"
        , "runtime.work-values"
        , "7841e8fe97c81820f9d7da8b3f58c51c3b46065fe1bf90c06a603610a71a3db7"
        , [ "test/agent_docs/docs_chatml_authoring.ml"
          ; "test/chatml_composition/job_launch_tests.ml"
          ; "lib/chatml/chatml_extension_surface.ml"
          ]
        , [ ( "type_alias/work_ref"
            , "b3a055128a835cf98a1b42148754b48d3e241eae6d1ff11294bc27a74e40a59b" )
          ; ( "type_alias/completion"
            , "9bdacde70dbc4145323166b927a62d40b3a5c380660d311a4d8ea312e50c37f6" )
          ; ( "type_alias/wake_policy"
            , "15efd28568eb0c0956e9454f8a6202fbba728b619f519d1358ad1e9bb528027f" )
          ; ( "type_alias/schedule_misfire"
            , "ce422a4a58d0af48f9b9c1fffdef0ff86e2d1e821839eeeb7edb92b38884672c" )
          ; ( "type_alias/notification_correlation"
            , "e39db05f9f9e194715b00c951c71eb2729db2932bc1fdaa3d905599a3df2416e" )
          ; ( "type_alias/work_completion"
            , "bb3b7e4bf29a417e257f9498373bd5fad7a9d930fef7207f5ce2851ede34fa10" )
          ] )
      ]
      ~f:(fun (surface_id, topic_id, topic_closure_sha256, evidence, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id
          ; topic_closure_sha256
          ; evidence
          }))
  ;;

  let invocation_context_mappings =
    List.concat_map
      [ ( "moderator_v1"
        , "ed65c3e86d0167c15fca03b216bf77618c730463f4b9c6210291a43943bb449d"
        , [ ( "module/Invocation"
            , "534a62f3a88e211fb3c1ace77f59a5f33ea3634dfc9be30fcee7f7b636b1114c" )
          ; ( "module_export/Invocation.resolve"
            , "703ac84ccfb5341b0985c02d779f9fabee3960ebfba7c52735741b2b8c331625" )
          ; ( "type_alias/tool_context"
            , "817f31ad844b776eee76ecf23c3a38cf18e6b201acf058b75a18d4b2061590d8" )
          ; ( "type_alias/tool_capability"
            , "83e52c8c67bf334a11341fb019b435ae14ce33e16e4a0c45c4e89f39d0dc4457" )
          ; ( "type_alias/tool_limits"
            , "8c79cbc7bb5df7b12772f8ee38c2265f48ba79746a5f347a77e28f55219dd058" )
          ; ( "type_alias/tool_error"
            , "36a67257d779a5a74200cbf65d9c5294f88615a0d891bcdd1b4b911416c90083" )
          ; ( "type_alias/tool_outcome"
            , "99899d0f269ae7346e73011ad99a93aae1877d061f4afd572b75cb1fbac1e1d2" )
          ; ( "type_alias/tool_invocation"
            , "4a66e84669a4db1293f265318afcc7166fc56f83aef2ea969e10371f1b1ec0e2" )
          ; ( "type_alias/moderator_event"
            , "7702861302d82a452c7515297728f81f20ce2ac6a62a1d2e5062f05bc1f94c64" )
          ] )
      ; ( "delegated_moderator_v1"
        , "afb4c583227bd379469a524e5f64fee58db1fd1900a96de4aeedd12c27de742e"
        , [ ( "module/Invocation"
            , "6ffc1b9ef3a698ebfcccfa15fc29eb607d7dc42f4df3ad2897be1668b6e306fe" )
          ; ( "module_export/Invocation.resolve"
            , "862de3e2961180623847cc272f59d0f292c6463bac11f97a96eab96258c365d3" )
          ; ( "type_alias/tool_context"
            , "d43d9c009ae1e8314bcd5a49ed4e7781cdc0d97a3c2f0c813924791367eb4438" )
          ; ( "type_alias/tool_capability"
            , "d67cc7b192ecebfdacbc30704f702515195313396f762a57f628b0aa83ffa460" )
          ; ( "type_alias/tool_limits"
            , "f87e2c8a5f21375c0c065784b55702b61ce40048f2b9788f9c98f43f199a9f3e" )
          ; ( "type_alias/tool_error"
            , "990077f9a806818485eaa1b47aeeb33c2c6f3e17fc80535428a0cfd48e2b12dd" )
          ; ( "type_alias/tool_outcome"
            , "2c112aa0e5acb882da3ee04bfa230b73a21b7d80b7ccd2c82f656e79713c4af4" )
          ; ( "type_alias/tool_invocation"
            , "54d89d184e764a0d952e1a2c4ef9d847b9edb55e2c64d723f93871b59db04e24" )
          ; ( "type_alias/moderator_event"
            , "8ebdf669114ca7f326ff28b7147fd61dcbee49a8e28d33900df326fc66a05291" )
          ] )
      ; ( "tool_v1"
        , "a5628eb6641d6bd5189ac0cfb02af68db390508f43da0898084d29dead57f2a9"
        , [ ( "type_alias/tool_context"
            , "e39d59aecc0de098fe9396e57ce61ea41c8ab2056e4f78cadf1945428d09eb8b" )
          ; ( "type_alias/tool_capability"
            , "c01e327df808b40080c2b3982f7114f7b9b6a242774f1903e0f015169f884044" )
          ; ( "type_alias/tool_limits"
            , "6c95c31f06f4f47b7a1bc30bc0ea38642e42a5b6f0c9a30e3926e7aa6a8c845c" )
          ; ( "type_alias/tool_error"
            , "7833727f6bf5233f1e884c047c02173710f5ad20746cff57ed46df8d8c2320df" )
          ; ( "type_alias/tool_outcome"
            , "bcf8eb5d5098b9d8738a746272d6c7c1b0770932e8330750b042591a3fe9a8e8" )
          ] )
      ]
      ~f:(fun (surface_id, topic_closure_sha256, contracts) ->
        List.map contracts ~f:(fun (name, contract_sha256) ->
          { target_id = surface_id ^ "/" ^ name
          ; contract_sha256
          ; topic_id = "runtime.invocation-context"
          ; topic_closure_sha256
          ; evidence =
              [ "test/chatml_composition/standalone_tests.ml"
              ; "test/moderation/moderator_invocation_test.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ; "lib/chat_response/moderator_invocation.ml"
              ; "lib/chat_response/moderator_manager.ml"
              ; "lib/chatml/chatml_extension_surface.ml"
              ]
          }))
  ;;

  let reviewed_mappings =
    entrypoint_mappings
    @ task_mappings
    @ string_mappings
    @ array_mappings
    @ option_mappings
    @ outcome_mappings
    @ json_mappings
    @ hashtbl_mappings
    @ global_mappings
    @ json_alias_mappings
    @ moderator_data_mappings
    @ host_effect_mappings
    @ runtime_control_mappings
    @ run_mappings
    @ background_mappings
    @ invocation_context_mappings
  ;;
end

let language_foundation ~sources =
  let make ?(path = "guide/chatml-ocaml-differences.md") id title prerequisites sections =
    { id
    ; title
    ; prerequisites
    ; surfaces = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
    ; excerpts =
        List.map sections ~f:(fun (heading, _) ->
          { path; heading; include_children = false })
    ; review =
        Audited
          { excerpt_sha256 = List.map sections ~f:snd
          ; evidence =
              [ "test/agent_docs/docs_chatml_authoring.ml"
              ; "test/chatml_typechecker_test.ml"
              ; "lib/chatml/chatml_parser.mly"
              ; "lib/chatml/chatml_builtin_spec.ml"
              ]
          }
    }
  in
  create
    ~sources
    [ make
        "chatml.introduction"
        "ChatML identity and example contracts"
        []
        [ ( "# ChatML differences from OCaml"
          , "0b4293c4b5abdd44a0683f87258d7c4e26031b7ea17096a878f60e8da93a8366" )
        ]
    ; make
        "chatml.syntax.calls"
        "Explicit calls and function arity"
        [ "chatml.introduction" ]
        [ ( "## Calls have explicit arity"
          , "c55da5935d7814b63011188393163116efa9bb58d52f34f8549f2fbed6a8d723" )
        ]
    ; make
        "chatml.syntax.containers"
        "Array, record and variant syntax"
        [ "chatml.syntax.calls" ]
        [ ( "## Arrays, records and variant payloads use different delimiters"
          , "0345daa287be67288a018781f0182ddf58bee9af6725a6f22e1cdc1a90830d52" )
        ]
    ; make
        "chatml.types"
        "Type differences, matching and mutation"
        [ "chatml.syntax.containers" ]
        [ ( "## Records are structural, with matching branch shapes"
          , "75361d7c3b760ec24648c5abe538450e2eb2b3ecc0b22ce7aa88940c80edcfa6" )
        ; ( "## Match coverage depends on the inferred type"
          , "ee17ae31fb3f1f32964b346de33d57fca0a67e6bc3b79ca570ce837a2ba14261" )
        ; ( "## Annotate bindings; declare recursive data explicitly"
          , "ddbb952ff1bc935d15c577ddfa7923f7a81a4446ec0f4292fde091b9d41fff24" )
        ; ( "## Mutation restricts polymorphism"
          , "58b4cc03f4dadf55ce7e147868765def1ed0594798bd2fde12d86978c44115f2" )
        ]
    ; make
        "chatml.modules"
        "Module exports and qualified access"
        [ "chatml.syntax.calls" ]
        [ ( "## Modules export their own declarations"
          , "27c564241f01715cf08abaf293e0cca8fda4c743f9a83d9ff7e9b252e965cddb" )
        ]
    ; make
        "chatml.operators"
        "ChatML operators and builtin conventions"
        [ "chatml.syntax.containers" ]
        [ ( "## Operators and builtins are ChatML's own API"
          , "a630f832c6d693026ac0c883b508ea681968459d2806394f8bdcdaeff299589e" )
        ]
    ; make
        "chatml.tasks"
        "Task composition, failures and JSON"
        [ "chatml.types"; "chatml.operators" ]
        [ ( "## Tasks are values; the host runs the returned task"
          , "3e9ec46d97c490b33b4894755df3d0addd8023e7ea95fab91c02bc6a6bfe1d04" )
        ]
    ; make
        ~path:"guide/chatml-authoring-language.md"
        "chatml.programs"
        "Writing programs: source, control flow, matching and structured data"
        [ "chatml.tasks"; "chatml.modules" ]
        [ ( "# Writing ChatML programs"
          , "6a630398a3512067a4bf364a12a27e4fcd7e66ca1b6aa3a908f1ba6ec9a6f3b2" )
        ; ( "## Source text and operators"
          , "301cfb8e0996f3c3736fc6986fd70fbcfea34559638f36041e70c7f17e860075" )
        ; ( "## Functions, loops and modules"
          , "bbb770d9c56624e6777728273b2fa9aff289ff1676104326f37d751a7b2bd3e1" )
        ; ( "## Matching and explicit data types"
          , "5271cae0e9f14c484cd89e166b9dfbba74ddcff277f294b1757e5ca4e67972cc" )
        ; ( "## Standard library for structured data"
          , "7096551fff9d2f77779a577d846c44e4bfecfcfa3ba8fad171e0160d3c7ad640" )
        ; ( "## Effects, errors and execution boundaries"
          , "00f02e2fb345cd50ba3f9ab75dc834f3d95e8f62895ce214faa63cd74d0d0f8a" )
        ]
    ; { id = "chatml.inference"
      ; title = "Type inference, annotations and structural contracts"
      ; prerequisites = [ "chatml.programs" ]
      ; surfaces = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
      ; excerpts =
          [ { path = "guide/chatml-inference.md"
            ; heading = "# Type inference and annotations"
            ; include_children = true
            }
          ]
      ; review =
          Audited
            { excerpt_sha256 =
                [ "241b73ddb7b5d03691cc20c644734cc4a92d3cea5cb0c4e76da1d347e1d4ede9" ]
            ; evidence =
                [ "lib/chatml/chatml_typechecker.ml"
                ; "test/chatml_typechecker_test.ml"
                ; "test/agent_docs/docs_chatml_authoring.ml"
                ]
            }
      }
    ; { id = "chatml.evaluation"
      ; title = "Lexical boundaries, evaluation order and failures"
      ; prerequisites = [ "chatml.inference" ]
      ; surfaces = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
      ; excerpts =
          [ { path = "guide/chatml-evaluation.md"
            ; heading = "# Source text and evaluation order"
            ; include_children = true
            }
          ]
      ; review =
          Audited
            { excerpt_sha256 =
                [ "ba2f407ab376c109d77391e135cce1e1752979c3a73c42750df8d8067ace09f2" ]
            ; evidence =
                [ "lib/chatml/chatml_lexer.mll"
                ; "lib/chatml/chatml_parser.mly"
                ; "lib/chatml/chatml_eval.ml"
                ; "test/chatml_diagnostics/diagnostic_invariants.ml"
                ; "test/chatml_owned_diagnostics/ownership_tests.ml"
                ; "test/chatml_owned_diagnostics/task_tests.ml"
                ; "test/chatml_owned_diagnostics/session_tests.ml"
                ; "test/chatml_linked_runtime/runtime_tests.ml"
                ; "test/chatml_linked_runtime/order_model_tests.ml"
                ; "test/agent_docs/docs_chatml_authoring.ml"
                ]
            }
      }
    ]
;;

let runtime_foundation ~sources =
  let open Result.Let_syntax in
  let%bind language = language_foundation ~sources in
  let shared = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ] in
  let managed = [ "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ] in
  let make id title surfaces prerequisites sections =
    { id
    ; title
    ; prerequisites
    ; surfaces
    ; excerpts =
        List.map sections ~f:(fun (heading, _) ->
          { path = "guide/chatml-authoring-runtime.md"
          ; heading
          ; include_children = false
          })
    ; review =
        Audited
          { excerpt_sha256 = List.map sections ~f:snd
          ; evidence =
              [ "test/agent_docs/docs_chatml_authoring.ml"
              ; "test/chatml_composition/one_off_tests.ml"
              ; "test/chatml_composition/standalone_tests.ml"
              ; "test/agent_server_restart_test.ml"
              ; "lib/chatml/chatml_extension_surface.mli"
              ; "lib/chat_response/moderator_invocation.mli"
              ; "lib/chat_response/managed_tool_registry.mli"
              ; "lib/chatmd_shell_spec/tool_schema.ml"
              ]
          }
    }
  in
  let runtime =
    [ { id = "chatmd.capabilities"
      ; title = "Root tool implementations, file roots, MCP and message resources"
      ; prerequisites = [ "chatmd.definitions" ]
      ; surfaces = managed
      ; excerpts =
          [ { path = "guide/chatmd-authoring-capabilities.md"
            ; heading = "# Authoring ChatMD capabilities and resource content"
            ; include_children = true
            }
          ]
      ; review =
          Audited
            { excerpt_sha256 =
                [ "467099991d41d1e93499aa3bdde4928cbd8c644b36af1f5fc2cec87f9926923d" ]
            ; evidence =
                [ "lib/chatmd/prompt.ml"
                ; "lib/chat_response/generated_admission.ml"
                ; "lib/chat_response/tool.ml"
                ; "test/agent_docs/docs_chatmd_capabilities.ml"
                ]
            }
      }
    ; { id = "chatmd.definitions"
      ; title = "Agent definitions, captured sources and inherited capabilities"
      ; prerequisites =
          [ "runtime.invocations.contracts"; "runtime.authority.tool-selection" ]
      ; surfaces = managed
      ; excerpts =
          [ { path = "guide/chatmd-authoring-definitions.md"
            ; heading = "# Authoring ChatMD agent definitions"
            ; include_children = true
            }
          ]
      ; review =
          Audited
            { excerpt_sha256 =
                [ "0309f7693f172a95000878f412e582acb7c73b6197fb02ec75c3281cdadfc8da" ]
            ; evidence =
                [ "test/agent_docs/docs_chatmd_authoring.ml"
                ; "lib/chatmd/prompt.ml"
                ; "lib/chat_response/generated_admission.ml"
                ; "lib/chatmd/chatmd_extension_declaration.ml"
                ]
            }
      }
    ; make
        "runtime.invocations.contracts"
        "Execution categories and entrypoints"
        shared
        [ "chatml.introduction" ]
        [ ( "# ChatML authoring: execution and invocation contracts"
          , "3bbd7f159ef6de27c1407146394e4fcd050d2f22c3a204dea31b391898657b03" )
        ; ( "## Choose the execution contract"
          , "f5a91ea0ac03dd83274f223c1f838975d53438b6b1a80319f4bd6981b930ff2f" )
        ]
    ; make
        "chatmd.declarations.schemas"
        "Extension bindings and schema dialect"
        managed
        [ "runtime.invocations.contracts"; "runtime.authority.tool-selection" ]
        [ ( "## Bind scripts and schemas in ChatMD"
          , "7202fa9d2c52f73204d61a26ca2267e526bf84b916c78144c1839095f4e60b88" )
        ]
    ; make
        "runtime.authority.tool-selection"
        "Selected tools and target authority"
        shared
        [ "runtime.invocations.contracts" ]
        [ ( "## Authority and target surfaces"
          , "0b13a35c7e4faf79b31e02be822c931b449893521651c5860681928ad742a635" )
        ]
    ; make
        "runtime.invocations.validation"
        "Static checks versus execution admission"
        shared
        [ "runtime.authority.tool-selection" ]
        [ ( "## Non-executing validation"
          , "ec87fc05c16551292822f460a8e051525b8c8762bc4bf7ed7e37dec87d6ea2d0" )
        ]
    ; make
        "runtime.invocations.one-off"
        "One-off tool-using computations"
        [ "one_off_v1" ]
        [ "runtime.invocations.contracts"
        ; "chatml.tasks"
        ; "runtime.authority.tool-selection"
        ; "runtime.invocations.validation"
        ]
        [ ( "## One-off tool-using computations"
          , "8f7045d961dee6784659981f6de4c218c0150aeedd1184422b91e2becf5608a5" )
        ]
    ; make
        "runtime.invocations.standalone"
        "Standalone tools and outcomes"
        [ "tool_v1" ]
        [ "runtime.invocations.contracts"
        ; "chatml.tasks"
        ; "chatmd.declarations.schemas"
        ; "runtime.invocations.validation"
        ]
        [ ( "## Standalone tools and explicit outcomes"
          , "1bcd783a8252f96a6352522370df2bdde262c8276b772a828dffc723315160ea" )
        ]
    ; make
        "runtime.invocations.moderator"
        "Moderator resolution and retained state"
        [ "moderator_v1"; "delegated_moderator_v1" ]
        [ "runtime.invocations.contracts"
        ; "chatml.tasks"
        ; "chatmd.declarations.schemas"
        ; "runtime.invocations.validation"
        ]
        [ ( "## Moderator tools and session-owned state"
          , "1c4d8b72701c7419b1eb39015c932a4b5aa603ed4b283b5c5851b0eeef63d563" )
        ]
    ]
  in
  let make_child id title prerequisites sections =
    { id
    ; title
    ; prerequisites
    ; surfaces = shared
    ; excerpts =
        List.map sections ~f:(fun (heading, _) ->
          { path = "guide/chatml-authoring-children.md"
          ; heading
          ; include_children = false
          })
    ; review =
        Audited
          { excerpt_sha256 = List.map sections ~f:snd
          ; evidence =
              [ "test/agent_docs/docs_child_authoring.ml"
              ; "test/authoring_validation_test.ml"
              ; "test/agent_server_generated_test.ml"
              ; "test/agent_server_generated_shell_test.ml"
              ; "test/agent_server_helper_test.ml"
              ; "lib/agent_session/generated_session_request.ml"
              ; "lib/agent_session/session_management.ml"
              ; "lib/agent_session/managed_session_service.mli"
              ; "lib/agent_session/native_tool_invocation.ml"
              ; "lib/agent_session/script_tool_calls.ml"
              ; "lib/chat_response/generated_admission.ml"
              ]
          }
    }
  in
  let children =
    [ make_child
        "runtime.delegation.generated"
        "Captured child definitions and static validation"
        [ "runtime.authority.tool-selection"
        ; "runtime.invocations.validation"
        ; "chatml.tasks"
        ]
        [ ( "# ChatML authoring: persisted child sessions"
          , "928f6305d1a1276069889a94f78439cb75b1a20e1ca24e8c32ed1bdfd7fb67a5" )
        ; ( "## Capture and validate a generated definition"
          , "cd825e7dca6042c6bbeae0a1eebd3b037db196e5f8954e24f0ebf8a89a4984da" )
        ]
    ; make_child
        "runtime.delegation.creation"
        "Creation retries, lifetimes and inherited authority"
        [ "runtime.delegation.generated" ]
        [ ( "## Create, retry and retain authority"
          , "dc6b4d3d15cc186f4459c2a0b07462afa8bd6d4221d75c9b2f2cab3e2f591610" )
        ]
    ; make_child
        "runtime.delegation.submissions"
        "Durable submissions and terminal receipt waits"
        [ "runtime.delegation.creation" ]
        [ ( "## Submit work and track completion"
          , "ae01ce63d8baeb96aa426655003c1ed82d864ae20583102414e28ef6952ea0b2" )
        ]
    ; make_child
        "runtime.delegation.output"
        "Output pages, fragments and cursor recovery"
        [ "runtime.delegation.submissions" ]
        [ ( "## Read output and recover cursors"
          , "2f85f7dc54b7b1deb4ec680f5254b6b86fe581bf5d3462008e56382f7d290873" )
        ]
    ; make_child
        "runtime.delegation.stop-helper"
        "Stop receipts and shared helper authority"
        [ "runtime.delegation.output" ]
        [ ( "## Stop and use the shared helper path"
          , "df76e24e3c872f2f6ebb04dec7b2a550ead1ea92b338caa3ae2473a754250c08" )
        ]
    ]
  in
  let moderators = [ "moderator_v1"; "delegated_moderator_v1" ] in
  let make_background id title surfaces prerequisites sections =
    { id
    ; title
    ; prerequisites = "runtime.work-values" :: prerequisites
    ; surfaces
    ; excerpts =
        List.map sections ~f:(fun (heading, _) ->
          { path = "guide/chatml-authoring-background.md"
          ; heading
          ; include_children = false
          })
    ; review =
        Audited
          { excerpt_sha256 = List.map sections ~f:snd
          ; evidence =
              [ "test/agent_docs/docs_chatml_authoring.ml"
              ; "test/chatml_composition/background_shell_tests.ml"
              ; "test/chatml_composition/ingress_socket_tests.ml"
              ; "lib/chatml/chatml_extension_surface.ml"
              ; "lib/chat_response/background_job_operations.mli"
              ; "lib/chat_response/background_delivery.ml"
              ; "lib/chat_response/schedule_delivery.ml"
              ; "lib/chat_response/ingress_delivery.ml"
              ; "lib/agent_session/script_subscription_service.mli"
              ; "lib/agent_session/script_notification_service.mli"
              ; "lib/agent_session/notification_delivery.mli"
              ; "lib/agent_protocol/subscription.mli"
              ]
          }
    }
  in
  let background =
    [ make_background
        "runtime.jobs.owned"
        "Owned tool/script jobs and terminal results"
        shared
        [ "chatml.tasks"
        ; "runtime.authority.tool-selection"
        ; "runtime.recovery.background"
        ]
        [ ( "# ChatML authoring: background work and delivery"
          , "9325ab62bf13da4e28435229745a845394a656faa4c55a74f17655c20adad49d" )
        ; ( "## Start and inspect owned jobs"
          , "85e40afc8b2384462aa77386aad3ae9b38af656639c3683acf72c902fa3e5674" )
        ]
    ; make_background
        "runtime.jobs.acknowledgement"
        "Pending acknowledgements and source-owned completion events"
        moderators
        [ "runtime.jobs.owned"; "runtime.invocations.moderator" ]
        [ ( "## Acknowledge before publishing a result"
          , "8464fb4c18ff6b012ea34334723ee1e6d7b124d6fc79e440ed174ea5d26a40e5" )
        ]
    ; make_background
        "runtime.jobs.shell-example"
        "Checked shell-backed asynchronous coordinator"
        moderators
        [ "runtime.delivery.notifications" ]
        [ ( "## Shell-backed coordinator example"
          , "5ebb1b6d3993ef75fc4d39ea87ea44900faeb89a9f4056f1c234c01f87be7aea" )
        ]
    ; make_background
        "runtime.jobs.subscriptions"
        "Subscription lifetimes, epochs and retained terminal winners"
        moderators
        [ "runtime.jobs.acknowledgement" ]
        [ ( "## Track a workflow with subscriptions"
          , "1e05496cdb7c3f35f9c8bd403e017db94b1665f554fcb4cf0f427213efd55fd4" )
        ]
    ; make_background
        "runtime.jobs.timers"
        "One-shot timers, misfire policy and bounded polling"
        moderators
        [ "runtime.jobs.subscriptions" ]
        [ ( "## Schedule checks and choose recovery behavior"
          , "9736013396d1444d85a26a1f15a377796d57d8a60f27b15269e0c2306ccd5d69" )
        ]
    ; make_background
        "runtime.delivery.notifications"
        "Acknowledgement ordering, publication and model wake-ups"
        moderators
        [ "runtime.jobs.acknowledgement" ]
        [ ( "## Publish data and request a model turn"
          , "9cbc50caaec23ef9ac8362612502405cfeed103f48eb5c826574e84232caac3d" )
        ]
    ; make_background
        "runtime.delivery.ingress"
        "External producer registration and data delivery"
        moderators
        [ "runtime.jobs.subscriptions"; "runtime.delivery.notifications" ]
        [ ( "## Receive external completion data"
          , "6cbb2723d0f49f6db020dd620bcae79bb12390bb4937310b6fd3466a65d77eae" )
        ]
    ; make_background
        "runtime.recovery.background"
        "Staged transactions, cancellation and interrupted execution"
        shared
        [ "chatml.tasks"; "runtime.authority.tool-selection" ]
        [ ( "## Keep transaction and restart guarantees precise"
          , "8c4c7cb181f94d2fd3ed244d8b1a9f65a97bdbc449374eb4f635cb3c989115de" )
        ]
    ]
  in
  create
    ~sources
    (List.map (topics language) ~f:(fun topic -> topic.specification)
     @ [ { id = "chatml.strings"
         ; title = "String operations, literal search and UTF-8 byte boundaries"
         ; prerequisites = [ "chatml.programs"; "chatml.task-effects" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-strings.md"
               ; heading = "# String operations and byte boundaries"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "c782e47dd2da47b9460abfaf2c8edb11433934473c8106a4c6408acae9f9ca83" ]
               ; evidence =
                   [ "lib/chatml/chatml_builtin_spec.ml"
                   ; "test/agent_docs/docs_chatml_authoring.ml"
                   ]
               }
         }
       ; { id = "chatml.collections"
         ; title =
             "Array transformations, shared mutation, task iteration and optional values"
         ; prerequisites = [ "chatml.programs"; "chatml.task-effects" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-collections.md"
               ; heading = "# Arrays and optional values"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "e12df5727939e53fa82f59761300a9cd13a10e06d229705ba8ad05fd075ca564" ]
               ; evidence =
                   [ "lib/chatml/chatml_builtin_spec.ml"
                   ; "test/agent_docs/docs_chatml_authoring.ml"
                   ]
               }
         }
       ; { id = "chatml.json"
         ; title = "JSON conversion, missing values, duplicate keys and shared payloads"
         ; prerequisites = [ "chatml.programs"; "chatml.task-effects" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-json.md"
               ; heading = "# JSON values, access and conversion"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "4e200c727ae2eebbb41bdd5b8cfbb3c59c9eb92b98fb3698839c73603af6ba63" ]
               ; evidence =
                   [ "lib/chatml/chatml_builtin_spec.ml"
                   ; "lib/chatml/chatml_value_codec.ml"
                   ; "test/agent_docs/docs_chatml_authoring.ml"
                   ]
               }
         }
       ; { id = "chatml.tables"
         ; title = "String-keyed tables, mutable aliases and recovery boundaries"
         ; prerequisites = [ "chatml.programs"; "chatml.task-effects" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-tables.md"
               ; heading = "# String-keyed mutable tables"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "2eda419e025c884bce951a28be0f9601abe65976fd00bfe360c638e5fa847c36" ]
               ; evidence =
                   [ "lib/chatml/chatml_builtin_spec.ml"
                   ; "test/agent_docs/docs_chatml_authoring.ml"
                   ]
               }
         }
       ; { id = "chatml.globals"
         ; title = "Global helpers, reflection, rendering and surface-specific print"
         ; prerequisites = [ "chatml.programs"; "chatml.task-effects" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-global-helpers.md"
               ; heading = "# Global helpers and language-value rendering"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "b8444f16bd85803d8aa64ee8c85c38903e6b7cd53bf12f658a429e3f767c15ce" ]
               ; evidence =
                   [ "lib/chatml/chatml_builtin_spec.ml"
                   ; "lib/chatml/chatml_extension_surface.ml"
                   ; "test/chatml_runtime_test.ml"
                   ; "test/agent_docs/docs_chatml_authoring.ml"
                   ]
               }
         }
       ; { id = "chatml.moderator-data"
         ; title = "Moderator item, context and tool-call data inspection"
         ; prerequisites = [ "runtime.invocations.moderator"; "chatml.json" ]
         ; surfaces = moderators
         ; excerpts =
             [ { path = "guide/chatml-moderator-data.md"
               ; heading = "# Inspecting moderator items, context and tool calls"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "e3fe693cec5eb813737194c444e8ba75290286819768d8680779244030f26741" ]
               ; evidence =
                   [ "lib/chatml/chatml_builtin_spec.ml"
                   ; "lib/chatml/chatml_extension_surface.ml"
                   ; "test/agent_docs/docs_chatml_authoring.ml"
                   ]
               }
         }
       ; { id = "runtime.effects"
         ; title = "Logging, transactional turn edits and scoped tool effects"
         ; prerequisites =
             [ "chatml.task-effects"
             ; "runtime.invocations.contracts"
             ; "runtime.authority.tool-selection"
             ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-host-effects.md"
               ; heading = "# Logging, turn edits and tool effects"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "b74f7e25bdfe7a4fb4eeade8f0f377a20f6cb7f078566a8c78e877bd043e7e02" ]
               ; evidence =
                   [ "lib/chatml/chatml_builtin_spec.ml"
                   ; "lib/chatml/chatml_extension_surface.ml"
                   ; "lib/chatml/chatml_host_runtime.ml"
                   ; "lib/chat_response/moderation.ml"
                   ; "lib/chat_response/in_memory_stream.ml"
                   ; "test/agent_docs/docs_chatml_effects.ml"
                   ; "test/chatml_composition/moderator_job_tests.ml"
                   ; "test/moderation/chat_response_moderator_manager_test.ml"
                   ]
               }
         }
       ; { id = "runtime.invocation-context"
         ; title = "Tool context, exact outcomes and moderator event payloads"
         ; prerequisites =
             [ "runtime.invocations.contracts"; "chatml.json"; "runtime.work-values" ]
         ; surfaces = managed
         ; excerpts =
             [ { path = "guide/chatml-invocation-context.md"
               ; heading = "# Invocation context and moderator events"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "bebf28a10930124d13be65955e1f2920faded0797bf999816511ee6f61efde28" ]
               ; evidence =
                   [ "lib/chatml/chatml_extension_surface.ml"
                   ; "lib/chat_response/moderator_invocation.ml"
                   ; "lib/chat_response/moderator_manager.ml"
                   ; "lib/chat_response/moderation.ml"
                   ; "test/agent_docs/docs_chatml_authoring.ml"
                   ; "test/chatml_composition/standalone_tests.ml"
                   ; "test/moderation/moderator_invocation_test.ml"
                   ]
               }
         }
       ; { id = "runtime.lifecycle"
         ; title = "Stop, compaction, reset, source replacement and workflow recovery"
         ; prerequisites = [ "runtime.recovery.background" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-session-lifecycle.md"
               ; heading = "# Session lifecycle and workflow recovery"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "bfeead5e6f60c7ed6d3a6a548e68d741d852532d9aca896f3f4bc9e42dda114e" ]
               ; evidence =
                   [ "lib/agent_session/administration.ml"
                   ; "lib/agent_session/session_actor.ml"
                   ; "lib/agent_server/command_handler.ml"
                   ; "lib/agent_server/runtime_owner.ml"
                   ; "lib/agent_session/managed_submission_tracking.ml"
                   ; "lib/agent_server/managed_output_cursor.ml"
                   ; "test/chatml_composition/notification_administration_tests.ml"
                   ; "test/agent_session/notification_compaction_tests.ml"
                   ; "test/agent_session/managed_submission_recovery_tests.ml"
                   ; "test/agent_session/extension_stop_tests.ml"
                   ; "test/chatml_composition/moderator_upgrade_tests.ml"
                   ; "test/chatml_composition/timer_upgrade_tests.ml"
                   ]
               }
         }
       ; { id = "runtime.execution"
         ; title = "Execution budgets, cancellation, nested scope and persisted state"
         ; prerequisites = [ "chatml.evaluation"; "chatml.task-effects" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-execution-limits.md"
               ; heading = "# Execution limits, cancellation and persisted state"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "c3363bbc5a826d476c2938832dc97081aa7901b3ebb82b52b379da6063bd6f3b" ]
               ; evidence =
                   [ "lib/chatml/chatml_compilation.ml"
                   ; "lib/chatml/chatml_execution.ml"
                   ; "lib/chatml/chatml_value_codec.ml"
                   ; "lib/chat_response/moderator_invocation.ml"
                   ; "lib/chat_response/moderator_manager.ml"
                   ; "test/chatml_compilation_test.ml"
                   ; "test/chatml_execution_budget_test.ml"
                   ; "test/chatml_linked_runtime/control_tests.ml"
                   ; "test/chatml_linked_runtime/invocation_tests.ml"
                   ; "test/moderation/moderator_execution_budget_test.ml"
                   ]
               }
         }
       ; { id = "runtime.work-values"
         ; title = "Background value types, result envelopes and script-job requests"
         ; prerequisites = [ "chatml.json"; "runtime.invocations.contracts" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-background-values.md"
               ; heading = "# Background work values and result formats"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "5043a833337e82d989560e954fbe2a16b758c031761c4c3fd705df20162362d6" ]
               ; evidence =
                   [ "lib/chatml/chatml_extension_surface.ml"
                   ; "lib/chat_response/one_off_request.ml"
                   ; "lib/agent_session/script_job_service.ml"
                   ; "lib/agent_session/script_ingress_service.ml"
                   ; "lib/agent_protocol/subscription.ml"
                   ; "lib/agent_protocol/schedule.ml"
                   ; "lib/agent_protocol/delivery.ml"
                   ; "test/agent_docs/docs_chatml_authoring.ml"
                   ; "test/chatml_composition/job_launch_tests.ml"
                   ; "test/chatml_composition/timer_tests.ml"
                   ; "test/chatml_composition/subscription_tests.ml"
                   ; "test/chatml_composition/notification_admission_tests.ml"
                   ; "test/chatml_composition/ingress_tests.ml"
                   ]
               }
         }
       ; { id = "runtime.control"
         ; title =
             "Model recipes, shell process output and transactional session requests"
         ; prerequisites =
             [ "chatml.task-effects"; "runtime.invocations.moderator"; "chatml.json" ]
         ; surfaces = moderators
         ; excerpts =
             [ { path = "guide/chatml-runtime-control.md"
               ; heading = "# Model, process and runtime control"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "0afc20f2866592fe16b34fd7b268470947223270a54756cea0ba0eeb341f7205" ]
               ; evidence =
                   [ "lib/chatml/chatml_builtin_spec.ml"
                   ; "lib/chatml/chatml_extension_surface.ml"
                   ; "lib/chatml/chatml_host_runtime.ml"
                   ; "lib/chat_response/moderator_invocation.ml"
                   ; "lib/chat_response/model_executor.ml"
                   ; "lib/shell_runtime/moderator_process_adapter.ml"
                   ; "lib/agent_session/runtime_builder.ml"
                   ; "lib/agent_server/session_factory.ml"
                   ; "test/agent_docs/docs_chatml_control.ml"
                   ; "test/moderation/moderator_native_requests_test.ml"
                   ]
               }
         }
       ; { id = "runtime.native.requests"
         ; title =
             "Native computation requests, limits and non-executing validation reports"
         ; prerequisites = [ "runtime.invocations.validation" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-native-requests.md"
               ; heading = "# Native computation and validation requests"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "9e24239b9f0faefb1095ec4302f9847871ea9071019fc9e2a3c9ef57c14e3ce3" ]
               ; evidence =
                   [ "lib/chat_response/one_off_request.ml"
                   ; "lib/chat_response/authoring_validation.ml"
                   ; "lib/agent_session/run_chatml_tool.ml"
                   ; "lib/agent_session/authoring_validation_tool.ml"
                   ; "test/chatml_composition/native_contract_tests.ml"
                   ; "test/authoring_validation_test.ml"
                   ]
               }
         }
       ; { id = "authoring.reference"
         ; title = "Documentation requests, feature discovery and continuation pages"
         ; prerequisites = []
         ; surfaces = shared
         ; excerpts =
             List.map
               [ "## One strict request schema"
               ; "## Interpret availability and completeness"
               ; "## Budgets and continuation"
               ]
               ~f:(fun heading ->
                 { path = "guide/authoring-context-tool.md"
                 ; heading
                 ; include_children = false
                 })
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "854c730bd39b61ed4339ea9c625d1259583f32fcd9168c6798f66c00d00fe3ef"
                   ; "8bbaa8b6aa72f9ed301d17ac5ce4ddb94f35e07b49ee679a8e9b0cfdf2465d6b"
                   ; "94d2d6461640995f15b97e69675fa0133b46f79bdfc7216f321751a73a5a149c"
                   ]
               ; evidence =
                   [ "lib/chat_response/authoring_context.ml"
                   ; "lib/agent_session/authoring_context_tool.ml"
                   ; "test/chatml_composition/native_contract_tests.ml"
                   ; "test/chatml_composition/authoring_context_tests.ml"
                   ]
               }
         }
       ; { id = "authoring.primer"
         ; title = "Shared ChatML and ChatMD authoring orientation"
         ; prerequisites = []
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-authoring-primer.md"
               ; heading = "# ChatML authoring primer"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "0fddbca3861ef32cd8498810e38147c1fdbd5519bc59b0c4d08f8849965e65fb" ]
               ; evidence =
                   [ "test/agent_docs/docs_chatml_authoring.ml"
                   ; "test/authoring_materialization_test.ml"
                   ]
               }
         }
       ; { id = "chatml.task-effects"
         ; title = "Task sequencing, reuse, error recovery and effect boundaries"
         ; prerequisites = [ "chatml.tasks" ]
         ; surfaces = shared
         ; excerpts =
             [ { path = "guide/chatml-task-effects.md"
               ; heading = "# Task composition and failure boundaries"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "949ca1709625e7273fb5f4c55724871abd6de643e787b27ba6c791c2a028c464" ]
               ; evidence =
                   [ "test/agent_docs/docs_chatml_authoring.ml"
                   ; "test/chatml_composition/ingress_tests.ml"
                   ; "lib/chatml/chatml_host_runtime.ml"
                   ; "lib/chatml/chatml_builtin_spec.ml"
                   ]
               }
         }
       ]
     @ runtime
     @ children
     @ [ { id = "runtime.runs"
         ; title = "Scoped run decisions and transactional receipt custody"
         ; prerequisites = []
         ; surfaces = moderators
         ; excerpts =
             [ { path = "guide/chatml-authoring-runs.md"
               ; heading = "# Scoped run decisions"
               ; include_children = true
               }
             ]
         ; review =
             Audited
               { excerpt_sha256 =
                   [ "563066363cdfe7bd1e806d7bd02d1179b5370565b11961a2759c8cc8e3b1fa94" ]
               ; evidence =
                   [ "test/agent_session/run_action_scope_tests.ml"
                   ; "test/run_lifecycle_protocol_test.ml"
                   ; "lib/chat_response/run_operations.ml"
                   ; "lib/agent_session/run_action_service.ml"
                   ]
               }
         }
       ]
     @ background)
;;
