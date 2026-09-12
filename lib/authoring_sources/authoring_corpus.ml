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

  let audit corpus ~targets ~mappings =
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
        "23c3696f85a3a6c1e947b8e65c12066294eb57f9b7ea5668b23fcd6fe5d46273"
        "runtime.invocations.one-off"
        "b1bd3b4204bc034f37ebeb05afb34a2fdf7a89d34267845b9ce1a394ee057161"
    ; make
        "tool_v1/entrypoint/run"
        "776c8a90b01720d82560e9ad51ff7d507d904cc82ddae1f46b9f9388658e6a9f"
        "runtime.invocations.standalone"
        "8652b88c2cf4a91ed5974d049bd206c976ed0190d7de69c3992db8de06d21e4c"
    ; make
        "moderator_v1/entrypoint/initial_state"
        "83c4550d994027c5d2347ec8fe8b1efe366e48105a2d8563b8b17293b534807b"
        "runtime.invocations.moderator"
        "f58992cc9c62f7bfec65e3381309595e527c2e94c008f0b62a79180082230071"
    ; make
        "moderator_v1/entrypoint/on_event"
        "dae4c9cf7e731d167b4088108fa0dded53478857a436e69163fe4c8215ba5775"
        "runtime.invocations.moderator"
        "f58992cc9c62f7bfec65e3381309595e527c2e94c008f0b62a79180082230071"
    ; make
        "delegated_moderator_v1/entrypoint/initial_state"
        "c179907ea7bf651414fa4400607c3f75beb87fa748e048e10abb1e8feaff7f8f"
        "runtime.invocations.moderator"
        "4f6e6c33e84bef5b5f8a01906c9c87d51f6a80df28e40181666545c3f1dd4257"
    ; make
        "delegated_moderator_v1/entrypoint/on_event"
        "5d25a5033754052c6e715e2dffa44385a922c922c41639eb52c1e9baf235c93e"
        "runtime.invocations.moderator"
        "4f6e6c33e84bef5b5f8a01906c9c87d51f6a80df28e40181666545c3f1dd4257"
    ]
  ;;

  let task_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "de4730371e88a3bea91ba98f2ef303c9ebf38aac237ba8d701153c8e644e4d19"
        , [ ( "module/Task"
            , "a5057743d97256c6e123f089d84a21a57b42d89eed928a4d5183f527edf69a92" )
          ; ( "module_export/Task.bind"
            , "274e1738b20e0621475597d8e172be47420d32180c79c5f1f18f0dea4afe0555" )
          ; ( "module_export/Task.catch"
            , "ccf3b08ab8d322b5b1c1f85dda60a6e5143abac4845f91b25ce7d9bc25a314f5" )
          ; ( "module_export/Task.fail"
            , "87144726ec28b6b741dbe69314d4cf53d535c61844cd4a876bde87ef16b830cc" )
          ; ( "module_export/Task.map"
            , "6ac2714682bf18c3899679e3c23ed8117a3bcce32b9e9c3f5043a510470d5f65" )
          ; ( "module_export/Task.pure"
            , "0bb01ffcc976325a794463b2a16cbbd97116d10adb1bec9a4e376afff93c8f9e" )
          ] )
      ; ( "tool_v1"
        , "891d098b492e63eceafc9c1eba9b6069a82e08a06c18661be8ce155dcfc89d1f"
        , [ ( "module/Task"
            , "2113974a76db0e60a5d9f9b676c78a2863ea283b282fa841c4e0c4f5d20da7df" )
          ; ( "module_export/Task.bind"
            , "051f4da2e8ed1ea6c1cb5198986da7659195191ddc5c6180d291989d294d9a66" )
          ; ( "module_export/Task.catch"
            , "5b7bd9fc5248785682c5cc974bafcf32b19c0c011f86373860a27471bc15a967" )
          ; ( "module_export/Task.fail"
            , "c94bcea233605c0e7bfffad449191798368d862d09fc56cdab6e61c9055b16a2" )
          ; ( "module_export/Task.map"
            , "52871b04f55a9776e388b8354036e23f4c5fdce9c7d87d55fe3d41ae30b16c72" )
          ; ( "module_export/Task.pure"
            , "d76749102b2bdee5923fea7df2847544ea3e2a5337b7487c8a5cd96eb4bd5721" )
          ] )
      ; ( "moderator_v1"
        , "739d75ff8d4b2183b00b13967ccc35b26d23f07fe319370c0575bbdf28564b65"
        , [ ( "module/Task"
            , "6b60971e6b0bc872f863dcc3dc1b62660e028b863021ec7fc79f23ea824c32b8" )
          ; ( "module_export/Task.bind"
            , "d853de56e33e37aad546cb7e4ea6a026b8bae3681e40592fd327aacb13ca3000" )
          ; ( "module_export/Task.catch"
            , "15a4f3ef868ec6287a2e7898b869bc49bc6c542cf93c1ef3275987b249fb9270" )
          ; ( "module_export/Task.fail"
            , "3e54225f13fdd5444cfca2db2b81be6031cd16a6ec9e708879f3d842d81411e2" )
          ; ( "module_export/Task.map"
            , "1b25952904e5653c0bc27ec2396f01297e61fb5f9b1583246caefce41730d508" )
          ; ( "module_export/Task.pure"
            , "99f009e6fe16865e89ada34f70e71376560ba382fa373456fbb75fc811363b08" )
          ] )
      ; ( "delegated_moderator_v1"
        , "11f63fb503104c5515168c91d1d13a53043320ead580f0062c459aaa39ea40e4"
        , [ ( "module/Task"
            , "f77899678f7eb95edec85f308d5c47bc51a4e2fd8e91ba9698ad8d857f652b9a" )
          ; ( "module_export/Task.bind"
            , "61efaf1e960bf73ec2c8fd58800a36428ef66074ce13dfedde6ba6758c29d4dc" )
          ; ( "module_export/Task.catch"
            , "dcdfcf90251c9ad6d68cb6f0d0c90027e420ee6f9d4a55665f63edde324b1123" )
          ; ( "module_export/Task.fail"
            , "414d7662fbe5be4c1f107294ad704a15ae03f236273275229a99ed68ca8edaff" )
          ; ( "module_export/Task.map"
            , "7d1fea8b83f24c8b9423274b2219f22c1c9adb59e77cf74fc9fec24caad047b7" )
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
        , "8929a779461f25385ec1580d736eea0c628373811577e9306c2faf8cc3738a43"
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
            , "18b4c30dba9b41bfb7dfbd190e7c42610f588c1f716553515ba7729354f00e85" )
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
        , "8e6445c29792c36284332aba6ac7b4cfdddf7b44d0ae378041c71784a0c53dfa"
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
            , "43ce98e62bd5765390a55f412e0ed19a331e66459033c249ae9b524e8146d59f" )
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
        , "7c89096edc2d8b14fd11d9cf95d28d0f2823e334f02af83c6f35ba0b3c337519"
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
            , "db04d0437065161403ab0e7cff4b75d5b9060e585ebb32e2bb5456005e4e5a24" )
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
        , "b28fe686da0c259e769bba79f166bf4a6284cf18463c7428c32bd611cd05deb7"
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
            , "b924f1227208975805401d72a457e554222ecb6d3c63f83ea0ffe7a4ea192d85" )
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
        , "dfff5a377f9451cf3ecb4a156ebea0cc64a8c90e34899de6d38e43364510e441"
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
          ; ( "module_export/Array.find"
            , "4ba8cc93160f643c6aa4a3d197adb00b821d886871fdb4c9dc6077907e7fee5e" )
          ; ( "module_export/Array.find_map"
            , "cc9708690b2d190552316b57b6915335832191fb58a2558d47d4215ca30c9f0a" )
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
        , "7a8c4ce3a13e58d43d656273c217f39f6425b95049ca7a416f9bf125e3d1ddf5"
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
          ; ( "module_export/Array.find"
            , "5be83cacfa738ac8ab412471243f862be25b65e22eeaa047708ae1c1b16896f6" )
          ; ( "module_export/Array.find_map"
            , "70df0066e4ddce1d2339fcbdd601f1e286b5672900032fcf1779dca3737c0ea9" )
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
        , "2f46d515b7bb75760db1b771f37332eb8b79e009e308554fc5096e7360527859"
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
          ; ( "module_export/Array.find"
            , "ccd09f629de239eb748d5881cf7aabb17a5d9676d7554e6efca835f42774709d" )
          ; ( "module_export/Array.find_map"
            , "31756a051cdb48dc45706665f59d24a2297f7f81003162952b1217bcab1280b9" )
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
        , "a7c5e60e6dd2eba1dc6c2d63c56cbb3e2dc273cffad332dc297ce4ddef3d0528"
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
          ; ( "module_export/Array.find"
            , "3ac6675f0a342b1899bf6898811262055cc76f2030731d02d0708c4a57ec30d2" )
          ; ( "module_export/Array.find_map"
            , "98b89596322203d3149f3f2308f01009e29b5cad110841fdbd35dd7ca40c7dce" )
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
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let option_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "dfff5a377f9451cf3ecb4a156ebea0cc64a8c90e34899de6d38e43364510e441"
        , [ ( "module/Option"
            , "35625c7916a5431fdcef89a05a6f24dfe4154b12e3076e6a38db17e8dc484b96" )
          ; ( "module_export/Option.get_or"
            , "0abc06c13404ebd2ddbf702b5232c0db81dd6b2cf70b071a8c52716d2872acae" )
          ; ( "module_export/Option.is_none"
            , "6c6964a6eac1916c7f5d9e6bf94b8477296a9a2f4fcd4891739b13e7f76b9234" )
          ; ( "module_export/Option.is_some"
            , "c54300490f4d5e6fd98aefce8d782e37c4ac0d98927dc7b75be5316ebbeab4c7" )
          ; ( "module_export/Option.none"
            , "d66487403fa7b0c00358e9f6a907cd16c5bbea4dd6c20a78b0a18a399ab2e574" )
          ; ( "module_export/Option.some"
            , "af6d3e053171d385c9315f96394ad854b84e2d6d5a7c8af8090dcb9a4b2bacfb" )
          ] )
      ; ( "tool_v1"
        , "7a8c4ce3a13e58d43d656273c217f39f6425b95049ca7a416f9bf125e3d1ddf5"
        , [ ( "module/Option"
            , "8139706127f4fb46b6b7d2c8f8951efe78e58e59452de6cf2c5a78e552a489f8" )
          ; ( "module_export/Option.get_or"
            , "8e11881fb0df7f31a7090fdf89f4bb2eabac437f627695803dadeaebdfb1f257" )
          ; ( "module_export/Option.is_none"
            , "208dc46fc81ffe56960800c5973603d8f2273261cd8652974913a3f607753a51" )
          ; ( "module_export/Option.is_some"
            , "78f0688ad13fc626f451beb98655507315ee224684a4ef3875ef9f8bf3600993" )
          ; ( "module_export/Option.none"
            , "e3177d43a8525e9361f64fa810c60a964b56994c15487774686a95e17aa263d7" )
          ; ( "module_export/Option.some"
            , "9800ca939bd5dd2a790a94f29faca30ca578942749ce4aebccb8dd671b3a9f28" )
          ] )
      ; ( "moderator_v1"
        , "2f46d515b7bb75760db1b771f37332eb8b79e009e308554fc5096e7360527859"
        , [ ( "module/Option"
            , "eed8a04643315785f17a45f6ff41a87b0936217c1205317613c9aefbd8375690" )
          ; ( "module_export/Option.get_or"
            , "6e0a346b981d05e61420c1d7eb7ebda14f88351ae748408062544d9f59b25146" )
          ; ( "module_export/Option.is_none"
            , "acb5174812a5e45076da81f79d77c0c0daab34b68f878876db7818e061bb2a2f" )
          ; ( "module_export/Option.is_some"
            , "aa242496799284da2832b2c29fcc888432c46c60a751ce5810073e5fbc7b256e" )
          ; ( "module_export/Option.none"
            , "41281aeee1ac78155785e553c3db78a35ff757cae49c6bbbe36a82745bd3693e" )
          ; ( "module_export/Option.some"
            , "38d8c3c4754e9da41c9d4fcecb4deaceedcda41aaa5200d8bcc037cfea004d92" )
          ] )
      ; ( "delegated_moderator_v1"
        , "a7c5e60e6dd2eba1dc6c2d63c56cbb3e2dc273cffad332dc297ce4ddef3d0528"
        , [ ( "module/Option"
            , "eb0a6714a9d24b6c7e72a1c0e54a424080e40e700fd464aaa8e8bd672fab7d53" )
          ; ( "module_export/Option.get_or"
            , "70e7edb127ddeec0c3794f0af81a7e9cb245a33480aacd5033a428723f481301" )
          ; ( "module_export/Option.is_none"
            , "8941d3d78e609baee63d6c5cd4ae70c7fc1d71c4913d645fe2291f51cdf91258" )
          ; ( "module_export/Option.is_some"
            , "5a01f4d4d605dcad2f0a50e052258e2460230be0b93111cf498f7059f5d88640" )
          ; ( "module_export/Option.none"
            , "f0ad57be8cc8a8ababddab73cb6dd318661df8f75085dab0df554153d05a90f7" )
          ; ( "module_export/Option.some"
            , "f8d8f7d47d3f0b9abd83683752d373b0ca7bb5388c7bb83dd0235cf1b2523e06" )
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
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let reviewed_mappings =
    entrypoint_mappings
    @ task_mappings
    @ string_mappings
    @ array_mappings
    @ option_mappings
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
          , "5af96134b1afc60510d2cd3f76d8387ec199117695f78b5512230dfb06cebd53" )
        ]
    ; make
        "chatml.types"
        "Type differences, matching and mutation"
        [ "chatml.syntax.containers" ]
        [ ( "## Records are structural, with conservative joins"
          , "f8da96fda8d1121048679c04ecbffa9c81409027e1a5543e390b4fdfe39c660c" )
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
          , "0ee9e0eb854feaeadd0cddc91187e061da19e017a7602245575f633b9be52e17" )
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
          , "e27196e5dfde0d4be194493d8a3c74cfc75154edfa793b25e29b7806cde3b03f" )
        ; ( "## Functions, loops and modules"
          , "0fb2be402c3218392e48d5db0e0b31ca4321ccc033c4b643df1df203a4b42290" )
        ; ( "## Matching and explicit data types"
          , "edb7f09d1277d7e1c3aed88669efebd7efaddf375eccd1b383a5aae8506b89e0" )
        ; ( "## Standard library for structured data"
          , "7096551fff9d2f77779a577d846c44e4bfecfcfa3ba8fad171e0160d3c7ad640" )
        ; ( "## Effects, errors and execution boundaries"
          , "00f02e2fb345cd50ba3f9ab75dc834f3d95e8f62895ce214faa63cd74d0d0f8a" )
        ]
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
    [ make
        "runtime.invocations.contracts"
        "Execution categories and entrypoints"
        shared
        [ "chatml.introduction" ]
        [ ( "# ChatML authoring: execution and invocation contracts"
          , "cbc5f77d3ba722e09f7e77655c7a9253d1717f64159212b0ddd026c519ebbd9d" )
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
          , "4bcbe559d74a660b742b4d5a5479071d8d060664dc76c8b0fb329b296ec7bf57" )
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
          , "26842dcf524875eb6a6f542406d089049512b4fd951fd2c3804a4afe56f7841c" )
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
          , "65364456f6d94b00a5f4b0cf028ceecc08b2b4b7f31bb3b95d3ff7300edd63dd" )
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
          , "f9d854a4e0b53f824bb4c20197c3432fe4854dfa02a70db1e5eae5c45197af6a" )
        ]
    ; make_child
        "runtime.delegation.creation"
        "Creation retries, lifetimes and inherited authority"
        [ "runtime.delegation.generated" ]
        [ ( "## Create, retry and retain authority"
          , "1fe4d0034367286bcfbb781acf2a01eac7fdf5f72a416143cd04c60fd1033f83" )
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
          , "9fe135857a2020bbafbe080538233609db986ca86f683b09bc1d8a04cba6c3d1" )
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
    ; prerequisites
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
          , "972d192ed9347e8fbde22ced410558f5be8972ad47b4b7e5b81af652aafefeef" )
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
                   [ "6a6d720f71c08569629b640b4f13158683453c7cc0a7c71dd02b3c43c596a804" ]
               ; evidence =
                   [ "lib/chatml/chatml_builtin_spec.ml"
                   ; "test/agent_docs/docs_chatml_authoring.ml"
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
                   [ "73fa478eef95e5a3f5dd32efee06200850ad2f00bd462dbb9bb1f89af83fe25b" ]
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
     @ background)
;;
