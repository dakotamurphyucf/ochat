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

  let json_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "9598e35b5722b27e3d9e0fd0b8a79c6069aab8252ac06fd9931d26a8d61fddfd"
        , [ ( "module/Json"
            , "8276161ec095028a7648fb61044eca879e3255f69d1280b8ad20b13c350235c1" )
          ; ( "module_export/Json.as_array"
            , "acb9138f4f2dc5fc8c5dd3c00cdddd4d904a36ec35dfa381db73354707983015" )
          ; ( "module_export/Json.as_bool"
            , "3208189b3d4bff9f57a7a1f3e7074d696c22c482ea5d3aa46b5199bd5709c2f6" )
          ; ( "module_export/Json.as_number"
            , "1381277412a3982e3efe0ea48a33dd18af6cd22e81fdcf865fc9fffba51f20e3" )
          ; ( "module_export/Json.as_object"
            , "c9d8866bf84395ffe566cf32835808400d0b428d07dc6a2ceadcd814dc7430e2" )
          ; ( "module_export/Json.as_string"
            , "5c011d6c14a946301316cde608eb9bab337973e054eb2073320da5d45e7755cc" )
          ; ( "module_export/Json.get_field"
            , "e9b748cb126e65ed07ad6a12b78f09c5ea1c2d14d28ccbe888cb10a355538b54" )
          ; ( "module_export/Json.get_path"
            , "8809f1b560a810f2640c6d6a46f21777514499754bedb50fedc09d7e78ff35c0" )
          ; ( "module_export/Json.object_keys"
            , "641eb85495b4bd5c0822528732873edc8fbf282fb0d7e54e16f24cb1343b2e9a" )
          ; ( "module_export/Json.parse"
            , "898d914d7003ada6aa1a598ce9d1ef88fd6d078d26dbc9ed52d99b3081e9e0d2" )
          ; ( "module_export/Json.parse_opt"
            , "db919ef6abcdb7a1a8b020477dafc7ec164911080b41dffd883e25f096bdfaa1" )
          ; ( "module_export/Json.pretty"
            , "fec1ce2eda4b84f520fd5c57819128aa9ccf1e2fe1b0655e30bf5ea0e9c7b50f" )
          ; ( "module_export/Json.remove_field"
            , "fc66ef9a047a92c9d94425cce2a76a4e93e5ef0db9307e2588fea0c96d109756" )
          ; ( "module_export/Json.set_field"
            , "9a41b05c4f1e6f1b76479adb4e8cfbe7f0dbd611f9e201dbcd85e42146070f56" )
          ; ( "module_export/Json.stringify"
            , "df1e0c7734299ebee37ebf1e0f0a23302c1aed33684c1777eb2d15f7fd395bf9" )
          ; ( "module_export/Json.tag"
            , "da5713de082beb3f4e662ddab1978aeebdfb32828c7e050c05ff7192d3f81064" )
          ; ( "module_export/Json.validate"
            , "775f180b4c48e9955bb980993420ca28ba4a832139e5905eb31edb35b940369b" )
          ] )
      ; ( "tool_v1"
        , "0598fd72875d8502f88d4152d83666f7d10edecc175b39a63f7bfbc519ef967b"
        , [ ( "module/Json"
            , "f594a3d6bedb39aa513044c62ffa6eccab150fdc787b030a370c47b61b9038d0" )
          ; ( "module_export/Json.as_array"
            , "830c7e8ebafd97b25569d1f141574b2eead88a05ee66a93b3db4996859920242" )
          ; ( "module_export/Json.as_bool"
            , "3b49f28d7d9c3bcd733dfdc055484905e9caca96d63e8f382eb2cef967f630ef" )
          ; ( "module_export/Json.as_number"
            , "a814ede555a4997f60a026ce5596195343e26cca881f943806eb4b00322a6d21" )
          ; ( "module_export/Json.as_object"
            , "7adc77634396be5e26b235891bdf2c01b5bacf3970092bb45522972260276daa" )
          ; ( "module_export/Json.as_string"
            , "737615f66ab5debaaca79eadd065882a689cdbcfafa92755c94990d9cb2a478d" )
          ; ( "module_export/Json.get_field"
            , "db8c8826c1f21fee9289d6379ac0c0adc2ee6de10d74e53f882e63ecd8944903" )
          ; ( "module_export/Json.get_path"
            , "7a602e410751fec4d10b27bf4d0a66ef31ccca1d3a23bc57e1ad038a0122c9ee" )
          ; ( "module_export/Json.object_keys"
            , "ae36feba847f9845d7bc62fa439655799e0434484b3ac6df9ec518bf0aef5f16" )
          ; ( "module_export/Json.parse"
            , "57dfd9bfcfca70689c47c0b40f6c00b86e5ec8188ab0bea5d4398a2b313182b5" )
          ; ( "module_export/Json.parse_opt"
            , "cb910f7cba2f2c63f6642a3d67d04a99c186073d95db9f2ed543e79b22d2e418" )
          ; ( "module_export/Json.pretty"
            , "76de2cf6b7b3fe7339f73d38a9503abf240d023cdcc07b588d228ddff281b10b" )
          ; ( "module_export/Json.remove_field"
            , "85ddff4f6bfc4ecf10aff9566f0043be81643bea45107974db24a53718da772b" )
          ; ( "module_export/Json.set_field"
            , "c88620a3e775c91577133be0a1048c84f15aac477c40f591963b22e512566a59" )
          ; ( "module_export/Json.stringify"
            , "0921a4e77f524bc421206f66c0e7693a3d3b71972a6b80a1bf1530bbe8ba7a46" )
          ; ( "module_export/Json.tag"
            , "768c6d0bacbfee179befcc8f01cb2d15e5c0c74b8f370416a988d11745d4e77d" )
          ; ( "module_export/Json.validate"
            , "91a455c705c41b4b046e84ad03f0125533c4872d94745e1f0a67c9e334eeaf3d" )
          ] )
      ; ( "moderator_v1"
        , "7666e108b1139830fea1f55c5858064e945093f96929db3d8253778f2715ae66"
        , [ ( "module/Json"
            , "5693ccaf8653d32a31d1f478d215fbb447bac00435cfc10c3df8caee1ad85670" )
          ; ( "module_export/Json.as_array"
            , "7d54f626a3438ca3c1a1ed4591337a4f1d63cb73b5c64aaa1850af9d5ccbc4be" )
          ; ( "module_export/Json.as_bool"
            , "00f8f1f23c8257eb3818363e963c23aa8cf3c95addcb1c4fbc3072c100f0fb95" )
          ; ( "module_export/Json.as_number"
            , "1fb240ff3ab1f7f1d2fddd09df5db3e5c3356c4f086e072c3e646774ea70baca" )
          ; ( "module_export/Json.as_object"
            , "b729e45268bed9e64755ba351d4d5e5ed0f4a62a4fdd161925564325ce169297" )
          ; ( "module_export/Json.as_string"
            , "3dca1502dc3c13f11d72a1000480e8ea384033bce523d8454fd0cd1ed8d5a5d1" )
          ; ( "module_export/Json.get_field"
            , "a2f3801a42a6b3f0216bf62ef2b5d39e29e62550adb2d4c8c57fe852f2bceb03" )
          ; ( "module_export/Json.get_path"
            , "d0007da2ca77a56c3e8eb8055b8b05348595c59649d0993e0293285a47dd9e58" )
          ; ( "module_export/Json.object_keys"
            , "887eb0c300397770278e55ca49f5d63acbbbabb5fca0a23a364e438a54a5aa9b" )
          ; ( "module_export/Json.parse"
            , "7ee8770e07837980813c83d1b773ff71b1ec75da3b7b2389f1b6558a2c54473e" )
          ; ( "module_export/Json.parse_opt"
            , "10bfa1639e0dcaff9044e687c3eb90dbf8a096e449a80fc61bb8d1d41acf5d5e" )
          ; ( "module_export/Json.pretty"
            , "9632e92997d6d4aee55fe3672142f0dd9d02e4b946a095aa59f1b6e22c962ee2" )
          ; ( "module_export/Json.remove_field"
            , "f6f1daf56b819ece76d7d33bf0200ca16d7c148bb874e577700056d8c34bb550" )
          ; ( "module_export/Json.set_field"
            , "3c81042fb9219af0e0c81753a826af0604410f4b22a5ce3e2b933a9b1695b29b" )
          ; ( "module_export/Json.stringify"
            , "9057f43bc5984c237b2433e09c1d93f5fb3e194dd9ddc87225a67a91e95780a5" )
          ; ( "module_export/Json.tag"
            , "83724c4fc80b6d37e34840e955b2e38d366b7ec518168f264a99815705b3de79" )
          ; ( "module_export/Json.validate"
            , "fd94d4e57a65a4381f7c6c87a98cc99f5b1c080b4eea5f9a6f20da11bd20b988" )
          ] )
      ; ( "delegated_moderator_v1"
        , "1525675290f78af03c14e06661a0d8389ca4e039b16ec5e1cb5b23a3b00501d5"
        , [ ( "module/Json"
            , "dd36cafbd30b3b4396d4d7b02394ed09a988f3d818cf4626c2fe7da26cefc457" )
          ; ( "module_export/Json.as_array"
            , "9ed75c04bf045f0b7035bfe3217d111e4f40c5ce5b93ee6b5b603b51577fa715" )
          ; ( "module_export/Json.as_bool"
            , "721442474c0056df4e531b19bcbe328199510bf28d238cf35e9f9c63b607c3d4" )
          ; ( "module_export/Json.as_number"
            , "215760d1a53014b0fdb630f7b5032ed6b5b87354c3da0bc038c07d427a4c151b" )
          ; ( "module_export/Json.as_object"
            , "bb28b2c70ef3b93c59375e83bc7736564d8c7a8802b94d64a79c158c0c131fa0" )
          ; ( "module_export/Json.as_string"
            , "f65d73eb603fe23c074b8631df302e9b811e7f87ad2a9444a0e707c7dd1e0729" )
          ; ( "module_export/Json.get_field"
            , "d88f230597506ceebf2feb35052d46e0804dcce4fa86f7c471ccd4c5590171ad" )
          ; ( "module_export/Json.get_path"
            , "9e6f69a7597e5576f814f1d36c730cd0ab16d0d584547c2fe29d8a95eff9a4a7" )
          ; ( "module_export/Json.object_keys"
            , "768abce10c876981ade1c5048eec781e077b494e3afd6d0d2c0bea465bafac65" )
          ; ( "module_export/Json.parse"
            , "2473b066ffce99284e2e2093a1a136eb3e21033ae747b6a0e1bd0e894d46d0a8" )
          ; ( "module_export/Json.parse_opt"
            , "a720b67a46adfacb2c0cdd5f5fa9a1a138068817edc429275c69937464bcbc43" )
          ; ( "module_export/Json.pretty"
            , "f9809da3480ca335a7f538380b96490eeb52f8b2c88d4dbd534a040a0381cc10" )
          ; ( "module_export/Json.remove_field"
            , "c1afdc54a714014c005c208cd5d069ae6e2d9d56acfb912d75ed2ba3f9ea1cb1" )
          ; ( "module_export/Json.set_field"
            , "b2d59b8987a476a5b496d9f9e7dfed9c70bfd0134f2113ea7f5ab241e787bd82" )
          ; ( "module_export/Json.stringify"
            , "f9d390aeffd1c5b15ee318a642abd5cca184da0cb616a575d2e6739852e48e4d" )
          ; ( "module_export/Json.tag"
            , "085c7596379a6ad0087d35dac11a3ef1831ebfbd2eb3c2cdfc4a70e466a39bf0" )
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
              ; "lib/chatml/chatml_value_codec.ml"
              ; "test/agent_docs/docs_chatml_authoring.ml"
              ]
          }))
  ;;

  let hashtbl_mappings =
    List.concat_map
      [ ( "one_off_v1"
        , "2fc1d29574db1d5fe819b36d879cb2232e1c96f6dce3778292a7daeb12c8f045"
        , [ ( "module/Hashtbl"
            , "44d97e5d36cbf23bb3b2e234a045a2f976bbc486da1bdcb2cf65c69b37d188e7" )
          ; ( "module_export/Hashtbl.create"
            , "1213527868f4fe7ebdda853ed034372dd4179fd3e2036ffeafe34145d0af4152" )
          ; ( "module_export/Hashtbl.get"
            , "820c0a1f61afe2d5558912cd693817af4eac2956f491f3fd557107d1855eff70" )
          ; ( "module_export/Hashtbl.mem"
            , "776c3ae80fddf50e10db2835e2b8ee84a1b5e8869f0d061b1e3a1e72fdfa5917" )
          ; ( "module_export/Hashtbl.remove"
            , "d78dfb76b213b6520389c43d36adb73e2bf9587fe59b0f2de5e5a0d2f98ebd65" )
          ; ( "module_export/Hashtbl.set"
            , "b1c0b7ec1710258f0df03ca217b9ee7a0a4696aaeed773439f5c920d3555c11b" )
          ] )
      ; ( "tool_v1"
        , "e6eeb78a70551334e9424d937d4428c899a2304832eca55511a31a1237795bce"
        , [ ( "module/Hashtbl"
            , "866a8d298a7e56dbdb498517254086e88873c396a4595d494a8a05a26e8d3414" )
          ; ( "module_export/Hashtbl.create"
            , "a4d73c58e8d5fbb11a8d152fd6ced4a181342cbeec5977f4e4e8338431660fa8" )
          ; ( "module_export/Hashtbl.get"
            , "7bfa8adb4df12a16710b29dd6e433ea03cbc3ce3a002c70e598d635192c9c6dc" )
          ; ( "module_export/Hashtbl.mem"
            , "287de86a7494590e25cb3d3ea473fc0012fa6996518b6cd074324970f7f1fc10" )
          ; ( "module_export/Hashtbl.remove"
            , "fef23aca6a955ca9f9cb7fccbfdfa909e4432a17ff351264a913d2a229ecaee4" )
          ; ( "module_export/Hashtbl.set"
            , "a8aed9d5f05a6de5b81289f27bf5b5828d13abb93e2dd1635ba4249c83774f27" )
          ] )
      ; ( "moderator_v1"
        , "e518ac351ede12e897d4616abca9790adaf3588bdca89773a7732e77c0bf93db"
        , [ ( "module/Hashtbl"
            , "d3de0228abd1f243c2d62e30fee6d770908c8db4724e639bd1cef1200bcbcfdc" )
          ; ( "module_export/Hashtbl.create"
            , "4918800b94fe6812967df0847b9efe2ba2bb69c94f4a056e9fb5094da3f8ac07" )
          ; ( "module_export/Hashtbl.get"
            , "5a833eb3c9fc49af48d9ddd801ed6cd1f1c78aab993f4541734e50c942312349" )
          ; ( "module_export/Hashtbl.mem"
            , "2783288d3fa1e31f7a884deefadd4559ab223e6edd1fb087a08d7150fc674f0f" )
          ; ( "module_export/Hashtbl.remove"
            , "2ec781e9883f73b471980084be795eadb0d15f117712134a565267b7cdb9eb97" )
          ; ( "module_export/Hashtbl.set"
            , "37a78e5e2cfd9c7b5258d1d360cf3450ef66cf4bb0040b5356b5d95c18b9d407" )
          ] )
      ; ( "delegated_moderator_v1"
        , "e92ed0332f90ca8663517d269bfb4e671b481a5d72de4044bdbd6ebf556d9be1"
        , [ ( "module/Hashtbl"
            , "8460da183ee7f58302eed019d338422946e327f2522208d193c50ee7c80dabe0" )
          ; ( "module_export/Hashtbl.create"
            , "b8ccd94c7f31132b76d8aad33fadf709b3214bf7eeb5b83285d3c96fa56f0259" )
          ; ( "module_export/Hashtbl.get"
            , "112dbe3d725b1b8c525bf7d2f9457e7c73ff6fa83d0afde70b6972eed68ce2ee" )
          ; ( "module_export/Hashtbl.mem"
            , "0334be1e766167ca50ac23386019a7199d4104b920eee4a2b3fb709d8e38a177" )
          ; ( "module_export/Hashtbl.remove"
            , "02cd071b617ccc445c91c470f618d105e5966d094edea323efc3a03134942e7d" )
          ; ( "module_export/Hashtbl.set"
            , "727b76361304424162a6d4429b81e4109b719ab0e00ba79735a4c91a32dbbf95" )
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
        , "9b0979dfe9ea5e136b26a324ab4c0cbf35107501919de75d01c072a050f0d825"
        , [ ( "global/array_copy"
            , "326c36574ab0637860405f83de93ae1cc46de5d4773bb6c5ae899820b9847b98" )
          ; ( "global/fail"
            , "0cb59f2292447d4ef3be1a2ea4ddeed8b13982e43df7afd2b5469ce76a356c27" )
          ; ( "global/hash_md5"
            , "41da44cf55bae0214bfc1246fe0894cafd88b18528f1c09e7443542306d4c031" )
          ; ( "global/length"
            , "aeace1ff131b4ee17d25261203e769409faead1f8ceb016c5146c55c7ecada4a" )
          ; ( "global/record_keys"
            , "efbbb1e24fb1713576736aa91c2beba8836307770cbec3ae430f9eee4dcd703a" )
          ; ( "global/string_is_empty"
            , "aa275489b18cef598b4a907ca70ba380ab894b1beb72c895bfe536b8149d2655" )
          ; ( "global/string_length"
            , "8fa44df51298bdbe02e5101299945fc7aaa66ff43c7d002ed99ee1aaf5dc21b7" )
          ; ( "global/swap_ref"
            , "49f6064a50ce43b7554c6bd7104703ef296ac78227ac420543349ec1db5a7c54" )
          ; ( "global/to_string"
            , "4edf84ecbeeb1233ed58c0c79bc6456e841d5a060fa8b56880b47de2f177909c" )
          ; ( "global/variant_tag"
            , "8531aaf9c5bb13b869bf64a5a6e1e6fc765ce939f286963144ff94bdc2017858" )
          ] )
      ; ( "tool_v1"
        , "09dc54ef4d5fe5464cd0d06333ae758e14f27c3af30d7c5a9ff995781a5cf84d"
        , [ ( "global/array_copy"
            , "a853a83afa1c9f7446e785af3bc34d652573e62e51a7350bbf7b4a197cf11893" )
          ; ( "global/fail"
            , "82c4d09469ba872e37f953906236530f127e1debb245b2b4207cdaa4b041741d" )
          ; ( "global/hash_md5"
            , "c3eb0f36573d265dacd55ca075ff34ceff33d302a2b0b067cebb5ab520b33be2" )
          ; ( "global/length"
            , "8368904b15fc8a4b40f90b8e0e67cdbb0da831d38b651abc092820d21d84b93d" )
          ; ( "global/record_keys"
            , "5b2cdb4712a64f62e1dd063565c048bdd948aecf5a0eb3b6aef7277170bc0a40" )
          ; ( "global/string_is_empty"
            , "8f24aa783e6637561c7d6a617ef244388296781ba97e2ca605f77ba49b087253" )
          ; ( "global/string_length"
            , "7c98966606f5bd97aa5baeb1347dfed587e3eef3db267f81ed8c88ea86e448da" )
          ; ( "global/swap_ref"
            , "70a8a3eedcf4d82f7918170d70036bd7ee0a641aa00ce023ebbede032f7b1d08" )
          ; ( "global/to_string"
            , "d168492b867408fd316135febc077c75d6a27322f051eab516a85cf60919ab33" )
          ; ( "global/variant_tag"
            , "97c0ae9a490897032699bc49604a53667792ab832e52fa28595fbd62b865515f" )
          ] )
      ; ( "moderator_v1"
        , "dd7588489044aa53d0c907bc2722971c6b67e70d8e7b0ea8b38df27fd41cb76d"
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
            , "3b18c065f56fe4c9701711f2082ba2e9cf661323e3b1ca839c189fbe1ac79bd0" )
          ; ( "global/string_is_empty"
            , "298e95885e51043d3fef9428f55e52fff5bff2aa0c7393d48081e89f5a801909" )
          ; ( "global/string_length"
            , "b17096c250edd2bbc0ebbfe84fbdd33df14e630f47f692be8647d6da26e8c733" )
          ; ( "global/swap_ref"
            , "676f3d3e98659e6c48288437d9865f7dcca9412d18b7c93d19e750ebfa537f89" )
          ; ( "global/to_string"
            , "f8c225211e90e3e883f1741f4734930ae23fc096c8ef823c4cce086d9d5392bc" )
          ; ( "global/variant_tag"
            , "56997194476c168676fbe8bf5a1a471f170beb71dfb13583aa64191f86d4056f" )
          ] )
      ; ( "delegated_moderator_v1"
        , "43e5d45dd05197a255e9fbe7c767bcb1b30f60c74ee970e5a3226ebac34a21d0"
        , [ ( "global/array_copy"
            , "aa75d71c337e070bee24369a8d14eb038601e57c106751310299f9890875b5e4" )
          ; ( "global/fail"
            , "221b75a5f71e51114075d7ff0344bb96e711bdb4ff4848dd0cef501b9acb9605" )
          ; ( "global/hash_md5"
            , "aafec401a51ce3c607238bf3a3b8854f88db55dc6f9ff64cf585c27a47f87468" )
          ; ( "global/length"
            , "ddac7a36eb0b29756c08c9bbe22ce78df37ee3f3a53384057b309bd40cd10241" )
          ; ( "global/record_keys"
            , "6b0ef823850e8b4b2b1c6da89dc66af42ebf05ce603f7e7a037bdc188dda56a8" )
          ; ( "global/string_is_empty"
            , "0646ea2618ee94ed73cbafe60631d9404d6186bea2f2dc8b88017527e0c780f1" )
          ; ( "global/string_length"
            , "6b9a1bb68fc5e33064099d52c4e83fdb9896f973eebc2804cd8d6d45f1e10c86" )
          ; ( "global/swap_ref"
            , "28868319f208e4940446d8aa0e8053811247438e3f509f74b404b310184f5b24" )
          ; ( "global/to_string"
            , "20d56853e18b107288211b460fa40f234924a9045a27d49abafe27f563860247" )
          ; ( "global/variant_tag"
            , "2b376c415c701069e6333dee9de5de2f93f141fef408f6b8f9d2ea32866aafd5" )
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
        , "9598e35b5722b27e3d9e0fd0b8a79c6069aab8252ac06fd9931d26a8d61fddfd"
        , [ ( "type_alias/json"
            , "de373e490c4ffee533ebbe5ca820e68a85731092a8fbf19a7205108d6ab26a0e" )
          ] )
      ; ( "tool_v1"
        , "0598fd72875d8502f88d4152d83666f7d10edecc175b39a63f7bfbc519ef967b"
        , [ ( "type_alias/json"
            , "fc0bb666163b3ce1934c8ac08c558fb9eaa90c41f08407271b55b76b7c8c226f" )
          ] )
      ; ( "moderator_v1"
        , "7666e108b1139830fea1f55c5858064e945093f96929db3d8253778f2715ae66"
        , [ ( "type_alias/json"
            , "5b5af3a1b53bbccfdfc3ffc34996a6c0e061f45809b1689b0a29fc24a138755a" )
          ] )
      ; ( "delegated_moderator_v1"
        , "1525675290f78af03c14e06661a0d8389ca4e039b16ec5e1cb5b23a3b00501d5"
        , [ ( "type_alias/json"
            , "701bb077522f52d7894c6560b985a967f478a0f343e55f704186a1f32224295f" )
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
        , "d4ce8120be3e86db7402d7219ef6c20d7be6f33c61c726163175867d25b6913e"
        , [ ( "module/Item"
            , "30a1aec7224048beb927ac1cff733daaaebb03fc4144e90f3ec7319bb0694158" )
          ; ( "module_export/Item.assistant_text"
            , "cf578bc994928f4348b8e067b2461572d66e1604ff461e6e37182c5a58244d94" )
          ; ( "module_export/Item.create"
            , "704fda1c71dcb740a91d2fd943aa750353bad90d9c194bacc0726d1778253e15" )
          ; ( "module_export/Item.id"
            , "532ddc88cfd83472978223b9ade524e79f44fbd2249260d69b2d8ac993fafea4" )
          ; ( "module_export/Item.input_text_message"
            , "a4bdfb3ddaf6407d4be3e41b07abe5058bc61da6ed66865294634573d52fcafe" )
          ; ( "module_export/Item.is_assistant"
            , "4e346c0d7de146e1ba9027acc449fedd19f7a4188e87c5df476de4a9afceb52c" )
          ; ( "module_export/Item.is_system"
            , "9e103c00e4484437419546317daf16d06201c9a86fd13175c1677bf17fafa09c" )
          ; ( "module_export/Item.is_tool_call"
            , "15aa85c8320799fc6c30852b5c56d382d02e4dd790eff040f53f27400133d0e1" )
          ; ( "module_export/Item.is_tool_result"
            , "1bcad8669f63f03eb08873a94c1f33a698d8efa692496d36044bcdec9627749c" )
          ; ( "module_export/Item.is_user"
            , "5b5e494465dbcad2a191cd62f796161ae47fe6fa2cefd9d93a71d99352956da6" )
          ; ( "module_export/Item.kind"
            , "767450723e3f4a1268f02be829af3cd499189702e31f516c829a36b4e297fd02" )
          ; ( "module_export/Item.notice"
            , "bb0ad752aeef9745666d7b664c6dd8cfcc161af3f90f266b3c5614e22ce1a694" )
          ; ( "module_export/Item.output_text_message"
            , "10d40acab895a4e2070bf59a793d5c03a54391f48de1cb79e0e19c6d45167dd5" )
          ; ( "module_export/Item.role"
            , "4a2fe4c1f6a687303dc9ccfc96685e33901eae729651b9c225238db52d6c65e8" )
          ; ( "module_export/Item.system_text"
            , "cf8f7f4c56dfb94c0c5349a378b03068076acadbdf70ee15e0d6e98a3c80c6a9" )
          ; ( "module_export/Item.text"
            , "e4253815a8748b57f8a188950d988c76bf4ffdd584e77e88772b0ff4a605aba2" )
          ; ( "module_export/Item.text_parts"
            , "b878659118156cdb333bbf655ae91f5d08cca8016b6bfa593fcdc1dd83705ee6" )
          ; ( "module_export/Item.user_text"
            , "45e6ceae4da7869937c2243b7ee26ad8629f35f1cb4b0d7289294fadf5a1af7b" )
          ; ( "module_export/Item.value"
            , "9cabc262c61b9ba48e9bd55ace070fd562726681337d0bd825eb18cc716e299f" )
          ; ( "module/Context"
            , "043cc243406f1aa4741526787e90f317d226bd14b92760e36ef8a9ba3eef2744" )
          ; ( "module_export/Context.find_item"
            , "a8d0272c2226938cc40ae557c20aadd116b9666b66e553ceca85ae17d64871f3" )
          ; ( "module_export/Context.find_tool"
            , "d7ffe66f65805602fa631ac544bd36f9b26aded3eeb01f7afe207edbeaaab2b2" )
          ; ( "module_export/Context.has_tool"
            , "02ae43507446afe8943d42b9d802cac242b47dd7a917dcb9f968a1a7ae0df3cb" )
          ; ( "module_export/Context.items_by_role"
            , "198343498107c7fa8437f13f274843de60f4f59bed49105bcc37007fa807e6f8" )
          ; ( "module_export/Context.items_since_last_assistant_turn"
            , "58c9a361b0a161c425889c729cd8259b5183ba1950ae146c4ceae8a3c46a3e28" )
          ; ( "module_export/Context.items_since_last_user_turn"
            , "d7b087b72cfd1824dfc8c86e0a9f118e42458ddc9f2d5de408acb7ba0c9ece18" )
          ; ( "module_export/Context.last_assistant_item"
            , "7e56934cf1ce82cda82ee1af9b4981d910338cb8547067bb658d3765500c08ad" )
          ; ( "module_export/Context.last_item"
            , "6e04c6d34aa9ffe8a2d47f9ef45ac86d80d803cdf990e433c625cfc0700facf2" )
          ; ( "module_export/Context.last_system_item"
            , "7e60d76843cc05b63ca6ca2ac8b2d520ee7da5b68e0275093305869bdc5987e4" )
          ; ( "module_export/Context.last_tool_call"
            , "dcefc73376380fbb4202917913a8d7cff86d1f298b78175c0547f562fb82751f" )
          ; ( "module_export/Context.last_tool_result"
            , "efe92cf875383db48ade2f6ab8b41f825a29695ab36d52e32834add52ddca11f" )
          ; ( "module_export/Context.last_user_item"
            , "505edd3235a262b04e00cc926747b1579f117dcdc39fa5c2541506f21f74c7f7" )
          ; ( "module/Tool_call"
            , "111edba6b295387104b0daccf838d3bc4a1bc1d28a33ed408f9e3602bb848389" )
          ; ( "module_export/Tool_call.arg"
            , "ff23aaf75ece3686c3f4c561eb14044cc3b6ef59c380ded64866d5027d0f86c5" )
          ; ( "module_export/Tool_call.arg_array"
            , "1137b0615c993dfe16891439d506ccc5196b8ce288905d1162548a648acb4bc7" )
          ; ( "module_export/Tool_call.arg_bool"
            , "93a8a9a3d182c14d76e892c747abb903e486cf3a74331b298288032b54dced08" )
          ; ( "module_export/Tool_call.arg_string"
            , "cebfd4d7276c5193e6fcdaadc2eb483875eeedfb6caf370fd92c2ac2a7ff4b96" )
          ; ( "module_export/Tool_call.is_named"
            , "4616ee49bf74e25bbbc5c5659d966ba87ddca0ada28e7d7cce71a5762dbd6872" )
          ; ( "module_export/Tool_call.is_one_of"
            , "9a9959bf2f20bb67ce8601f8d6e7093bdb8b1caccf7075984bac6698b68ddcc7" )
          ; ( "type_alias/item"
            , "beef73d64b592da99811f34b299e75303d8c12e97cfb40431abde2b0f9a33c03" )
          ; ( "type_alias/tool_desc"
            , "cd826999c2542ae21c78dcb2940d332aed76840cc9a5f0c65a38b7c5852bb022" )
          ; ( "type_alias/tool_call"
            , "09a1aa6f00a9112f05fb6464a6afd6a61a40044997e6a2250273d03e7acbdb66" )
          ; ( "type_alias/tool_result"
            , "734a1df5a6e56c584f3f4daba9ffbdf3411e38a5d2ec196745a423968e585bad" )
          ; ( "type_alias/context"
            , "a7b9f78044d68da69b89b360bcf237eda4850f54c188c113720e779b1fbfa678" )
          ] )
      ; ( "delegated_moderator_v1"
        , "096df7d0f9d867430390952d1709e1224bbf71396c5780bc23ef2bed40cb3741"
        , [ ( "module/Item"
            , "0d8a88fe4ea744ffef1c1e2c8ae566e17f6f27503da74c9ca8072fbb85f1b612" )
          ; ( "module_export/Item.assistant_text"
            , "3adfc202ac93250c4e07746f32d92c0f2250bb0e09e0d503d1405a5220b27970" )
          ; ( "module_export/Item.create"
            , "c8ba4855c84164e00b135cab440fbd80f9c6f86d84997ca4a2656762bc33f64d" )
          ; ( "module_export/Item.id"
            , "42a186df0cb6277bf66c16281908ff7677035f3eefd636af09a44ba931d26992" )
          ; ( "module_export/Item.input_text_message"
            , "85b0a03f74d1bd46bfea84005c73f2d64747a5753ce50488c18166adbc41ebae" )
          ; ( "module_export/Item.is_assistant"
            , "c2d3d90d5a8853ddb80d3a814b836b1142f2c15b429e3a63cee5119321d03a4c" )
          ; ( "module_export/Item.is_system"
            , "7292d21e3e3ee9d95cbc1388fa588655275dd7a1ba6d2664e49c9ca588d32ff2" )
          ; ( "module_export/Item.is_tool_call"
            , "8900de12fef3a2a92d2682d0d71282985a62c509479bdefa5241eb0ef27eab79" )
          ; ( "module_export/Item.is_tool_result"
            , "8c67babd17c68b9df53e221801471c94e16a7cfbb44688cf914bc764adbe2609" )
          ; ( "module_export/Item.is_user"
            , "a89a1a3c1c013247f66674f8a4785842e457671cf7409fe2f2a1edfedebf1847" )
          ; ( "module_export/Item.kind"
            , "dfd0d585688145e2754307371cfff203ee8615a35a70f83b57877a72e751a7aa" )
          ; ( "module_export/Item.notice"
            , "45029382b3634b9a4549673a0810a80244fe3e327a8e609466b483d2d48e0036" )
          ; ( "module_export/Item.output_text_message"
            , "f3c566ed335cbc5ac0b1c762bc7e98723ab4503681f16a11c462c7bd671fc3a5" )
          ; ( "module_export/Item.role"
            , "841f5b281d54fa62153ad8b1ddd4067d6944d11247a46cd1ea76f2c865d8dafb" )
          ; ( "module_export/Item.system_text"
            , "f0a794bb32a97371b3e07c998accc6dffdd2b80d8555acad35d3acfc75835871" )
          ; ( "module_export/Item.text"
            , "1eec79aa6b59e0c49053f23d53b75d411f47359e542c1bcbe8d266bb31319ef7" )
          ; ( "module_export/Item.text_parts"
            , "f9dc702b51db124dd34fe36dc27b40e630670572d64c46578b4f0b5741f2f282" )
          ; ( "module_export/Item.user_text"
            , "3d22c6642c585b36a0005916d68512637a5740f5a4bb53ccc32e29365e25cb96" )
          ; ( "module_export/Item.value"
            , "e98f64390a63417572f4fb00a3251e7ef272b93b226c8bd8511ab90893a92ce1" )
          ; ( "module/Context"
            , "f504951196837027022c550e85b3f7955f6776d45fe26eb1798afee2014f8ad4" )
          ; ( "module_export/Context.find_item"
            , "f8116d44d87140c57816551319164101fa19a3c0b1a78e0c4cf8fa5b3e421940" )
          ; ( "module_export/Context.find_tool"
            , "f5ab3a9f626b554a8f86ec1e7c2db741bc671c483e9c4ea8d440a4e4308f76d3" )
          ; ( "module_export/Context.has_tool"
            , "09f7ea897c0ba9c71d018f1a1b3859e77e07f7dbb1f49a092c2410f109c7aaf6" )
          ; ( "module_export/Context.items_by_role"
            , "557819a6dd9a90f6b30dc4f16393d89a7717b041cf6d71f70fb737db69febf83" )
          ; ( "module_export/Context.items_since_last_assistant_turn"
            , "05bb2547ea71936f43f55a780cfdc518e0a8bbd8df324cae843e1a6846458231" )
          ; ( "module_export/Context.items_since_last_user_turn"
            , "a43bb672ac3f778455f93616ce562c88229e727521f7e897831a4d8cb5dedf2c" )
          ; ( "module_export/Context.last_assistant_item"
            , "c04fdd7c7c8c584bbd182083e465615ec5faeb46e7cb9a00ae7faa63c71ef1f9" )
          ; ( "module_export/Context.last_item"
            , "bfdb43de466793fd74614563a25440977fab92f6f7bb1e77712ec22458015043" )
          ; ( "module_export/Context.last_system_item"
            , "d078a6428444090b671728ebefda3e2ff792b437a58de7ba5baeaf3c94a4d714" )
          ; ( "module_export/Context.last_tool_call"
            , "e315beee6f8bb3bee7c740b26c18b989b318a273c826752b55c7d7d4df5479d4" )
          ; ( "module_export/Context.last_tool_result"
            , "f0ec9f7c7b8dd0cacba71f60bde816331c6c4e413dd09fc04387b1bbb3acae5f" )
          ; ( "module_export/Context.last_user_item"
            , "6275e676a682560182f1422c153263ae25430fc115a47cdb9898e1c1276bb241" )
          ; ( "module/Tool_call"
            , "b499b0cb8501b221b1099eeac7c45a8a07f475ae341c7ceee839d74e574ce6a3" )
          ; ( "module_export/Tool_call.arg"
            , "bbe41b104a4ccfd15bdb2ade37884353286b168b3b125ab2b6e57c45725e5584" )
          ; ( "module_export/Tool_call.arg_array"
            , "ba41adb1a59a64c021838b87ac4ff19f62b3562eda465772b4e1ac0bea52f1e3" )
          ; ( "module_export/Tool_call.arg_bool"
            , "d48e0b051210a1d6c1c9c49797c1a9de92dd4a13c57c6e69f889e5e1c47f3a6f" )
          ; ( "module_export/Tool_call.arg_string"
            , "34cccc4e251168b7aa03fd465bd7187b6668a829262c87468d0a601ae8f612ff" )
          ; ( "module_export/Tool_call.is_named"
            , "4112e3c9d65d86b7d344bdd921b40c84a2ee498cd40754eff7d17e8ed54275ab" )
          ; ( "module_export/Tool_call.is_one_of"
            , "7fd2a29ce7d0113d2fc3ef2e28b19860b7d62c2eb57bdc612ef8e8806e8a58b7" )
          ; ( "type_alias/item"
            , "628df52e13e1d7e441045a492cf29eb5b4c1dbe73737267d69cd32c06d3e0f61" )
          ; ( "type_alias/tool_desc"
            , "cdb74ea1a94bc0ec5d130f01d7ab658a62ce64a9c8fef489535e876659769af8" )
          ; ( "type_alias/tool_call"
            , "30fd3f543f345e877ba33d259e73f1c9dbe3bf35baf625370ea406a9ad07cbd0" )
          ; ( "type_alias/tool_result"
            , "de8443bb4f09b27bfc6695008ed0cfbd6ee20d3a07f9b78a755eb91ff62015c1" )
          ; ( "type_alias/context"
            , "31a2f45106393766153ef623ff3c658b50c7cbe14d8fb6bcbac1e31fb1da3cf8" )
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
        , "4a94e46ff3a00587ead7baffae617d869e8ed2bfaa26a43b026eeefc221d5aa1"
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
            , "41a0b2a8fb0ba5500b2ca8c569740450f53f9a5f22b69f5ce2fcd912650f3a54" )
          ; ( "module_export/Tool.spawn"
            , "028ac3d67d7024a24bc0a593c101802600a2c3f3e59751c6da738f0ec64c59fe" )
          ] )
      ; ( "tool_v1"
        , "74b032714520fe0d50a1169893fed4f9e614e1c2e5cbe348952d8a474da0a2d1"
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
            , "bb611c148b20cc79a98f50f5ab0281eebadb596f6fda6a6754e1a288717beec9" )
          ; ( "module_export/Tool.spawn"
            , "6651cd9b8567afaf2ecee63645f00d82dc79c60e596ad044497f561cef67de82" )
          ] )
      ; ( "moderator_v1"
        , "456ffbd37f6614a15fe6a97fb7e4ec9ae5e31cec1eb739ce041cfac2349f2990"
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
            , "8923978bf683b1c9d0e0a2161a7a44d152395aa424386fa539699261f714f44f" )
          ; ( "module_export/Tool.redirect"
            , "1e7e84a8cfeb56b97b6989eafb673b37f05c07adec9b02003d132a6cc097d2b9" )
          ; ( "module_export/Tool.reject"
            , "0ca7d1cf1b0bef06206b900f5162bdf6d85396fdcff95558ce8b9ac931583405" )
          ; ( "module_export/Tool.rewrite_args"
            , "dde819fe1642cfa51d883461ab45785da9b54aee41fa4ea616f7ef421fa3f685" )
          ; ( "module_export/Tool.spawn"
            , "cba9420e9726709ff64b5c8f317f7981e9d713ce7a9b8668de79727f14cd88c4" )
          ; ( "module/Turn"
            , "1e28ee53d9b5bda92bb82bfd26e3dc56a67c111903f83c794f5c7339273a93e2" )
          ; ( "module_export/Turn.append_item"
            , "9d69d90e9542c89e441b0727385b98c0dedc4e0643b3e7d5891d1d5e786fa271" )
          ; ( "module_export/Turn.append_message"
            , "7ee8aadbc7899f4cc96035b6c0a18bd59f30479b1b139f62b2ee5c7c49fb7723" )
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
            , "77ce43494aebf37706d4b95b6bf8c19ec614e4dbc40abb25cdb9b03d78825f20" )
          ; ( "module_export/Turn.replace_message"
            , "f1eac988c6e2cc8f6a1803934e4460d9e352efe554f63934eebde9b43f4411a9" )
          ; ( "module_export/Turn.replace_or_append"
            , "fcae7de888d753e97b1b01203e8f0917fc035971bb5c3bf72755f732399bc89b" )
          ] )
      ; ( "delegated_moderator_v1"
        , "ddb43c2be5935284b28640c84310057c7d4961b708866b761577fa7fee954088"
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
            , "7169f22230525f7f19abf17c754acdc3943adfe2b5726c3f9dc86bf777582351" )
          ; ( "module_export/Tool.redirect"
            , "0adcd86df6f82772c6df514c462bf1ef80a393342e421fa31cfa59b2af618af4" )
          ; ( "module_export/Tool.reject"
            , "750429786a525fa4af39e96dfc60cd329fba459a051d92a306676ed72abee88d" )
          ; ( "module_export/Tool.rewrite_args"
            , "48de739642f0b3766d6cc1e394b823dbda77ccbb09ceb8418a80db053cf61c60" )
          ; ( "module_export/Tool.spawn"
            , "d53ad5b4b7915ae0d6dd6761531ed7d2305de3a126667cafe392a3a3c9e2eaf9" )
          ; ( "module/Turn"
            , "e0a0406931a6c5156b39ca4e43ab61584afc884027deec39c42002c3345b8d92" )
          ; ( "module_export/Turn.append_item"
            , "411ab5f40b3343b1ab55b0ca8e7ecdc0fdb4f1ffb7e6a701ec27ea8446b963b8" )
          ; ( "module_export/Turn.append_message"
            , "ac59a05c1a1cc55aa536bd8c0be8d8a75f9c28ad6e1464e1f17bf066b2a2f6f5" )
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
            , "7725ec529310ecf86d92248b72fe9044b3f61f9dea46b05602d70e920990568c" )
          ; ( "module_export/Turn.replace_message"
            , "c3f79c8ebdaede49de7298809037c69b6a0905d4ef6d016264f4e95fda09f2c9" )
          ; ( "module_export/Turn.replace_or_append"
            , "fb7e721dff88bae60309e2f17de79c3a85b7bd5812d9a6d50e4f465d4a7057c0" )
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

  let reviewed_mappings =
    entrypoint_mappings
    @ task_mappings
    @ string_mappings
    @ array_mappings
    @ option_mappings
    @ json_mappings
    @ hashtbl_mappings
    @ global_mappings
    @ json_alias_mappings
    @ moderator_data_mappings
    @ host_effect_mappings
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
                   [ "7fb6ad97910adb04e42e441f47aec76109e2a938c346cd8313740aeb0f03ca04" ]
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
                   [ "b0fc09f0f4247b576a29c878c684c8d552edebf09f97ee6ea3edb765320568a9" ]
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
                   [ "dc48ddcc95daa0af13600e6864719db323ede7644f3f282eac6e6b1e1ed50cea" ]
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
