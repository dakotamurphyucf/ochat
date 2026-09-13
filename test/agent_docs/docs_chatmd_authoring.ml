open Core
module V = Chat_response.Authoring_validation
module D = Docs_chatml_authoring

let guide = "guide/chatmd-authoring-definitions.md"

type expectation =
  | Valid
  | Rejected of
      { code : string
      ; contains : string
      }

type example =
  { id : string
  ; tools : string list
  ; expectation : expectation
  ; source : string
  }

let metadata line =
  let json =
    String.chop_prefix_exn line ~prefix:D.marker
    |> fun text -> String.chop_suffix_exn text ~suffix:" -->" |> Jsonaf.of_string
  in
  let fields = D.fields_exn "ChatMD metadata" json in
  let id = D.string_exn "ChatMD metadata" fields "id" in
  (match D.string_exn id fields "surface" with
   | "generated_chatmd" -> ()
   | _ -> D.fail id "ChatMD examples must use the generated validation surface");
  let expectation =
    match Jsonaf.member_exn "diagnostic" json with
    | `Null ->
      D.exact_fields_exn id fields [ "id"; "surface"; "tools"; "diagnostic" ];
      Valid
    | diagnostic ->
      D.exact_fields_exn id fields [ "id"; "surface"; "tools"; "stage"; "diagnostic" ];
      (match D.string_exn id fields "stage" with
       | "validation" -> ()
       | _ -> D.fail id "expected validation rejection stage");
      let fields = D.fields_exn id diagnostic in
      D.exact_fields_exn id fields [ "code"; "contains" ];
      Rejected
        { code = D.string_exn id fields "code"
        ; contains = D.string_exn id fields "contains"
        }
  in
  let tools =
    Jsonaf.member_exn "tools" json |> Jsonaf.list_exn |> List.map ~f:Jsonaf.string_exn
  in
  id, tools, expectation
;;

let examples text =
  let rec source id reversed = function
    | [] -> D.fail id "unclosed ChatMD example"
    | "```" :: rest -> String.concat ~sep:"\n" (List.rev reversed), rest
    | line :: rest -> source id (line :: reversed) rest
  in
  let rec scan reversed = function
    | [] -> List.rev reversed
    | line :: rest when String.is_prefix line ~prefix:D.marker ->
      let id, tools, expectation = metadata line in
      (match rest with
       | "```xml" :: rest ->
         let source, rest = source id [] rest in
         scan ({ id; tools; expectation; source } :: reversed) rest
       | _ -> D.fail id "metadata must immediately precede an xml code fence")
    | line :: _ when String.is_prefix line ~prefix:"```" ->
      D.fail "ChatMD metadata" "every ChatMD guide fence must be checked"
    | _ :: rest -> scan reversed rest
  in
  let examples = scan [] (String.split_lines text) in
  (match examples with
   | [] -> D.fail "ChatMD metadata" "guide has no examples"
   | _ -> ());
  (match
     List.find_a_dup
       (List.map examples ~f:(fun example -> example.id))
       ~compare:String.compare
   with
   | None -> ()
   | Some id -> D.fail id "duplicate ChatMD example");
  examples
;;

let run env root =
  let sources = Authoring_sources.installed () |> Result.ok_or_failwith in
  let corpus = Authoring_corpus.runtime_foundation ~sources |> Result.ok_or_failwith in
  let text = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / "docs-src" / guide) in
  D.check_topic_coverage corpus ~path:guide ~text;
  let examples = examples text in
  let capabilities = Docs_child_authoring.fixture_capabilities () in
  let host =
    V.create_host
      ~runtime_identity:"chatmd-documentation-check-v1"
      ~targets:[ Generated_chatmd ]
      ~moderator_surface:Delegated
      ~compilation:Chatml_compilation.default_limits
    |> Result.ok_or_failwith
  in
  List.iter examples ~f:(fun example ->
    let request =
      `Object
        [ "version", `Number "1"
        ; "target", `String "generated_chatmd"
        ; "root_file", `String "agent.chatmd"
        ; ( "sources"
          , `Array
              [ `Object [ "path", `String "agent.chatmd"; "text", `String example.source ]
              ] )
        ; "tools", `Array (List.map example.tools ~f:(fun name -> `String name))
        ]
    in
    let report = V.validate ~env ~host ~capabilities request in
    let result = V.to_json report in
    let fail () =
      D.fail example.id ("unexpected validation report: " ^ Jsonaf.to_string result)
    in
    match example.expectation, V.valid report with
    | Valid, true -> ()
    | Rejected { code; contains }, false ->
      (match
         List.exists report.diagnostics ~f:(fun issue ->
           String.equal issue.diagnostic.code code
           && String.is_substring issue.diagnostic.message ~substring:contains
           && List.mem issue.topic_ids "chatmd.definitions" ~equal:String.equal)
       with
       | true -> ()
       | false -> fail ())
    | _ -> fail ());
  List.iter
    (Authoring_corpus.Coverage.declaration_features
     @ Authoring_corpus.Coverage.native_features)
    ~f:(fun feature ->
      let closure =
        Authoring_corpus.assemble
          corpus
          ~surface_id:"delegated_moderator_v1"
          ~roots:[ feature.topic_id ]
        |> Result.ok_or_failwith
        |> List.concat_map ~f:(fun topic -> topic.Authoring_corpus.fragments)
        |> List.concat_map ~f:(fun fragment -> String.split_lines fragment.text)
        |> List.filter_map ~f:(fun line ->
          match String.is_prefix line ~prefix:D.marker with
          | false -> None
          | true ->
            String.chop_prefix_exn line ~prefix:D.marker
            |> fun text ->
            String.chop_suffix_exn text ~suffix:" -->"
            |> Jsonaf.of_string
            |> Jsonaf.member_exn "id"
            |> Jsonaf.string_exn
            |> Option.some)
      in
      List.iter feature.evidence ~f:(fun reference ->
        match String.is_prefix reference ~prefix:"test/" with
        | true ->
          let source = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / reference) in
          if String.is_empty source then D.fail feature.id ("empty evidence: " ^ reference)
        | false ->
          (match
             List.mem closure reference ~equal:String.equal
             && List.exists examples ~f:(fun example -> String.equal example.id reference)
           with
           | true -> ()
           | false -> D.fail feature.id ("missing checked ChatMD example: " ^ reference))));
  Eio.Flow.copy_string
    (sprintf
       "ChatMD authoring reference: %d generated-definition examples, including \
        admission failures and inert initialization PASS (offline)\n"
       (List.length examples))
    (Eio.Stdenv.stdout env)
;;
