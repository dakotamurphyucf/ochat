open! Core
module Runtime = Chatml_host_runtime
module Surface = Chatml.Chatml_extension_surface
module Codec = Chatml.Chatml_value_codec

let fixture = "docs-src/guide/chatml-ocaml-differences.md"
let marker = "<!-- ochat-authoring-example: "
let runtime_fixture = "docs-src/guide/chatml-authoring-runtime.md"
let background_fixture = "docs-src/guide/chatml-authoring-background.md"
let language_fixture = "docs-src/guide/chatml-authoring-language.md"
let fail id message = failwith (sprintf "ChatML authoring reference [%s]: %s" id message)

type target =
  | One_off
  | Standalone
  | Moderator
  | Delegated
[@@deriving compare]

let target_exn id = function
  | "one_off_v1" -> One_off
  | "tool_v1" -> Standalone
  | "moderator_v1" -> Moderator
  | "delegated_moderator_v1" -> Delegated
  | other -> fail id ("unsupported example surface: " ^ other)
;;

let contract = function
  | One_off -> Surface.one_off_v1, Surface.one_off_entrypoints
  | Standalone -> Surface.tool_v1, Surface.tool_entrypoints
  | Moderator -> Surface.moderator_v1, Surface.moderator_entrypoints
  | Delegated -> Surface.delegated_moderator_v1, Surface.moderator_entrypoints
;;

type expectation =
  | Result of Jsonaf.t
  | Fixture of
      { path : string
      ; also_check : target list
      }
  | Diagnostic of
      { stage : Runtime.compilation_stage
      ; contains : string
      ; has_span : bool
      }

type example =
  { id : string
  ; target : target
  ; expectation : expectation
  ; source : string
  }

let fields_exn id = function
  | `Object fields ->
    (match List.find_a_dup (List.map fields ~f:fst) ~compare:String.compare with
     | None -> fields
     | Some name -> fail id ("duplicate metadata field: " ^ name))
  | _ -> fail id "expected a metadata object"
;;

let string_exn id fields name =
  match List.Assoc.find fields name ~equal:String.equal with
  | Some (`String value) when not (String.is_empty value) -> value
  | _ -> fail id ("expected nonempty string field: " ^ name)
;;

let exact_fields_exn id fields names =
  let actual = List.map fields ~f:fst |> List.sort ~compare:String.compare in
  let expected = List.sort names ~compare:String.compare in
  match List.equal String.equal actual expected with
  | true -> ()
  | false ->
    fail id ("expected exactly these metadata fields: " ^ String.concat ~sep:", " names)
;;

let metadata_exn line =
  let json =
    String.chop_prefix_exn line ~prefix:marker
    |> fun text -> String.chop_suffix_exn text ~suffix:" -->" |> Jsonaf.of_string
  in
  let fields = fields_exn "metadata" json in
  let id = string_exn "metadata" fields "id" in
  let target = target_exn id (string_exn id fields "surface") in
  let expectation =
    match
      ( List.Assoc.find fields "result" ~equal:String.equal
      , List.Assoc.find fields "fixture" ~equal:String.equal )
    with
    | Some result, None ->
      exact_fields_exn id fields [ "id"; "surface"; "result" ];
      (match target with
       | One_off -> ()
       | _ -> fail id "pure result fixtures must use the one-off contract");
      Result result
    | None, Some (`String path)
      when String.is_prefix path ~prefix:"test/chatml_extensibility_fixtures/"
           && String.is_suffix path ~suffix:".chatml"
           && not (List.exists (String.split path ~on:'/') ~f:(String.equal "..")) ->
      let also_check =
        match List.Assoc.find fields "also_check" ~equal:String.equal with
        | None ->
          exact_fields_exn id fields [ "id"; "surface"; "fixture" ];
          []
        | Some (`Array (_ :: _ as values)) ->
          exact_fields_exn id fields [ "id"; "surface"; "fixture"; "also_check" ];
          List.map values ~f:(function
            | `String name -> target_exn id name
            | _ -> fail id "also_check must contain surface IDs")
        | _ -> fail id "also_check must be a nonempty surface list"
      in
      (match List.find_a_dup (target :: also_check) ~compare:compare_target with
       | None -> ()
       | Some _ -> fail id "duplicate example surface");
      Fixture { path; also_check }
    | _, Some _ -> fail id "invalid or conflicting source fixture metadata"
    | None, None ->
      exact_fields_exn id fields [ "id"; "surface"; "stage"; "contains"; "span" ];
      let stage =
        match string_exn id fields "stage" with
        | "parse" -> Runtime.Parse
        | "typecheck" -> Runtime.Typecheck
        | other -> fail id ("unknown diagnostic stage: " ^ other)
      in
      let has_span =
        match List.Assoc.find fields "span" ~equal:String.equal with
        | Some `True -> true
        | Some `False -> false
        | _ -> fail id "expected boolean span field"
      in
      Diagnostic { stage; contains = string_exn id fields "contains"; has_span }
  in
  id, target, expectation
;;

let examples_exn text =
  let rec take_code id reversed = function
    | [] -> fail id "unclosed code fence"
    | "```" :: rest -> String.concat ~sep:"\n" (List.rev reversed), rest
    | line :: rest -> take_code id (line :: reversed) rest
  in
  let rec scan reversed = function
    | [] -> List.rev reversed
    | line :: rest when String.is_prefix line ~prefix:marker ->
      let id, target, expectation = metadata_exn line in
      (match rest with
       | "```ocaml" :: rest ->
         let source, rest = take_code id [] rest in
         scan ({ id; target; expectation; source } :: reversed) rest
       | _ -> fail id "metadata must immediately precede an ocaml code fence")
    | line :: _ when String.is_prefix line ~prefix:"```" ->
      fail "metadata" "every code fence in this reference must have example metadata"
    | _ :: rest -> scan reversed rest
  in
  let examples = scan [] (String.split_lines text) in
  (match
     List.find_a_dup (List.map examples ~f:(fun e -> e.id)) ~compare:String.compare
   with
   | Some id -> fail id "duplicate example ID"
   | None -> ());
  match examples with
  | [] -> fail "metadata" "reference has no checked examples"
  | _ -> examples
;;

let check_exn env root { id; target; expectation; source } =
  (match expectation with
   | Fixture { path; also_check } ->
     let original = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / path) in
     if not (String.equal (String.rstrip original) (String.rstrip source))
     then fail id ("displayed example differs from its integration fixture: " ^ path);
     List.iter also_check ~f:(fun target ->
       let surface, required_bindings = contract target in
       match Runtime.compile_script_detailed ~surface ~required_bindings ~source () with
       | Ok _ -> ()
       | Error diagnostic -> fail id ("additional surface: " ^ diagnostic.formatted))
   | Result _ | Diagnostic _ -> ());
  let surface, required_bindings = contract target in
  let compiled = Runtime.compile_script_detailed ~surface ~required_bindings ~source () in
  match expectation, compiled with
  | Diagnostic { stage; contains; has_span }, Error diagnostic ->
    (match
       Runtime.equal_compilation_stage stage diagnostic.stage
       && String.is_substring diagnostic.message ~substring:contains
       && Bool.equal has_span (Option.is_some diagnostic.span)
     with
     | true -> ()
     | false -> fail id ("unexpected diagnostic:\n" ^ diagnostic.formatted))
  | Diagnostic _, Ok _ -> fail id "expected compilation to reject this example"
  | (Result _ | Fixture _), Error diagnostic -> fail id diagnostic.formatted
  | Fixture _, Ok _ -> ()
  | Result expected, Ok compiled ->
    (* Deliberately install no operations. These are pure language examples;
       accidental host calls must fail, never access a tool or a provider. *)
    let config : Runtime.runtime_config =
      { surface = Surface.one_off_v1; operations = [] }
    in
    let result =
      Runtime.run_entrypoint
        ~limits:{ fuel = 10_000; max_tasks = 1_000 }
        config
        compiled
        ~entrypoint:"main"
        ~arguments:[ Codec.jsonaf_to_value `Null ]
        ()
      |> Result.bind ~f:Codec.value_to_jsonaf_result
    in
    (match result with
     | Error message -> fail id ("execution failed: " ^ message)
     | Ok actual ->
       (* Normalize numeric spellings through the same JSON codec, so 3 and
          the runtime's 3.0 compare as the same documented JSON number. *)
       let normalize json =
         Codec.jsonaf_to_value json |> Codec.value_to_jsonaf_exn |> Jsonaf.to_string
       in
       (match String.equal (normalize expected) (normalize actual) with
        | true -> ()
        | false ->
          fail
            id
            (sprintf
               "expected %s, got %s"
               (Jsonaf.to_string expected)
               (Jsonaf.to_string actual))))
;;

let check_topic_coverage corpus ~path ~text =
  let topic_text =
    Authoring_corpus.topics corpus
    |> List.concat_map ~f:(fun topic -> topic.Authoring_corpus.fragments)
    |> List.filter ~f:(fun fragment -> String.equal fragment.source.path path)
    |> List.map ~f:(fun fragment ->
      match String.substr_index text ~pattern:fragment.text with
      | None -> fail "topic coverage" ("topic is outside its checked guide: " ^ path)
      | Some position -> position, fragment.text)
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> List.map ~f:snd
    |> String.concat
  in
  match String.equal text topic_text with
  | true -> ()
  | false ->
    fail
      "topic coverage"
      ("topics must partition their entire checked guide without gaps or duplication: "
       ^ path)
;;

let run env root =
  let installed = Authoring_sources.installed () |> Result.ok_or_failwith in
  let corpus =
    Authoring_corpus.runtime_foundation ~sources:installed |> Result.ok_or_failwith
  in
  let unmapped =
    List.filter_map Chat_response.Authoring_validation.topics ~f:(fun (id, path) ->
      match Authoring_corpus.topic corpus ~id with
      | Error _ -> Some id
      | Ok topic ->
        (match
           List.exists topic.fragments ~f:(fun fragment ->
             String.equal fragment.source.path path)
         with
         | true -> None
         | false ->
           fail
             id
             "validation topic points at a different source than the installed corpus"))
  in
  (match unmapped with
   | [] -> ()
   | _ ->
     fail
       "validation routing"
       ("missing installed validation topics: " ^ String.concat ~sep:", " unmapped));
  List.iter (Authoring_sources.documents installed) ~f:(fun document ->
    let authored =
      Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / "docs-src" / document.path)
    in
    match String.equal authored document.text with
    | true -> ()
    | false ->
      fail document.path "installed source differs from shared human documentation");
  let examples =
    List.concat_map
      [ fixture; runtime_fixture; background_fixture; language_fixture ]
      ~f:(fun file ->
        let text = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file) in
        let path = String.chop_prefix_exn file ~prefix:"docs-src/" in
        check_topic_coverage corpus ~path ~text;
        examples_exn text)
  in
  (match
     List.find_a_dup
       (List.map examples ~f:(fun example -> example.id))
       ~compare:String.compare
   with
   | None -> ()
   | Some id -> fail id "duplicate ID across authoring guides");
  List.iter examples ~f:(check_exn env root);
  let integration_count =
    List.count examples ~f:(fun example ->
      match example.expectation with
      | Fixture _ -> true
      | _ -> false)
  in
  Eio.Flow.copy_string
    (sprintf
       "ChatML authoring reference: %d language checks, %d integration-source/contract \
        checks PASS\n"
       (List.length examples - integration_count)
       integration_count)
    (Eio.Stdenv.stdout env)
;;
