open! Core
module Runtime = Chatml_host_runtime
module Surface = Chatml.Chatml_extension_surface
module Codec = Chatml.Chatml_value_codec

let fixture = "docs-src/guide/chatml-ocaml-differences.md"
let marker = "<!-- ochat-authoring-example: "
let fail id message = failwith (sprintf "%s [%s]: %s" fixture id message)

type expectation =
  | Result of Jsonaf.t
  | Diagnostic of
      { stage : Runtime.compilation_stage
      ; contains : string
      ; has_span : bool
      }

type example =
  { id : string
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
  (match string_exn id fields "surface" with
   | "one_off_v1" -> ()
   | other -> fail id ("unsupported example surface: " ^ other));
  let expectation =
    match List.Assoc.find fields "result" ~equal:String.equal with
    | Some result ->
      exact_fields_exn id fields [ "id"; "surface"; "result" ];
      Result result
    | None ->
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
  id, expectation
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
      let id, expectation = metadata_exn line in
      (match rest with
       | "```ocaml" :: rest ->
         let source, rest = take_code id [] rest in
         scan ({ id; expectation; source } :: reversed) rest
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

let check_exn { id; expectation; source } =
  let compiled =
    Runtime.compile_script_detailed
      ~surface:Surface.one_off_v1
      ~required_bindings:Surface.one_off_entrypoints
      ~source
      ()
  in
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
  | Result _, Error diagnostic -> fail id diagnostic.formatted
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

let run env root =
  let installed = Authoring_sources.installed () |> Result.ok_or_failwith in
  List.iter (Authoring_sources.documents installed) ~f:(fun document ->
    let authored =
      Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / "docs-src" / document.path)
    in
    match String.equal authored document.text with
    | true -> ()
    | false ->
      fail document.path "installed source differs from shared human documentation");
  let examples =
    Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / fixture) |> examples_exn
  in
  List.iter examples ~f:check_exn;
  Eio.Flow.copy_string
    (sprintf
       "ChatML authoring reference: %d compiler/behavior examples PASS\n"
       (List.length examples))
    (Eio.Stdenv.stdout env)
;;
