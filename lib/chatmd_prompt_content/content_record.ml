open Core
module B = Content_budget
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Occurrence = Prompt.Chat_markdown.Document_occurrence
module R = Content_rendered
module Member = Source.Portable_path

module Kind = struct
  type t =
    | Dynamic
    | Captured of
        { provenance : R.Provenance.t
        ; input_digest : string
        ; input_bytes : int
        ; output_digest : string
        ; contents : string
        }
end

type t =
  { key : string
  ; root : Input.t
  ; range : Input.Range.t
  ; ordinal : int
  ; src_digest : string
  ; src_bytes : int
  ; local : bool
  ; cleanup_html : bool
  ; markdown : bool
  ; kind : Kind.t
  }

let key t = t.key
let root t = t.root
let input t = Input.Range.input t.range
let range t = t.range
let ordinal t = t.ordinal
let src_digest t = t.src_digest
let src_bytes t = t.src_bytes
let local t = t.local
let cleanup_html t = t.cleanup_html
let markdown t = t.markdown
let kind t = t.kind

let input_key input ~budget =
  let open Result.Let_syntax in
  let%bind _ =
    Member.create (Input.path input)
    |> Result.map_error ~f:(fun _ -> B.Error.invalid "nonportable content parser input")
  in
  let%bind () = B.work budget (String.length (Input.path input) + 512) in
  Ok (Input.identity_digest input ~checkpoint:(fun () -> B.checkpoint budget))
;;

let input_equal left right ~budget =
  let open Result.Let_syntax in
  (* Range/Input affinity deliberately includes owner identity. A shared immutable
     owner is already the same revision; distinct owners still compare exact bytes. *)
  if phys_equal left right
  then Result.map (B.work budget 1) ~f:(fun () -> true)
  else (
    let%bind left_key = input_key left ~budget in
    let%bind right_key = input_key right ~budget in
    if not (String.equal left_key right_key)
    then Ok false
    else (
      let%bind authored =
        B.equal_bytes budget (Input.authored left) (Input.authored right)
      in
      let%map effective =
        B.equal_bytes budget (Input.effective left) (Input.effective right)
      in
      authored && effective))
;;

let valid_digest text = Result.is_ok (Chatml_source.Digest.of_hex text)

let create
      ~root
      ~range
      ~ordinal
      ~src_digest
      ~src_bytes
      ~local
      ~cleanup_html
      ~markdown
      ~kind
      ~budget
  =
  let open Result.Let_syntax in
  let invalid reason = Error (B.Error.invalid ~range reason) in
  let%bind () = B.work budget 1 in
  let%bind () =
    if
      (not (Input.Role.equal (Input.role root) Root))
      || ordinal < 0
      || src_bytes < 0
      || src_bytes > Input.effective_byte_length (Input.Range.input range)
      || not (valid_digest src_digest)
    then invalid "invalid stored document occurrence"
    else Ok ()
  in
  let%bind root_key = input_key root ~budget in
  let%bind input_key = input_key (Input.Range.input range) ~budget in
  let%bind input_bytes, output_bytes =
    match kind with
    | Kind.Dynamic -> Ok (0, 0)
    | Captured { provenance; input_digest; input_bytes; output_digest; contents } ->
      if
        input_bytes < 0
        || (not (valid_digest input_digest))
        || not (valid_digest output_digest)
      then invalid "invalid rendered provenance"
      else (
        let%bind () =
          match provenance with
          | R.Provenance.Local _ ->
            if local then Ok () else invalid "captured local document requires local flag"
          | Builtin id ->
            let%bind _ =
              Builtin_instructions.Id.of_string id
              |> Result.map_error ~f:(fun _ ->
                B.Error.invalid ~range "invalid stored builtin identifier")
            in
            let reference = "builtin:" ^ id in
            let%bind actual = B.hash budget reference in
            if String.equal actual src_digest && String.length reference = src_bytes
            then Ok ()
            else invalid "builtin selector digest mismatch"
        in
        if
          input_bytes
          > B.Limits.input_bytes (B.limits budget)
            - B.Accounting.input_bytes (B.accounting budget)
          || String.length contents
             > B.Limits.output_bytes (B.limits budget)
               - B.Accounting.output_bytes (B.accounting budget)
        then invalid "content aggregate byte limit"
        else (
          let%bind actual = B.hash budget contents in
          if String.equal actual output_digest
          then Ok (input_bytes, String.length contents)
          else invalid "rendered output digest mismatch"))
  in
  let%bind () =
    B.add
      budget
      (B.Accounting.create
         ~occurrences:1
         ~input_bytes
         ~output_bytes
         ~work_steps:0
         ~nodes:0)
  in
  (* All fields have admitted fixed bounds; length prefixes avoid ambiguity. *)
  let fields =
    [ "ochat.prompt-document-slot.v1"
    ; root_key
    ; input_key
    ; Int.to_string (Input.Range.start_byte range)
    ; Int.to_string (Input.Range.end_byte range)
    ; Int.to_string ordinal
    ]
  in
  let encoded =
    String.concat
      (List.map fields ~f:(fun x -> Int.to_string (String.length x) ^ ":" ^ x))
  in
  let%map key = B.hash budget encoded in
  { key
  ; root
  ; range
  ; ordinal
  ; src_digest
  ; src_bytes
  ; local
  ; cleanup_html
  ; markdown
  ; kind
  }
;;

let matches t ~root occurrence ~budget =
  let open Result.Let_syntax in
  let%bind () = B.work budget 1 in
  let range = Occurrence.range occurrence in
  if
    t.ordinal <> Occurrence.ordinal occurrence
    || (not (Bool.equal t.local (Occurrence.local occurrence)))
    || (not (Bool.equal t.cleanup_html (Occurrence.cleanup_html occurrence)))
    || (not (Bool.equal t.markdown (Occurrence.markdown occurrence)))
    || Input.Range.start_byte t.range <> Input.Range.start_byte range
    || Input.Range.end_byte t.range <> Input.Range.end_byte range
    || t.src_bytes <> String.length (Occurrence.src occurrence)
  then Ok false
  else (
    let%bind root_equal = input_equal t.root root ~budget in
    if not root_equal
    then Ok false
    else (
      let%bind input_equal = input_equal (input t) (Input.Range.input range) ~budget in
      if not input_equal
      then Ok false
      else (
        let%bind src = B.hash budget (Occurrence.src occurrence) in
        if not (String.equal src t.src_digest)
        then Ok false
        else (
          match t.kind, Occurrence.selection occurrence with
          | Dynamic, Dynamic -> Ok true
          | Captured { provenance = Builtin expected; _ }, Builtin actual ->
            Ok (String.equal expected actual)
          | Captured { provenance = Local member; _ }, Local_capture ->
            let%bind base =
              Member.create (Input.path (Input.Range.input range))
              |> Result.map_error ~f:(fun _ ->
                B.Error.invalid ~range "nonportable document declaring input")
            in
            let%bind () =
              B.work
                budget
                (String.length (Occurrence.src occurrence)
                 + String.length (Member.to_string base))
            in
            (match
               Source_loader.resolve_member
                 base
                 ~reference:(Occurrence.src occurrence)
                 ~max_reference_bytes:1024
                 ~checkpoint:(fun () -> B.checkpoint budget)
             with
             | Ok expected -> Ok (Member.equal member expected)
             | Error _ -> Ok false)
          | Dynamic, (Local_capture | Builtin _)
          | Captured _, Dynamic
          | Captured { provenance = Builtin _; _ }, Local_capture
          | Captured { provenance = Local _; _ }, Builtin _ -> Ok false))))
;;

let equal left right ~budget =
  let open Result.Let_syntax in
  let%bind () = B.work budget 1 in
  if
    (not
       (String.equal left.key right.key && String.equal left.src_digest right.src_digest))
    || left.src_bytes <> right.src_bytes
    || (not (Bool.equal left.local right.local))
    || (not (Bool.equal left.cleanup_html right.cleanup_html))
    || not (Bool.equal left.markdown right.markdown)
  then Ok false
  else (
    let%bind root = input_equal left.root right.root ~budget in
    let%bind input = input_equal (input left) (input right) ~budget in
    if not (root && input)
    then Ok false
    else (
      match left.kind, right.kind with
      | Kind.Dynamic, Dynamic -> Ok true
      | Captured l, Captured r ->
        let provenance =
          match l.provenance, r.provenance with
          | R.Provenance.Local a, Local b -> Member.equal a b
          | Builtin a, Builtin b -> String.equal a b
          | Local _, Builtin _ | Builtin _, Local _ -> false
        in
        if
          (not provenance)
          || l.input_bytes <> r.input_bytes
          || (not (String.equal l.input_digest r.input_digest))
          || not (String.equal l.output_digest r.output_digest)
        then Ok false
        else B.equal_bytes budget l.contents r.contents
      | Dynamic, Captured _ | Captured _, Dynamic -> Ok false))
;;
