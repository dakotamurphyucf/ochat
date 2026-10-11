open Core
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Declaration = Prompt.Chat_markdown.Declaration
module Captured = Chatml_source

module Identity = struct
  type t =
    { role : Input.Role.t
    ; path : string
    ; authored : string
    ; effective : string
    ; root_path : string
    ; root_authored : string
    ; root_effective : string
    ; start_byte : int
    ; end_byte : int
    ; script_id : string
    ; script_digest : string
    }
  [@@deriving compare, equal, sexp_of]
end

type t =
  { identity : Identity.t
  ; input : Input.t
  ; root : Input.t
  ; path : Source.Portable_path.t
  ; digest : Captured.Digest.t
  ; body_digest : Captured.Digest.t
  ; key : string
  }

let compare left right = Identity.compare left.identity right.identity
let equal left right = Identity.equal left.identity right.identity
let sexp_of_t t = Identity.sexp_of_t t.identity
let declaration_path t = t.path
let registration_input t = t.root
let declaration_digest t = t.digest
let parser_input t = t.input
let start_byte t = t.identity.start_byte
let end_byte t = t.identity.end_byte
let script_id t = t.identity.script_id
let script_digest t = t.body_digest
let key t = t.key

let same_input left right =
  String.length (Input.path left) <= 1024
  && String.length (Input.path right) <= 1024
  && Input.Role.equal (Input.role left) (Input.role right)
  && String.equal (Input.path left) (Input.path right)
  && String.equal (Input.authored_sha256 left) (Input.authored_sha256 right)
  && String.equal (Input.effective_sha256 left) (Input.effective_sha256 right)
  && Int.equal (Input.authored_byte_length left) (Input.authored_byte_length right)
  && Int.equal (Input.effective_byte_length left) (Input.effective_byte_length right)
;;

let hash fields ~checkpoint =
  let context = ref (Digestif.SHA256.init ()) in
  List.iter fields ~f:(fun value ->
    checkpoint ();
    context := Digestif.SHA256.feed_string !context (Int.to_string (String.length value));
    context := Digestif.SHA256.feed_string !context ":";
    let offset = ref 0 in
    while !offset < String.length value do
      checkpoint ();
      let len = Int.min 4096 (String.length value - !offset) in
      context := Digestif.SHA256.feed_string !context ~off:!offset ~len value;
      offset := !offset + len
    done);
  Digestif.SHA256.(get !context |> to_hex)
;;

let role_name = function
  | Input.Role.Root -> "root"
  | Import -> "import"
;;

let input_key input ~checkpoint =
  hash
    [ "ochat.parser-input.v1"
    ; role_name (Input.role input)
    ; Input.path input
    ; Input.authored_sha256 input
    ; Input.effective_sha256 input
    ; Int.to_string (Input.authored_byte_length input)
    ; Int.to_string (Input.effective_byte_length input)
    ]
    ~checkpoint
;;

let of_metadata
      input
      ~registration_input:root
      ~start_byte
      ~end_byte
      ~script_id
      ~script_digest
      ~checkpoint
  =
  checkpoint ();
  let open Or_error.Let_syntax in
  let%bind path = Source.Portable_path.create (Input.path input) in
  let%bind _ = Source.Portable_path.create (Input.path root) in
  let%bind () =
    if (not (Input.Role.equal (Input.role root) Root)) || String.is_empty script_id
    then Or_error.error_string "Binding requires a Root context and script ID"
    else Ok ()
  in
  let%bind _ =
    Input.Range.create input ~start_byte ~end_byte ~checkpoint
    |> Result.map_error ~f:(fun _ -> Error.of_string "Invalid declaration byte range")
  in
  let%map digest = Captured.Digest.of_hex (Input.effective_sha256 input) in
  let identity =
    Identity.
      { role = Input.role input
      ; path = Input.path input
      ; authored = Input.authored_sha256 input
      ; effective = Input.effective_sha256 input
      ; root_path = Input.path root
      ; root_authored = Input.authored_sha256 root
      ; root_effective = Input.effective_sha256 root
      ; start_byte
      ; end_byte
      ; script_id
      ; script_digest = Captured.Digest.to_hex script_digest
      }
  in
  let key =
    hash
      [ "ochat.script-binding.v1"
      ; role_name identity.role
      ; identity.path
      ; identity.authored
      ; identity.effective
      ; identity.root_path
      ; identity.root_authored
      ; identity.root_effective
      ; Int.to_string start_byte
      ; Int.to_string end_byte
      ; script_id
      ; identity.script_digest
      ]
      ~checkpoint
  in
  { identity; input; root; path; digest; body_digest = script_digest; key }
;;

let of_declaration parsed declaration ~checkpoint =
  let owned =
    List.exists (Parsed.declarations parsed) ~f:(fun actual ->
      checkpoint ();
      phys_equal actual declaration)
  in
  let range = Declaration.range declaration in
  let input = Input.Range.input range in
  let input_owned =
    List.exists (Parsed.inputs parsed) ~f:(fun actual ->
      checkpoint ();
      phys_equal actual input)
  in
  if (not owned) || not input_owned
  then Or_error.error_string "Declaration does not belong to this parser context"
  else
    of_metadata
      input
      ~registration_input:(Parsed.root_input parsed)
      ~start_byte:(Input.Range.start_byte range)
      ~end_byte:(Input.Range.end_byte range)
      ~script_id:(Declaration.script_id declaration)
      ~script_digest:
        (Captured.Digest.of_contents (Declaration.script_body declaration) ~checkpoint)
      ~checkpoint
;;

let bytes_equal left right ~checkpoint =
  if String.length left <> String.length right
  then false
  else (
    let same = ref true
    and offset = ref 0 in
    while !same && !offset < String.length left do
      if !offset mod 4096 = 0 then checkpoint ();
      same := Char.equal left.[!offset] right.[!offset];
      incr offset
    done;
    checkpoint ();
    !same)
;;

let find_declaration binding parsed ~checkpoint =
  if not (same_input binding.root (Parsed.root_input parsed))
  then None
  else
    List.find (Parsed.declarations parsed) ~f:(fun declaration ->
      checkpoint ();
      let range = Declaration.range declaration in
      if
        (not (same_input binding.input (Input.Range.input range)))
        || (not (Int.equal binding.identity.start_byte (Input.Range.start_byte range)))
        || (not (Int.equal binding.identity.end_byte (Input.Range.end_byte range)))
        || not
             (bytes_equal
                binding.identity.script_id
                (Declaration.script_id declaration)
                ~checkpoint)
      then false
      else (
        match of_declaration parsed declaration ~checkpoint with
        | Ok actual -> String.equal actual.key binding.key && equal actual binding
        | Error _ -> false))
;;

let matches_parsed binding parsed ~checkpoint =
  Option.is_some (find_declaration binding parsed ~checkpoint)
;;

let same_declaration left right =
  let a = left.identity
  and b = right.identity in
  String.equal a.path b.path
  && String.equal a.authored b.authored
  && String.equal a.effective b.effective
  && Int.equal a.start_byte b.start_byte
  && Int.equal a.end_byte b.end_byte
  && String.equal a.script_digest b.script_digest
;;

let matches_entry binding parsed contents ~checkpoint =
  match find_declaration binding parsed ~checkpoint with
  | None -> false
  | Some declaration ->
    bytes_equal (Declaration.script_body declaration) contents ~checkpoint
;;
