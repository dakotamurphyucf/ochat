open! Core

module Role = struct
  type t =
    | Root
    | Import
  [@@deriving compare, equal, sexp_of]
end

module Error = struct
  type t =
    | Empty_path
    | Input_byte_limit
    | Import_changed
    | Invalid_range
  [@@deriving sexp_of]
end

type t =
  { role : Role.t
  ; path : string
  ; authored : string
  ; effective : string
  ; authored_sha256 : string
  ; effective_sha256 : string
  ; positions : Source.Position_index.t
  }

type input = t

let role t = t.role
let path t = t.path
let authored t = t.authored
let effective t = t.effective
let authored_sha256 t = t.authored_sha256
let effective_sha256 t = t.effective_sha256
let authored_byte_length t = String.length t.authored
let effective_byte_length t = String.length t.effective

let identity_digest t ~checkpoint =
  let role =
    match t.role with
    | Role.Root -> "root"
    | Import -> "import"
  in
  let context = ref (Digestif.SHA256.init ()) in
  List.iter
    [ "ochat.parser-input.v1"
    ; role
    ; t.path
    ; t.authored_sha256
    ; t.effective_sha256
    ; Int.to_string (authored_byte_length t)
    ; Int.to_string (effective_byte_length t)
    ]
    ~f:(fun value ->
      checkpoint ();
      context
      := Digestif.SHA256.feed_string !context (Int.to_string (String.length value));
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

let equal_bytes first second ~checkpoint =
  if String.length first <> String.length second
  then false
  else (
    let equal = ref true in
    let offset = ref 0 in
    while !equal && !offset < String.length first do
      if !offset mod 4096 = 0 then checkpoint ();
      equal := Char.equal first.[!offset] second.[!offset];
      incr offset
    done;
    checkpoint ();
    !equal)
;;

let authored_matches t contents ~checkpoint = equal_bytes t.authored contents ~checkpoint

let effective_matches t contents ~checkpoint =
  equal_bytes t.effective contents ~checkpoint
;;

let create ~(role : Role.t) ~path ~authored ~effective ~max_input_bytes ~checkpoint =
  checkpoint ();
  if String.is_empty path
  then Error Error.Empty_path
  else if
    max_input_bytes <= 0
    || String.length authored > max_input_bytes
    || String.length effective > max_input_bytes
  then Error Error.Input_byte_limit
  else (
    let identical = equal_bytes authored effective ~checkpoint in
    let effective = if identical then authored else effective in
    match role, identical with
    | Import, false -> Error Error.Import_changed
    | Root, _ | Import, true ->
      let authored_sha256 =
        Chatml_source.Digest.of_contents authored ~checkpoint
        |> Chatml_source.Digest.to_hex
      in
      let effective_sha256 =
        if identical
        then authored_sha256
        else
          Chatml_source.Digest.of_contents effective ~checkpoint
          |> Chatml_source.Digest.to_hex
      in
      let positions = Source.Position_index.create effective ~checkpoint in
      Ok { role; path; authored; effective; authored_sha256; effective_sha256; positions })
;;

module Range = struct
  type t =
    { input : input
    ; start_byte : int
    ; end_byte : int
    ; start_position : Chatmd_shell_spec.Source_ref.position
    ; end_position : Chatmd_shell_spec.Source_ref.position
    }

  let create input ~start_byte ~end_byte ~checkpoint =
    checkpoint ();
    if start_byte < 0 || end_byte <= start_byte || end_byte > effective_byte_length input
    then Error Error.Invalid_range
    else (
      let position byte_offset =
        Source.Position_index.position input.positions ~byte_offset ~checkpoint
        |> Or_error.ok_exn
      in
      let left = position start_byte in
      let right = position end_byte in
      let convert (position : Source.position) =
        Chatmd_shell_spec.Source_ref.
          { offset = position.offset; line = position.line; column = position.column + 1 }
      in
      Ok
        { input
        ; start_byte
        ; end_byte
        ; start_position = convert left
        ; end_position = convert right
        })
  ;;

  let input t = t.input
  let start_byte t = t.start_byte
  let end_byte t = t.end_byte
  let start_position t = t.start_position
  let end_position t = t.end_position
end
