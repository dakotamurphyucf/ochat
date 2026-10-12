open Core
open Jsonaf.Export
module Hook = Manifest_prepared_hook

module Wire = struct
  type t =
    { prepared_hooks : Hook.t list
    ; resource_sha256 : string
    }
  [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]
end

include Wire

let compare_string left right ~checkpoint =
  let limit = Int.min (String.length left) (String.length right) in
  let rec loop offset =
    if offset >= limit
    then Int.compare (String.length left) (String.length right)
    else (
      if offset mod 1024 = 0 then checkpoint ();
      match Char.compare left.[offset] right.[offset] with
      | 0 -> loop (offset + 1)
      | order -> order)
  in
  loop 0
;;

let compare_key left right ~checkpoint =
  match compare_string (Hook.id left) (Hook.id right) ~checkpoint with
  | 0 -> compare_string (Hook.entrypoint left) (Hook.entrypoint right) ~checkpoint
  | order -> order
;;

let validate_sorted t ~checkpoint =
  let rec loop previous = function
    | [] -> Ok t
    | hook :: rest ->
      checkpoint ();
      if
        Option.exists previous ~f:(fun previous ->
          compare_key previous hook ~checkpoint >= 0)
      then Or_error.error_string "duplicate or unordered prepared hook entrypoints"
      else loop (Some hook) rest
  in
  if Hook.valid_digest t.resource_sha256 && not (List.is_empty t.prepared_hooks)
  then loop None t.prepared_hooks
  else Or_error.error_string "invalid resource manifest digest"
;;

let create ~resource_sha256 hooks ~checkpoint =
  let prepared_hooks =
    List.sort hooks ~compare:(fun a b -> compare_key a b ~checkpoint)
  in
  validate_sorted { prepared_hooks; resource_sha256 } ~checkpoint
;;

let prepared_hooks t = t.prepared_hooks
let resource_sha256 t = t.resource_sha256
let decoded_exn t = validate_sorted t ~checkpoint:(fun () -> ()) |> Or_error.ok_exn
let t_of_sexp value = Wire.t_of_sexp value |> decoded_exn
let t_of_jsonaf value = Wire.t_of_jsonaf value |> decoded_exn
let bin_read_t buffer ~pos_ref = Wire.bin_read_t buffer ~pos_ref |> decoded_exn

let __bin_read_t__ buffer ~pos_ref tag =
  Wire.__bin_read_t__ buffer ~pos_ref tag |> decoded_exn
;;

let bin_reader_t = { Bin_prot.Type_class.read = bin_read_t; vtag_read = __bin_read_t__ }
let bin_t = { Wire.bin_t with reader = bin_reader_t }

module Encoding = struct
  let prefix = "{\"encoding_version\":\"ochat.shell.execution.v2\",\"prepared_hooks\":["
  let resource_separator = "],\"resource_manifest\":"
  let kind hook = Chatmd_script_spec.jsonaf_of_kind (Hook.kind hook) |> Jsonaf.to_string

  let escaped_width = function
    | '"' | '\\' | '\b' | '\012' | '\n' | '\r' | '\t' -> 2
    | c when Char.to_int c < 32 -> 6
    | _ -> 1
  ;;

  let walk_string value ~checkpoint ~f =
    for offset = 0 to String.length value - 1 do
      if offset mod 1024 = 0 then checkpoint ();
      f value.[offset]
    done
  ;;

  let escape = function
    | '"' -> "\\\""
    | '\\' -> "\\\\"
    | '\b' -> "\\b"
    | '\012' -> "\\f"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\t' -> "\\t"
    | c -> sprintf "\\u%04x" (Char.to_int c)
  ;;

  module Output = struct
    type t =
      { bytes : Bytes.t
      ; mutable offset : int
      }

    let create size = { bytes = Bytes.create size; offset = 0 }

    let char t c =
      Bytes.set t.bytes t.offset c;
      t.offset <- t.offset + 1
    ;;

    let literal t value ~checkpoint =
      let rec loop offset =
        if offset < String.length value
        then (
          checkpoint ();
          let length = Int.min 4096 (String.length value - offset) in
          Bytes.From_string.blit
            ~src:value
            ~src_pos:offset
            ~dst:t.bytes
            ~dst_pos:t.offset
            ~len:length;
          t.offset <- t.offset + length;
          loop (offset + length))
      in
      loop 0
    ;;

    let finish t =
      assert (t.offset = Bytes.length t.bytes);
      (* The private output is consumed here: no mutable alias or later write. *)
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t.bytes
    ;;
  end

  let encode_string output value ~checkpoint =
    Output.char output '"';
    walk_string value ~checkpoint ~f:(fun c ->
      if escaped_width c = 1
      then Output.char output c
      else Output.literal output (escape c) ~checkpoint);
    Output.char output '"'
  ;;

  let digest value ~checkpoint =
    let rec loop state offset =
      if offset >= String.length value
      then Digestif.SHA256.(to_hex (get state))
      else (
        checkpoint ();
        let length = Int.min 4096 (String.length value - offset) in
        loop
          (Digestif.SHA256.feed_string state value ~off:offset ~len:length)
          (offset + length))
    in
    loop Digestif.SHA256.empty 0
  ;;
end

let canonical_size hooks ~resource_json ~max_bytes ~checkpoint =
  let module Rejected = struct
    exception Limit
  end
  in
  let size = ref 0 in
  let add length =
    if length < 0 || length > max_bytes - !size then raise Rejected.Limit;
    size := !size + length
  in
  let string_size value =
    add 2;
    Encoding.walk_string value ~checkpoint ~f:(fun c -> add (Encoding.escaped_width c))
  in
  let hook_size hook =
    checkpoint ();
    add (String.length "{\"entrypoint\":,\"id\":,\"kind\":,\"program_fingerprint\":}");
    string_size (Hook.entrypoint hook);
    string_size (Hook.id hook);
    add (String.length (Encoding.kind hook));
    string_size (Hook.program_fingerprint hook)
  in
  try
    checkpoint ();
    if max_bytes < 0 then raise Rejected.Limit;
    add (String.length Encoding.prefix);
    List.iteri hooks ~f:(fun index hook ->
      if index > 0 then add 1;
      hook_size hook);
    add (String.length Encoding.resource_separator);
    add (String.length resource_json);
    add 1;
    Ok !size
  with
  | Rejected.Limit ->
    Or_error.error_string "complete shell manifest exceeds canonical byte allowance"
;;

let encode t ~resource_json ~canonical_bytes ~checkpoint =
  let module Output = Encoding.Output in
  let output = Output.create canonical_bytes in
  Output.literal output Encoding.prefix ~checkpoint;
  List.iteri t.prepared_hooks ~f:(fun index hook ->
    checkpoint ();
    if index > 0 then Output.char output ',';
    Output.literal output "{\"entrypoint\":" ~checkpoint;
    Encoding.encode_string output (Hook.entrypoint hook) ~checkpoint;
    Output.literal output ",\"id\":" ~checkpoint;
    Encoding.encode_string output (Hook.id hook) ~checkpoint;
    Output.literal output ",\"kind\":" ~checkpoint;
    Output.literal output (Encoding.kind hook) ~checkpoint;
    Output.literal output ",\"program_fingerprint\":" ~checkpoint;
    Encoding.encode_string output (Hook.program_fingerprint hook) ~checkpoint;
    Output.char output '}');
  Output.literal output Encoding.resource_separator ~checkpoint;
  Output.literal output resource_json ~checkpoint;
  Output.char output '}';
  let canonical = Output.finish output in
  canonical, Encoding.digest canonical ~checkpoint
;;
