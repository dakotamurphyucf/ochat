open Core

module Limits = struct
  type t =
    { max_nodes : int
    ; max_edges : int
    ; max_depth : int
    ; max_work_steps : int
    ; max_text_bytes : int
    ; max_output_bytes : int
    }

  let create
        ~max_nodes
        ~max_edges
        ~max_depth
        ~max_work_steps
        ~max_text_bytes
        ~max_output_bytes
    =
    if
      List.exists
        [ max_nodes
        ; max_edges
        ; max_depth
        ; max_work_steps
        ; max_text_bytes
        ; max_output_bytes
        ]
        ~f:(fun bound -> bound <= 0)
    then Or_error.error_string "type display limits must be positive"
    else
      Ok
        { max_nodes
        ; max_edges
        ; max_depth
        ; max_work_steps
        ; max_text_bytes
        ; max_output_bytes
        }
  ;;
end

module Completeness = struct
  type reason =
    | Node_limit
    | Edge_limit
    | Depth_limit
    | Work_limit
    | Byte_limit
  [@@deriving equal, sexp_of]

  type t =
    | Complete
    | Truncated of reason
  [@@deriving equal, sexp_of]
end

open Completeness

type session =
  { limits : Limits.t
  ; mutable nodes : int
  ; mutable edges : int
  ; mutable work : int
  ; mutable output_bytes : int
  }

exception Stop of Completeness.reason

let create ~limits = { limits; nodes = 0; edges = 0; work = 0; output_bytes = 0 }

let take current maximum reason =
  if current >= maximum then raise (Stop reason) else current + 1
;;

let work session ~checkpoint =
  session.work <- take session.work session.limits.max_work_steps Work_limit;
  checkpoint ()
;;

let node session ~checkpoint =
  work session ~checkpoint;
  session.nodes <- take session.nodes session.limits.max_nodes Node_limit
;;

let edge session ~checkpoint =
  work session ~checkpoint;
  session.edges <- take session.edges session.limits.max_edges Edge_limit
;;

let reverse_list session values ~checkpoint =
  List.fold values ~init:[] ~f:(fun reversed value ->
    work session ~checkpoint;
    value :: reversed)
;;

module Budgeted_labels = struct
  type 'a t = (string * 'a) list Int.Table.t

  let create () = Int.Table.create ()

  let hash session label ~checkpoint =
    let value = ref 5381 in
    String.iter label ~f:(fun char ->
      work session ~checkpoint;
      value := !value * 33 lxor Char.to_int char land Int.max_value);
    !value
  ;;

  let equal session left right ~checkpoint =
    let length = String.length left in
    if not (Int.equal length (String.length right))
    then false
    else (
      let index = ref 0 in
      let same = ref true in
      while !same && !index < length do
        work session ~checkpoint;
        same := Char.equal left.[!index] right.[!index];
        Int.incr index
      done;
      !same)
  ;;

  let find table label ~session ~checkpoint =
    let key = hash session label ~checkpoint in
    let entries = Hashtbl.find table key |> Option.value ~default:[] in
    List.find_map entries ~f:(fun (stored, value) ->
      work session ~checkpoint;
      if equal session stored label ~checkpoint then Some value else None)
  ;;

  let find_or_add table label ~session ~checkpoint ~default =
    let key = hash session label ~checkpoint in
    let entries = Hashtbl.find table key |> Option.value ~default:[] in
    match
      List.find_map entries ~f:(fun (stored, value) ->
        work session ~checkpoint;
        if equal session stored label ~checkpoint then Some value else None)
    with
    | Some value -> value, false
    | None ->
      work session ~checkpoint;
      let value = default () in
      Hashtbl.set table ~key ~data:((label, value) :: entries);
      value, true
  ;;
end

let max_depth session = session.limits.max_depth

module Writer = struct
  type t =
    { session : session
    ; buffer : Buffer.t
    ; mutable bytes : int
    ; checkpoint : unit -> unit
    }

  let reserve writer length =
    let limits = writer.session.limits in
    if
      length > limits.max_text_bytes - writer.bytes
      || length > limits.max_output_bytes - writer.session.output_bytes - writer.bytes
    then raise (Stop Byte_limit)
    else writer.bytes <- writer.bytes + length
  ;;

  let emit writer value =
    work writer.session ~checkpoint:writer.checkpoint;
    reserve writer (String.length value);
    Buffer.add_string writer.buffer value
  ;;

  let hex_digit value = "0123456789ABCDEF".[value land 15]

  let emit_hex_byte writer byte =
    work writer.session ~checkpoint:writer.checkpoint;
    reserve writer 4;
    Buffer.add_string writer.buffer "\\x";
    Buffer.add_char writer.buffer (hex_digit (byte lsr 4));
    Buffer.add_char writer.buffer (hex_digit byte)
  ;;

  let escape_label writer label =
    if String.is_empty label
    then emit writer "<empty>"
    else
      String.iter label ~f:(fun char ->
        work writer.session ~checkpoint:writer.checkpoint;
        let byte = Char.to_int char in
        if
          (byte >= Char.to_int 'a' && byte <= Char.to_int 'z')
          || (byte >= Char.to_int 'A' && byte <= Char.to_int 'Z')
          || (byte >= Char.to_int '0' && byte <= Char.to_int '9')
          || Char.equal char '_'
        then (
          reserve writer 1;
          Buffer.add_char writer.buffer char)
        else emit_hex_byte writer byte)
  ;;

  let create session ~checkpoint =
    { session
    ; buffer =
        Buffer.create
          (Int.min
             128
             (Int.min
                session.limits.max_text_bytes
                (session.limits.max_output_bytes - session.output_bytes)))
    ; bytes = 0
    ; checkpoint
    }
  ;;

  let finish writer =
    let text = Buffer.contents writer.buffer in
    writer.session.output_bytes <- writer.session.output_bytes + writer.bytes;
    text
  ;;

  let truncated writer =
    let marker = "..." in
    let text =
      if
        String.length marker <= writer.session.limits.max_text_bytes - writer.bytes
        && String.length marker
           <= writer.session.limits.max_output_bytes
              - writer.session.output_bytes
              - writer.bytes
      then (
        reserve writer (String.length marker);
        Buffer.add_string writer.buffer marker;
        Buffer.contents writer.buffer)
      else ""
    in
    writer.session.output_bytes <- writer.session.output_bytes + String.length text;
    text
  ;;
end
