open! Core

module Error = struct
  type t =
    | Invalid_limits
    | Output_limit
    | Work_limit
    | Json_error of Document_schema.Error.t
  [@@deriving equal, sexp_of]
end

module Limits = struct
  type t =
    { max_bytes : int
    ; max_work_steps : int
    ; json_limits : Document_schema.Limits.t
    }

  let create ~max_bytes ~max_work_steps ~json_limits =
    if max_bytes <= 0 || max_bytes > Sys.max_string_length || max_work_steps <= 0
    then Error Error.Invalid_limits
    else Ok { max_bytes; max_work_steps; json_limits }
  ;;
end

module Piece = struct
  type content =
    | Text of string
    | Quoted of
        { prefix : string
        ; value : string
        ; suffix : string
        }
    | Json of Jsonaf.t

  type t =
    { content : content
    ; bytes : int
    }
end

type t =
  { domain : int
  ; limits : Limits.t option
  ; checkpoint : unit -> unit
  ; rejection : Error.t -> exn
  ; mutable open_ : bool
  ; mutable failed : Error.t option
  ; mutable work : int
  ; mutable bytes : int
  ; mutable rev_pieces : Piece.t list
  }

let reject t error =
  t.failed <- Some error;
  raise (t.rejection error)
;;

let check t =
  if (not (Int.equal t.domain (Stdlib.Domain.self () :> int))) || not t.open_
  then invalid_arg "ChatMD rendering output is outside its owning invocation";
  Option.iter t.failed ~f:(reject t)
;;

let poll t =
  check t;
  t.checkpoint ();
  check t
;;

let charge t ~steps =
  poll t;
  if steps < 0 then reject t Error.Invalid_limits;
  match t.limits with
  | None -> ()
  | Some limits ->
    if steps > limits.max_work_steps - t.work then reject t Error.Work_limit;
    t.work <- t.work + steps
;;

let remaining_bytes t =
  match t.limits with
  | None -> Sys.max_string_length - t.bytes
  | Some limits -> limits.max_bytes - t.bytes
;;

let retain t content bytes =
  check t;
  if bytes > remaining_bytes t then reject t Error.Output_limit;
  t.bytes <- t.bytes + bytes;
  t.rev_pieces <- { Piece.content; bytes } :: t.rev_pieces
;;

let text t value =
  charge t ~steps:1;
  let bytes = String.length value in
  if bytes > remaining_bytes t then reject t Error.Output_limit;
  charge t ~steps:bytes;
  retain t (Text value) bytes
;;

let chunk_bytes = 256

(* This measures pinned OCaml %S; Printf remains the actual encoder. *)
let quoted t ?(prefix = "") ?(suffix = "") value =
  charge t ~steps:1;
  let available = remaining_bytes t in
  if available < 2 then reject t Error.Output_limit;
  charge t ~steps:(String.length prefix);
  charge t ~steps:(String.length value);
  charge t ~steps:(String.length suffix);
  let bytes = ref 2 in
  let measure text =
    let offset = ref 0 in
    while !offset < String.length text do
      poll t;
      let stop = !offset + Int.min chunk_bytes (String.length text - !offset) in
      while !offset < stop do
        let width =
          match text.[!offset] with
          | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
          | ' ' .. '~' -> 1
          | _ -> 4
        in
        if width > available - !bytes then reject t Error.Output_limit;
        bytes := !bytes + width;
        incr offset
      done
    done
  in
  measure prefix;
  measure value;
  measure suffix;
  (match t.limits with
   | None -> ()
   | Some limits ->
     if !bytes > (limits.max_work_steps - t.work) / 2 then reject t Error.Work_limit;
     charge t ~steps:(!bytes * 2));
  retain t (Quoted { prefix; value; suffix }) !bytes
;;

let json t value =
  charge t ~steps:1;
  match t.limits with
  | None ->
    (* Legacy rendering had no schema admission. Keep that behavior, using the
       same actual Jsonaf encoder, before retaining its already encoded bytes. *)
    poll t;
    let encoded = Jsonaf.to_string value in
    poll t;
    retain t (Text encoded) (String.length encoded)
  | Some limits ->
    let output_available = remaining_bytes t in
    let work_available = (limits.max_work_steps - t.work) / 3 in
    if output_available = 0 then reject t Error.Output_limit;
    if work_available = 0 then reject t Error.Work_limit;
    let module J = Document_schema in
    let byte_cap =
      Int.min
        (J.Limits.max_bytes limits.json_limits)
        (Int.min output_available work_available)
    in
    let effective =
      J.Limits.create
        ~max_bytes:byte_cap
        ~max_depth:(J.Limits.max_depth limits.json_limits)
        ~max_nodes:(Int.min byte_cap (J.Limits.max_nodes limits.json_limits))
        ~max_fields:(Int.min byte_cap (J.Limits.max_fields limits.json_limits))
      |> function
      | Ok effective -> effective
      | Error error -> reject t (Error.Json_error error)
    in
    poll t;
    let measured = J.Json.validate_and_measure ~limits:effective value in
    poll t;
    let bytes =
      match measured with
      | Ok bytes -> bytes
      | Error (J.Error.Limit_exceeded "bytes" as error) ->
        if
          output_available <= work_available
          && output_available < J.Limits.max_bytes limits.json_limits
        then reject t Error.Output_limit
        else if work_available < J.Limits.max_bytes limits.json_limits
        then reject t Error.Work_limit
        else reject t (Error.Json_error error)
      | Error error -> reject t (Error.Json_error error)
    in
    charge t ~steps:(bytes * 3);
    retain t (Json value) bytes
;;

let finish t =
  poll t;
  let pieces =
    List.fold t.rev_pieces ~init:[] ~f:(fun pieces piece ->
      poll t;
      piece :: pieces)
  in
  let buffer = Bytes.create t.bytes in
  poll t;
  let position = ref 0 in
  List.iter pieces ~f:(fun { Piece.content; bytes } ->
    poll t;
    let encoded =
      match content with
      | Text value -> value
      | Quoted { prefix; value; suffix } ->
        let value =
          if String.is_empty prefix && String.is_empty suffix
          then value
          else String.concat [ prefix; value; suffix ]
        in
        Printf.sprintf "%S" value
      | Json value -> Jsonaf.to_string value
    in
    poll t;
    if not (Int.equal (String.length encoded) bytes)
    then invalid_arg "ChatMD encoder disagrees with admitted byte count";
    let offset = ref 0 in
    while !offset < bytes do
      poll t;
      let count = Int.min chunk_bytes (bytes - !offset) in
      Stdlib.Bytes.blit_string encoded !offset buffer !position count;
      offset := !offset + count;
      position := !position + count
    done);
  poll t;
  Stdlib.Bytes.unsafe_to_string buffer
;;

let run limits ~checkpoint ~f =
  let module Failure = struct
    exception Rejected of Error.t
  end
  in
  let t =
    { domain = (Stdlib.Domain.self () :> int)
    ; limits
    ; checkpoint
    ; rejection = (fun error -> Failure.Rejected error)
    ; open_ = true
    ; failed = None
    ; work = 0
    ; bytes = 0
    ; rev_pieces = []
    }
  in
  try
    Exn.protect
      ~f:(fun () ->
        poll t;
        f t;
        Ok (finish t))
      ~finally:(fun () -> t.open_ <- false)
  with
  | Failure.Rejected error -> Error error
;;

let render limits ~checkpoint ~f = run (Some limits) ~checkpoint ~f

let render_unbounded ~f =
  match run None ~checkpoint:ignore ~f with
  | Ok output -> output
  | Error error -> raise_s [%sexp "ChatMD rendering invariant", (error : Error.t)]
;;
