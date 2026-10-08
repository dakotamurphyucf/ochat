open! Core

module Limits = struct
  type t =
    { frame : int
    ; message : int
    ; fragments : int
    ; controls : int
    ; write : int
    ; framing : int
    }

  let create
        ~max_frame_bytes
        ~max_message_bytes
        ~max_write_bytes
        ~max_framing_bytes
        ~max_fragments
        ~max_control_frames
    =
    if
      max_framing_bytes <= 0
      || max_write_bytes <= 0
      || max_frame_bytes <= 0
      || max_message_bytes <= 0
      || max_fragments <= 0
      || max_control_frames <= 0
    then Or_error.error_string "WebSocket limits must be positive"
    else
      Ok
        { frame = max_frame_bytes
        ; message = max_message_bytes
        ; fragments = max_fragments
        ; controls = max_control_frames
        ; write = max_write_bytes
        ; framing = max_framing_bytes
        }
  ;;
end

type error =
  | Protocol
  | Framing_limit
  | Limit
  | Closed
[@@deriving equal, sexp_of]

exception Invalid of error

type t =
  { limits : Limits.t
  ; read : int -> string
  ; write : string -> unit
  ; random : int -> string
  ; mutable closed : bool
  ; mutable framing_left : int
  }

let create ~limits ~read ~write ~random =
  { limits; read; write; random; closed = false; framing_left = limits.framing }
;;

let fail error = raise (Invalid error)
let byte s i = Char.to_int s.[i]

let read_exact t n =
  let s = t.read n in
  if String.length s <> n then fail Closed;
  s
;;

let write_frame t ~opcode payload =
  let len = String.length payload in
  let extended = if len < 126 then 0 else if len <= 65535 then 2 else 8 in
  let mask = t.random 4 in
  if String.length mask <> 4 then invalid_arg "WebSocket entropy length";
  let frame = Bytes.create (2 + extended + 4 + len) in
  Bytes.set frame 0 (Char.of_int_exn (0x80 lor opcode));
  Bytes.set
    frame
    1
    (Char.of_int_exn
       (0x80 lor if extended = 0 then len else if extended = 2 then 126 else 127));
  for i = 0 to extended - 1 do
    let shift = (extended - i - 1) * 8 in
    let value =
      Int64.(to_int_exn (bit_and (shift_right_logical (of_int len) shift) 255L))
    in
    Bytes.set frame (2 + i) (Char.of_int_exn value)
  done;
  for i = 0 to 3 do
    Bytes.set frame (2 + extended + i) mask.[i]
  done;
  for i = 0 to len - 1 do
    Bytes.set
      frame
      (6 + extended + i)
      (Char.of_int_exn (byte payload i lxor byte mask (i mod 4)))
  done;
  t.write (Bytes.to_string frame)
;;

let valid_close payload =
  let len = String.length payload in
  if len = 1
  then false
  else if len = 0
  then true
  else (
    let code = (byte payload 0 lsl 8) lor byte payload 1 in
    let valid =
      (code >= 1000 && code <= 1014 && code <> 1004 && code <> 1005 && code <> 1006)
      || (code >= 3000 && code <= 4999)
    in
    valid && String.Utf8.is_valid (String.sub payload ~pos:2 ~len:(len - 2)))
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    write_frame t ~opcode:8 "\003\232")
;;

let begin_response t = t.framing_left <- t.limits.framing

let framing t bytes =
  if bytes > t.framing_left then fail Framing_limit;
  t.framing_left <- t.framing_left - bytes
;;

let read_text t =
  if t.closed
  then Error Closed
  else (
    try
      let buffer = Buffer.create (Int.min t.limits.message 4096) in
      let fragments = ref 0 in
      let controls = ref 0 in
      let fragmented = ref false in
      let rec loop () =
        framing t 2;
        let header = read_exact t 2 in
        let first = byte header 0 in
        let second = byte header 1 in
        let fin = first land 0x80 <> 0 in
        let opcode = first land 15 in
        if first land 0x70 <> 0 || second land 0x80 <> 0 then fail Protocol;
        let short = second land 127 in
        let len =
          if short < 126
          then short
          else (
            framing t (if short = 126 then 2 else 8);
            let extended = read_exact t (if short = 126 then 2 else 8) in
            if short = 127 && byte extended 0 land 128 <> 0 then fail Protocol;
            let value =
              String.fold extended ~init:0L ~f:(fun acc c ->
                (* Comparing before shifting prevents int64 overflow. *)
                if Int64.(acc > shift_right_logical max_value 8) then fail Limit;
                Int64.(bit_or (shift_left acc 8) (of_int (Char.to_int c))))
            in
            if Int64.(value < if Int.equal short 126 then 126L else 65536L)
            then fail Protocol;
            if Int64.(value > of_int t.limits.frame) then fail Limit;
            Int64.to_int_exn value)
        in
        let control = opcode land 8 <> 0 in
        if len > t.limits.frame then fail Limit;
        if control && ((not fin) || len > 125) then fail Protocol;
        if not (List.mem [ 0; 1; 8; 9; 10 ] opcode ~equal:Int.equal) then fail Protocol;
        if control
        then (
          incr controls;
          if !controls > t.limits.controls then fail Limit;
          framing t len;
          let payload = read_exact t len in
          match opcode with
          | 8 ->
            if not (valid_close payload) then fail Protocol;
            t.closed <- true;
            write_frame t ~opcode:8 payload;
            Error Closed
          | 9 ->
            write_frame t ~opcode:10 payload;
            loop ()
          | 10 -> loop ()
          | _ -> fail Protocol)
        else (
          if (opcode = 0 && not !fragmented) || (opcode = 1 && !fragmented)
          then fail Protocol;
          incr fragments;
          if
            !fragments > t.limits.fragments
            || len > t.limits.message - Buffer.length buffer
          then fail Limit;
          Buffer.add_string buffer (read_exact t len);
          if fin
          then (
            let message = Buffer.contents buffer in
            if not (String.Utf8.is_valid message) then fail Protocol;
            Ok message)
          else (
            fragmented := true;
            loop ()))
      in
      loop ()
    with
    | Invalid error ->
      t.closed <- true;
      Error error)
;;

let write_text t text =
  if t.closed
  then Error Closed
  else if String.length text > t.limits.write
  then Error Limit
  else if not (String.Utf8.is_valid text)
  then Error Protocol
  else (
    write_frame t ~opcode:1 text;
    Ok ())
;;

let accept ~nonce =
  Digestif.SHA1.digest_string (nonce ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
  |> Digestif.SHA1.to_raw_string
  |> Base64.encode_exn
;;

let validate_upgrade ~nonce ~status ~headers =
  let values name =
    List.filter_map headers ~f:(fun (key, value) ->
      if String.Caseless.equal key name then Some value else None)
  in
  let token name expected =
    List.exists (values name) ~f:(fun value ->
      String.split value ~on:','
      |> List.exists ~f:(fun token -> String.Caseless.equal (String.strip token) expected))
  in
  if
    status <> 101
    || (not (token "upgrade" "websocket"))
    || (not (token "connection" "upgrade"))
    || (not (List.equal String.equal (values "sec-websocket-accept") [ accept ~nonce ]))
    || (not (List.is_empty (values "sec-websocket-extensions")))
    || not (List.is_empty (values "sec-websocket-protocol"))
  then Error Protocol
  else Ok ()
;;
