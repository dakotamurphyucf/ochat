open Core
module Error = Zone_data_error

module Version = struct
  type t =
    | V2
    | V3
  [@@deriving equal]
end

module Local_type = struct
  type t =
    { offset_seconds : int
    ; is_dst : bool
    ; designation : string
    }
  [@@deriving equal]
end

module Transition = struct
  type t =
    { at : int64
    ; local_type : Local_type.t
    }
end

type t =
  { version : Version.t
  ; initial : Local_type.t
  ; transitions : Transition.t array
  ; footer : string
  }

let maximum_bytes = 1_048_576
let maximum_transitions = 4096
let maximum_types = 256
let maximum_designation_bytes = 65_536
let maximum_footer_bytes = 4096

(* A private checked cursor rejects only through this exception. The public
   boundary catches only it; programming failures are never disguised as data. *)
exception Rejected of Error.t

module Cursor = struct
  type t =
    { bytes : string
    ; mutable position : int
    }

  let reject t reason = raise (Rejected (Malformed { byte_offset = t.position; reason }))

  let ensure t length =
    if length < 0 || length > String.length t.bytes - t.position
    then reject t "truncated TZif structure"
  ;;

  let byte t =
    ensure t 1;
    let value = Char.to_int t.bytes.[t.position] in
    t.position <- t.position + 1;
    value
  ;;

  let take t length =
    ensure t length;
    let value = String.sub t.bytes ~pos:t.position ~len:length in
    t.position <- t.position + length;
    value
  ;;

  let integer t width =
    ensure t width;
    let value = ref 0L in
    for _ = 1 to width do
      value := Int64.(bit_or (shift_left !value 8) (of_int (byte t)))
    done;
    !value
  ;;

  let signed t width =
    let value = integer t width in
    if width = 4 && Int64.(value >= 0x8000_0000L)
    then Int64.(value - 0x1_0000_0000L)
    else value
  ;;

  let count t ~maximum =
    let value = integer t 4 in
    if Int64.(value > of_int maximum) then reject t "TZif count exceeds resource bound";
    Int64.to_int_exn value
  ;;

  let boolean t =
    match byte t with
    | 0 -> false
    | 1 -> true
    | _ -> reject t "invalid TZif boolean"
  ;;
end

let header cursor =
  if not (String.equal (Cursor.take cursor 4) "TZif")
  then Cursor.reject cursor "invalid TZif magic";
  let version =
    match Char.of_int_exn (Cursor.byte cursor) with
    | '2' -> Version.V2
    | '3' -> V3
    | version -> raise (Rejected (Unsupported_version version))
  in
  let reserved = Cursor.take cursor 15 in
  if not (String.for_all reserved ~f:(Char.equal '\000'))
  then Cursor.reject cursor "nonzero TZif reserved header";
  version
;;

let block cursor width =
  let utc_count = Cursor.count cursor ~maximum:maximum_types in
  let std_count = Cursor.count cursor ~maximum:maximum_types in
  let leaps = Cursor.integer cursor 4 in
  if not (Int64.equal leaps 0L) then raise (Rejected Unsupported_leap_seconds);
  let transition_count = Cursor.count cursor ~maximum:maximum_transitions in
  let type_count = Cursor.count cursor ~maximum:maximum_types in
  let designation_count = Cursor.count cursor ~maximum:maximum_designation_bytes in
  if type_count = 0 || designation_count = 0
  then Cursor.reject cursor "empty TZif type or designation table";
  if
    (utc_count <> 0 && utc_count <> type_count)
    || (std_count <> 0 && std_count <> type_count)
  then Cursor.reject cursor "indicator counts must be zero or type count";
  (* Every operand has already passed a small fixed bound; this sum cannot
     overflow OCaml int. Check all remaining bytes before any derived array. *)
  Cursor.ensure
    cursor
    ((transition_count * (width + 1))
     + (type_count * 6)
     + designation_count
     + std_count
     + utc_count);
  let times = Array.init transition_count ~f:(fun _ -> Cursor.signed cursor width) in
  Array.iteri times ~f:(fun index at ->
    if index > 0 && Int64.(at <= times.(Int.pred index))
    then Cursor.reject cursor "transition times must increase strictly");
  let indices = Array.init transition_count ~f:(fun _ -> Cursor.byte cursor) in
  Array.iter indices ~f:(fun index ->
    if index >= type_count then Cursor.reject cursor "transition type index out of range");
  let raw_types =
    Array.init type_count ~f:(fun _ ->
      let offset = Cursor.signed cursor 4 in
      if Int64.equal offset (-2_147_483_648L)
      then Cursor.reject cursor "reserved minimum UTC offset";
      let is_dst = Cursor.boolean cursor in
      let index = Cursor.byte cursor in
      Int64.to_int_exn offset, is_dst, index)
  in
  let designations = Cursor.take cursor designation_count in
  if not (Char.equal designations.[designation_count - 1] '\000')
  then Cursor.reject cursor "designation array must end with NUL";
  let types =
    Array.map raw_types ~f:(fun (offset_seconds, is_dst, index) ->
      if index >= designation_count
      then Cursor.reject cursor "designation index out of range";
      let until =
        match String.index_from designations index '\000' with
        | Some until -> until
        | None -> Cursor.reject cursor "unterminated designation"
      in
      { Local_type.offset_seconds
      ; is_dst
      ; designation = String.sub designations ~pos:index ~len:(until - index)
      })
  in
  let indicators count =
    if count = 0
    then Array.create ~len:type_count false
    else Array.init count ~f:(fun _ -> Cursor.boolean cursor)
  in
  let standard = indicators std_count in
  let utc = indicators utc_count in
  Array.iteri utc ~f:(fun index is_utc ->
    if is_utc && not standard.(index)
    then Cursor.reject cursor "UTC indicator requires standard indicator");
  let transitions =
    Array.mapi times ~f:(fun index at ->
      { Transition.at; local_type = types.(indices.(index)) })
  in
  types.(0), transitions
;;

let of_string bytes =
  if String.length bytes > maximum_bytes
  then Error Error.Too_large
  else (
    let cursor = { Cursor.bytes; position = 0 } in
    try
      let version = header cursor in
      let (_ : Local_type.t * Transition.t array) = block cursor 4 in
      let second_version = header cursor in
      if not (Version.equal version second_version)
      then Cursor.reject cursor "TZif headers disagree on version";
      let initial, transitions = block cursor 8 in
      let remaining = String.length bytes - cursor.position in
      if remaining < 2 then Cursor.reject cursor "missing TZif footer framing";
      if remaining - 2 > maximum_footer_bytes
      then Cursor.reject cursor "TZif footer exceeds resource bound";
      if Cursor.byte cursor <> 10
      then Cursor.reject cursor "missing initial footer newline";
      let footer = Cursor.take cursor (remaining - 2) in
      if String.exists footer ~f:(fun c -> Char.equal c '\000' || Char.equal c '\n')
      then Cursor.reject cursor "embedded footer NUL or newline";
      if Cursor.byte cursor <> 10 then Cursor.reject cursor "missing final footer newline";
      Ok { version; initial; transitions; footer }
    with
    | Rejected error -> Error error)
;;
