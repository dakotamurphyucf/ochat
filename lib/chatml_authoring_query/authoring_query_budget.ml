open Core
module Wire = Agent_protocol.Authoring_analysis

module Limits = struct
  type t =
    { max_items : int
    ; max_member_depth : int
    ; max_member_edges : int
    ; max_label_bytes : int
    ; max_work_steps : int
    ; type_display : Chatml_type_display.Limits.t
    }

  let create
        ~max_items
        ~max_member_depth
        ~max_member_edges
        ~max_label_bytes
        ~max_work_steps
        ~type_display
    =
    if
      max_items <= 0
      || max_items > Wire.Limits.max_query_items
      || max_member_depth <= 0
      || max_member_depth > 6
      || max_member_edges <= 0
      || max_member_edges > 2048
      || max_label_bytes <= 0
      || max_label_bytes > Wire.Limits.max_label_bytes
      || max_work_steps <= 0
    then Or_error.error_string "invalid semantic query limits"
    else
      Ok
        { max_items
        ; max_member_depth
        ; max_member_edges
        ; max_label_bytes
        ; max_work_steps
        ; type_display
        }
  ;;
end

type failure =
  | Work_limit
  | Label_byte_limit
  | Member_edge_limit

exception Exhausted of failure

type t =
  { limits : Limits.t
  ; checkpoint_callback : unit -> unit
  ; display : Chatml_type_display.Session.session
  ; mutable work : int
  ; mutable items : int
  ; mutable member_edges : int
  ; mutable label_bytes : int
  }

let create limits ~checkpoint =
  { limits
  ; checkpoint_callback = checkpoint
  ; display = Chatml_type_display.Session.create ~limits:limits.type_display
  ; work = 0
  ; items = 0
  ; member_edges = 0
  ; label_bytes = 0
  }
;;

let checkpoint t =
  if t.work >= t.limits.max_work_steps then raise (Exhausted Work_limit);
  t.work <- t.work + 1;
  t.checkpoint_callback ()
;;

let max_items t = t.limits.max_items
let max_work_steps t = t.limits.max_work_steps
let max_member_depth t = t.limits.max_member_depth
let display_session t = t.display
let remaining_labels t = t.limits.max_label_bytes - t.label_bytes

let item t =
  checkpoint t;
  if t.items >= t.limits.max_items
  then false
  else (
    t.items <- t.items + 1;
    true)
;;

let member_edge t =
  checkpoint t;
  if t.member_edges >= t.limits.max_member_edges
  then false
  else (
    t.member_edges <- t.member_edges + 1;
    true)
;;

let charge_bytes t length =
  if length > remaining_labels t then raise (Exhausted Label_byte_limit);
  t.label_bytes <- t.label_bytes + length
;;

let charge_display t value = charge_bytes t (String.length value)

let equal_string t left right =
  let length = String.length left in
  if not (Int.equal length (String.length right))
  then false
  else (
    let offset = ref 0 in
    let equal = ref true in
    while !equal && !offset < length do
      checkpoint t;
      equal := Char.equal left.[!offset] right.[!offset];
      Int.incr offset
    done;
    !equal)
;;

let equal_source_id t left right =
  equal_string
    t
    (Chatml_source.Id.package_namespace left)
    (Chatml_source.Id.package_namespace right)
  && equal_string
       t
       (Chatml_source.Id.relative_path left)
       (Chatml_source.Id.relative_path right)
;;

let label t value =
  let output = Buffer.create (Int.min 64 (remaining_labels t)) in
  String.iter value ~f:(fun char ->
    checkpoint t;
    let byte = Char.to_int char in
    if byte >= 32 && byte <= 126 && not (Char.equal char '\\')
    then (
      charge_bytes t 1;
      Buffer.add_char output char)
    else (
      charge_bytes t 4;
      Buffer.add_string output (sprintf "\\x%02X" byte)));
  Buffer.contents output
;;

let run _t ~f =
  try Ok (f ()) with
  | Exhausted reason -> Error reason
;;
