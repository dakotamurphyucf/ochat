open Core

module Selection = struct
  type t =
    | Dynamic
    | Local_capture
    | Builtin of string
  [@@deriving equal, sexp_of]

  let of_attributes ~src ~local ~capture =
    match String.chop_prefix src ~prefix:"builtin:" with
    | Some id ->
      let%map.Or_error _ = Builtin_instructions.Id.of_string id in
      Builtin id
    | None ->
      if capture && not local
      then Or_error.error_string "captured documents require a relative local source"
      else if capture && String.length src > 1024
      then Or_error.error_string "captured document reference exceeds1024bytes"
      else Ok (if capture then Local_capture else Dynamic)
  ;;
end

module Source = struct
  type t =
    { range : Chatmd_parser_input.Range.t
    ; src : string
    ; selection : Selection.t
    ; local : bool
    ; cleanup_html : bool
    ; markdown : bool
    }

  let create range ~src ~selection ~local ~cleanup_html ~markdown =
    { range; src; selection; local; cleanup_html; markdown }
  ;;

  let range t = t.range
  let src t = t.src
  let selection t = t.selection
end

module Owner = struct
  type t = unit ref

  let create () = ref ()
end

module Slot = struct
  module T = struct
    type t =
      { top_level : int
      ; items : int list
      }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let create ~top_level ~items = { top_level; items }
  let top_level t = t.top_level
  let items t = t.items
end

type t =
  { owner : Owner.t
  ; source : Source.t
  ; slot : Slot.t
  ; ordinal : int
  }

let create owner ~source ~slot ~ordinal = { owner; source; slot; ordinal }
let belongs t owner = phys_equal t.owner owner
let slot t = t.slot
let range t = t.source.range
let ordinal t = t.ordinal
let src t = t.source.src
let selection t = t.source.selection
let local t = t.source.local
let cleanup_html t = t.source.cleanup_html
let markdown t = t.source.markdown
