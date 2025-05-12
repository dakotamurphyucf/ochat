open Core
open Jsonaf.Export

let string_of_bool b = if b then "y" else "n"

(** {2 Merlin} *)

type t =
  { server : bool
  ; bin_path : string
  ; dot_merlin : string
  ; context : Buffer.t
  }

let create ?(server = true) ?(bin_path = "ocamlmerlin") ?(dot_merlin = ".merlin") () =
  { server; bin_path; dot_merlin; context = Buffer.create 16 }
;;

let add_context merlin code =
  Buffer.add_string merlin.context code;
  Buffer.add_string merlin.context " ;; "
;;

let call env merlin command flags code =
  let mode = if merlin.server then "server" else "single" in
  let args =
    merlin.bin_path :: mode :: command :: "-dot-merlin" :: merlin.dot_merlin :: flags
  in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  Eio.Process.parse_out
    proc_mgr
    Eio.Buf_read.line
    args
    ~stdin:(Eio.Flow.string_source code)
;;

(** {2 Top-level merlin replies} *)

type merlin_reply_body =
  { klass : string [@key "class"]
  ; value : string
  ; notifications : string list
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

let parse_merlin_reply str = Jsonaf.of_string str |> [%of_jsonaf: merlin_reply_body]

(** {2 Detection of identifiers} *)

type ident_position =
  { id_line : int [@key "line"]
  ; id_col : int [@key "col"]
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

type ident_reply =
  { id_start : ident_position [@key "start"]
  ; id_end : ident_position [@key "end"]
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

let occurrences env ~pos merlin code =
  let args = [ "-identifier-at"; string_of_int pos ] in
  let s = call env merlin "occurrences" args code in
  (parse_merlin_reply s).value |> Jsonaf.of_string |> [%of_jsonaf: ident_reply list]
;;

let abs_position code pos =
  let n = String.length code in
  let rec aux lnum cpos i =
    if i = n
    then n
    else if lnum = pos.id_line && (cpos = pos.id_col || Char.(code.[i] = '\n'))
    then i
    else (
      match code.[i] with
      | '\n' -> aux (succ lnum) 0 (succ i)
      | _ -> aux lnum (succ cpos) (succ i))
  in
  aux 1 0 0
;;

(** {2 Completion} *)

type kind =
  | CMPL_VALUE [@name "Value"]
  | CMPL_VARIANT [@name "Variant"]
  | CMPL_CONSTR [@name "Constructor"]
  | CMPL_LABEL [@name "Label"]
  | CMPL_MODULE [@name "Module"]
  | CMPL_SIG [@name "Signature"]
  | CMPL_TYPE [@name "Type"]
  | CMPL_METHOD [@name "Method"]
  | CMPL_METHOD_CALL [@name "#"]
  | CMPL_EXN [@name "Exn"]
  | CMPL_CLASS [@name "Class"]
[@@deriving jsonaf]

type candidate =
  { cmpl_name : string [@key "name"]
  ; cmpl_kind : kind
  ; cmpl_type : string [@key "desc"]
  ; cmpl_doc : string [@key "info"]
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

type reply =
  { cmpl_candidates : candidate list [@key "entries"]
  ; cmpl_start : int [@key "start"] [@default 0]
  ; cmpl_end : int [@key "end"] [@default 0]
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

let empty = { cmpl_candidates = []; cmpl_start = 0; cmpl_end = 0 }

let rec rfind_prefix_start s = function
  | 0 -> 0
  | pos ->
    (match s.[pos - 1] with
     | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' | '\'' | '`' | '.' ->
       rfind_prefix_start s (pos - 1)
     | _ -> pos)
;;

let rec find_cmpl_end s pos =
  if pos < String.length s
  then (
    match s.[pos] with
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' | '\'' | '`' | '.' ->
      find_cmpl_end s (pos + 1)
    | _ -> pos)
  else pos
;;

let complete env ?(doc = false) ?(types = false) ~pos merlin code =
  let context = Buffer.contents merlin.context in
  let offset = String.length context in
  let prefix_start = rfind_prefix_start code pos in
  let prefix_length = pos - prefix_start in
  let prefix = String.sub code ~pos:prefix_start ~len:prefix_length in
  let args =
    [ "-position"
    ; string_of_int (offset + pos)
    ; "-prefix"
    ; prefix
    ; "-doc"
    ; string_of_bool doc
    ; "-types"
    ; string_of_bool types
    ]
  in
  let s = call env merlin "complete-prefix" args code in
  let reply = (parse_merlin_reply s).value |> Jsonaf.of_string |> [%of_jsonaf: reply] in
  { reply with
    cmpl_start =
      (match String.rindex_from_exn prefix (prefix_length - 1) '.' with
       | pos -> prefix_start + pos + 1
       | exception Not_found_s _ -> prefix_start)
  ; cmpl_end = find_cmpl_end code pos
  }
;;
