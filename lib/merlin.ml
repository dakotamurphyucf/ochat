open Core
open Jsonaf.Export

let string_of_bool b = if b then "y" else "n"

(** Merlin – thin OCaml wrapper around the [ocamlmerlin] CLI.

    The module spawns an [`ocamlmerlin`](https://github.com/ocaml/merlin)
    process in {e server} or {e single-shot} mode, talks to it through
    stdin/stdout and converts JSON replies into typed OCaml values with
    `ppx_jsonaf_conv`.

    It is geared towards {b interactive tooling}: code editors, ChatGPT
    plugins, REPL helpers …  The implementation covers two high-level
    Merlin commands that are most useful in such scenarios:

    • {!occurrences} – find the start/end positions of an identifier
      under the cursor.
    • {!complete}     – get auto-completion candidates together with
      types and (optionally) documentation strings.

    The API is intentionally minimal; extend it as needed.  All
    functions are {b non-blocking} and must be called from inside an
    [Eio] fibre (e.g. the callback passed to [Eio_main.run]). *)

type t =
  { server : bool
  ; bin_path : string
  ; dot_merlin : string
  ; context : Buffer.t
  }

(** Handle to a running Merlin session.

    • [server] – whether to run `ocamlmerlin server` (recommended for
      performance) or `ocamlmerlin single` (stateless).
    • [bin_path] – path to the executable (default "ocamlmerlin").
    • [dot_merlin] – configuration file used to locate build artefacts
      such as `.cmt` files (default `.merlin`).
    • [context] – buffer accumulating phrases previously evaluated via
      {!add_context}.  Merlin needs the full context to analyse the
      fragment passed to {!complete}. *)

(** [create ?server ?bin_path ?dot_merlin ()] spawns a fresh Merlin
    session description.

    The function does {b not} start the external process yet – this is
    handled lazily by {!call}.  The returned value can be reused across
    multiple completion / occurrence requests.

    @param server     if [true] (default), invoke `ocamlmerlin server`
                       for better latency; [false] selects the legacy
                       stateless mode.
    @param bin_path   name or absolute path of the executable.
    @param dot_merlin alternative configuration file name.

    Example creating a handle and adding the contents of a file so that
    later requests are evaluated in the right context:
    {[
      let merlin = Merlin.create () in
      Merlin.add_context merlin "open Core";
    ]} *)
let create ?(server = true) ?(bin_path = "ocamlmerlin") ?(dot_merlin = ".merlin") () =
  { server; bin_path; dot_merlin; context = Buffer.create 16 }
;;

(** [add_context t code] appends [code] to the internal context buffer.

    Merlin resolves types and completions by looking at all phrases
    that came {i before} the current cursor position.  Call this
    function each time you “execute” a toplevel phrase so that future
    {!complete} / {!occurrences} invocations see it.

    The helper simply concatenates the snippet and a terminating " ;; "
    delimiter used by Merlinʼs protocol. *)
let add_context merlin code =
  Buffer.add_string merlin.context code;
  Buffer.add_string merlin.context " ;; "
;;

(* Internal: run [ocamlmerlin <mode> <command> ...] and capture the
   first line of stdout.  Errors are propagated as [Eio.Io] exceptions
   by [Eio.Process.parse_out]. *)
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

(** Low-level representation of a single JSON line returned by Merlin.

    The shape is documented in Merlinʼs [protocol manual][1].  The
    wrapper converts the polymorphic JSON object into a record for
    further decoding.

    [1]: https://github.com/ocaml/merlin/blob/master/doc/PROTOCOL.md *)
type merlin_reply_body =
  { klass : string [@key "class"] (** "return", "error", … *)
  ; value : string (** JSON string containing the payload *)
  ; notifications : string list (** currently ignored *)
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

let parse_merlin_reply str = Jsonaf.of_string str |> [%of_jsonaf: merlin_reply_body]

(** {2 Detection of identifiers} *)

type ident_position =
  { id_line : int [@key "line"] (** 1-based line number *)
  ; id_col : int [@key "col"] (** 0-based column within [id_line] *)
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

(** Range (start, end) in (line, column) coordinates identifying a piece
    of source code, as returned by the "occurrences" command. *)
type ident_reply =
  { id_start : ident_position [@key "start"]
  ; id_end : ident_position [@key "end"]
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

let occurrences env ~pos merlin code =
  (*** [occurrences env ~pos t code] asks Merlin to find all usages of
       the identifier whose {i character} position in [code] is [pos].

       The function returns a list of start/end coordinates (in
       1-based line × 0-based column form) that can be mapped back to
       absolute indices with {!abs_position}.

       The position [pos] must point {i inside} the identifier token –
       any character of it will do.

       Performance – the call spawns an external process (unless a
       persistent server is already running) and therefore takes a few
       milliseconds; cache the connection if you issue many queries.

       Example – highlight all mentions of a variable:
       {[
         Eio_main.run @@ fun env ->
           let merlin = Merlin.create () in
           let code   = "let foo x = x + foo 1" in
           let occs   = Merlin.occurrences env ~pos:4 merlin code in
           List.iter occs ~f:(fun {id_start; id_end} ->
             Format.printf "from line %d col %d to line %d col %d@."
               id_start.id_line id_start.id_col id_end.id_line id_end.id_col);
       ]} *)
  let args = [ "-identifier-at"; string_of_int pos ] in
  let s = call env merlin "occurrences" args code in
  (parse_merlin_reply s).value |> Jsonaf.of_string |> [%of_jsonaf: ident_reply list]
;;

let abs_position code pos =
  (*** [abs_position code p] converts Merlinʼs (line, column) [p] into a
       0-based absolute index in [code].  Lines are counted from 1,
       columns from 0 – exactly the convention used by Merlinʼs JSON
       replies.  The helper is convenient when you need to slice the
       original string with {!String.sub} or friends. *)
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

(** Category of a completion entry (mirrors Merlinʼs enumeration). *)
type kind =
  | CMPL_VALUE [@name "Value"] (** ordinary value, let-binding *)
  | CMPL_VARIANT [@name "Variant"] (** variant constructor *)
  | CMPL_CONSTR [@name "Constructor"] (** record/variant constructor *)
  | CMPL_LABEL [@name "Label"] (** record field label *)
  | CMPL_MODULE [@name "Module"]
  | CMPL_SIG [@name "Signature"]
  | CMPL_TYPE [@name "Type"]
  | CMPL_METHOD [@name "Method"]
  | CMPL_METHOD_CALL [@name "#"]
  | CMPL_EXN [@name "Exn"]
  | CMPL_CLASS [@name "Class"]
[@@deriving jsonaf]

type candidate =
  { cmpl_name : string [@key "name"] (** text inserted into buffer *)
  ; cmpl_kind : kind (** category, see {!kind} *)
  ; cmpl_type : string [@key "desc"] (** human-readable type *)
  ; cmpl_doc : string [@key "info"] (** optional doc string *)
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

(** Structured response of the "complete-prefix" command. *)
type reply =
  { cmpl_candidates : candidate list [@key "entries"] (** suggestions *)
  ; cmpl_start : int [@key "start"] [@default 0]
  ; cmpl_end : int [@key "end"] [@default 0]
    (** slice of [code] to be replaced by the selected candidate *)
  }
[@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

let empty = { cmpl_candidates = []; cmpl_start = 0; cmpl_end = 0 }

(** Convenience value returned by {!complete} when there is nothing to
    suggest.  All indices are 0, [cmpl_candidates] is empty. *)

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
  (*** [complete env ?doc ?types ~pos t code] returns auto-completion
       candidates at character offset [pos] in [code].

       Merlin inspects the prefix (token under/just before the cursor),
       the previously recorded {!add_context} buffer, and the build
       artefacts referenced by [.merlin] to produce the list.

       • If [doc]   is [true] (default [false]) the [cmpl_doc] field of
         the returned {!candidate}s is filled with the beginning of the
         associated documentation comment.
       • If [types] is [true] Merlin spends extra cycles computing a
         more precise type description (expensive).

       The function also post-processes the raw reply:

       • [cmpl_start] and [cmpl_end] delimit the identifier that
         should be replaced; they are computed locally because Merlinʼs
         indices include the context buffer.

       Example – print the first 5 suggestions:
       {[
         Eio_main.run @@ fun env ->
           let merlin = Merlin.create () in
           let code   = "let _ = Strin" in
           let {Merlin.cmpl_candidates; _} =
             Merlin.complete env ~pos:13 merlin code
           in
           List.take cmpl_candidates 5
           |> List.iter ~f:(fun c -> printf "%s : %s\n" c.cmpl_name c.cmpl_type);
       ]} *)
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
