(**************************************************************************
   chatmd_lexer.mll

   Lexer for ChatMarkdown.  It recognises only the *official* chatmd tags

       msg user agent system developer doc img config

   and converts them into specialised tokens.  Any other XML‐like tag that
   appears *inside* a recognised element is returned verbatim as TEXT so
   that the parser can treat it as raw content.

   The lexer is intentionally simple – it relies on regular expressions
   rather than attempting full XML validation.
**************************************************************************)

{
open Core
open Chatmd_ast
open Chatmd_parser

(*--------------------------------------------------------------------------*)
(* Helpers                                                                  *)
(*--------------------------------------------------------------------------*)

let is_recognised (name : string) : bool =
  Option.is_some (tag_of_string_opt name)

(* Parse attribute list contained in the raw string *)
(* ------------------------------------------------------------------ *)
(* Attribute scanner – improved                                         *)
(* ------------------------------------------------------------------ *)

(* Decode a handful of HTML entities that are common in prompts.  We do *not*
   attempt full entity decoding – just the basics so authors can write
   `&amp;`, `&lt;`, … without having them arrive verbatim in the runtime. *)
let decode_entities (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let flush_entity entity =
    match entity with
    | "amp" -> Buffer.add_char buf '&'
    | "lt" -> Buffer.add_char buf '<'
    | "gt" -> Buffer.add_char buf '>'
    | "quot" -> Buffer.add_char buf '"'
    | "apos" -> Buffer.add_char buf '\''
    | _ -> Buffer.add_string buf "&"; Buffer.add_string buf entity; Buffer.add_char buf ';'
  in
  let len = String.length s in
  let rec loop i =
    if i >= len then ()
    else
      match s.[i] with
      | '&' ->
          let rec find_semi k =
            if k >= len then None
            else if Char.equal s.[k] ';' then Some k else find_semi (k + 1)
          in
          (match find_semi (i + 1) with
           | None -> Buffer.add_char buf '&'; loop (i + 1)
           | Some j ->
               let entity = String.sub s ~pos:(i + 1) ~len:(j - i - 1) in
               flush_entity entity;
               loop (j + 1))
      | _ as c -> Buffer.add_char buf c; loop (i + 1)
  in
  loop 0; Buffer.contents buf

let parse_attrs (s : string) : attribute list =
  (* Imperative scan to handle quoted values that may contain spaces and to
     detect single / double quotes as well as unterminated strings. *)
  let len = String.length s in
  let pos = ref 0 in

  let raise_unterminated quote_char name start_pos =
    let chr = if Char.equal quote_char '"' then '"' else '\'' in
    failwithf "Unterminated quoted attribute value for %s starting at offset %d (expected %c)" name start_pos chr ()
  in

  (* Whitespace skipper *)
  let skip_ws () =
    while !pos < len && Char.is_whitespace s.[!pos] do
      incr pos
    done
  in

  (* Attribute name  [A-Za-z0-9_:-]+ *)
  let read_name () =
    let start = !pos in
    while
      !pos < len &&
      (match s.[!pos] with
       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | ':' | '-' -> true
       | _ -> false)
    do incr pos done;
    String.sub s ~pos:start ~len:(!pos - start)
  in

  (* Quoted value – handles ' and "\"" and decodes basic HTML entities. *)
  let read_quoted ~quote (attr_name : string) : string =
    (* consume opening quote *)
    incr pos;
    let start_content = !pos in
    let rec find_closing () =
      if !pos >= len then raise_unterminated quote attr_name start_content;
      if Char.equal s.[!pos] quote then ()
      else begin
        (* Support escaping the quote via backslash so authors can write
           alt="A \"quoted\" sample" *)
        if Char.equal s.[!pos] '\\' && !pos + 1 < len && Char.equal s.[!pos + 1] quote then
          (* skip the backslash, keep the quote char as content *)
          incr pos;
        incr pos;
        find_closing ()
      end
    in
    find_closing ();
    let raw = String.sub s ~pos:start_content ~len:(!pos - start_content) in
    (* consume closing quote *)
    incr pos;
    raw |> decode_entities
  in

  let read_unquoted () : string option =
    let start_v = !pos in
    while !pos < len && not (Char.is_whitespace s.[!pos]) do
      incr pos
    done;
    if !pos = start_v then None
    else Some (String.sub s ~pos:start_v ~len:(!pos - start_v))
  in

  let rec collect acc =
    skip_ws ();
    if !pos >= len then List.rev acc
    else
      let name = read_name () in
      if String.is_empty name then (
        (* Avoid infinite loop by consuming one char and retry *)
        incr pos;
        collect acc)
      else (
        skip_ws ();
        let value =
          if !pos < len && Char.equal s.[!pos] '=' then (
            incr pos;
            skip_ws ();
            if !pos < len then (
              match s.[!pos] with
              | '"' | '\'' as q -> Some (read_quoted ~quote:q name)
              | _ -> read_unquoted ())
            else None)
          else None (* flag attribute *)
        in
        collect ((name, value) :: acc))
  in
  collect []

(* Take a raw tag like "<msg role=\"user\">" or "<img src=.../>" and
   return (tagName, attrs, self_closing)  – *without* the surrounding <>. *)
let dissect (raw : string) : string * attribute list * bool =
  let len = String.length raw in
  let self_closing =
    if len >= 2 then Char.equal raw.[len - 2] '/' else false
  in
  let body =
    if self_closing
    then String.sub raw ~pos:1 ~len:(len - 3) (* drop <   /> *)
    else String.sub raw ~pos:1 ~len:(len - 2) (* drop <   >  *)
  in
  match String.lsplit2 body ~on:' ' with
  | Some (name, attrs_str) -> name, parse_attrs attrs_str, self_closing
  | None -> body, [] , self_closing

(* ------------------------------------------------------------------ *)
(* Unknown tag collapsing                                             *)
(* ------------------------------------------------------------------ *)

(* Collapse an entire *unknown* element – including any nested tags –
   into a single raw string so that the parser only sees one TEXT token.
   This eliminates the previous behaviour where unknown tags were split
   into several TEXT tokens and nested recognised tags leaked through.

   [name]         – the tag name (without <> or /).
   [init_depth]   – usually 1 (called right after the opening tag has
                    been consumed).
   [lexbuf]       – the source buffer.

   The algorithm keeps a simple [depth] counter that is incremented when
   we see another start tag with *the same* name and decremented on the
   matching end tag.  All input is copied verbatim into an internal
   buffer which is finally returned as a string. *)

(* The buffer into which we accumulate the raw text of an unknown tag *)
let scratch_buf = Buffer.create 256

(* Recursive rule that skips over *unknown* elements (and anything nested
   inside them) so that we can emit a single consolidated TEXT token. *)

(* [skip_unknown] lives outside the OCaml code block so that it can use
   the normal ocamllex [parse] syntax. *)

(*--------------------------------------------------------------------------*)
(* Lexer rules                                                              *)
(*--------------------------------------------------------------------------*)
}

let ws = [' ' '\t' '\r' '\n']+

rule token = parse
  | "<!--"                     { comment lexbuf; token lexbuf }

  (* self‐closing recognised tag, e.g. <img .../>                     *)
  | '<' ['a'-'z''A'-'Z''_'] [^'>' ]* "/>" as raw_tag {
      let name, attrs, _ = dissect raw_tag in
      if is_recognised name then
        let tag = tag_of_string name in
        SELF (tag, attrs)
      else
        TEXT raw_tag
    }

  (* normal start tag                                                 *)
  | '<' ['a'-'z''A'-'Z''_'] [^'>' ]* '>' as raw_tag {
      let name, attrs, _ = dissect raw_tag in
      if is_recognised name then
        let tag = tag_of_string name in
        START (tag, attrs)
      else
        (* Unknown tag – emit the *opening* tag verbatim as TEXT so that the
           parser still sees any recognised children.  The corresponding
           closing tag is handled by the dedicated rule below that also
           emits a TEXT token.  This behaviour yields the following token
           stream for `<yo><doc/></yo>`:

             TEXT "<yo>"  – opening unknown
             SELF Doc      – recognised child
             TEXT "</yo>" – closing unknown

           This is exactly what higher layers expect.  We no longer try to
           collapse the whole unknown subtree into a single token. *)
        TEXT raw_tag
    }

  (* end tag                                                          *)
  | "</" ['a'-'z''A'-'Z''_']+ [^'>' ]* '>' as raw_tag {
      let len = String.length raw_tag in
      let name = String.sub raw_tag ~pos:2 ~len:(len - 3) in
      if is_recognised name then
        END (tag_of_string name)
      else
        TEXT raw_tag
    }


  (*--------------------------------------------------------------------*)
  (* Lone '<' (or a *run* of consecutive '<' characters) that is *not*  *)
  (* the start of a recognised / unknown tag.  We emit the whole run     *)
  (* as *one* TEXT token so sequences like "<<<<<" do **not** explode   *)
  (* into five separate tokens anymore.                                  *)
  (*--------------------------------------------------------------------*)
  | '<'+ as lt_run { TEXT lt_run }

  (* raw text – sequence of characters that does *not* contain '<'.      *)
  | [^'<']+                      { TEXT (Lexing.lexeme lexbuf) }

  | eof                          { EOF }

  | _ as c {
      let p = Lexing.lexeme_start_p lexbuf in
      failwithf "Unexpected char %c at %d:%d" c p.pos_lnum
        (p.pos_cnum - p.pos_bol) ()
    }

(*--------------------------------------------------------------------------*)
(* Helper rule – skip an entire unknown element and return a single TEXT   *)
(*--------------------------------------------------------------------------*)

and skip_unknown name depth = parse
  | "</" ['a'-'z''A'-'Z''_']+ [^'>' ]* '>' as raw_tag {
      Buffer.add_string scratch_buf raw_tag;
      let len = String.length raw_tag in
      let closing_name = String.sub raw_tag ~pos:2 ~len:(len - 3) in
      let depth = if String.equal closing_name name then depth - 1 else depth in
      if depth = 0 then (
        let txt = Buffer.contents scratch_buf in
        Buffer.clear scratch_buf;
        TEXT txt
      ) else skip_unknown name depth lexbuf
    }

  | '<' ['a'-'z''A'-'Z''_'] [^'>' ]* "/>" as raw_tag {
      Buffer.add_string scratch_buf raw_tag;
      skip_unknown name depth lexbuf
    }

  | '<' ['a'-'z''A'-'Z''_'] [^'>' ]* '>' as raw_tag {
      Buffer.add_string scratch_buf raw_tag;
      let open_name, _, self = dissect raw_tag in
      let depth = if (not self) && String.equal open_name name then depth + 1 else depth in
      skip_unknown name depth lexbuf
    }

  | '<' { Buffer.add_string scratch_buf "<"; skip_unknown name depth lexbuf }

  | [^ '<' ]+ as chunk { Buffer.add_string scratch_buf chunk; skip_unknown name depth lexbuf }

  | eof { failwithf "Unterminated unknown tag <%s>" name () }

and comment = parse
  | "-->" { () }
  | eof    { failwith "Unterminated <!-- comment-->" }
  | _      { comment lexbuf }

