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
(* Token queue – allows a lexer rule to emit *two* tokens without forcing    *)
(* callers to be aware of any buffering.  We push the secondary token onto   *)
(* [pending_token] and serve it on the very next invocation.                *)
(*--------------------------------------------------------------------------*)

let pending_token : Chatmd_parser.token option ref = ref None

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

(* The buffer into which we accumulate the raw text of an unknown tag *)
let scratch_buf = Buffer.create 256



(*--------------------------------------------------------------------------*)
(* Lexer rules                                                              *)
(*--------------------------------------------------------------------------*)
}

let ws = [' ' '\t' '\r' '\n']+

rule token_inner = parse
  (*--------------------------------------------------------------------*)
  (* Raw text block – everything between RAW| and |RAW is returned verbatim as
     a single TEXT token, with *no* lexical analysis of the enclosed
     content.  This allows authors to embed arbitrary XML / chatmd tags
     without the lexer treating them specially.

     Historically the lexer required the opening delimiter to be
     preceded only by whitespace.  That restriction meant that a
     sequence such as "intro RAW| ... |RAW" was tokenised into the
     *text* "intro RAW" followed by a stray pipe character rather than
     entering the dedicated [raw_block] rule.  By relaxing the pattern
     to detect the delimiter regardless of leading content we ensure
     that the RAW markers themselves never leak into the token stream –
     exactly what callers expect. *)

  (* Opening RAW delimiter – optional leading horizontal or vertical
     whitespace is permitted, but we also accept the delimiter directly
     after arbitrary text. *)
  | [' ' '\t' '\r' '\n']* "RAW|" { Buffer.clear scratch_buf; raw_block lexbuf }
  | "RAW|"                            { Buffer.clear scratch_buf; raw_block lexbuf }

  (* RAW opener preceded by in-line text – we split the token stream into two   *)
  (* events: the leading text as a TEXT token (returned now) and the token      *)
  (* produced by [raw_block] on the *next* invocation.  We achieve this by      *)
  (* placing the latter into [pending_token].                                   *)
  | [^'<' '|']+ "RAW|" as raw_prefix_delim {
      (* [raw_prefix_delim] contains the user text immediately followed by the
         RAW‐block opener.  Split the two pieces: everything *except* the
         trailing "RAW|" forms the leading TEXT token. *)
      let txt_len = String.length raw_prefix_delim - 4 (* len "RAW|" *) in
      let prefix = String.sub raw_prefix_delim ~pos:0 ~len:txt_len in
      Buffer.clear scratch_buf;
      let next_tok = raw_block lexbuf in
      pending_token := Some next_tok;
      TEXT prefix
    }

 

  | "<!--"                     { comment lexbuf; token_inner lexbuf }

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

  (* raw text – sequence of characters that does *not* contain '<' or the pipe
     character.  We *exclude* `|` so that the RAW| / |RAW delimiters are
     detected by their dedicated rules above instead of being swallowed by
     this catch-all clause due to the longest-match heuristic of ocamllex. *)
  | [^'<' '|']+                  { TEXT (Lexing.lexeme lexbuf) }

  (* lone pipe – emitted verbatim as text *)
  | '|'                          { TEXT (Lexing.lexeme lexbuf) }

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

and raw_block = parse
  (* Terminating delimiter *)
  | "|RAW" {
      let txt = Buffer.contents scratch_buf in
      Buffer.clear scratch_buf;
      TEXT txt
    }

  (* Any character – accumulate and continue *)
  | _ as c {
      Buffer.add_char scratch_buf c;
      raw_block lexbuf
    }

  | eof { failwith "Unterminated raw block RAW| ... |RAW" }

and comment = parse
  | "-->" { () }
  | eof    { failwith "Unterminated <!-- comment-->" }
  | _      { comment lexbuf }


(* 
 public entry point – obeys the original [token] signature expected by   
 callers.  If a token has been stashed in [pending_token] we serve it     
 immediately; otherwise we delegate to the real lexer [token_inner].     
                                                                        
 This indirection allows rules such as the RAW-block opener to return    
 a *prefix* TEXT token first and defer the token produced after the      
 delimiter to the following call – thereby emitting both tokens without  
 violating the OCamlLex contract that each rule returns exactly one. *)

{
let  token lexbuf =
  match !pending_token with
  | Some tok ->
      pending_token := None;
      tok
  | None -> token_inner lexbuf

}
