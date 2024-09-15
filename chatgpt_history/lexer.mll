{

open Parser 

exception SyntaxError of string
}

rule read = parse
  | "(*"          { 
    
    let buf = Buffer.create 17 in
    Buffer.add_string buf "(*";
    let start = lexbuf.lex_curr_p in
    comment start 1 (buf)  lexbuf }
  | _             {read lexbuf}
  | eof      { EOF }

and comment start c buf =
  parse
  | "*)"       {

    Buffer.add_string buf "*)";
    if c - 1 = 0 then
    let end_ = lexbuf.lex_curr_p in
     DOC_STRING (start, end_, Buffer.contents buf)
     else
     comment start (c-1) buf lexbuf
      }
  | "(*"  { Buffer.add_string buf (Lexing.lexeme lexbuf);
      comment start (c + 1) buf lexbuf
    }
  | '\\' '/'  { Buffer.add_char buf '/'; comment start c buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; comment start c buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; comment start c buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; comment start c buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; comment start c buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; comment start c buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; comment start c buf lexbuf }
  | _
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      comment start c buf lexbuf
    }
  (* | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) } *)
  | eof { raise (SyntaxError ("String is not terminated")) }
