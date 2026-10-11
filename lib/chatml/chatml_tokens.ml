(* Menhir's --only-tokens preserves %parameter. Fix this generated token type
   once; --external-tokens shares it across every invocation-local parser. *)
include Chatml_parser_tokens.Make (Chatml_syntax_sites.Disabled)
