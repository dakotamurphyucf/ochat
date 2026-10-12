(* Fix the parameterized generated token representation once. Both builders
   consume this exact type; lexer and grammar have one token authority. *)
include Chatmd_parser_tokens.Make (Chatmd_parser_builder.Legacy)
