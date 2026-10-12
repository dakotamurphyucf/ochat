(** Ordinary callers retain the generated parser's existing semantic API. *)
include Chatmd_parser_engine.Make (Chatmd_parser_builder.Legacy)

include Chatmd_tokens
