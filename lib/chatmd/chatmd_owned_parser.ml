open! Core

module Limits = struct
  type t =
    { input_bytes : int
    ; parse_calls : int
    ; tokens : int
    ; depth : int
    }
  [@@deriving sexp_of]

  let create ~max_input_bytes ~max_parse_calls ~max_tokens ~max_depth =
    if max_input_bytes <= 0 || max_parse_calls <= 0 || max_tokens <= 0 || max_depth <= 0
    then Or_error.error_string "owned ChatMD limits must be positive"
    else
      Ok
        { input_bytes = max_input_bytes
        ; parse_calls = max_parse_calls
        ; tokens = max_tokens
        ; depth = max_depth
        }
  ;;

  let policy_identity = sexp_of_t
  let input_bytes t = t.input_bytes
  let token_limit t = t.tokens
end

module Input_key = struct
  module T = struct
    type t = Chatmd_parser_input.Role.t * string [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type t =
  { limits : Limits.t
  ; mutable calls : int
  ; mutable tokens : int
  ; mutable input_revisions : int
  ; mutable inputs : Chatmd_parser_input.t Map.M(Input_key).t
  ; poll : unit -> unit
  }

let create limits ~checkpoint =
  { limits
  ; calls = 0
  ; tokens = 0
  ; input_revisions = 0
  ; inputs = Map.empty (module Input_key)
  ; poll = checkpoint
  }
;;

let limits t = t.limits

let checkpoint t =
  Eio.Fiber.check ();
  t.poll ();
  Eio.Fiber.check ()
;;

let bound_loader t loader =
  Source_loader.with_read_limit loader ~max_bytes:(fun _ ->
    Int.min t.limits.input_bytes (8 * 1024 * 1024))
;;

let check_expansion t ~depth node =
  checkpoint t;
  match node with
  | Chatmd_ast.Text _ -> ()
  | Element (_, _, _) ->
    if depth >= t.limits.depth
    then failwith "owned ChatMD expanded markup nesting limit exceeded"
;;

let next_parse t =
  checkpoint t;
  if t.calls >= t.limits.parse_calls
  then failwith "owned ChatMD source expansion limit exceeded (parse calls)"
;;

let input t ~role ~path ~authored ~effective =
  checkpoint t;
  if
    String.length authored > t.limits.input_bytes
    || String.length effective > t.limits.input_bytes
  then failwith "owned ChatMD input byte limit exceeded";
  let key = role, path in
  let previous = Map.find t.inputs key in
  checkpoint t;
  match previous with
  | Some input
    when Chatmd_parser_input.authored_matches input authored ~checkpoint:(fun () ->
           checkpoint t)
         && Chatmd_parser_input.effective_matches input effective ~checkpoint:(fun () ->
           checkpoint t) -> input
  | Some _ | None ->
    if t.input_revisions >= t.limits.parse_calls
    then failwith "owned ChatMD source expansion limit exceeded (input revisions)";
    let input =
      Chatmd_parser_input.create
        ~role
        ~path
        ~authored
        ~effective
        ~max_input_bytes:t.limits.input_bytes
        ~checkpoint:(fun () -> checkpoint t)
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Chatmd_parser_input.Error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    t.inputs <- Map.set t.inputs ~key ~data:input;
    t.input_revisions <- t.input_revisions + 1;
    input
;;

let read_import t loader source =
  next_parse t;
  let result =
    Source_loader.read_bounded
      ~max_bytes:(Int.min t.limits.input_bytes (8 * 1024 * 1024))
      loader
      source
  in
  checkpoint t;
  result
;;

let parse_worker t ?(check_token = ignore) input =
  next_parse t;
  t.calls <- t.calls + 1;
  let callback_failure = ref None in
  let call f =
    try f () with
    | exn ->
      let trace = Backtrace.Exn.most_recent () in
      callback_failure := Some (exn, trace);
      Exn.raise_with_original_backtrace exn trace
  in
  let poll () = call (fun () -> checkpoint t) in
  let module Builder =
    Chatmd_located_ast.Make (struct
      let input = input
      let checkpoint = poll
    end)
  in
  let module Parser = Chatmd_parser_engine.Make (Builder) in
  let next = Chatmd_lexer.create () in
  let depth = ref 0 in
  let token lexbuf =
    poll ();
    if t.tokens >= t.limits.tokens then failwith "owned ChatMD token limit exceeded";
    t.tokens <- t.tokens + 1;
    let token = next lexbuf in
    (match token with
     | Chatmd_tokens.START _ -> incr depth
     | END _ -> decr depth
     | SELF _ | TEXT _ | EOF -> ());
    if !depth > t.limits.depth then failwith "owned ChatMD markup nesting limit exceeded";
    call (fun () -> check_token token);
    token
  in
  try
    let bytes = Chatmd_parser_input.effective input in
    let next_byte = ref 0 in
    let lexbuf =
      Lexing.from_function (fun destination requested ->
        poll ();
        let available = String.length bytes - !next_byte in
        let count = Int.min 4096 (Int.min requested available) in
        Stdlib.Bytes.blit_string bytes !next_byte destination 0 count;
        next_byte := !next_byte + count;
        count)
    in
    let document =
      Parser.document token lexbuf
    in
    poll ();
    Ok document
  with
  | Parser.Error as exn ->
    let trace = Backtrace.Exn.most_recent () in
    (match !callback_failure with
     | Some (original, trace) -> Exn.raise_with_original_backtrace original trace
     | None -> Error (exn, trace))
;;

let parse t ?check_token input =
  match parse_worker t ?check_token input with
  | Ok document -> document
  | Error (exn, trace) -> Exn.raise_with_original_backtrace exn trace
;;

let parse_result t ?check_token input =
  parse_worker t ?check_token input
  |> Result.map_error ~f:(fun _ -> "invalid ChatMarkdown transcript syntax")
;;
