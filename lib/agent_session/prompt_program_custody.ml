open Core
module C = Chatml_prompt_programs
module Storage = Agent_store.Prompt_program_bundle

type t = C.t

let limits () =
  (* Every program/source descriptor exceeds100 encoded bytes. Root/Import are
     distinct roles, at most two per admitted authored path. Project work is not
     used during structural admission; actual graph restore receives host limits. *)
  C.Limits.create
    ~max_programs:(262144 / 100)
    ~max_parser_inputs:512
    ~max_logical_sources:(262144 / 100)
    ~max_total_input_bytes:(8 * 1024 * 1024)
    ~project_work_steps:1
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let authored ~root_file ~sources =
  Chatmd_source_bundle.create ~root_file ~sources ()
  |> Result.map_error ~f:Error.of_string
;;

let admit storage ~authored ~checkpoint =
  C.of_storage storage ~authored ~limits:(limits ()) ~checkpoint
;;

let prepare authored ~parsed_contexts ~root ~supplied ~checkpoint =
  let open Result.Let_syntax in
  let%bind custody =
    match supplied with
    | None ->
      C.capture
        authored
        ~parsed_contexts
        ~root
        ~programs:[]
        ~active_programs:[]
        ~limits:(limits ())
        ~checkpoint
    | Some supplied -> admit (C.to_storage supplied) ~authored ~checkpoint
  in
  let%map () =
    C.validate_capture custody ~parsed_contexts ~root ~limits:(limits ()) ~checkpoint
  in
  custody
;;

let fingerprint t ~checkpoint =
  let limits =
    Document_schema.Limits.create
      ~max_bytes:262144
      ~max_depth:128
      ~max_fields:100_000
      ~max_nodes:1_000_000
    |> Result.map_error ~f:(fun _ -> "Invalid fixed artifact policy")
    |> Result.ok_or_failwith
  in
  Storage.Manifest.fingerprint (Storage.manifest (C.to_storage t)) ~limits ~checkpoint
  |> Result.map_error ~f:(fun error -> C.Error.Document error)
;;

let replay ?(checkpoint = Eio.Fiber.check) input ~authored ~dir =
  Prompt_parser_boundary.run
    ~checkpoint
    ~on_parser_error:Or_error.of_exn
    ~f:(fun checkpoint ->
      checkpoint ();
      let source_loader =
        Source_loader.captured_filesystem
          ~root:dir
          ~sources:(Chatmd_source_bundle.sources authored)
      in
      let limits =
        Prompt.Chat_markdown.Limits.create
          ~max_input_bytes:(8 * 1024 * 1024)
          ~max_parse_calls:1024
          ~max_tokens:100_000
          ~max_depth:128
        |> Or_error.ok_exn
      in
      Ok
        (Prompt.Chat_markdown.replay_chat_inputs
           input
           ~source_loader
           ~dir
           ~limits
           ~checkpoint))
;;
