(** {1 𝚐𝚙𝚝 – multi-purpose CLI for Ochat, embeddings and code search}

    This executable is installed under the {!val:main_command} {!Command.group}
    and exposed as the [ochat] binary by [dune].  It bundles several loosely
    related developer-oriented utilities that build on the libraries living in
    the *ochat* code-base:

    • {{!val:index_command} index}      – crawl an OCaml project and write a
      hybrid semantic / lexical search corpus (see {!module:Indexer}).

    • {{!val:query_command} query}      – run natural-language retrieval over a
      previously created corpus using {!module:Vector_db.query_hybrid}.

    • {{!val:chat_completion_command} chat-completion} – convenience wrapper
      around {!Chat_response.Driver.run_completion_stream} for chatmd prompt
      files.

    • {{!val:tokenize_command} tokenize} – count {e Tikitoken} tokens of an
      arbitrary file; useful for prompt budgeting.

    • {{!val:html_to_markdown_command} html-to-markdown} / {b h2md} – convert
      static HTML to Markdown and preview the chunking heuristics used by
      {!module:Odoc_snippet}.

    A maintainer-oriented walk-through of the source file can be found in
    {b docs-src/bin/main.doc.md} (generated together with this comment).

    Invoke [ochat help SUBCOMMAND] for the fine-grained flag reference rendered
    by {!module:Core.Command}.  The sections below document each helper in
    more depth than the auto-generated usage strings.
*)

open Core
open Eio
open Command.Let_syntax
open Io

(** [index_command] builds a dense-vector + BM25 corpus from a directory tree.

    The command is exposed as:

{[ ochat index -folder-to-index ./lib -vector-db-folder ./vector ]}

    Flags:
    • [-folder-to-index] — root of the source tree to scan (defaults to
      [./lib]).  Both [*.ml] and [*.mli] files are parsed with
      {!module:Ocaml_parser} and their ocamldoc comments are chunked into
      token-bounded snippets.

    • [-vector-db-folder] — destination directory for the generated
      [vectors.{ml,mli}.binio] and [bm25.{ml,mli}.binio] artefacts.

    The heavy-lifting is delegated to {!Indexer.index}.  Concurrency is managed
    by a fresh {!Eio.Switch.t}, while HTTP calls to the OpenAI *Embeddings* API
    reuse the network capability from [env].  The function blocks until all
    files are written to disk.

    Example:
    {[
      $ ochat index -folder-to-index ./lib -vector-db-folder ./vector
    ]}
*)
let index_command =
  Command.basic
    ~summary:
      "Index OCaml code in the specified folder for a code vector search database using \
       OpenAI embeddings."
    (let%map_open folder_to_index =
       flag
         "-folder-to-index"
         (optional_with_default "./lib" string)
         ~doc:"FOLDER Path to the folder containing OCaml code to index (default: ./)"
     and vector_db_folder =
       flag
         "-vector-db-folder"
         (optional_with_default "./vector" string)
         ~doc:
           "FOLDER Path to the folder to store vector database data (default: ./vector)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       log ~dir @@ sprintf "Indexing OCaml code in folder: %s\n" folder_to_index;
       log ~dir @@ sprintf "Storing vector database data in folder: %s\n" vector_db_folder;
       Switch.run
       @@ fun sw ->
       let pool =
         Eio.Executor_pool.create
           ~sw
           (Eio.Stdenv.domain_mgr env)
           ~domain_count:(Domain.recommended_domain_count () - 1)
       in
       Indexer.index ~dir ~pool ~net:env#net ~vector_db_folder ~folder_to_index)
;;

(** [query_command] performs hybrid semantic × lexical retrieval over a corpus.

    Usage example:
    {[ ochat query -vector-db-folder ./vector -query-text "tail-recursive map" ]}

    * [-vector-db-folder] must point at the directory that holds the artefacts
      produced by {{!val:index_command} index_command}.

    * [-query-text] is the natural-language prompt.  A single embedding is
      requested from the OpenAI API and compared against every column of the
      corpus matrix.

    * [-num-results] upper-bounds the number of lines printed to stdout.

    The ranking function is {!Vector_db.query_hybrid}.  A fallback empty
    {!module:Bm25} index is created if the BM25 file cannot be found so that
    cosine similarity still works in isolation.
*)
let query_command =
  Command.basic
    ~summary:"Query the indexed OCaml code using natural language."
    (let%map_open vector_db_folder =
       flag
         "-vector-db-folder"
         (optional_with_default "./vector" string)
         ~doc:
           "FOLDER Path to the folder containing vector database data (default: ./vector)"
     and query_text =
       flag
         "-query-text"
         (required string)
         ~doc:"TEXT Natural language query text to search the indexed OCaml code"
     and num_results =
       flag
         "-num-results"
         (optional_with_default 5 int)
         ~doc:"NUM Number of top results to return (default: 5)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       log ~dir @@ sprintf "Querying indexed OCaml code with text: **%s**\n" query_text;
       log ~dir
       @@ sprintf "Using vector database data from folder: **%s**\n" vector_db_folder;
       log ~dir @@ sprintf "Returning top **%d** results\n" num_results;
       let vf = Eio.Stdenv.fs env / vector_db_folder in
       let vec_file = String.concat [ vector_db_folder; "/vectors.ml.binio" ] in
       let bm25_file = String.concat [ vector_db_folder; "/bm25.ml.binio" ] in
       let vecs = Vector_db.Vec.read_vectors_from_disk (Eio.Stdenv.fs env / vec_file) in
       let corpus = Vector_db.create_corpus vecs in
       let bm25 =
         try Bm25.read_from_disk (Eio.Stdenv.fs env / bm25_file) with
         | _ -> Bm25.create []
       in
       let response =
         Openai.Embeddings.post_openai_embeddings env#net ~input:[ query_text ]
       in
       let query_vector =
         Owl.Mat.of_arrays [| Array.of_list (List.hd_exn response.data).embedding |]
         |> Owl.Mat.transpose
       in
       let top_indices =
         Vector_db.query_hybrid
           corpus
           ~bm25
           ~beta:0.1
           ~embedding:query_vector
           ~text:query_text
           ~k:num_results
       in
       let docs = Vector_db.get_docs vf corpus top_indices in
       List.iteri
         ~f:(fun i doc ->
           print_endline @@ sprintf "\n**Result %d:**\n" (i + 1);
           print_endline @@ sprintf "```ocaml\n%s\n```\n" doc)
         docs)
;;

(** [chat_completion_command] feeds a *chatmd* conversation to the OpenAI
    Chat Completion endpoint and streams the assistant’s reply to a Markdown
    file.

    Flags:
    • [-prompt-file] – optional template prepended exactly once at the start of
      the output file.  If omitted the conversation continues from the
      existing [output-file] only.

    • [-output-file] – path where the running transcript is stored (default:
      {b ./prompts/default.md}).  The file is created on first run so you can
      resume later.

    Implementation note: the heavy lifting is done by
    {!Chat_response.Driver.run_completion_stream} which handles tool calling
    and incremental rendering.
*)
let chat_completion_command =
  Command.basic
    ~summary:
      "Call OpenAI API to provide chat completion based on the content of a chatmd \
       prompt file ."
    (let%map_open prompt_file =
       flag
         "-prompt-file"
         (optional string)
         ~doc:
           "FILE Path to the file containing inital prompt (optional). Think of this as \
            a  prompt template, and output-file as a conversation instance using the \
            prompt-template. If you are trying to continue a previous conversation do \
            not include this flag and only provide the output file"
     and output_file =
       flag
         "-output-file"
         (optional_with_default "./prompts/default.md" string)
         ~doc:
           "FILE Path to the file to save the chat completion output (default: \
            /prompts/default.md). If prompt-file is provided contents of prompt file \
            will be appended to the output file. "
     in
     fun () ->
       run_main
       @@ fun env ->
       Chat_response.Driver.run_completion_stream ~env ?prompt_file ~output_file ())
;;

(** [tokenize_command] prints the number of {e Tikitoken} tokens in a file.

    This is a thin wrapper around {!Tikitoken.encode}.  It loads the
    {e cl100k_base} Byte Pair Encoding rules once (~500 KB), encodes the file
    and outputs a single integer.

    Typical usage:
    {[ ochat tokenize -file bin/main.ml ]}
*)
let tokenize_command =
  Command.basic
    ~summary:"Tokenize the provided file using the OpenAI Tikitoken spec"
    (let%map_open file =
       flag
         "-file"
         (optional_with_default "bin/main.ml" string)
         ~doc:"FILE Path to the file to tokenize (default: bin/main.ml)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       let tiki_token_bpe = Tiktoken_data.o200k_base in
       let text = load_doc ~dir:(Eio.Stdenv.fs env) file in
       let codec = Tikitoken.create_codec tiki_token_bpe in
       log ~dir @@ sprintf "Tokenizing file: %s\n" file;
       let encoded = Tikitoken.encode ~codec ~text in
       Io.console_log ~stdout:env#stdout @@ sprintf "tokens: %i\n" (List.length encoded))
;;

(** [html_to_markdown_command] converts static HTML to Markdown and shows the
    internal chunking performed by {!Odoc_snippet}.

    It is primarily a debugging helper used when tweaking the snippet
    extraction heuristics.  The command prints:

    1. The full Markdown rendering.
    2. A delimiter line followed by every block returned by
       {!Odoc_snippet.Chunker.chunk_by_heading_or_blank}.
    3. The final slice passed to the embedding pipeline (sexp-encoded).

    Example:
    {[ ochat html-to-markdown -file docs/tutorial.html ]}
*)
let html_to_markdown_command =
  Command.basic
    ~summary:"Convert HTML file to Markdown"
    (let%map_open file =
       flag
         "-file"
         (optional_with_default "bin/main.ml" string)
         ~doc:"FILE Path to the HTML file to convert (default: bin/main.ml)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       let path = Eio.Path.(dir / file) in
       let markdown =
         Webpage_markdown.Driver.(convert_html_file path |> Markdown.to_string)
       in
       (* let block_strings =
         markdown |> String.split_lines |> Odoc_snippet.Chunker.chunk_by_heading_or_blank
       in *)
       (* let slice =
         Odoc_snippet.slice
           ~pkg:"html_to_markdown"
           ~doc_path:file
           ~markdown
           ~tiki_token_bpe:(load_doc ~dir "./out-cl100k_base.tikitoken.txt")
           ()
       in
       let s =
         Sexp.to_string_hum ~indent:2 [%sexp (slice : (Odoc_snippet.meta * string) list)]
       in *)
       Io.console_log ~stdout:env#stdout @@ sprintf "%s" markdown)
;;

let print_audit_errors errors =
  List.iter errors ~f:(fun (error : Shell_runtime.Audit_replay.error) ->
    eprintf
      "%s%s: %s\n"
      error.code
      (Option.value_map error.line ~default:"" ~f:(sprintf " line=%d"))
      error.message)
;;

let load_audit env path =
  Shell_runtime.Audit_replay.load_rotated ~fs:(Eio.Stdenv.fs env) ~path
;;

let print_shell_diagnostics diagnostics =
  List.iter diagnostics ~f:(fun diagnostic ->
    eprintf "%s\n" (Chat_response.Agent_runtime.diagnostic_to_string diagnostic))
;;

let signature_summary status =
  match status.Shell_runtime.Manifest_security.signature_key_id with
  | None -> "unsigned; accepted by administrative policy"
  | Some key_id ->
    sprintf
      "verified; key=%s%s"
      key_id
      (Option.value_map status.signature_issuer ~default:"" ~f:(fun issuer ->
         "; issuer=" ^ issuer))
;;

let shell_inspect_command =
  Command.basic
    ~summary:"Inspect requested/live shell authority without authorizing or executing it."
    (let%map_open path = anon ("CHATMD" %: string)
     and canonical =
       flag "-canonical" no_arg ~doc:" Print the canonical requested manifest JSON"
     in
     fun () ->
       run_main (fun env ->
         let fs = Eio.Stdenv.fs env in
         let prompt = Eio.Path.(fs / path) |> Eio.Path.load in
         let dir = Eio.Path.(fs / Filename.dirname path) in
         let elements = Prompt.Chat_markdown.parse_chat_inputs ~source:path ~dir prompt in
         match
           Chat_response.Agent_runtime.inspect_shell
             ~env
             ~platform:(Chat_response.Agent_runtime.platform ())
             ~prompt_elements:elements
         with
         | Error diagnostics ->
           print_shell_diagnostics diagnostics;
           Core.exit 1
         | Ok inspection ->
           let manifest = inspection.manifest in
           printf "requested manifest: %s\n" manifest.sha256;
           printf "live manifest:      %s\n" manifest.sha256;
           printf "administrative:     %s\n" inspection.administrative_policy.source;
           printf
             "signature:          %s\n"
             (signature_summary inspection.security_status);
           printf
             "trusted sources:    %d\n"
             (List.length inspection.security_status.trusted_sources);
           printf "live runtimes:      %d\n" (List.length inspection.live_runtimes);
           List.iter inspection.live_runtimes ~f:(fun runtime ->
             printf
               "  %s  profile=%s\n"
               (Chatmd_shell_spec.Shell_spec.Runtime_id.to_string runtime.id)
               (Option.value runtime.resolved_profile ~default:"custom"));
           if canonical then printf "\n%s\n" manifest.canonical_json))
;;

let shell_audit_validate_command =
  Command.basic
    ~summary:"Validate a shell audit JSONL file and its integrity chain."
    (let%map_open path = anon ("PATH" %: string) in
     fun () ->
       run_main (fun env ->
         match load_audit env path with
         | Error errors ->
           print_audit_errors errors;
           Core.exit 1
         | Ok events ->
           (match Shell_runtime.Audit_replay.validate events with
            | Error errors ->
              print_audit_errors errors;
              Core.exit 1
            | Ok () -> printf "valid audit log: %d events\n" (List.length events))))
;;

let shell_audit_replay_command =
  Command.basic
    ~summary:"Render a read-only shell audit timeline."
    (let%map_open path = anon ("PATH" %: string) in
     fun () ->
       run_main (fun env ->
         match load_audit env path with
         | Error errors ->
           print_audit_errors errors;
           Core.exit 1
         | Ok events ->
           List.iter events ~f:(fun event ->
             printf "%s\n" (Shell_runtime.Audit_replay.render_event event))))
;;

let shell_audit_request_command =
  Command.basic
    ~summary:"Reconstruct one shell request from an audit log without executing it."
    (let%map_open path = anon ("PATH" %: string)
     and request_id = anon ("REQUEST_ID" %: string) in
     fun () ->
       run_main (fun env ->
         match load_audit env path with
         | Error errors ->
           print_audit_errors errors;
           Core.exit 1
         | Ok events ->
           (match Shell_runtime.Audit_replay.request events ~request_id with
            | Error error ->
              print_audit_errors [ error ];
              Core.exit 1
            | Ok request ->
              printf "%s\n" (Shell_runtime.Audit_replay.render_request request))))
;;

let shell_audit_command =
  Command.group
    ~summary:"Validate and replay shell runtime audit logs."
    [ "validate", shell_audit_validate_command
    ; "replay", shell_audit_replay_command
    ; "request", shell_audit_request_command
    ]
;;

let load_shell_session env id =
  match Session_store.read_existing ~env ~id with
  | Some session -> session
  | None ->
    eprintf "Session not found or unreadable: %s\n" id;
    Core.exit 1
;;

let grant_scope = function
  | Session.Shell_state.Approval_scope.Exact_session -> "exact-session"
  | Prefix_session { prefix } -> "prefix-session(" ^ String.concat ~sep:" " prefix ^ ")"
  | Durable_exact -> "durable-exact"
;;

let grant_status grant =
  match grant.Session.Shell_state.Approval_grant.revoked_at_ns with
  | Some _ -> "revoked"
  | None ->
    let now = Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_int64 in
    if Option.exists grant.expires_at_ns ~f:(Int64.( < ) now) then "expired" else "active"
;;

let shell_grants_list_command =
  Command.basic
    ~summary:"List persisted shell approval grants for a session."
    (let%map_open session_id = anon ("SESSION_ID" %: string) in
     fun () ->
       run_main (fun env ->
         let session = load_shell_session env session_id in
         let grants = session.Session.shell_state.approval_grants in
         if List.is_empty grants
         then printf "No persisted shell approval grants.\n"
         else
           List.iter grants ~f:(fun grant ->
             printf
               "%s  %-16s  %-14s  runtime=%s  command=%s\n"
               grant.grant_id
               (grant_scope grant.scope)
               (grant_status grant)
               grant.runtime_id
               (String.prefix grant.command_sha256 16))))
;;

let find_grant session grant_id =
  List.find session.Session.shell_state.approval_grants ~f:(fun grant ->
    String.equal grant.Session.Shell_state.Approval_grant.grant_id grant_id)
;;

let shell_grants_explain_command =
  Command.basic
    ~summary:"Explain the non-secret identity and bindings of one shell grant."
    (let%map_open session_id = anon ("SESSION_ID" %: string)
     and grant_id = anon ("GRANT_ID" %: string) in
     fun () ->
       run_main (fun env ->
         let session = load_shell_session env session_id in
         match find_grant session grant_id with
         | None ->
           eprintf "Unknown shell grant: %s\n" grant_id;
           Core.exit 1
         | Some grant ->
           printf
             "grant: %s\n\
              status: %s\n\
              scope: %s\n\
              runtime: %s\n\
              manifest: %s\n\
              command: %s\n\
              executable: %s\n\
              cwd: %s\n\
              environment: %s\n\
              stdin: %s (%d bytes)\n\
              script: %s\n\
              session binding: %s\n\
              user binding: %s\n\
              host binding: %s\n\
              reviewer: %s\n\
              created ns: %Ld\n\
              expires ns: %s\n\
              last used ns: %s\n"
             grant.grant_id
             (grant_status grant)
             (grant_scope grant.scope)
             grant.runtime_id
             grant.manifest_sha256
             grant.command_sha256
             grant.executable_sha256
             grant.cwd_sha256
             grant.environment_sha256
             (Option.value grant.stdin_sha256 ~default:"none")
             grant.stdin_bytes
             (Option.value grant.script_sha256 ~default:"none")
             (Option.value grant.session_id ~default:"none")
             (Option.value grant.user_id ~default:"none")
             (Option.value grant.host_id ~default:"none")
             grant.reviewer.source
             grant.created_at_ns
             (Option.value_map grant.expires_at_ns ~default:"none" ~f:Int64.to_string)
             (Option.value_map grant.last_used_at_ns ~default:"never" ~f:Int64.to_string)))
;;

let shell_grants_revoke_command =
  Command.basic
    ~summary:"Revoke a persisted shell grant and append a chained audit event."
    (let%map_open session_id = anon ("SESSION_ID" %: string)
     and grant_id = anon ("GRANT_ID" %: string)
     and reason = flag "-reason" (optional string) ~doc:"TEXT Revocation reason"
     and confirm = flag "-confirm" no_arg ~doc:" Confirm the revocation mutation" in
     fun () ->
       if not confirm
       then (
         eprintf "Refusing to revoke without -confirm.\n";
         Core.exit 2);
       run_main (fun env ->
         let session = load_shell_session env session_id in
         let grant =
           match find_grant session grant_id with
           | Some grant -> grant
           | None ->
             eprintf "Unknown shell grant: %s\n" grant_id;
             Core.exit 1
         in
         let state = ref session in
         let persist updated =
           try
             Session_store.save ~env updated;
             Ok ()
           with
           | exn -> Error (Core.Exn.to_string exn)
         in
         let store =
           Shell_runtime.Approval_store.session
             ~session:state
             ~persist
             ~bindings:{ user_id = None; host_id = None }
         in
         (match
            Shell_runtime.Approval_store.revoke
              store
              ~now:(Time_ns.now ())
              ~grant_id
              ~reason
          with
          | Error error ->
            eprintf "%s: %s\n" error.code error.message;
            Core.exit 1
          | Ok () -> ());
         let audit_path =
           Filename.concat (Session_store.rel_path session_id) ".chatmd/shell-audit.jsonl"
         in
         match
           Shell_runtime.Audit_sink.append_management_event
             ~env
             ~path:audit_path
             ~session_id:(Some session_id)
             ~runtime_id:grant.runtime_id
             ~manifest_sha256:grant.manifest_sha256
             ~request_id:("grant-revoke:" ^ grant_id)
             ~event:"grant_revoked"
             ~fields:
               [ "grant_id", `String grant_id
               ; "scope", `String (grant_scope grant.scope)
               ; "reason", `String (Option.value reason ~default:"revoked by user")
               ]
         with
         | Error error ->
           eprintf
             "Grant was revoked, but audit persistence failed (%s): %s\n"
             error.code
             error.message;
           Core.exit 1
         | Ok sequence ->
           let shell_state =
             { !state.Session.shell_state with last_audit_sequence = Some sequence }
           in
           let updated = { !state with shell_state } in
           Session_store.save ~env updated;
           state := updated;
           printf "Revoked grant %s (audit sequence %Ld).\n" grant_id sequence))
;;

let shell_grants_command =
  Command.group
    ~summary:"Inspect, explain, and revoke persisted shell grants."
    [ "list", shell_grants_list_command
    ; "explain", shell_grants_explain_command
    ; "revoke", shell_grants_revoke_command
    ]
;;

let manifest_grant_status grant =
  match grant.Session.Shell_state.Manifest_grant.revoked_at_ns with
  | Some _ -> "revoked"
  | None ->
    let now = Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_int64 in
    if Option.exists grant.expires_at_ns ~f:(Int64.( < ) now) then "expired" else "active"
;;

let find_manifest_grant session grant_id =
  List.find session.Session.shell_state.manifest_grants ~f:(fun grant ->
    String.equal grant.Session.Shell_state.Manifest_grant.grant_id grant_id)
;;

let shell_manifest_grants_list_command =
  Command.basic
    ~summary:"List exact canonical manifest grants for a session."
    (let%map_open session_id = anon ("SESSION_ID" %: string) in
     fun () ->
       run_main (fun env ->
         let session = load_shell_session env session_id in
         let grants = session.Session.shell_state.manifest_grants in
         if List.is_empty grants
         then printf "No persisted shell manifest grants.\n"
         else
           List.iter grants ~f:(fun grant ->
             printf
               "%s  %-8s  manifest=%s  source=%s\n"
               grant.grant_id
               (manifest_grant_status grant)
               (String.prefix grant.manifest_sha256 16)
               (String.prefix grant.source_sha256 16))))
;;

let shell_manifest_grants_explain_command =
  Command.basic
    ~summary:"Explain one exact canonical manifest grant."
    (let%map_open session_id = anon ("SESSION_ID" %: string)
     and grant_id = anon ("GRANT_ID" %: string) in
     fun () ->
       run_main (fun env ->
         let session = load_shell_session env session_id in
         match find_manifest_grant session grant_id with
         | None ->
           eprintf "Unknown shell manifest grant: %s\n" grant_id;
           Core.exit 1
         | Some grant ->
           printf
             "grant: %s\n\
              status: %s\n\
              manifest: %s\n\
              source root: %s\n\
              source digest: %s\n\
              repository: %s\n\
              signer: %s\n\
              issuer: %s\n\
              session binding: %s\n\
              user binding: %s\n\
              host binding: %s\n\
              encoding schema: %d\n\
              builtin versions: %s\n\
              imports: %s\n"
             grant.grant_id
             (manifest_grant_status grant)
             grant.manifest_sha256
             grant.canonical_source_root
             grant.source_sha256
             (Option.value grant.repository_identity ~default:"none")
             (Option.value grant.signer ~default:"none")
             (Option.value grant.issuer ~default:"none")
             (Option.value grant.session_id ~default:"none")
             (Option.value grant.user_id ~default:"none")
             (Option.value grant.host_id ~default:"none")
             grant.schema_version
             (List.map grant.builtin_versions ~f:(fun (id, version) -> id ^ "=" ^ version)
              |> String.concat ~sep:", ")
             (List.map grant.imported_source_sha256 ~f:(fun (file, digest) ->
                file ^ "=" ^ String.prefix digest 16)
              |> String.concat ~sep:", ")))
;;

let shell_manifest_grants_revoke_command =
  Command.basic
    ~summary:"Revoke an exact canonical manifest grant."
    (let%map_open session_id = anon ("SESSION_ID" %: string)
     and grant_id = anon ("GRANT_ID" %: string)
     and reason = flag "-reason" (optional string) ~doc:"TEXT Revocation reason"
     and confirm = flag "-confirm" no_arg ~doc:" Confirm the revocation mutation" in
     fun () ->
       if not confirm
       then (
         eprintf "Refusing to revoke without -confirm.\n";
         Core.exit 2);
       run_main (fun env ->
         let session = load_shell_session env session_id in
         let grant =
           match find_manifest_grant session grant_id with
           | Some grant -> grant
           | None ->
             eprintf "Unknown shell manifest grant: %s\n" grant_id;
             Core.exit 1
         in
         let state = ref session in
         let persist updated =
           try
             Session_store.save ~env updated;
             Ok ()
           with
           | exn -> Error (Core.Exn.to_string exn)
         in
         (match
            Shell_runtime.Manifest_grant_store.revoke
              ~session:state
              ~persist
              ~grant_id
              ~reason
          with
          | Error message ->
            eprintf "%s\n" message;
            Core.exit 1
          | Ok () -> ());
         let audit_path =
           Filename.concat (Session_store.rel_path session_id) ".chatmd/shell-audit.jsonl"
         in
         match
           Shell_runtime.Audit_sink.append_management_event
             ~env
             ~path:audit_path
             ~session_id:(Some session_id)
             ~runtime_id:"manifest"
             ~manifest_sha256:grant.manifest_sha256
             ~request_id:("manifest-grant-revoke:" ^ grant_id)
             ~event:"manifest_grant_revoked"
             ~fields:
               [ "grant_id", `String grant_id
               ; "reason", `String (Option.value reason ~default:"revoked by user")
               ]
         with
         | Error error ->
           eprintf
             "Manifest grant was revoked, but audit persistence failed (%s): %s\n"
             error.code
             error.message;
           Core.exit 1
         | Ok sequence ->
           let shell_state =
             { !state.Session.shell_state with last_audit_sequence = Some sequence }
           in
           let updated = { !state with shell_state } in
           Session_store.save ~env updated;
           printf "Revoked manifest grant %s (audit sequence %Ld).\n" grant_id sequence))
;;

let shell_manifest_grants_command =
  Command.group
    ~summary:"Inspect and revoke exact canonical shell manifest grants."
    [ "list", shell_manifest_grants_list_command
    ; "explain", shell_manifest_grants_explain_command
    ; "revoke", shell_manifest_grants_revoke_command
    ]
;;

let shell_interrupted_list_command =
  Command.basic
    ~summary:"List interrupted shell requests; requests are never resumed."
    (let%map_open session_id = anon ("SESSION_ID" %: string) in
     fun () ->
       run_main (fun env ->
         let session = load_shell_session env session_id in
         let requests = session.Session.shell_state.interrupted_requests in
         if List.is_empty requests
         then printf "No interrupted shell requests.\n"
         else
           List.iter requests ~f:(fun request ->
             printf
               "%s  runtime=%s  retryable=%b  reason=%s\n  %s\n"
               request.request_id
               request.runtime_id
               request.retryable
               request.reason
               request.redacted_command)))
;;

let shell_interrupted_command =
  Command.group
    ~summary:"Inspect interrupted requests without resuming execution."
    [ "list", shell_interrupted_list_command ]
;;

let shell_command =
  Command.group
    ~summary:"Inspect and manage ChatMD shell runtimes."
    [ "audit", shell_audit_command
    ; "grants", shell_grants_command
    ; "inspect", shell_inspect_command
    ; "manifest-grants", shell_manifest_grants_command
    ; "interrupted", shell_interrupted_command
    ]
;;

(** [main_command] is the top-level {!Command.group} executed by the [ochat]
    binary.  It merely delegates to the sub-commands documented above.

    Run {b ochat help} or {b ochat help SUBCOMMAND} for the auto-generated manual
    pages provided by {!module:Command_unix}.
*)
let main_command =
  Command.group
    ~summary:
      "A command-line apps for using OpenAI Models for running chat completion on chatmd \
       files. Also provides Ocaml specfic functionality for indexing files into a vector \
       database, and natural language search of that ocaml code."
    [ "chat-completion", chat_completion_command
    ; "index", index_command
    ; "query", query_command
    ; "tokenize", tokenize_command
    ; "html-to-markdown", html_to_markdown_command
    ; "h2md", html_to_markdown_command
    ; "shell", shell_command
    ]
;;

let () = Command_unix.run main_command
