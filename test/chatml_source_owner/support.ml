open Core
module Owner = Agent_session.Prompt_source_owner
module Receipt = Agent_store.Prompt_source_receipt
module Builder = Agent_session.Prompt_revision_builder
module Catalog = Agent_session.Prompt_catalog

let () = Mirage_crypto_rng_unix.use_default ()

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected source-owner fixture failure"
;;

let with_workspace f =
  Eio_main.run (fun env ->
    let name = "och218-" ^ Agent_protocol.Id.Transaction.(create () |> to_string) in
    let parent = Eio.Path.(env#fs / "/tmp" / name) in
    Eio.Path.mkdir ~perm:0o700 parent;
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree parent))
      (fun () ->
         let workspace = Eio.Path.(parent / "workspace") in
         Eio.Path.mkdir ~perm:0o700 workspace;
         Eio.Path.with_open_dir workspace (fun root -> f env parent root)))
;;

let save root name text =
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) text
;;

let definition ?(enabled = true) root =
  Agent_session.Prompt_definition.create
    ~id:(Agent_protocol.Id.Prompt_definition.of_string "prd_original" |> ok)
    ~config_name:"original"
    ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
    ~allowed_workspaces:[]
    ~permission_profile:"interactive"
    ~runtime_policy:None
    ~enabled
    ~description:None
  |> ok
;;

let receipt definition =
  Receipt.create
    ~definition_id:definition.Agent_session.Prompt_definition.id
    ~root_file:definition.root_file
    ~max_path_bytes:4096
    ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let limits ?(leases = 4) () =
  Owner.Limits.create ~max_path_components:32 ~max_runtime_leases:leases
  |> Or_error.ok_exn
;;

let parser_limits () =
  Prompt.Chat_markdown.Limits.create
    ~max_input_bytes:100_000
    ~max_parse_calls:100
    ~max_tokens:10_000
    ~max_depth:64
  |> Or_error.ok_exn
;;

let open_owner sw root definition =
  Owner.open_owned
    root
    ~sw
    ~policy:(Owner.Policy.create (receipt definition) ~limits:(limits ()))
    ~checkpoint:ignore
  |> ok
;;

let capture ?(checkpoint = ignore) env scope definition =
  Owner.Scope.capture
    scope
    ~env
    ~definition
    ~preprocess:Fn.id
    ~limits:(parser_limits ())
    ~checkpoint
;;

let root_text captured =
  Owner.Capture.value captured
  |> Builder.Capture.root
  |> Prompt.Chat_markdown.Parsed.root_input
  |> Chatmd_parser_input.authored
;;

let error result code =
  match result with
  | Ok _ -> failwith "expected source-owner rejection"
  | Error error -> assert (Owner.Error.equal_code (Owner.Error.code error) code)
;;

let catalog env parent =
  let artifact_store =
    Agent_store.Prompt_artifact_store.create
      ~env
      ~root:(Eio.Path.native_exn Eio.Path.(parent / "artifacts"))
    |> ok
  in
  Catalog.create ~env ~artifact_store
;;

let prepare ?prepare_programs ?(checkpoint = ignore) catalog sw definitions =
  Catalog.prepare_owned
    catalog
    ?prepare_programs
    ~sw
    ~source_limits:(limits ())
    ~max_roots:8
    ~checkpoint
    ~transaction_id:(fun _ -> Agent_protocol.Id.Transaction.create ())
    ~created_at:(Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok)
    definitions
;;

let selection catalog definition =
  Catalog.source_selection catalog definition.Agent_session.Prompt_definition.id
  |> Option.value_exn
;;

let revision catalog definition =
  match Catalog.find catalog definition.Agent_session.Prompt_definition.id with
  | Some { availability = Ready revision; _ } -> revision
  | None | Some { availability = Unavailable _ | Disabled; _ } ->
    failwith "expected ready original revision"
;;

let descriptors env = Eio.Path.read_dir Eio.Path.(env#fs / "/dev/fd") |> List.length
