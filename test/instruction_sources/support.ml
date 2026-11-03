open! Core
module C = Instruction_content
module I = Agent_server.Instruction_sources
module Sources = Agent_server.Authoring_source_host
module P = Agent_protocol
module Storage = Agent_store.Prompt_program_bundle
module D = Document_schema

let limits = C.Limits.standard
let path value = Source.Portable_path.create value |> Or_error.ok_exn
let root_id value = C.Root_id.of_string value |> Or_error.ok_exn

let content_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : C.Error.t)]
;;

let io_ok = function
  | Ok value -> value
  | Error error -> raise_s (I.Error.sexp_of_t error)
;;

let source_ok = function
  | Ok value -> value
  | Error error -> raise_s (Sources.Error.sexp_of_t error)
;;

let storage_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let doc_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let source ?(namespace = "untrusted-capture-label") member contents =
  let id =
    Chatml_source.Id.create ~package_namespace:namespace ~relative_path:member
    |> Or_error.ok_exn
  in
  Chatml_source.create
    id
    ~contents
    ~expected_digest:None
    ~max_bytes:(Some 1_048_576)
    ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let observed ?(root = "project") ?(kind = C.Kind.Skill) member text =
  C.observe (source member text) ~root:(root_id root) ~kind ~limits ~checkpoint:ignore
  |> content_ok
;;

let loaded ?root ?kind member text =
  let observed = observed ?root ?kind member text in
  C.load observed ~source:(source member text) ~limits ~checkpoint:ignore |> content_ok
;;

let skill ?(description = "Useful instructions") name =
  sprintf "---\nname: %s\ndescription: %s\n---\n# Exact body\n" name description
;;

let actor id =
  P.Principal.create
    ~id:(P.Id.Principal.of_string id |> Project_support.ok)
    ~authentication_kind:"local.instruction_test"
    ~scopes:P.Scope.Set.empty
    ~attributes:[]
  |> Project_support.ok
  |> Operator_authorization.trusted_local
;;

let save root member text =
  let parent = Filename.dirname member in
  if not (String.equal parent ".")
  then Eio.Path.mkdirs ~perm:0o700 ~exists_ok:true Eio.Path.(root / parent);
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / member) text
;;

let with_roots ?(max_read_bytes = 1_048_576) ?(max_browse_entries = 256) f =
  Project_support.with_workspace (fun env home data ->
    let left = Eio.Path.(home / "left") in
    let right = Eio.Path.(home / "right") in
    List.iter [ left; right ] ~f:(Eio.Path.mkdir ~perm:0o700);
    Eio.Switch.run (fun sw ->
      let configuration =
        Project_support.Configuration.
          { external_roots = [ Eio.Path.native_exn left; Eio.Path.native_exn right ]
          ; git = None
          }
      in
      let project = Project_support.open_host ~configuration env data sw in
      let allowed = actor "pri_instruction_allowed" in
      let denied = actor "pri_instruction_denied" in
      let admitted = ref true in
      let policy =
        Sources.Read_policy.create ~authorize:(fun candidate ~root_index ->
          !admitted
          && (root_index = 0 || root_index = 1)
          && P.Id.Principal.equal
               (Operator_authorization.principal candidate).id
               (Operator_authorization.principal allowed).id)
      in
      let source_limits =
        Sources.Limits.create
          ~max_contexts:4
          ~max_context_requests:16
          ~max_manifest_entries:32
          ~max_browse_entries
          ~max_read_bytes
          ~max_context_read_bytes:8_388_608
        |> Or_error.ok_exn
      in
      let host =
        Sources.create
          project
          ~sw
          ~read_policy:policy
          ~document_policy:Sources.Document_policy.deny_all
          ~limits:source_limits
      in
      Exn.protect
        ~finally:(fun () ->
          Sources.close_and_wait host;
          Project_support.Host.close_and_wait project)
        ~f:(fun () ->
          let roots = Sources.roots host ~actor:allowed ~checkpoint:ignore |> source_ok in
          let raw_left = List.nth_exn roots 0 in
          let raw_right = List.nth_exn roots 1 in
          let bind raw name =
            I.bind_root host ~actor:allowed ~root:raw ~id:(root_id name) |> io_ok
          in
          f
            env
            home
            left
            right
            host
            raw_left
            (bind raw_left "left")
            (bind raw_right "right")
            allowed
            denied
            admitted)))
;;

let candidate_names discovery =
  I.Discovery.candidates discovery
  |> List.map ~f:(fun candidate ->
    C.Id.member (C.Observed.id (I.Candidate.observed candidate))
    |> Source.Portable_path.to_string)
;;

let discover_skills ?(limits = limits) ?(checkpoint = ignore) root actor =
  I.discover_skills root ~actor ~limits ~checkpoint
;;

let load ?(limits = limits) root actor candidate =
  I.load root ~actor candidate ~limits ~checkpoint:ignore
;;
