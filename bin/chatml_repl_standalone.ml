open! Core

type t =
  { policy_file : string
  ; prompt_file : string
  ; workspace : string
  ; source : string
  }

let policy directory =
  let root = Sexp.to_string (Atom directory) in
  let capacity =
    Agent_server.Developer_startup_configuration.Capacity.(
      sexp_of_t standard_v1 |> Sexp.to_string)
  in
  Printf.sprintf
    {|(project_sources ((external_roots (%s))))
(authoring_project_access
 ((read_grants (((root %s) (principal_id Embedded_operator)
                 (authentication_kind embedded.local))))
  (selections (((root %s) (project repl.chatml)
                (request (entry ((name main)))) (target one_off_v1))))))
(developer
 ((selections
   (((source (Local_source (root %s) (member repl.chatml)))
     (target One_off_v1)
     (principals (((principal_id Embedded_operator)
                   (authentication_kind embedded.local)))))))
  (capacity %s)
  (model Disabled) (approvals Disabled) (repl Standard_v1) (portable true)))
(developer_scopes (developer.repl developer.live.run))
|}
    root
    root
    root
    root
    capacity
;;

let with_environment ~env ~f =
  Eio.Switch.run (fun sw ->
    let directory, root =
      Eio.Cancel.protect (fun () ->
        let directory =
          (* Eio.Path has no mkdtemp operation. Use atomic private directory
             creation in a systhread; all subsequent I/O is Eio-owned. *)
          Eio_unix.run_in_systhread (fun () -> Core_unix.mkdtemp "/tmp/ochat-repl-XXXXXX")
        in
        let root = Eio.Path.(Eio.Stdenv.fs env / directory) in
        Eio.Switch.on_release sw (fun () -> Eio.Path.rmtree root);
        directory, root)
    in
    let save name contents =
      Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(root / name) contents
    in
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "workspace");
    save "repl.chatml" "let main input = Task.pure(input)\n";
    save "agent.chatmd" "<developer>Standalone ChatML REPL.</developer>\n";
    save "policy.sexp" (policy directory);
    f
      { policy_file = Filename.concat directory "policy.sexp"
      ; prompt_file = Filename.concat directory "agent.chatmd"
      ; workspace = Filename.concat directory "workspace"
      ; source = "repl.chatml"
      })
;;
