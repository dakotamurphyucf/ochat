open! Core
module P = Agent_protocol
module W = P.Instruction_control
module Client = Agent_client
module Controls = Client.Instruction_controls
module Fixture = Support.Config_fixture

let fail message = raise_s [%sexp "Instruction operator conformance", (message : string)]
let require condition message = if not condition then fail message

let checked = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "Instruction operator protocol error", (error : P.Error.t)]
;;

let configure ~env fixture =
  Fixture.grant_public_all_scopes fixture;
  let fs = Eio.Stdenv.fs env in
  let root = Fixture.physical_workspace fixture in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(fs / root / "AGENTS.md")
    "Conformance instructions for the selected workspace.\n";
  let fields =
    [%sexp
      { project_sources = { external_roots = ([ root ] : string list) }
      ; authoring_project_access =
          { read_grants =
              [ { root : string
                ; principal_id =
                    (P.Id.Principal.to_string (Fixture.public_principal fixture) : string)
                ; authentication_kind = ("http.static_bearer" : string)
                }
              ]
          ; selections = ([] : Sexp.t list)
          }
      }]
  in
  let config =
    let sections =
      match Sexp.of_string ("(" ^ Fixture.configuration fixture () ^ ")") with
      | Sexp.List sections -> sections
      | Atom _ -> fail "invalid conformance fixture configuration"
    in
    sections
    |> List.map ~f:(function
      | Sexp.List [ Atom "server"; List server ] ->
        (match fields with
         | Sexp.List fields -> Sexp.List [ Atom "server"; List (server @ fields) ]
         | Atom _ -> fail "invalid instruction fixture fields")
      | section -> section)
    |> List.map ~f:Sexp.to_string_hum
    |> String.concat ~sep:"\n"
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(fs / Fixture.config_path fixture)
    config
;;

let check ~sw ~env fixture ~spec =
  let connection =
    Agent_transport_http.Client.connect
      ~sw
      ~env
      ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" (Fixture.http_port fixture)))
      ~bearer_token:(Some (Fixture.public_token fixture))
      ~notification_capacity:64
    |> checked
  in
  Exn.protect
    ~finally:(fun () -> Client.Connection.close connection)
    ~f:(fun () ->
      let initialized = Support.Unix_driver.initialize connection |> checked in
      require
        (P.Id.Principal.equal initialized.principal.id (Fixture.public_principal fixture))
        "HTTP connection did not use the granted Instruction principal";
      let handle =
        Client.Session_handle.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~connection
          ~spec
          ~mode:Read_write
          ~subscribe:false
          ()
        |> checked
      in
      Exn.protect
        ~finally:(fun () -> Client.Session_handle.close handle)
        ~f:(fun () ->
          let control = Client.Session_handle.instructions handle in
          let initial = Controls.inspect control |> checked in
          require
            (Int64.equal initial.summary.revision 0L
             && not initial.summary.project_selected)
            "new session did not have an empty Instruction selection";
          let root =
            match
              W.Discovery.roots (Controls.refresh control ~selection:None |> checked)
            with
            | [ root ] -> root
            | _ -> fail "expected exactly one granted project root"
          in
          let selection = W.Selection.Project { root; scope = None } in
          let observed =
            Controls.refresh control ~selection:(Some selection) |> checked
          in
          let members = W.Discovery.members observed in
          require
            (List.equal
               String.equal
               [ "AGENTS.md" ]
               (List.map members ~f:(fun member ->
                  Source.Portable_path.to_string member.W.Member.member)))
            "Refresh did not return the real project Instruction file";
          let source = W.Source_request.create ~selection ~members |> checked in
          let capture = Controls.capture control source |> checked in
          let key suffix =
            P.Idempotency_key.of_string ("instruction-conformance:" ^ suffix) |> checked
          in
          let activate () =
            Controls.activate
              control
              ~expected_generation:initial.generation
              ~expected_revision:initial.summary.revision
              ~key:(key "activate")
              capture
            |> checked
          in
          let activated = activate () in
          require
            (W.Receipt.equal activated (activate ()) && Int64.equal activated.revision 1L)
            "repeat Activate did not retain its exact first receipt";
          let selected = Controls.inspect control |> checked in
          require
            (selected.summary.project_selected
             && selected.summary.project_files = 1
             && Int64.equal selected.summary.revision activated.revision)
            "Inspect did not publish the selected project Instruction";
          let removed =
            Controls.remove
              control
              ~expected_generation:selected.generation
              ~expected_revision:selected.summary.revision
              ~key:(key "remove")
              All
            |> checked
          in
          let cleared = Controls.inspect control |> checked in
          require
            ((not cleared.summary.project_selected)
             && Int64.equal removed.revision 2L
             && Int64.equal cleared.summary.revision removed.revision)
            "Remove did not publish the cleared Instruction selection"))
;;
