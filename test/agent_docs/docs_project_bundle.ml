open Core
module Path = Source.Portable_path
module Daemon = Agent_server.Daemon
module Catalog = Agent_session.Prompt_catalog
module Revision = Agent_session.Prompt_revision
module Programs = Chatml_prompt_programs

let require condition message = if not condition then failwith message

(* A failed gate remains the primary diagnostic even if cleanup also fails. *)
let with_cleanup ~finally f =
  match f () with
  | value ->
    Eio.Cancel.protect finally;
    value
  | exception exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect finally with
     | _ -> ());
    Exn.raise_with_original_backtrace exn trace
;;

let stage directory ~sources =
  let paths = List.map sources ~f:fst in
  require
    (Option.is_none (Path.first_collision paths ~checkpoint:Eio.Fiber.check))
    "tutorial bundle contains conflicting paths";
  List.iter sources ~f:(fun (path, contents) ->
    let name = Path.to_string path in
    Eio.Path.mkdirs
      ~exists_ok:true
      ~perm:0o700
      Eio.Path.(directory / Filename.dirname name);
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(directory / name) contents)
;;

let admission env ~home_parent ~bundle ~entry =
  let suffix = Agent_protocol.Id.Transaction.(create () |> to_string) in
  let home = Eio.Path.(home_parent / ("project-check-" ^ suffix)) in
  Eio.Path.mkdir ~perm:0o700 home;
  Eio.Switch.run (fun sw ->
    let home_name = Eio.Path.native_exn home in
    let bundle_name = Eio.Path.native_exn bundle in
    let configuration =
      Agent_server_test_support.config
        home_name
        bundle_name
        (Eio.Path.native_exn Eio.Path.(bundle / Path.to_string entry))
    in
    let daemon =
      Daemon.start
        ~sw
        ~env
        ~config:configuration
        ~tool_dir:bundle_name
        ~home:home_name
        ~process_start_identity:None
        ~options:{ Daemon.default_options with startup_mode = On_demand }
        ()
      |> Agent_server_test_support.protocol_ok
    in
    with_cleanup
      ~finally:(fun () -> Daemon.shutdown daemon |> Agent_server_test_support.protocol_ok)
      (fun () ->
         match Catalog.entries (Daemon.prompts daemon) with
         | [ { availability = Ready revision; _ } ] ->
           let custody =
             Revision.programs revision
             |> Option.value_exn ~message:"configured tutorial has no retained programs"
           in
           let retained =
             Programs.retained
               custody
               ~max_work_steps:10_000_000
               ~checkpoint:Eio.Fiber.check
             |> Result.map_error ~f:(fun _ ->
               "tutorial retained-program admission failed")
             |> Result.ok_or_failwith
           in
           require
             (List.exists (Programs.Retained.programs retained) ~f:(fun program ->
                match Programs.Retained.Program.input program with
                | Selected _ -> true
                | Provided -> false))
             "configured tutorial has no selected project";
           true
         | [ { availability = Unavailable diagnostics; _ } ] ->
           require
             (not (List.is_empty diagnostics))
             "unavailable tutorial lacks diagnostics";
           false
         | [] | _ :: _ -> failwith "tutorial needs exactly one enabled catalog prompt"))
;;

let check ~env ~home_parent ~bundle ~entry ~required =
  require (not (List.is_empty required)) "project check needs reachable required members";
  let ready () = admission env ~home_parent ~bundle ~entry in
  require (ready ()) ("configured bundle preparation failed: " ^ Path.to_string entry);
  List.iter (List.dedup_and_sort required ~compare:Path.compare) ~f:(fun member ->
    let path = Eio.Path.(bundle / Path.to_string member) in
    let contents = Eio.Path.load path in
    Eio.Path.unlink path;
    with_cleanup
      ~finally:(fun () -> Eio.Path.save ~create:(`Exclusive 0o600) path contents)
      (fun () ->
         require
           (not (ready ()))
           ("missing project companion unexpectedly resolved: " ^ Path.to_string member)))
;;
