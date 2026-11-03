open! Core
module Sources = Capability_sources
module Export = Capability_source_export

let source_ok = function
  | Ok value -> value
  | Error error -> failwith (Sources.Error.message error)
;;

let export_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Export.Error.t)]
;;

let resource () =
  Sources.installed ~limits:Sources.Limits.default ~checkpoint:Eio.Fiber.check
  |> source_ok
  |> fun inventory ->
  Sources.find inventory (Sources.Id.of_string "lifecycle" |> source_ok) |> source_ok
;;

let path name = Source.Portable_path.create name |> Or_error.ok_exn
let directory name = Export.Directory_name.create name |> export_ok

let with_workspace f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root_name = Agent_server_test_support.temporary_root env in
    let root = Eio.Path.(Eio.Stdenv.fs env / root_name) in
    Exn.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let parent = Eio.Path.open_dir ~sw root in
          f env root parent)))
;;

let export ?(checkpoint = Eio.Fiber.check) resource parent name =
  Export.export resource ~parent ~directory:(directory name) ~checkpoint
;;

let require_error result ~code ~progress =
  match result with
  | Ok _ -> failwith "export unexpectedly succeeded"
  | Error error ->
    assert (Export.Error.equal_code (Export.Error.code error) code);
    assert (Export.Error.equal_progress (Export.Error.progress error) progress)
;;

let missing path =
  match Eio.Path.kind ~follow:false path with
  | `Not_found -> true
  | _ -> false
;;
