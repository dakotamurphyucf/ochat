open Core
open Agent_store_test_fixtures
open Job_store_fixtures
module P = Agent_protocol
module Store = Agent_store.Job_result_store
module Intent = Agent_store.Job_result_intent

let%expect_test
    "intent and metadata failures retry one selected completion and one blob identity"
  =
  List.iter
    [ "intent", ".frame", 3; "temporary", ".sexp", 1; "durable", ".sexp", 3 ]
    ~f:(fun (stage, suffix, failures) ->
      List.iter [ false; true ] ~f:(fun after_rename ->
        let armed = ref None in
        let paths = ref [] in
        let uploads = ref 0 in
        let matches_rename path =
          String.is_suffix path ~suffix
          &&
          match stage with
          | "temporary" -> String.is_substring path ~substring:"/temporary/"
          | "durable" -> String.is_substring path ~substring:"/blobs/"
          | _ -> true
        in
        let before_open_out path =
          if String.is_suffix path ~suffix:".part" then incr uploads
        in
        with_store
          ~wrap_env:(fun env ->
            fault_env
              ~matches_rename
              ~before_open_out
              ~on_failure:(fun path -> paths := path :: !paths)
              env
              armed)
          (fun env sw blobs _ session _ ->
             let job = job session in
             let completion = P.Completion.Succeeded (`String (String.make 512 'x')) in
             let publisher =
               Store.Publisher.create
                 ~env
                 ~blobs
                 ~sw
                 ~session
                 ~principal
                 ~inline_bytes:64
                 ~max_bytes:4096
               |> protocol_ok
             in
             let published = ref 0 in
             let publish completion =
               Store.Publisher.publish
                 publisher
                 ~jobs:[ job ]
                 ~job
                 ~now:timestamp
                 completion
                 ~persist:(fun stored ->
                   incr published;
                   Ok stored)
             in
             for _ = 1 to failures do
               armed := Some after_rename;
               assert (Result.is_error (publish completion));
               assert (Option.is_none !armed);
               assert (
                 P.Completion.equal
                   completion
                   (Store.Publisher.pending_completion publisher ~job |> Option.value_exn));
               [%test_eq: int] 0 !published;
               let intents = Intent.list ~env ~session ~max_count:8 |> store_ok in
               [%test_eq: int]
                 (if String.equal stage "intent" && not after_rename then 0 else 1)
                 (List.length intents)
             done;
             assert (
               Result.is_error (publish (P.Completion.Succeeded (`String "replacement"))));
             let stored = publish completion |> protocol_ok in
             [%test_eq: int] 1 !published;
             [%test_eq: int] 1 !uploads;
             let reference =
               match stored with
               | P.Stored_completion.Artifact { reference; _ } -> reference
               | _ -> failwith "expected staged artifact"
             in
             List.iter !paths ~f:(fun path ->
               [%test_eq: string]
                 (P.Id.Blob.to_string reference.blob.id)
                 (Filename.basename path |> String.chop_suffix_exn ~suffix));
             assert (List.is_empty (Intent.list ~env ~session ~max_count:8 |> store_ok));
             assert (Option.is_none (Store.Publisher.pending_completion publisher ~job));
             assert (
               P.Completion.equal
                 completion
                 (Store.Publisher.load publisher reference |> protocol_ok));
             print_s
               [%sexp
                 { stage : string
                 ; after_rename : bool
                 ; failed_writes = (failures : int)
                 ; uploads = (!uploads : int)
                 ; publications = (!published : int)
                 }])));
  [%expect
    {|
    ((stage intent) (after_rename false) (failed_writes 3) (uploads 1)
     (publications 1))
    ((stage intent) (after_rename true) (failed_writes 3) (uploads 1)
     (publications 1))
    ((stage temporary) (after_rename false) (failed_writes 1) (uploads 1)
     (publications 1))
    ((stage temporary) (after_rename true) (failed_writes 1) (uploads 1)
     (publications 1))
    ((stage durable) (after_rename false) (failed_writes 3) (uploads 1)
     (publications 1))
    ((stage durable) (after_rename true) (failed_writes 3) (uploads 1)
     (publications 1))
    |}]
;;

let%expect_test
    "partial upload retries refuse conflicting bytes and links before rebuilding the \
     selected data"
  =
  let reject_open = ref false in
  let partial = ref None in
  let before_open_out path =
    match !reject_open && String.is_suffix path ~suffix:".part" with
    | false -> ()
    | true ->
      reject_open := false;
      partial := Some path;
      failwith "injected upload open failure"
  in
  with_store
    ~wrap_env:(fun env -> fault_env ~before_open_out env (ref None))
    (fun env sw blobs _ session _ ->
       let job = job session in
       let completion = P.Completion.Succeeded (`String (String.make 512 'x')) in
       let content = P.Completion.to_json completion |> Jsonaf.to_string in
       let publisher =
         Store.Publisher.create
           ~env
           ~blobs
           ~sw
           ~session
           ~principal
           ~inline_bytes:64
           ~max_bytes:4096
         |> protocol_ok
       in
       let published = ref 0 in
       let publish () =
         Store.Publisher.publish
           publisher
           ~jobs:[ job ]
           ~job
           ~now:timestamp
           completion
           ~persist:(fun stored ->
             incr published;
             Ok stored)
       in
       reject_open := true;
       assert (Result.is_error (publish ()));
       let intent = Intent.list ~env ~session ~max_count:8 |> store_ok |> List.hd_exn in
       let reference = Intent.reference intent in
       let file = Eio.Path.(Eio.Stdenv.fs env / Option.value_exn !partial) in
       Eio.Path.save ~create:(`Exclusive 0o600) file "alien bytes";
       assert (Result.is_error (publish ()));
       [%test_eq: string] "alien bytes" (Eio.Path.load file);
       Eio.Path.unlink file;
       let target_path =
         Filename.concat
           (Agent_store.Session_store.Handle.directory session)
           "foreign-fixture"
       in
       let target = Eio.Path.(Eio.Stdenv.fs env / target_path) in
       Eio.Path.save ~create:(`Exclusive 0o600) target (String.prefix content 20);
       Eio.Path.symlink ~link_to:target_path file;
       assert (Result.is_error (publish ()));
       [%test_eq: string] (String.prefix content 20) (Eio.Path.load target);
       [%test_eq: int] 0 !published;
       Eio.Path.unlink file;
       Eio.Path.save ~create:(`Exclusive 0o600) file (String.prefix content 20);
       let stored = publish () |> protocol_ok in
       [%test_eq: int] 1 !published;
       assert (P.Stored_completion.matches stored completion |> protocol_ok);
       assert (
         P.Completion.equal
           completion
           (Store.Publisher.load publisher reference |> protocol_ok));
       print_endline
         "conflicting partial and symlink preserved; matching partial rebuilt under the \
          same reference; published once");
  [%expect
    {| conflicting partial and symlink preserved; matching partial rebuilt under the same reference; published once |}]
;;
