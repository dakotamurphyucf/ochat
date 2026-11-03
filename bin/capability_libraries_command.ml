open! Core
open Command.Let_syntax
module Sources = Capability_sources
module Export = Capability_source_export

let source_result result =
  Result.map_error result ~f:Sources.Error.message |> Result.ok_or_failwith
;;

let print env text = Eio.Flow.copy_string text (Eio.Stdenv.stdout env)

let inventory () =
  Sources.installed ~limits:Sources.Limits.default ~checkpoint:Eio.Fiber.check
  |> source_result
;;

let resource inventory name =
  Sources.Id.of_string name |> source_result |> Sources.find inventory |> source_result
;;

let list_command =
  Command.basic
    ~summary:"List installed ordinary capability source trees."
    (let%map_open () = return () in
     fun () ->
       Eio_main.run (fun env ->
         let installed = inventory () in
         let resources =
           Sources.list installed ~max_items:16 ~checkpoint:Eio.Fiber.check
           |> source_result
         in
         List.iter resources ~f:(fun resource ->
           print
             env
             (sprintf
                "%s\t%s\t%s\t%s\n"
                (Sources.Id.to_string (Sources.Resource.id resource))
                (Chatml_source.Digest.to_hex (Sources.Resource.digest resource))
                (Source.Portable_path.to_string (Sources.Resource.entry resource))
                (Sources.Resource.title resource)))))
;;

let show_command =
  Command.basic
    ~summary:"Show installed member identities and requirements, or exact member bytes."
    (let%map_open name = anon ("ID" %: string)
     and member =
       flag
         "-member"
         (optional string)
         ~doc:"PATH Emit this exact portable member's bytes."
     in
     fun () ->
       Eio_main.run (fun env ->
         let selected = resource (inventory ()) name in
         match member with
         | None -> print env (Jsonaf.to_string (Sources.Resource.manifest selected) ^ "\n")
         | Some path ->
           let path = Source.Portable_path.create path |> Or_error.ok_exn in
           Sources.Resource.member selected path
           |> source_result
           |> Sources.Member.contents
           |> print env))
;;

let export_error error =
  let detail = Sexp.to_string_hum (Export.Error.sexp_of_t error) in
  match Export.Error.progress error with
  | Not_created ->
    failwith ("Source export rejected; destination was not created: " ^ detail)
  | Incomplete ->
    failwith
      ("Source export did not complete; a partial destination may remain. Inspect or \
        remove it explicitly before retrying: "
       ^ detail)
;;

let export_command =
  Command.basic
    ~summary:"Copy one complete source tree into a new explicitly selected directory."
    (let%map_open name = anon ("ID" %: string)
     and parent =
       flag
         "-parent"
         (required string)
         ~doc:"DIRECTORY Existing caller-selected parent directory."
     and directory =
       flag
         "-directory"
         (required string)
         ~doc:"NAME New absent single child name; never overwritten."
     in
     fun () ->
       Eio_main.run (fun env ->
         let selected = resource (inventory ()) name in
         if String.is_empty parent
         then failwith "-parent must explicitly name an existing directory";
         let directory =
           match Export.Directory_name.create directory with
           | Ok value -> value
           | Error error -> export_error error
         in
         let receipt =
           Eio.Switch.run (fun sw ->
             let parent = Eio.Path.open_dir ~sw Eio.Path.(Eio.Stdenv.fs env / parent) in
             Export.export selected ~parent ~directory ~checkpoint:Eio.Fiber.check)
         in
         match receipt with
         | Error error -> export_error error
         | Ok receipt ->
           print
             env
             (sprintf
                "Exported %s (%s), %d members, %d bytes.\nEntry: %s/%s\n"
                (Sources.Id.to_string (Export.Receipt.resource receipt))
                (Chatml_source.Digest.to_hex (Export.Receipt.digest receipt))
                (Export.Receipt.members receipt)
                (Export.Receipt.bytes receipt)
                (Export.Directory_name.to_string directory)
                (Source.Portable_path.to_string (Export.Receipt.entry receipt)))))
;;

let command =
  Command.group
    ~summary:"Inspect and explicitly export installed capability source libraries."
    [ "list", list_command; "show", show_command; "export", export_command ]
;;
