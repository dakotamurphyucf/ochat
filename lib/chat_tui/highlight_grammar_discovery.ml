open Core

module Source = struct
  type t =
    { name : string
    ; json : Jsonaf.t
    }

  let create ~name json =
    let registry = Highlight_tm_loader.create_registry () in
    let open Or_error.Let_syntax in
    let%map () = Highlight_tm_loader.add_grammar_jsonaf registry json in
    { name; json }
  ;;

  let name t = t.name
  let json t = t.json
end

module Loaded_source = struct
  type t =
    { name : string
    ; contents : string
    }

  let name t = t.name
  let contents t = t.contents
end

let nonempty_env name =
  Sys.getenv name
  |> Option.bind ~f:(fun value -> Option.some_if (not (String.is_empty value)) value)
;;

let env_directories () =
  nonempty_env "OCHAT_GRAMMAR_DIR"
  |> Option.value_map ~default:[] ~f:(fun value ->
    String.split value ~on:':' |> List.filter ~f:(Fn.non String.is_empty))
;;

let default_directory () =
  match nonempty_env "XDG_CONFIG_HOME", nonempty_env "HOME" with
  | Some base, _ -> Some (Filename.concat base "ochat/grammars")
  | None, Some home -> Some (Filename.concat home ".config/ochat/grammars")
  | None, None -> None
;;

let eio_path ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~(cwd : Eio.Fs.dir_ty Eio.Path.t) path =
  if Filename.is_absolute path then Eio.Path.(fs / path) else Eio.Path.(cwd / path)
;;

let load_file registry path =
  let open Or_error.Let_syntax in
  let%bind contents = Or_error.try_with (fun () -> Eio.Path.load path) in
  let%bind json = Jsonaf.parse contents in
  Highlight_tm_loader.add_grammar_jsonaf registry json
;;

let load_source ~name path =
  let open Or_error.Let_syntax in
  let%bind contents = Or_error.try_with (fun () -> Eio.Path.load path) in
  let%bind json = Jsonaf.parse contents in
  Source.create ~name json
;;

let load_source_contents ~name path =
  Or_error.try_with (fun () -> Eio.Path.load path)
  |> Or_error.map ~f:(fun contents -> { Loaded_source.name; contents })
;;

let load_explicit_sources ~fs ~cwd files =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | filename :: rest ->
      let open Or_error.Let_syntax in
      let%bind source =
        load_source ~name:filename (eio_path ~fs ~cwd filename)
        |> Or_error.tag
             ~tag:(sprintf "Failed to load explicit TextMate grammar %S" filename)
      in
      loop (source :: acc) rest
  in
  loop [] files
;;

let load_explicit ~fs ~cwd ~registry files =
  let rec loop = function
    | [] -> Ok ()
    | filename :: rest ->
      let open Or_error.Let_syntax in
      let%bind () =
        load_file registry (eio_path ~fs ~cwd filename)
        |> Or_error.tag
             ~tag:(sprintf "Failed to load explicit TextMate grammar %S" filename)
      in
      loop rest
  in
  loop files
;;

let warn_error warn message error =
  warn (sprintf "%s: %s" message (Error.to_string_hum error))
;;

let load_discovered_file ~registry ~warn path =
  match load_file registry path with
  | Ok () -> ()
  | Error error ->
    warn_error
      warn
      (sprintf "Failed to load discovered TextMate grammar %S" (Eio.Path.native_exn path))
      error
;;

let discover_file ~warn path =
  let name = Eio.Path.native_exn path in
  match load_source ~name path with
  | Ok source -> Some source
  | Error error ->
    warn_error warn (sprintf "Failed to load discovered TextMate grammar %S" name) error;
    None
;;

let read_discovered_file ~warn path =
  let name = Eio.Path.native_exn path in
  match load_source_contents ~name path with
  | Ok source -> Some source
  | Error error ->
    warn_error warn (sprintf "Failed to load discovered TextMate grammar %S" name) error;
    None
;;

let discover_directory ~fs ~cwd ~warn directory =
  let path = eio_path ~fs ~cwd directory in
  match Or_error.try_with (fun () -> Eio.Path.read_dir path) with
  | Error error ->
    warn_error
      warn
      (sprintf "Failed to scan TextMate grammar directory %S" directory)
      error;
    []
  | Ok entries ->
    entries
    |> List.filter ~f:(fun filename -> Filename.check_suffix filename ".json")
    |> List.sort ~compare:String.compare
    |> List.filter_map ~f:(fun name ->
      let grammar_path = Eio.Path.(path / name) in
      match Or_error.try_with (fun () -> Eio.Path.is_file grammar_path) with
      | Ok true -> discover_file ~warn grammar_path
      | Ok false -> None
      | Error error ->
        warn_error
          warn
          (sprintf
             "Failed to inspect discovered TextMate grammar %S"
             (Eio.Path.native_exn grammar_path))
          error;
        None)
;;

let read_discovered_directory ~fs ~cwd ~warn directory =
  let path = eio_path ~fs ~cwd directory in
  match Or_error.try_with (fun () -> Eio.Path.read_dir path) with
  | Error error ->
    warn_error
      warn
      (sprintf "Failed to scan TextMate grammar directory %S" directory)
      error;
    []
  | Ok entries ->
    entries
    |> List.filter ~f:(fun filename -> Filename.check_suffix filename ".json")
    |> List.sort ~compare:String.compare
    |> List.filter_map ~f:(fun name ->
      let grammar_path = Eio.Path.(path / name) in
      match Or_error.try_with (fun () -> Eio.Path.is_file grammar_path) with
      | Ok true -> read_discovered_file ~warn grammar_path
      | Ok false -> None
      | Error error ->
        warn_error
          warn
          (sprintf
             "Failed to inspect discovered TextMate grammar %S"
             (Eio.Path.native_exn grammar_path))
          error;
        None)
;;

let discover_default_directory ~fs ~cwd ~warn directory =
  let path = eio_path ~fs ~cwd directory in
  match Or_error.try_with (fun () -> Eio.Path.kind ~follow:true path) with
  | Ok `Not_found -> []
  | Ok _ -> discover_directory ~fs ~cwd ~warn directory
  | Error error ->
    warn_error
      warn
      (sprintf "Failed to inspect TextMate grammar directory %S" directory)
      error;
    []
;;

let read_discovered_default_directory ~fs ~cwd ~warn directory =
  let path = eio_path ~fs ~cwd directory in
  match Or_error.try_with (fun () -> Eio.Path.kind ~follow:true path) with
  | Ok `Not_found -> []
  | Ok _ -> read_discovered_directory ~fs ~cwd ~warn directory
  | Error error ->
    warn_error
      warn
      (sprintf "Failed to inspect TextMate grammar directory %S" directory)
      error;
    []
;;

let read_discovered_sources ~fs ~cwd ~warn () =
  let environment =
    List.concat_map (env_directories ()) ~f:(read_discovered_directory ~fs ~cwd ~warn)
  in
  let defaults =
    Option.value_map
      (default_directory ())
      ~default:[]
      ~f:(read_discovered_default_directory ~fs ~cwd ~warn)
  in
  environment @ defaults
;;

let parse_discovered_sources loaded =
  List.fold loaded ~init:([], []) ~f:(fun (sources, warnings) loaded ->
    let result =
      let open Or_error.Let_syntax in
      let%bind json = Jsonaf.parse (Loaded_source.contents loaded) in
      Source.create ~name:(Loaded_source.name loaded) json
    in
    match result with
    | Ok source -> source :: sources, warnings
    | Error error ->
      let warning =
        sprintf
          "Failed to load discovered TextMate grammar %S: %s"
          (Loaded_source.name loaded)
          (Error.to_string_hum error)
      in
      sources, warning :: warnings)
  |> fun (sources, warnings) -> List.rev sources, List.rev warnings
;;

let load_discovered_sources ~fs ~cwd ~warn () =
  let loaded = read_discovered_sources ~fs ~cwd ~warn () in
  let sources, warnings = parse_discovered_sources loaded in
  List.iter warnings ~f:warn;
  sources
;;

let scan_directory ~fs ~cwd ~registry ~warn directory =
  let path = eio_path ~fs ~cwd directory in
  match Or_error.try_with (fun () -> Eio.Path.read_dir path) with
  | Error error ->
    warn_error
      warn
      (sprintf "Failed to scan TextMate grammar directory %S" directory)
      error
  | Ok entries ->
    entries
    |> List.filter ~f:(fun filename -> Filename.check_suffix filename ".json")
    |> List.sort ~compare:String.compare
    |> List.iter ~f:(fun name ->
      let grammar_path = Eio.Path.(path / name) in
      match Or_error.try_with (fun () -> Eio.Path.is_file grammar_path) with
      | Ok true -> load_discovered_file ~registry ~warn grammar_path
      | Ok false -> ()
      | Error error ->
        warn_error
          warn
          (sprintf
             "Failed to inspect discovered TextMate grammar %S"
             (Eio.Path.native_exn grammar_path))
          error)
;;

let scan_default_directory ~fs ~cwd ~registry ~warn directory =
  let path = eio_path ~fs ~cwd directory in
  match Or_error.try_with (fun () -> Eio.Path.kind ~follow:true path) with
  | Ok `Not_found -> ()
  | Ok _ -> scan_directory ~fs ~cwd ~registry ~warn directory
  | Error error ->
    warn_error
      warn
      (sprintf "Failed to inspect TextMate grammar directory %S" directory)
      error
;;

let load_discovered ~fs ~cwd ~registry ~warn () =
  List.iter (env_directories ()) ~f:(fun directory ->
    scan_directory ~fs ~cwd ~registry ~warn directory);
  Option.iter (default_directory ()) ~f:(fun directory ->
    scan_default_directory ~fs ~cwd ~registry ~warn directory)
;;

let load
      ~(fs : Eio.Fs.dir_ty Eio.Path.t)
      ~(cwd : Eio.Fs.dir_ty Eio.Path.t)
      ~registry
      ~explicit_files
      ~warn
      ()
  =
  let open Or_error.Let_syntax in
  let%map () = load_explicit ~fs ~cwd ~registry explicit_files in
  load_discovered ~fs ~cwd ~registry ~warn ()
;;
