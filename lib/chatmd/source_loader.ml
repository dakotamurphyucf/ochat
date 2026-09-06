open! Core

type source =
  { file_name : string
  ; relative_path : string
  ; directory : Eio.Fs.dir_ty Eio.Path.t
  ; path : Eio.Fs.dir_ty Eio.Path.t
  }

type t =
  { root : Eio.Fs.dir_ty Eio.Path.t
  ; confined : bool
  ; captured : string String.Map.t option
  ; observer : (source -> string -> unit) option
  ; agent_observer : (source -> unit) option
  }

let filesystem ~root =
  { root; confined = false; captured = None; observer = None; agent_observer = None }
;;

let confined_filesystem ~root =
  { root; confined = true; captured = None; observer = None; agent_observer = None }
;;

let captured_filesystem ~root ~sources =
  { (confined_filesystem ~root) with captured = Some (String.Map.of_alist_exn sources) }
;;

let with_observer t ~f = { t with observer = Some f }
let with_agent_observer t ~f = { t with agent_observer = Some f }
let root_dir t = t.root

let normalize path =
  String.substr_replace_all path ~pattern:"\\" ~with_:"/"
  |> String.split ~on:'/'
  |> List.fold ~init:(Ok []) ~f:(fun state component ->
    let open Result.Let_syntax in
    let%bind parts = state in
    match component with
    | "" | "." -> Ok parts
    | ".." ->
      (match parts with
       | [] -> Error "source reference escapes its root"
       | _ :: rest -> Ok rest)
    | value -> Ok (value :: parts))
  |> Result.map ~f:(fun parts -> List.rev parts |> String.concat ~sep:"/")
;;

let unconfined_relative path =
  match normalize path with
  | Ok value -> value
  | Error _ -> path
;;

let make_source ~file_name ~relative_path ~directory ~path =
  { file_name; relative_path; directory; path }
;;

let root t ~file =
  if t.confined && (Filename.is_absolute file || Result.is_error (normalize file))
  then Error "source reference escapes its root"
  else (
    let relative_path =
      if Filename.is_absolute file
      then Filename.basename file
      else unconfined_relative file
    in
    Ok
      (make_source
         ~file_name:file
         ~relative_path
         ~directory:
           (if String.equal (Filename.dirname relative_path) "."
            then t.root
            else Eio.Path.(t.root / Filename.dirname relative_path))
         ~path:Eio.Path.(t.root / file)))
;;

let relative_reference base reference =
  let parent = Filename.dirname base.relative_path in
  if String.equal parent "." then reference else Filename.concat parent reference
;;

let resolve t ~base ~reference =
  let candidate = relative_reference base reference in
  let normalized =
    if t.confined && Filename.is_absolute reference
    then Error "absolute source reference is not captured"
    else if t.confined
    then normalize candidate
    else Ok (unconfined_relative candidate)
  in
  Result.map normalized ~f:(fun relative_path ->
    make_source
      ~file_name:reference
      ~relative_path
      ~directory:Eio.Path.(base.directory / Filename.dirname reference)
      ~path:Eio.Path.(base.directory / reference))
;;

let read t source =
  try
    let contents =
      match t.captured with
      | Some sources ->
        (match Map.find sources source.relative_path with
         | Some contents -> contents
         | None -> failwith ("source is not captured: " ^ source.relative_path))
      | None -> Eio.Path.load source.path
    in
    Option.iter t.observer ~f:(fun observe -> observe source contents);
    Ok contents
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Exn.to_string exn)
;;

let agent_reference t ~base ~reference =
  if Filename.is_absolute reference
  then Ok reference
  else
    Result.map (resolve t ~base ~reference) ~f:(fun source ->
      Option.iter t.agent_observer ~f:(fun observe -> observe source);
      Option.value (Eio.Path.native source.path) ~default:reference)
;;

let file_name source = source.file_name
let relative_path source = source.relative_path
let materialized_dir source = source.directory
