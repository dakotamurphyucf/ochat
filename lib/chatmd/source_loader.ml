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
  ; generated : bool
  ; generated_checked : bool
  ; retained : (Chatml_local_source.t * (unit -> unit)) option
  ; captured : string String.Map.t option
  ; read_limit : (source -> int) option
  ; observer : (source -> string -> unit) option
  ; agent_observer : (source -> unit) option
  }

let filesystem ~root =
  { root
  ; confined = false
  ; generated = false
  ; generated_checked = false
  ; retained = None
  ; captured = None
  ; read_limit = None
  ; observer = None
  ; agent_observer = None
  }
;;

let confined_filesystem ~root = { (filesystem ~root) with confined = true }

let retained_filesystem ~root ~owner ~checkpoint =
  { (confined_filesystem ~root) with retained = Some (owner, checkpoint) }
;;

let captured_filesystem ~root ~sources =
  { (confined_filesystem ~root) with captured = Some (String.Map.of_alist_exn sources) }
;;

let generated_filesystem ~root ~sources =
  { (captured_filesystem ~root ~sources) with generated = true }
;;

module Generated_error = struct
  type t =
    | Invalid_reference
    | Source_unavailable

  exception Raised of t

  let of_exn = function
    | Raised error -> Some error
    | _ -> None
  ;;
end

let generated_filesystem_checked ~root ~sources =
  { (generated_filesystem ~root ~sources) with generated_checked = true }
;;

let checked_result t ~problem = function
  | Error _ when t.generated_checked -> raise (Generated_error.Raised problem)
  | result -> result
;;

let portable_reference reference =
  (not (String.is_empty reference))
  && not
       (String.exists reference ~f:(function
          | '\\' | ':' | '\000' | '\r' | '\n' -> true
          | _ -> false))
;;

let with_read_limit t ~max_bytes =
  let bound =
    match t.read_limit with
    | None -> max_bytes
    | Some previous -> fun source -> Int.min (previous source) (max_bytes source)
  in
  { t with read_limit = Some bound }
;;

let with_observer t ~f = { t with observer = Some f }
let with_agent_observer t ~f = { t with agent_observer = Some f }
let root_dir t = t.root
let is_captured t = Option.is_some t.captured

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

let resolve_member base ~reference ~max_reference_bytes ~checkpoint =
  checkpoint ();
  if max_reference_bytes < 0 || max_reference_bytes > 1024
  then Or_error.error_string "invalid document reference byte ceiling"
  else if String.length reference > max_reference_bytes
  then Or_error.error_string "document reference byte limit"
  else if (not (portable_reference reference)) || Filename.is_absolute reference
  then Or_error.error_string "document reference must be a portable relative path"
  else (
    let parent = Filename.dirname (Source.Portable_path.to_string base) in
    let candidate =
      if String.equal parent "." then reference else Filename.concat parent reference
    in
    checkpoint ();
    match normalize candidate with
    | Error reason -> Or_error.error_string reason
    | Ok path ->
      checkpoint ();
      Source.Portable_path.create path)
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
  (if
     (t.generated && not (portable_reference file))
     || (t.confined && (Filename.is_absolute file || Result.is_error (normalize file)))
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
          ~path:Eio.Path.(t.root / file))))
  |> checked_result t ~problem:Generated_error.Invalid_reference
;;

let relative_reference base reference =
  let parent = Filename.dirname base.relative_path in
  if String.equal parent "." then reference else Filename.concat parent reference
;;

let resolve t ~base ~reference =
  let candidate = relative_reference base reference in
  let normalized =
    if t.generated && not (portable_reference reference)
    then Error "generated source reference must be a portable relative path"
    else if t.confined && Filename.is_absolute reference
    then Error "absolute source reference is not captured"
    else if t.confined
    then normalize candidate
    else Ok (unconfined_relative candidate)
  in
  Result.map normalized ~f:(fun relative_path ->
    make_source
      ~file_name:reference
      ~relative_path
      ~directory:
        (if t.generated
         then (
           let parent = Filename.dirname relative_path in
           if String.equal parent "." then t.root else Eio.Path.(t.root / parent))
         else Eio.Path.(base.directory / Filename.dirname reference))
      ~path:
        (if t.generated
         then Eio.Path.(t.root / relative_path)
         else Eio.Path.(base.directory / reference)))
  |> checked_result t ~problem:Generated_error.Invalid_reference
;;

let read_legacy_unbounded t source =
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

(* New extension dependencies use a hard read ceiling before allocation and
   notify capture observers only after a complete successful bounded read. *)
let read_legacy_bounded ~max_bytes t source =
  if max_bytes < 0 || max_bytes > 8 * 1024 * 1024
  then Error "invalid source read bound"
  else (
    let max_bytes =
      match t.read_limit with
      | None -> max_bytes
      | Some bound -> Int.min max_bytes (bound source)
    in
    if max_bytes < 0
    then Error "invalid source read bound"
    else (
      try
        let contents =
          match t.captured with
          | Some sources ->
            (match Map.find sources source.relative_path with
             | Some text when String.length text <= max_bytes -> text
             | Some _ -> failwith "source byte limit exceeded"
             | None -> failwith ("source is not captured: " ^ source.relative_path))
          | None ->
            Eio.Path.with_open_in source.path (fun flow ->
              let buffer = Buffer.create (Int.min 4096 max_bytes) in
              let chunk = Cstruct.create (Int.min 4096 (max_bytes + 1)) in
              let rec loop () =
                match Eio.Flow.single_read flow chunk with
                | count ->
                  if Buffer.length buffer + count > max_bytes
                  then failwith "source byte limit exceeded";
                  Buffer.add_string buffer (Cstruct.to_string ~len:count chunk);
                  loop ()
                | exception End_of_file -> Buffer.contents buffer
              in
              loop ())
        in
        Option.iter t.observer ~f:(fun observe -> observe source contents);
        Ok contents
      with
      | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
      | exn -> Error (Exn.to_string exn)))
;;

let read_bounded ~max_bytes t source =
  (match t.retained with
   | None -> read_legacy_bounded ~max_bytes t source
   | Some (owner, checkpoint) ->
     let max_bytes =
       Option.value_map t.read_limit ~default:max_bytes ~f:(fun bound ->
         Int.min max_bytes (bound source))
     in
     if max_bytes < 0 || max_bytes > 8 * 1024 * 1024
     then Error "invalid source read bound"
     else (
       checkpoint ();
       match Source.Portable_path.create source.relative_path with
       | Error _ -> Error "source member is not a portable relative path"
       | Ok member ->
         (match Chatml_local_source.read owner ~member ~max_bytes ~checkpoint with
          | Error error ->
            Error (Sexp.to_string_hum (Chatml_local_source.Error.sexp_of_t error))
          | Ok contents ->
            checkpoint ();
            Option.iter t.observer ~f:(fun observe -> observe source contents);
            Ok contents)))
  |> checked_result t ~problem:Generated_error.Source_unavailable
;;

let read t source =
  (match t.read_limit with
   | None ->
     (match t.retained with
      | None -> read_legacy_unbounded t source
      | Some _ -> read_bounded ~max_bytes:(8 * 1024 * 1024) t source)
   | Some _ -> read_bounded ~max_bytes:(8 * 1024 * 1024) t source)
  |> checked_result t ~problem:Generated_error.Source_unavailable
;;

let resolve_within_root t ~base ~reference =
  if
    (t.generated && not (portable_reference reference))
    || Filename.is_absolute reference
    || String.is_substring reference ~substring:"://"
  then Error "extension source must be a relative local reference"
  else (
    match normalize (relative_reference base reference) with
    | Error _ -> Error "extension source reference escapes its root"
    | Ok relative_path ->
      let directory = Filename.dirname relative_path in
      Ok
        (make_source
           ~file_name:reference
           ~relative_path
           ~directory:
             (if String.equal directory "." then t.root else Eio.Path.(t.root / directory))
           ~path:Eio.Path.(t.root / relative_path)))
;;

let agent_reference ?on_member t ~base ~reference =
  if t.generated && Filename.is_absolute reference
  then Error "generated agent source must belong to the supplied bundle"
  else if Filename.is_absolute reference
  then Ok reference
  else
    Result.bind (resolve t ~base ~reference) ~f:(fun source ->
      if
        t.generated
        && not
             (Option.value_map t.captured ~default:false ~f:(fun sources ->
                Map.mem sources source.relative_path))
      then Error "generated agent source is not in the supplied bundle"
      else (
        Option.iter on_member ~f:(fun observe -> observe source.relative_path);
        Option.iter t.agent_observer ~f:(fun observe -> observe source);
        Ok (Option.value (Eio.Path.native source.path) ~default:reference)))
;;

let file_name source = source.file_name
let relative_path source = source.relative_path
let materialized_dir source = source.directory
