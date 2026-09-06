open Core

type t =
  { fs : Eio.Fs.dir_ty Eio.Path.t
  ; root : string
  ; mutable secret_variants : string list
  }

let secret_variants secrets =
  List.concat_map secrets ~f:(fun secret -> [ secret; Base64.encode_exn secret ])
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.dedup_and_sort ~compare:(fun a b ->
    match Int.compare (String.length b) (String.length a) with
    | 0 -> String.compare a b
    | order -> order)
;;

let create ~fs ~root ~secrets = { fs; root; secret_variants = secret_variants secrets }
let path t native_path = Eio.Path.(t.fs / native_path)

let register_secret t secret =
  t.secret_variants <- secret_variants (secret :: t.secret_variants)
;;

let redact t contents =
  List.fold t.secret_variants ~init:contents ~f:(fun redacted secret ->
    String.substr_replace_all redacted ~pattern:secret ~with_:"<redacted>")
;;

let is_safe_name name =
  (not (String.is_empty name))
  && String.equal name (Filename.basename name)
  && not (List.mem [ "."; ".." ] name ~equal:String.equal)
;;

let write_text t ~name ~contents =
  if not (is_safe_name name)
  then Or_error.error_s [%sexp "artifact name must be a safe basename", (name : string)]
  else
    Or_error.try_with (fun () ->
      let native_path = Filename.concat t.root name in
      Eio.Path.save ~create:(`Exclusive 0o600) (path t native_path) (redact t contents))
;;

let first_secret t contents =
  List.find t.secret_variants ~f:(fun secret ->
    String.is_substring contents ~substring:secret)
;;

let kind_to_string = function
  | `Not_found -> "not_found"
  | `Unknown -> "unknown"
  | `Fifo -> "fifo"
  | `Character_special -> "character_special"
  | `Directory -> "directory"
  | `Block_device -> "block_device"
  | `Regular_file -> "regular_file"
  | `Symbolic_link -> "symbolic_link"
  | `Socket -> "socket"
;;

let validate_file t name =
  let native_path = Filename.concat t.root name in
  let eio_path = path t native_path in
  match Eio.Path.kind ~follow:false eio_path with
  | `Regular_file ->
    let contents = Eio.Path.load eio_path in
    (match first_secret t contents with
     | None -> Ok ()
     | Some secret ->
       Or_error.error_s
         [%sexp
           "artifact contains a configured secret variant"
         , { name : string; secret_length = (String.length secret : int) }])
  | `Not_found -> Or_error.error_s [%sexp "artifact disappeared", (name : string)]
  | kind ->
    Or_error.error_s
      [%sexp
        "artifact is not a regular file"
      , { name : string; kind = (kind_to_string kind : string) }]
;;

let validate_redaction t =
  Or_error.try_with (fun () -> Eio.Path.read_dir (path t t.root))
  |> Or_error.bind ~f:(fun names ->
    List.map names ~f:(validate_file t) |> Or_error.combine_errors_unit)
;;

let copy_file t ~destination name =
  let source = path t (Filename.concat t.root name) in
  let target = path t (Filename.concat destination name) in
  match Eio.Path.kind ~follow:false source with
  | `Regular_file ->
    Eio.Path.save ~create:(`Exclusive 0o600) target (Eio.Path.load source)
  | `Not_found
  | `Unknown
  | `Fifo
  | `Character_special
  | `Directory
  | `Block_device
  | `Symbolic_link
  | `Socket -> ()
;;

let preserve t ~destination =
  let open Or_error.Let_syntax in
  let%bind () = validate_redaction t in
  Or_error.try_with (fun () ->
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 (path t destination);
    Eio.Path.read_dir (path t t.root) |> List.iter ~f:(copy_file t ~destination))
;;
