open Core

(* Internal helpers --------------------------------------------------- *)
let clean_html raw =
  let decompressed = Option.value ~default:raw (Result.ok (Ezgzip.decompress raw)) in
  let soup = Soup.parse decompressed in
  soup
  |> Soup.texts
  |> List.map ~f:String.strip
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:"\n"
;;

let tab_on_newline (input : string) : string =
  let buffer = Buffer.create (String.length input) in
  String.iter
    ~f:(fun c ->
      let open Char in
      Buffer.add_char buffer c;
      if c = '\n'
      then (
        Buffer.add_char buffer '\t';
        Buffer.add_char buffer '\t'))
    input;
  Buffer.contents buffer
;;

let get_remote ?(gzip = false) ~net url =
  let host = Io.Net.get_host url
  and path = Io.Net.get_path url in
  let headers =
    Http.Header.of_list
      (if gzip
       then [ "Accept", "*/*"; "Accept-Encoding", "gzip" ]
       else [ "Accept", "*/*" ])
  in
  Io.Net.get Io.Net.Default ~net ~host ~headers path
;;

(* Shared implementation --------------------------------------------- *)
let get_impl ~(ctx : _ Ctx.t) url ~is_local ~cleanup_html =
  if is_local
  then Io.load_doc ~dir:(Ctx.dir ctx) url
  else (
    let net = Ctx.net ctx in
    let raw = get_remote ~net url in
    if cleanup_html then clean_html raw else raw)
;;

(* Public helpers ----------------------------------------------------- *)
let get ~ctx url ~is_local = get_impl ~ctx url ~is_local ~cleanup_html:false
let get_html ~ctx url ~is_local = get_impl ~ctx url ~is_local ~cleanup_html:true
