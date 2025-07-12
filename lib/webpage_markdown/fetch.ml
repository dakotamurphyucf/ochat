open Core

let max_size_bytes = 5_000_000 (* 5 MB safety cap – large odoc pages *)

let decompress_if_needed (body : string) : string =
  match Ezgzip.decompress body with
  | Ok s -> s
  | Error _ -> body
;;

let get ~net url : (string, string) Result.t =
  let open Io.Net in
  try
    let host = get_host url in
    let path = get_path url in
    (* Use the Raw variant to inspect headers *)
    let res =
      get
        (Raw
           (fun (resp, body) ->
             let headers = Http.Response.headers resp in
             let ct = Http.Header.get headers "content-type" in
             let is_html =
               match ct with
               | None -> true (* many sites omit; assume HTML *)
               | Some s ->
                 String.is_prefix s ~prefix:"text/html"
                 || String.is_prefix s ~prefix:"text/plain"
                 || String.is_prefix s ~prefix:"application/json"
             in
             let is_json =
               match ct with
               | None -> false (* assume not JSON if no content-type *)
               | Some s -> String.is_prefix s ~prefix:"application/json"
             in
             if not is_html
             then
               Error
                 (sprintf "unsupported content-type: %s" (Option.value ~default:"" ct))
             else (
               (* read body string *)
               let html =
                 Eio.Buf_read.(parse_exn take_all) body ~max_size:max_size_bytes
               in
               (* decompress if gzipped (for safety; server might forget header) *)
               let html = decompress_if_needed html in
               if String.length html > max_size_bytes
               then Error (sprintf "document exceeds %d bytes" max_size_bytes)
               else if is_json
               then Error html
               else Ok html)))
        ~net
        ~host
        path
    in
    res
  with
  | exn -> Error (Exn.to_string exn)
;;
