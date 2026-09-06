open! Core

let crawl env =
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build/default/_doc/_html") in
  Odoc_crawler.crawl ~root (fun ~pkg ~doc_path ~markdown ->
    Printf.printf "[%s] %s - %d bytes\n%!" pkg doc_path (String.length markdown))
;;
