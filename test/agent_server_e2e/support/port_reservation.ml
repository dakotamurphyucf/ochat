open Core

type t =
  { listener : Eio_unix.Net.listening_socket_ty Eio.Resource.t
  ; port : int
  }

let create ~sw ~env =
  let listener =
    Eio.Net.listen
      ~sw
      ~reuse_addr:false
      ~reuse_port:false
      ~backlog:1
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix path ->
      raise_s [%sexp "loopback listener returned a Unix path", (path : string)]
  in
  { listener; port }
;;

let port t = t.port
let release t = Eio.Resource.close t.listener
