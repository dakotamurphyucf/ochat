open Core
module Host = Chatml_git_host

module Pack = struct
  type t =
    | Stock
    | Empty
    | Malformed
end

module Shallow = struct
  type t =
    | Stock
    | Excessive
end

type t =
  { pin : Host.Pin.t
  ; pack : Pack.t
  ; shallow : Shallow.t
  ; mutable supplied_pack : bool
  ; mutable supplied_shallow : bool
  }

let create ~pin ~pack ~shallow =
  { pin; pack; shallow; supplied_pack = false; supplied_shallow = false }
;;

let supplied_pack t = t.supplied_pack
let supplied_shallow t = t.supplied_shallow
let packet bytes = sprintf "%04x%s" (String.length bytes + 4) bytes

let empty_pack =
  let header = "PACK\000\000\000\002\000\000\000\000" in
  header ^ Digestif.SHA1.(to_raw_string (digest_string header))
;;

let pack_prefix body =
  let rec loop offset =
    if offset + 4 > String.length body
    then failwith "stock pack packet missing"
    else (
      let size = Int.of_string ("0x" ^ String.sub body ~pos:offset ~len:4) in
      if size = 0
      then loop (offset + 4)
      else if size < 5 || size > String.length body - offset
      then failwith "invalid stock packet"
      else if
        Char.equal body.[offset + 4] '\001'
        && String.is_prefix
             (String.sub body ~pos:(offset + 5) ~len:(size - 5))
             ~prefix:"PACK"
      then String.prefix body offset
      else loop (offset + size))
  in
  loop 0
;;

let response t ~stock request =
  let module R = Https_fixture.Request in
  let normal : Https_fixture.Response.t = stock request in
  if String.equal (R.method_ request) "POST"
  then
    if String.is_substring (R.body request) ~substring:"done\n"
    then (
      t.supplied_pack <- true;
      match t.pack with
      | Stock -> normal
      | Empty | Malformed ->
        let payload =
          match t.pack with
          | Empty -> empty_pack
          | Malformed -> String.make 32 'x'
          | Stock -> assert false
        in
        { normal with
          body = pack_prefix normal.body ^ packet ("\001" ^ payload) ^ "0000"
        })
    else (
      match t.shallow with
      | Stock -> normal
      | Excessive ->
        t.supplied_shallow <- true;
        let declarations =
          List.init 64 ~f:(fun i -> packet (sprintf "shallow %040x\n" (i + 1)))
          |> String.concat
        in
        { normal with
          body =
            declarations ^ packet ("shallow " ^ Host.Pin.commit t.pin ^ "\n") ^ "0000"
        })
  else normal
;;
