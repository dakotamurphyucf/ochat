open! Core
module Token = Oauth2_server_types.Token

type entry = { expires_at : float }

let table : (string, entry) Hashtbl.t = String.Table.create ()
let now () = Core_unix.gettimeofday ()

let insert (tok : Token.t) : unit =
  let expires_at = now () +. Float.of_int tok.expires_in in
  Hashtbl.set table ~key:tok.access_token ~data:{ expires_at }
;;

let find_valid (access_token : string) : bool =
  match Hashtbl.find table access_token with
  | None -> false
  | Some { expires_at; _ } ->
    if Float.(now () < expires_at)
    then true
    else (
      (* Eager eviction *)
      Hashtbl.remove table access_token;
      false)
;;

let pp_table fmt () =
  let open Format in
  fprintf
    fmt
    "@[<v>%a@]"
    (Format.pp_print_list (fun fmt (k, { expires_at }) ->
       fprintf fmt "%s exp: %.0f" k expires_at))
    (Hashtbl.to_alist table)
;;
