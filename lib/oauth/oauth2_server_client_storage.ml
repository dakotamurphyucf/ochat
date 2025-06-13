open! Core

module Client = Oauth2_server_types.Client

type entry = Client.t

let table : (string, entry) Hashtbl.t = String.Table.create ()


let b64url_no_pad ?(len = 32) () =
  Mirage_crypto_rng.generate len
  |> Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet

let now_int () = Int.of_float (Core_unix.time ())

let gen_client_id () = b64url_no_pad ()

let gen_client_secret () = b64url_no_pad ()

(* ------------------------------------------------------------------ *)
(* Public API                                                           *)
(* ------------------------------------------------------------------ *)

(** [insert_fixed ~client_id ~client_secret ?client_name ?redirect_uris ()]
    registers a confidential OAuth client using the provided identifier and
    secret.  This is primarily useful in development scenarios or unit tests
    where deterministic credentials are required (e.g. [dev-client] /
    [dev-secret]).  If a client with the same [client_id] already exists the
    stored entry is replaced. *)
let insert_fixed
    ~(client_id : string)
    ~(client_secret : string)
    ?client_name
    ?redirect_uris
    ()
  : Client.t
  =
  let issued_at = now_int () in
  let entry : Client.t =
    { client_id
    ; client_secret = Some client_secret
    ; client_name
    ; redirect_uris
    ; client_id_issued_at = Some issued_at
    ; client_secret_expires_at = None
    }
  in
  Hashtbl.set table ~key:client_id ~data:entry;
  entry


let register ?client_name ?redirect_uris ?(confidential = false) () : Client.t =
  let client_id = gen_client_id () in
  let client_secret = if confidential then Some (gen_client_secret ()) else None in
  let issued_at = now_int () in
  let client_secret_expires_at = None in
  let entry : Client.t =
    { client_id
    ; client_secret
    ; client_name
    ; redirect_uris
    ; client_id_issued_at = Some issued_at
    ; client_secret_expires_at
    }
  in
  Hashtbl.set table ~key:client_id ~data:entry;
  entry

let find (client_id : string) : Client.t option = Hashtbl.find table client_id

let validate_secret ~(client_id : string) ~(client_secret : string option) : bool =
  match Hashtbl.find table client_id with
  | None -> false
  | Some { client_secret = stored; _ } -> (
      match stored, client_secret with
      | None, None -> true
      | Some s, Some given -> String.equal s given
      | _ -> false)

let pp_table fmt () =
  let open Format in
  fprintf fmt "@[<v>%a@]"
    (pp_print_list
       (fun fmt (id, entry) ->
         let secret =
           (match entry with
            | { Client.client_secret; _ } -> Option.value ~default:"∅" client_secret)
         in
         fprintf fmt "%s -> secret=%s" id secret))
    (Hashtbl.to_alist table)

(* ------------------------------------------------------------------ *)
(* Pre-populate development credentials                                 *)
(* ------------------------------------------------------------------ *)

let () =
  let dev_id = Sys.getenv "MCP_DEV_CLIENT_ID" |> Option.value ~default:"dev-client" in
  let dev_secret =
    Sys.getenv "MCP_DEV_CLIENT_SECRET" |> Option.value ~default:"dev-secret" in
  ignore (insert_fixed ~client_id:dev_id ~client_secret:dev_secret ())
