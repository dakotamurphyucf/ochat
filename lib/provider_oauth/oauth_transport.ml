module Native_unix = Unix
open! Core

module Error = struct
  type t =
    | Closed
    | Connection
    | Tls
    | Invalid_http
    | Body_limit
    | Timeout
  [@@deriving equal, sexp_of]
end

exception Transport_error of Error.t

let fail error = raise (Transport_error error)

type endpoint =
  | User_code
  | Device_poll
  | Token

let path = function
  | User_code -> "/api/accounts/deviceauth/usercode"
  | Device_poll -> "/api/accounts/deviceauth/token"
  | Token -> "/oauth/token"
;;

type t =
  { connect : sw:Eio.Switch.t -> Eio.Flow.two_way_ty Eio.Resource.t
  ; timeout : Eio.Time.Timeout.t
  ; scripted :
      (endpoint
       -> body:string
       -> on_possible_submission:(unit -> unit)
       -> (int * string, Error.t) result)
        option
  ; mutex : Eio.Mutex.t
  ; mutable closed : bool
  }

let create ~net ~clock =
  match Ca_certs.authenticator () with
  | Error (`Msg _) -> Error Error.Tls
  | Ok authenticator ->
    (match Tls.Config.client ~authenticator () with
     | Error (`Msg _) -> Error Error.Tls
     | Ok config ->
       let connect ~sw =
         let address =
           match Eio.Net.getaddrinfo_stream ~service:"443" net "auth.openai.com" with
           | first :: _ -> first
           | [] -> fail Connection
         in
         let flow = Eio.Net.connect ~sw net address in
         let host = Domain_name.host_exn (Domain_name.of_string_exn "auth.openai.com") in
         (Tls_eio.client_of_flow ~host config flow :> Eio.Flow.two_way_ty Eio.Resource.t)
       in
       Ok
         { connect
         ; timeout = Eio.Time.Timeout.seconds clock 30.
         ; scripted = None
         ; mutex = Eio.Mutex.create ()
         ; closed = false
         })
;;

let scripted ~clock exchange =
  { connect = (fun ~sw:_ -> fail Connection)
  ; timeout = Eio.Time.Timeout.seconds clock 30.
  ; scripted = Some exchange
  ; mutex = Eio.Mutex.create ()
  ; closed = false
  }
;;

let close t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.closed <- true))
;;

let maximum_header = 16384
let maximum_body = 262144

let line reader =
  (* Require CRLF and a bounded cumulative header/framing budget. *)
  let output = Buffer.create 128 in
  let rec loop () =
    if Buffer.length output >= maximum_header then fail Invalid_http;
    match Eio.Buf_read.any_char reader with
    | '\r' ->
      Eio.Buf_read.char '\n' reader;
      Buffer.contents output
    | '\n' -> fail Invalid_http
    | c ->
      Buffer.add_char output c;
      loop ()
  in
  loop ()
;;

let headers reader =
  let rec loop bytes count fields =
    let text = line reader in
    let bytes = bytes + String.length text + 2 in
    if bytes > maximum_header || count > 64 then fail Invalid_http;
    if String.is_empty text
    then fields
    else (
      match String.lsplit2 text ~on:':' with
      | None -> fail Invalid_http
      | Some (name, value) ->
        if
          String.is_empty name
          || not
               (String.for_all name ~f:(fun c ->
                  Char.is_alphanum c || String.mem "!#$%&'*+-.^_`|~" c))
        then fail Invalid_http;
        let name = String.lowercase name in
        if
          Map.mem fields name
          && List.mem
               [ "content-length"
               ; "transfer-encoding"
               ; "content-type"
               ; "content-encoding"
               ; "host"
               ; "origin"
               ]
               name
               ~equal:String.equal
        then fail Invalid_http;
        let value = String.strip value in
        if
          not
            (String.for_all value ~f:(fun c ->
               Char.to_int c >= 0x20 && Char.to_int c < 0x7f))
        then fail Invalid_http;
        loop bytes (count + 1) (Map.set fields ~key:name ~data:value))
  in
  loop 0 0 String.Map.empty
;;

let natural text =
  if String.is_empty text || not (String.for_all text ~f:Char.is_digit)
  then fail Invalid_http;
  match Int.of_string text with
  | value when value >= 0 && value <= maximum_body -> value
  | _ -> fail Body_limit
  | exception (Failure _ | Invalid_argument _) -> fail Invalid_http
;;

let body reader fields =
  match Map.find fields "content-length", Map.find fields "transfer-encoding" with
  | Some length, None -> Eio.Buf_read.take (natural length) reader
  | None, Some encoding when String.Caseless.equal encoding "chunked" ->
    let output = Buffer.create 1024 in
    let rec loop framing =
      let text = line reader in
      let framing = framing + String.length text + 2 in
      if framing > maximum_header then fail Invalid_http;
      (* Extensions not selected; rejecting is safer than an unbounded parser. *)
      if String.is_empty text || not (String.for_all text ~f:Char.is_hex_digit)
      then fail Invalid_http;
      let count =
        match Int.of_string ("0x" ^ text) with
        | n when n >= 0 -> n
        | _ -> fail Invalid_http
        | exception (Failure _ | Invalid_argument _) -> fail Invalid_http
      in
      if count > maximum_body - Buffer.length output then fail Body_limit;
      if count = 0
      then (
        let trailers = headers reader in
        if not (Map.is_empty trailers) then fail Invalid_http;
        Buffer.contents output)
      else (
        Buffer.add_string output (Eio.Buf_read.take count reader);
        Eio.Buf_read.string "\r\n" reader;
        loop (framing + 2))
    in
    loop 0
  | None, None ->
    let output = Buffer.create 1024 in
    let rec loop () =
      match Eio.Buf_read.peek_char reader with
      | None -> Buffer.contents output
      | Some _ ->
        if Buffer.length output = maximum_body then fail Body_limit;
        Buffer.add_char output (Eio.Buf_read.any_char reader);
        loop ()
    in
    loop ()
  | _ -> fail Invalid_http
;;

let read_response reader =
  let status_line = line reader in
  let status =
    match String.split status_line ~on:' ' with
    | ("HTTP/1.1" | "HTTP/1.0") :: code :: _ ->
      (match Int.of_string code with
       | n when n >= 200 && n <= 599 -> n
       | _ -> fail Invalid_http
       | exception (Failure _ | Invalid_argument _) -> fail Invalid_http)
    | _ -> fail Invalid_http
  in
  let fields = headers reader in
  (* Never accept redirects, including token-bearing redirection. *)
  if status >= 300 && status < 400 then fail Invalid_http;
  (match Map.find fields "content-encoding" with
   | None | Some "identity" -> ()
   | Some _ -> fail Invalid_http);
  (match status >= 200 && status < 300, Map.find fields "content-type" with
   | true, Some value
     when String.Caseless.equal
            (String.lsplit2 value ~on:';'
             |> Option.value_map ~default:value ~f:fst
             |> String.strip)
            "application/json" -> ()
   | false, _ -> ()
   | _ -> fail Invalid_http);
  status, body reader fields
;;

let parse_response wire =
  try
    let status, body = read_response (Eio.Buf_read.of_string wire) in
    Ok (status, String.length body)
  with
  | Transport_error error -> Error error
  | End_of_file | Eio.Buf_read.Buffer_limit_exceeded | Failure _ ->
    Error Error.Invalid_http
;;

let post t endpoint ~content_type ~body:request_body ~on_possible_submission =
  Eio.Mutex.use_ro t.mutex (fun () ->
    if t.closed
    then Error Error.Closed
    else if String.length request_body > maximum_body
    then Error Error.Body_limit
    else if
      not
        (String.equal content_type "application/json"
         || String.equal content_type "application/x-www-form-urlencoded")
    then Error Error.Invalid_http
    else (
      match t.scripted with
      | Some exchange ->
        Eio.Time.Timeout.run_exn t.timeout (fun () ->
          exchange endpoint ~body:request_body ~on_possible_submission)
      | None ->
        (try
           Eio.Time.Timeout.run_exn t.timeout (fun () ->
             Eio.Switch.run (fun sw ->
               let flow = t.connect ~sw in
               if t.closed then fail Closed;
               let request =
                 sprintf
                   "POST %s HTTP/1.1\r\n\
                    Host: auth.openai.com\r\n\
                    User-Agent: ochat\r\n\
                    originator: ochat\r\n\
                    Connection: close\r\n\
                    Content-Type: %s\r\n\
                    Accept: application/json\r\n\
                    Content-Length: %d\r\n\
                    \r\n"
                   (path endpoint)
                   content_type
                   (String.length request_body)
               in
               on_possible_submission ();
               Eio.Flow.copy_string request flow;
               Eio.Flow.copy_string request_body flow;
               let reader =
                 Eio.Buf_read.of_flow flow ~max_size:(maximum_body + maximum_header)
               in
               Ok (read_response reader)))
         with
         | Transport_error error -> Error error
         | Eio.Time.Timeout -> Error Timeout
         | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ -> Error Tls
         | Eio.Io _ | Native_unix.Unix_error _ -> Error Connection
         | End_of_file | Eio.Buf_read.Buffer_limit_exceeded | Failure _ ->
           Error Invalid_http)))
;;
