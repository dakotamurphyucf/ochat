open! Core
module P = Agent_protocol

module Policy = struct
  type origin =
    { scheme : string
    ; host : string
    ; port : int
    }
  [@@deriving equal]

  type t =
    { origins : origin list
    ; max_response_bytes : int
    ; max_wall_seconds : float
    }

  let fail message = Or_error.error_string message

  let origin uri =
    match Uri.scheme uri, Uri.host uri with
    | Some scheme, Some host ->
      let scheme = String.lowercase scheme in
      let host = String.lowercase host in
      let host = if String.equal host "[::1]" then "::1" else host in
      if
        Option.is_some (Uri.userinfo uri)
        || Option.is_some (Uri.fragment uri)
        || String.is_empty host
      then fail "remote origin has userinfo, fragment or empty host"
      else (
        match scheme with
        | "https" ->
          let valid_domain =
            match Domain_name.of_string host with
            | Error _ -> false
            | Ok domain -> Result.is_ok (Domain_name.host domain)
          in
          let port = Option.value (Uri.port uri) ~default:443 in
          if (not valid_domain) || port <= 0 || port > 65535
          then fail "remote HTTPS origin is invalid"
          else Ok { scheme; host; port }
        | "http" when String.equal host "127.0.0.1" || String.equal host "::1" ->
          let port = Option.value (Uri.port uri) ~default:80 in
          if port <= 0 || port > 65535
          then fail "remote origin port is invalid"
          else Ok { scheme; host; port }
        | "http" -> fail "plain HTTP requires a literal loopback host"
        | _ -> fail "remote content requires HTTP or HTTPS")
    | None, _ | _, None -> fail "remote origin requires a scheme and host"
  ;;

  let create ~allowed_origins ~max_response_bytes ~max_wall_seconds =
    let open Or_error.Let_syntax in
    if
      max_response_bytes <= 0
      || max_response_bytes > 16 * 1024 * 1024
      || (not (Float.is_finite max_wall_seconds))
      || Float.(max_wall_seconds <= 0.)
      || Float.(max_wall_seconds > 300.)
      || List.length allowed_origins > 64
    then fail "remote content limits are invalid"
    else (
      let%map origins =
        List.map allowed_origins ~f:(fun text ->
          if String.length text > 512
          then fail "remote origin is too long"
          else (
            let uri = Uri.of_string text in
            if
              (not (String.is_empty (Uri.path uri) || String.equal (Uri.path uri) "/"))
              || Option.is_some (Uri.verbatim_query uri)
            then fail "remote policy requires an origin, without path or query"
            else origin uri))
        |> Or_error.all
      in
      { origins; max_response_bytes; max_wall_seconds })
  ;;
end

type t =
  { policy : Policy.t
  ; connect : sw:Eio.Switch.t -> Policy.origin -> Eio.Flow.two_way_ty Eio.Resource.t
  ; timeout : Eio.Time.Timeout.t
  }

exception Rejected of P.Error.t

let error code message = P.Error.create code ~message ~retryable:false ()
let reject code message = raise (Rejected (error code message))

let create ~policy ~net ~clock =
  match Ca_certs.authenticator () with
  | Error (`Msg _) ->
    Error (error Configuration_invalid "system CA setup failed for remote content")
  | Ok authenticator ->
    (match Tls.Config.client ~authenticator () with
     | Error (`Msg _) ->
       Error (error Configuration_invalid "TLS setup failed for remote content")
     | Ok tls ->
       let connect ~sw (origin : Policy.origin) =
         let address =
           match
             Eio.Net.getaddrinfo_stream
               ~service:(Int.to_string origin.port)
               net
               origin.host
           with
           | first :: _ -> first
           | [] -> reject Interrupted "remote content address unavailable"
         in
         let flow = Eio.Net.connect ~sw net address in
         if String.equal origin.scheme "https"
         then (
           let host = Domain_name.host_exn (Domain_name.of_string_exn origin.host) in
           (Tls_eio.client_of_flow ~host tls flow :> Eio.Flow.two_way_ty Eio.Resource.t))
         else (flow :> Eio.Flow.two_way_ty Eio.Resource.t)
       in
       Ok
         { policy
         ; connect
         ; timeout = Eio.Time.Timeout.seconds clock policy.max_wall_seconds
         })
;;

let line reader ~check ~max_header =
  let buffer = Buffer.create 128 in
  let rec loop () =
    if Buffer.length buffer land 0xff = 0 then check ();
    if Buffer.length buffer >= max_header
    then reject Resource_limit "remote response header exceeds limit";
    let next =
      match Eio.Buf_read.peek_char reader with
      | None -> reject Invalid_request "remote response header is incomplete"
      | Some _ -> Eio.Buf_read.any_char reader
    in
    match next with
    | '\r' ->
      (match Eio.Buf_read.peek_char reader with
       | Some '\n' -> ignore (Eio.Buf_read.any_char reader : char)
       | None | Some _ -> reject Invalid_request "remote response line ending is invalid");
      check ();
      Buffer.contents buffer
    | '\n' -> reject Invalid_request "remote response has invalid line ending"
    | c ->
      Buffer.add_char buffer c;
      loop ()
  in
  loop ()
;;

let headers reader ~check =
  let max_header = 16_384 in
  let rec loop bytes count fields =
    let row = line reader ~check ~max_header in
    let bytes = bytes + String.length row + 2 in
    if bytes > max_header || count > 64
    then reject Resource_limit "remote response headers exceed limit";
    if String.is_empty row
    then fields
    else (
      match String.lsplit2 row ~on:':' with
      | None -> reject Invalid_request "remote response header is malformed"
      | Some (name, value) ->
        let name = String.lowercase name in
        if
          String.is_empty name
          || not
               (String.for_all name ~f:(fun c ->
                  Char.is_alphanum c || String.mem "!#$%&'*+-.^_`|~" c))
        then reject Invalid_request "remote response header name is invalid";
        let value = String.strip value in
        if
          not
            (String.for_all value ~f:(fun c ->
               let code = Char.to_int c in
               code = 0x09 || (code >= 0x20 && code <> 0x7f)))
        then reject Invalid_request "remote response header value is invalid";
        if
          Map.mem fields name
          && List.mem
               [ "content-length"; "transfer-encoding"; "content-encoding" ]
               name
               ~equal:String.equal
        then reject Invalid_request "remote response framing header is duplicated";
        let fields =
          if Map.mem fields name then fields else Map.set fields ~key:name ~data:value
        in
        loop bytes (count + 1) fields)
  in
  loop 0 0 String.Map.empty
;;

let natural ~limit text =
  if String.is_empty text || not (String.for_all text ~f:Char.is_digit)
  then reject Invalid_request "remote response length is invalid";
  match Int.of_string text with
  | n when n >= 0 && n <= limit -> n
  | _ -> reject Resource_limit "remote response body exceeds limit"
  | exception (Failure _ | Invalid_argument _) ->
    reject Invalid_request "remote response length is invalid"
;;

let append_exact reader output ~count ~check =
  let rec loop remaining =
    if remaining > 0
    then (
      check ();
      let chunk = Int.min remaining 4096 in
      Buffer.add_string output (Eio.Buf_read.take chunk reader);
      check ();
      loop (remaining - chunk))
  in
  loop count
;;

let body reader fields ~check ~limit =
  match Map.find fields "content-length", Map.find fields "transfer-encoding" with
  | Some length, None ->
    let count = natural ~limit length in
    let output = Buffer.create count in
    append_exact reader output ~count ~check;
    Buffer.contents output
  | None, Some encoding when String.Caseless.equal encoding "chunked" ->
    let output = Buffer.create (Int.min limit 4096) in
    let rec loop framing =
      let row = line reader ~check ~max_header:16_384 in
      let framing = framing + String.length row + 2 in
      if framing > 16_384 || String.is_empty row
      then reject Invalid_request "remote chunk framing is invalid";
      if not (String.for_all row ~f:Char.is_hex_digit)
      then reject Invalid_request "remote chunk extensions are not admitted";
      let count =
        match Int.of_string ("0x" ^ row) with
        | n when n >= 0 -> n
        | _ -> reject Invalid_request "remote chunk size is invalid"
        | exception (Failure _ | Invalid_argument _) ->
          reject Invalid_request "remote chunk size is invalid"
      in
      if count > limit - Buffer.length output
      then reject Resource_limit "remote response body exceeds limit";
      if count = 0
      then (
        if not (Map.is_empty (headers reader ~check))
        then reject Invalid_request "remote trailers are not admitted";
        Buffer.contents output)
      else (
        append_exact reader output ~count ~check;
        (match Eio.Buf_read.peek_char reader with
         | Some '\r' -> ignore (Eio.Buf_read.any_char reader : char)
         | None | Some _ -> reject Invalid_request "remote chunk ending is invalid");
        (match Eio.Buf_read.peek_char reader with
         | Some '\n' -> ignore (Eio.Buf_read.any_char reader : char)
         | None | Some _ -> reject Invalid_request "remote chunk ending is invalid");
        check ();
        loop (framing + 2))
    in
    loop 0
  | None, None ->
    let output = Buffer.create (Int.min limit 4096) in
    let rec loop since_check =
      if since_check = 0 then check ();
      match Eio.Buf_read.peek_char reader with
      | None -> Buffer.contents output
      | Some _ ->
        if Buffer.length output >= limit
        then reject Resource_limit "remote response body exceeds limit";
        Buffer.add_char output (Eio.Buf_read.any_char reader);
        loop ((since_check + 1) mod 4096)
    in
    loop 0
  | Some _, Some _ | None, Some _ ->
    reject Invalid_request "remote response framing is ambiguous"
;;

let read t ~url ~max_bytes ~check_owner ~checkpoint ~on_acquire =
  let open Result.Let_syntax in
  let%bind () = check_owner () in
  if max_bytes <= 0
  then Error (error Resource_limit "remote content allowance is exhausted")
  else if String.length url > 8192
  then Error (error Resource_limit "remote content URL exceeds limit")
  else (
    let uri = Uri.of_string url in
    match Policy.origin uri with
    | Error _ -> Error (error Invalid_request "remote content URL is invalid")
    | Ok origin ->
      if not (List.exists t.policy.origins ~f:(Policy.equal_origin origin))
      then Error (error Permission_denied "remote content origin is not admitted")
      else (
        let path = Uri.path uri in
        let path = if String.is_empty path then "/" else path in
        let target =
          path
          ^ Option.value_map (Uri.verbatim_query uri) ~default:"" ~f:(fun query ->
            "?" ^ query)
        in
        if
          not
            (String.for_all target ~f:(fun c ->
               let code = Char.to_int c in
               code >= 0x21 && code <= 0x7e && code <> 0x23))
        then Error (error Invalid_request "remote content request target is invalid")
        else (
          let limit = Int.min max_bytes t.policy.max_response_bytes in
          let check () =
            checkpoint ();
            match check_owner () with
            | Ok () -> ()
            | Error error -> raise (Rejected error)
          in
          try
            Eio.Time.Timeout.run_exn t.timeout (fun () ->
              Eio.Switch.run (fun sw ->
                check ();
                on_acquire ();
                let flow = t.connect ~sw origin in
                check ();
                let host_header =
                  let host =
                    if String.equal origin.host "::1" then "[::1]" else origin.host
                  in
                  if
                    (String.equal origin.scheme "https" && origin.port = 443)
                    || (String.equal origin.scheme "http" && origin.port = 80)
                  then host
                  else Printf.sprintf "%s:%d" host origin.port
                in
                let request =
                  Printf.sprintf
                    "GET %s HTTP/1.1\r\n\
                     Host: %s\r\n\
                     User-Agent: ochat\r\n\
                     Accept: */*\r\n\
                     Accept-Encoding: identity\r\n\
                     Connection: close\r\n\
                     \r\n"
                    target
                    host_header
                in
                Eio.Flow.copy_string request flow;
                check ();
                let reader = Eio.Buf_read.of_flow flow ~max_size:(limit + 16_384) in
                let status_line = line reader ~check ~max_header:16_384 in
                let status =
                  match String.split status_line ~on:' ' with
                  | ("HTTP/1.1" | "HTTP/1.0") :: code :: _ -> natural ~limit:599 code
                  | _ -> reject Invalid_request "remote response status is invalid"
                in
                let fields = headers reader ~check in
                if status < 200 || status >= 300
                then reject Invalid_state "remote response is not successful";
                (match Map.find fields "content-encoding" with
                 | None | Some "identity" -> ()
                 | Some _ ->
                   reject Invalid_request "compressed remote content is not admitted");
                let contents = body reader fields ~check ~limit in
                check ();
                contents))
            |> fun contents -> Ok contents
          with
          | Rejected error -> Error error
          | Eio.Time.Timeout ->
            Error (error Interrupted "remote content request timed out")
          | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ ->
            Error (error Interrupted "remote content TLS failed")
          | Eio.Io _ -> Error (error Interrupted "remote content I/O failed")
          | End_of_file | Eio.Buf_read.Buffer_limit_exceeded ->
            Error (error Invalid_request "remote response is incomplete"))))
;;
