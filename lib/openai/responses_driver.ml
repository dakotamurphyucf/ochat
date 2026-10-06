open! Core
module Codec = Responses_codec
module Request = Codec.Request
module Wire = Codec.Wire
module Field = Request.Field

let setting_names =
  [ "instructions"
  ; "max_output_tokens"
  ; "parallel_tool_calls"
  ; "temperature"
  ; "top_p"
  ; "reasoning"
  ; "text"
  ; "tool_choice"
  ; "prompt_cache_key"
  ; "prompt_cache_retention"
  ; "prompt_cache_options"
  ]
;;

let nonempty text = not (String.is_empty (String.strip text))

let member json key =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal key
  | `Array _ | `String _ | `Number _ | `True | `False | `Null -> None
;;

let string_member json key =
  Option.bind (member json key) ~f:(function
    | `String s -> Some s
    | _ -> None)
;;

let fail message = Or_error.error_string message

module Capability = struct
  type feature =
    | Text_input
    | Image_input
    | Document_input
    | Function_tools
    | Custom_tools
    | Opaque_replay
    | Setting of string
  [@@deriving equal, compare, sexp_of]

  type support =
    | Supported
    | Unsupported
    | Unknown
  [@@deriving equal, sexp_of]

  type t =
    { baseline : (feature * support) list
    ; models : (string * (feature * support) list) list
    }

  let validate entries =
    let rec loop seen = function
      | [] -> Ok ()
      | (feature, _) :: rest ->
        if List.mem seen feature ~equal:equal_feature
        then fail "duplicate capability declaration"
        else (
          match feature with
          | Setting name when not (List.mem setting_names name ~equal:String.equal) ->
            fail "unknown setting capability"
          | Text_input
          | Image_input
          | Document_input
          | Function_tools
          | Custom_tools
          | Opaque_replay
          | Setting _ -> loop (feature :: seen) rest)
    in
    loop [] entries
  ;;

  let create ~baseline ~models =
    let%bind.Or_error () = validate baseline in
    let%bind.Or_error () =
      Or_error.all_unit
        (List.map models ~f:(fun (model, entries) ->
           if nonempty model then validate entries else fail "empty model declaration"))
    in
    if List.contains_dup (List.map models ~f:fst) ~compare:String.compare
    then fail "duplicate model declaration"
    else Ok { baseline; models }
  ;;

  let resolve t ~model ~feature =
    let baseline =
      List.Assoc.find t.baseline ~equal:equal_feature feature
      |> Option.value ~default:Unknown
    in
    let declared =
      List.Assoc.find t.models ~equal:String.equal model
      |> Option.bind ~f:(fun entries ->
        List.Assoc.find entries ~equal:equal_feature feature)
    in
    match baseline, declared with
    | Unsupported, _ | _, Some Unsupported -> Unsupported
    | _, Some Supported -> Supported
    | _, Some Unknown -> Unknown
    | Supported, None -> Supported
    | Unknown, None -> Unknown
  ;;
end

module Setting = struct
  type provenance =
    | Execution_override
    | Captured_prompt
    | Profile_default
  [@@deriving equal, sexp_of]

  type t =
    { name : string
    ; value : Jsonaf.t Field.t
    ; provenance : provenance
    }

  let create ~name ~value ~provenance =
    if List.mem setting_names name ~equal:String.equal
    then Ok { name; value; provenance }
    else fail "unknown request setting"
  ;;

  let name t = t.name
  let value t = t.value
  let provenance t = t.provenance

  let rank = function
    | Execution_override -> 2
    | Captured_prompt -> 1
    | Profile_default -> 0
  ;;

  let merge settings =
    let rec loop seen selected = function
      | [] -> Ok (Map.data selected)
      | t :: rest ->
        let key = sprintf "%d:%s" (rank t.provenance) t.name in
        if Set.mem seen key
        then fail "duplicate setting in precedence layer"
        else (
          let selected =
            match t.value, Map.find selected t.name with
            | Field.Absent, _ -> selected
            | (Null | Value _), Some existing
              when rank existing.provenance > rank t.provenance -> selected
            | (Null | Value _), _ -> Map.set selected ~key:t.name ~data:t
          in
          loop (Set.add seen key) selected rest)
    in
    loop (Set.empty (module String)) (Map.empty (module String)) settings
  ;;
end

module Profile = struct
  type t =
    { id : string
    ; account : string option
    ; endpoint : string
    ; uri : Uri.t
    ; capabilities : Capability.t
    ; defaults : Setting.t list
    }

  let create ~id ~account ~endpoint ~capabilities ~defaults =
    let uri = Uri.of_string endpoint in
    let host = Uri.host uri in
    let secure =
      match Uri.scheme uri, host with
      | Some "https", Some host ->
        (match Domain_name.of_string host with
         | Error _ -> false
         | Ok domain -> Result.is_ok (Domain_name.host domain))
      | Some "http", Some ("127.0.0.1" | "::1" | "[::1]") -> true
      | _ -> false
    in
    if (not (nonempty id)) || Option.exists account ~f:(fun s -> not (nonempty s))
    then fail "empty profile identity"
    else if
      (not secure)
      || (not (String.equal (Uri.to_string uri) endpoint))
      || Option.exists (Uri.port uri) ~f:(fun p -> p <= 0 || p > 65535)
      || Option.is_some (Uri.userinfo uri)
      || Option.is_some (Uri.verbatim_query uri)
      || Option.is_some (Uri.fragment uri)
      || String.is_empty (Uri.path uri)
      || String.exists endpoint ~f:(fun c -> Char.to_int c <= 32 || Char.to_int c = 127)
    then fail "invalid Responses endpoint"
    else if
      List.exists defaults ~f:(fun s ->
        not (Setting.equal_provenance (Setting.provenance s) Profile_default))
    then fail "profile defaults require Profile_default provenance"
    else (
      let%map.Or_error defaults = Setting.merge defaults in
      { id; account; endpoint; uri; capabilities; defaults })
  ;;

  let id t = t.id
  let account t = t.account
  let endpoint t = t.endpoint
end

module Prepared = struct
  type t =
    { profile : Profile.t
    ; model : string
    ; request : Request.t
    ; settings : Setting.t list
    ; fingerprint : string
    }

  let require profile ~model feature =
    match Capability.resolve profile.Profile.capabilities ~model ~feature with
    | Supported -> Ok ()
    | Unsupported ->
      fail
        (sprintf
           "unsupported capability: %s"
           (Sexp.to_string (Capability.sexp_of_feature feature)))
    | Unknown ->
      fail
        (sprintf
           "unknown capability: %s"
           (Sexp.to_string (Capability.sexp_of_feature feature)))
  ;;

  let content profile ~model json =
    let base64 text = nonempty text && Result.is_ok (Base64.decode text) in
    let inline_data data =
      match String.lsplit2 data ~on:',' with
      | Some (metadata, bytes) ->
        String.is_prefix metadata ~prefix:"data:"
        && String.is_suffix metadata ~suffix:";base64"
        && base64 bytes
      | None -> false
    in
    match string_member json "type" with
    | Some "input_image" ->
      let%bind.Or_error () = require profile ~model Image_input in
      (match string_member json "image_url" with
       | Some data when inline_data data -> Ok ()
       | _ -> fail "image must be resolved to immutable inline data")
    | Some "input_file" ->
      let%bind.Or_error () = require profile ~model Document_input in
      (match string_member json "file_data", member json "file_url" with
       | Some data, (None | Some `Null) when inline_data data || base64 data -> Ok ()
       | _ -> fail "document must be resolved to immutable inline data")
    | Some _ | None -> Ok ()
  ;;

  let history_features profile ~model history =
    Or_error.all_unit
      (List.map history ~f:(fun item ->
         let%bind.Or_error () =
           match string_member item "type" with
           | Some "reasoning" -> require profile ~model Opaque_replay
           | Some ("function_call" | "function_call_output") ->
             require profile ~model Function_tools
           | Some ("custom_tool_call" | "custom_tool_call_output") ->
             require profile ~model Custom_tools
           | Some _ | None -> Ok ()
         in
         let supplied key =
           match member item key with
           | None | Some `Null -> false
           | Some _ -> true
         in
         let%bind.Or_error () =
           if
             supplied "namespace"
             || supplied "caller"
             || Option.exists (member item "async") ~f:(function
               | `True -> true
               | _ -> false)
           then fail "unselected caller, namespace or asynchronous history"
           else Ok ()
         in
         Or_error.all_unit
           (List.concat_map [ "content"; "output" ] ~f:(fun key ->
              match member item key with
              | Some (`Array parts) -> List.map parts ~f:(content profile ~model)
              | _ -> []))))
  ;;

  let tools_features profile ~model tools =
    let names =
      List.filter_map tools ~f:(fun tool ->
        string_member (Request.Tool.jsonaf_of_t tool) "name")
    in
    if List.contains_dup names ~compare:String.compare
    then fail "tool name collision"
    else
      Or_error.all_unit
        (List.map tools ~f:(fun tool ->
           let json = Request.Tool.jsonaf_of_t tool in
           let%bind.Or_error () =
             if
               Option.exists (member json "async") ~f:(function
                 | `True -> true
                 | _ -> false)
             then fail "asynchronous tools require a host scheduling mapping"
             else Ok ()
           in
           match string_member json "type" with
           | Some "function" -> require profile ~model Function_tools
           | Some "custom" -> require profile ~model Custom_tools
           | Some _ | None -> fail "unsupported tool"))
  ;;

  let create profile ~model ~history ~tools ~settings =
    let%bind.Or_error () = require profile ~model Text_input in
    let%bind.Or_error () =
      if
        List.exists settings ~f:(fun s ->
          Setting.equal_provenance (Setting.provenance s) Profile_default)
      then fail "execution settings cannot supply profile defaults"
      else Ok ()
    in
    let%bind.Or_error settings = Setting.merge (profile.Profile.defaults @ settings) in
    let%bind.Or_error () =
      Or_error.all_unit
        (List.map settings ~f:(fun s -> require profile ~model (Setting (Setting.name s))))
    in
    let%bind.Or_error () = history_features profile ~model history in
    let%bind.Or_error () = tools_features profile ~model tools in
    let%bind.Or_error base =
      Request.create
        ~model
        ~input:history
        ~stream:true
        ~include_encrypted_reasoning:
          (Capability.equal_support
             (Capability.resolve profile.capabilities ~model ~feature:Opaque_replay)
             Supported)
        ()
    in
    let fields =
      match Request.jsonaf_of_t base with
      | `Object fields -> fields
      | _ -> assert false
    in
    let additions =
      List.map settings ~f:(fun s ->
        ( Setting.name s
        , match Setting.value s with
          | Field.Null -> `Null
          | Value json -> json
          | Absent -> assert false ))
    in
    let%bind.Or_error request =
      Request.of_jsonaf
        (`Object
            (fields
             @ [ "tools", `Array (List.map tools ~f:Request.Tool.jsonaf_of_t) ]
             @ additions))
    in
    let identity =
      `Object
        [ "profile", `String profile.id
        ; ( "account"
          , match profile.account with
            | None -> `Null
            | Some s -> `String s )
        ; "endpoint", `String profile.endpoint
        ; "request", Request.jsonaf_of_t request
        ; ( "provenance"
          , `Array
              (List.map settings ~f:(fun s ->
                 `Array
                   [ `String (Setting.name s)
                   ; `String
                       (Sexp.to_string
                          (Setting.sexp_of_provenance (Setting.provenance s)))
                   ])) )
        ]
    in
    let fingerprint =
      Digestif.SHA256.(digest_string (Jsonaf.to_string identity) |> to_hex)
    in
    Ok { profile; model; request; settings; fingerprint }
  ;;

  let profile t = t.profile
  let model t = t.model
  let request t = t.request
  let settings t = t.settings
  let fingerprint t = t.fingerprint
end

module Auth = struct
  type lease = string

  type error =
    | Missing
    | Denied
    | Invalid_credential
    | Timed_out
  [@@deriving equal, sexp_of]

  let bearer secret =
    if
      String.is_empty secret
      || String.length secret > 8192
      || String.exists secret ~f:(fun c -> Char.to_int c <= 32 || Char.to_int c >= 127)
    then Error Invalid_credential
    else Ok secret
  ;;

  type resolver = sw:Eio.Switch.t -> Profile.t -> (lease, error) Result.t
end

module Terminal = struct
  type delivery =
    | Definitely_not_submitted
    | Possibly_submitted
    | Response_started
  [@@deriving equal, sexp_of]

  type failure =
    | Http_status of int
    | Invalid_http
    | Invalid_content_type
    | Body_limit
    | Protocol
    | Connection
    | Timeout
  [@@deriving equal, sexp_of]

  type t =
    | Provider of Wire.Tracker.completion
    | Failed of
        { delivery : delivery
        ; reason : failure
        }
end

module Event = struct
  type t =
    | Update of Codec.Stream.update
    | Finalized of (int * Wire.Item.t) list
    | Terminal of Terminal.t
end

exception Transport_failure of Terminal.failure
exception Consumer_failure of exn * Stdlib.Printexc.raw_backtrace

let transport_failure reason = raise (Transport_failure reason)

(* Every entity byte, including unknown SSE events/comments, counts towards the
   aggregate bound. Framing bytes have independent bounded lines/header counts. *)
module Entity = struct
  type framing =
    | Length of int
    | Chunked
    | Close

  type t =
    { reader : Eio.Buf_read.t
    ; mutable framing : framing
    ; mutable chunk_remaining : int
    ; mutable chunk_started : bool
    ; mutable done_ : bool
    ; mutable bytes : int
    ; max_bytes : int
    ; max_headers : int
    }

  let bounded_line t =
    try Eio.Buf_read.line t.reader with
    | Eio.Buf_read.Buffer_limit_exceeded -> transport_failure Invalid_http
  ;;

  let trailers t =
    let rec loop bytes =
      let line = bounded_line t in
      let bytes = bytes + String.length line + 2 in
      if bytes > t.max_headers then transport_failure Invalid_http;
      if not (String.is_empty line) then loop bytes
    in
    loop 0
  ;;

  let chunk t =
    if t.chunk_remaining = 0
    then (
      if t.chunk_started && not (String.is_empty (bounded_line t))
      then transport_failure Invalid_http;
      let line = bounded_line t in
      let size = String.take_while line ~f:(fun c -> not (Char.equal c ';')) in
      if
        String.is_empty size
        || String.length size > 15
        || not
             (String.for_all size ~f:(fun c ->
                Char.is_digit c
                || (Char.to_int (Char.lowercase c) >= Char.to_int 'a'
                    && Char.to_int (Char.lowercase c) <= Char.to_int 'f')))
      then transport_failure Invalid_http;
      let n =
        try Int.of_string ("0x" ^ size) with
        | _ -> transport_failure Invalid_http
      in
      if n > t.max_bytes - t.bytes then transport_failure Body_limit;
      t.chunk_remaining <- n;
      t.chunk_started <- true;
      if n = 0
      then (
        trailers t;
        t.done_ <- true))
  ;;

  let single_read t dst =
    if t.done_ then raise End_of_file;
    (match t.framing with
     | Chunked -> chunk t
     | Length _ | Close -> ());
    if t.done_ then raise End_of_file;
    let available =
      match t.framing with
      | Length n -> n
      | Chunked -> t.chunk_remaining
      | Close -> Cstruct.length dst
    in
    if available = 0
    then (
      t.done_ <- true;
      raise End_of_file);
    let n = Int.min available (Int.min (Cstruct.length dst) 4096) in
    let data =
      try
        Eio.Buf_read.ensure t.reader 1;
        Eio.Buf_read.take (Int.min n (Eio.Buf_read.buffered_bytes t.reader)) t.reader
      with
      | End_of_file ->
        (match t.framing with
         | Length _ | Chunked -> transport_failure Connection
         | Close ->
           t.done_ <- true;
           raise End_of_file)
    in
    let n = String.length data in
    if n > t.max_bytes - t.bytes then transport_failure Body_limit;
    t.bytes <- t.bytes + n;
    (match t.framing with
     | Length remaining -> t.framing <- Length (remaining - n)
     | Chunked -> t.chunk_remaining <- t.chunk_remaining - n
     | Close -> ());
    Cstruct.blit_from_string data 0 dst 0 n;
    n
  ;;

  let read_methods = []

  let flow t =
    Eio.Resource.T
      ( t
      , Eio.Flow.Pi.source
          (module struct
            type nonrec t = t

            let single_read = single_read
            let read_methods = read_methods
          end) )
  ;;
end

type t =
  { connect : sw:Eio.Switch.t -> Uri.t -> Eio.Flow.two_way_ty Eio.Resource.t
  ; with_timeout : 'a. (unit -> 'a) -> 'a
  ; max_request_bytes : int
  ; max_header_bytes : int
  ; max_body_bytes : int
  ; max_frame_bytes : int
  }

let create
      ~net
      ~clock
      ?(max_request_bytes = 16_777_216)
      ?(max_header_bytes = 32_768)
      ?(max_body_bytes = 67_108_864)
      ?(max_frame_bytes = 1_048_576)
      ?(timeout_seconds = 300.)
      ()
  =
  if
    List.exists
      [ max_request_bytes; max_header_bytes; max_body_bytes; max_frame_bytes ]
      ~f:(fun n -> n <= 0)
    || max_header_bytes < 128
    || (not (Float.is_finite timeout_seconds))
    || Float.(timeout_seconds <= 0.)
  then fail "invalid transport limits"
  else (
    let%bind.Or_error authenticator =
      match Ca_certs.authenticator () with
      | Ok a -> Ok a
      | Error (`Msg _) -> fail "system CA setup failed"
    in
    let%bind.Or_error config =
      match Tls.Config.client ~authenticator () with
      | Ok c -> Ok c
      | Error (`Msg _) -> fail "TLS configuration failed"
    in
    let connect ~sw uri =
      let host = Uri.host uri |> Option.value_exn in
      let service =
        Option.value_map
          (Uri.port uri)
          ~default:(Uri.scheme uri |> Option.value_exn)
          ~f:Int.to_string
      in
      let address =
        match Eio.Net.getaddrinfo_stream ~service net host with
        | first :: _ -> first
        | [] -> transport_failure Connection
      in
      let flow = Eio.Net.connect ~sw net address in
      match Uri.scheme uri with
      | Some "https" ->
        let host = Domain_name.host_exn (Domain_name.of_string_exn host) in
        (Tls_eio.client_of_flow ~host config flow :> Eio.Flow.two_way_ty Eio.Resource.t)
      | Some "http" -> (flow :> Eio.Flow.two_way_ty Eio.Resource.t)
      | _ -> transport_failure Invalid_http
    in
    Ok
      { connect
      ; with_timeout = (fun f -> Eio.Time.with_timeout_exn clock timeout_seconds f)
      ; max_request_bytes
      ; max_header_bytes
      ; max_body_bytes
      ; max_frame_bytes
      })
;;

let io f =
  try f () with
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | Transport_failure _ as ex -> raise ex
  | Eio.Time.Timeout -> transport_failure Timeout
  | Eio.Buf_read.Buffer_limit_exceeded -> transport_failure Body_limit
  | Eio.Io _ | End_of_file | Failure _ | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ ->
    transport_failure Connection
;;

let headers reader ~max_bytes =
  let bytes = ref 0 in
  let line () =
    let line =
      try Eio.Buf_read.line reader with
      | Eio.Buf_read.Buffer_limit_exceeded -> transport_failure Invalid_http
    in
    bytes := !bytes + String.length line + 2;
    if !bytes > max_bytes then transport_failure Invalid_http;
    line
  in
  let status_line = line () in
  let status =
    match String.split status_line ~on:' ' with
    | ("HTTP/1.1" | "HTTP/1.0") :: code :: _
      when String.length code = 3 && String.for_all code ~f:Char.is_digit ->
      (try Int.of_string code with
       | _ -> transport_failure Invalid_http)
    | _ -> transport_failure Invalid_http
  in
  if status < 200 || status > 599 then transport_failure Invalid_http;
  let rec loop acc =
    let value = line () in
    if String.is_empty value
    then status, acc
    else (
      match String.lsplit2 value ~on:':' with
      | Some (key, value)
        when nonempty key
             && String.for_all key ~f:(fun c ->
               Char.is_alphanum c || String.mem "!#$%&'*+-.^_`|~" c) ->
        loop ((String.lowercase key, String.strip value) :: acc)
      | _ -> transport_failure Invalid_http)
  in
  loop []
;;

let body_framing headers ~max_bytes =
  let values name =
    List.filter_map headers ~f:(fun (key, value) ->
      if String.equal key name then Some value else None)
  in
  match values "transfer-encoding", values "content-length" with
  | [ encoding ], [] when String.Caseless.equal encoding "chunked" -> Entity.Chunked
  | [], [ value ] ->
    if String.is_empty value || not (String.for_all value ~f:Char.is_digit)
    then transport_failure Invalid_http;
    let bytes =
      try Int.of_string value with
      | _ -> transport_failure Body_limit
    in
    if bytes > max_bytes then transport_failure Body_limit;
    Entity.Length bytes
  | [], [] -> Entity.Close
  | _ -> transport_failure Invalid_http
;;

let dispatch t ~sw ~lease ~prepared ~on_event ~published ~submitted =
  let profile = Prepared.profile prepared in
  let body = Jsonaf.to_string (Request.jsonaf_of_t (Prepared.request prepared)) in
  if String.length body > t.max_request_bytes then transport_failure Body_limit;
  let uri = profile.Profile.uri in
  let flow = io (fun () -> t.connect ~sw uri) in
  let host = Uri.host uri |> Option.value_exn in
  let host = if String.mem host ':' then "[" ^ host ^ "]" else host in
  let authority =
    host ^ Option.value_map (Uri.port uri) ~default:"" ~f:(fun p -> ":" ^ Int.to_string p)
  in
  let header =
    sprintf
      "POST %s HTTP/1.1\r\n\
       Host: %s\r\n\
       Authorization: Bearer %s\r\n\
       Content-Type: application/json\r\n\
       Accept: text/event-stream\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n"
      (Uri.path uri)
      authority
      lease
      (String.length body)
  in
  submitted := true;
  io (fun () ->
    Eio.Flow.copy_string header flow;
    Eio.Flow.copy_string body flow);
  let reader =
    Eio.Buf_read.of_flow
      flow
      ~initial_size:(Int.min 4096 t.max_header_bytes)
      ~max_size:t.max_header_bytes
  in
  let status, headers = io (fun () -> headers reader ~max_bytes:t.max_header_bytes) in
  if status <> 200 then transport_failure (Http_status status);
  if
    List.exists headers ~f:(fun (key, value) ->
      String.equal key "content-encoding" && not (String.Caseless.equal value "identity"))
  then transport_failure Invalid_http;
  let content_types =
    List.filter_map headers ~f:(fun (key, value) ->
      if String.equal key "content-type" then Some value else None)
  in
  (match content_types with
   | [ value ]
     when String.Caseless.equal
            (String.strip (String.take_while value ~f:(fun c -> not (Char.equal c ';'))))
            "text/event-stream" -> ()
   | _ -> transport_failure Invalid_content_type);
  let framing = body_framing headers ~max_bytes:t.max_body_bytes in
  let entity =
    Entity.
      { reader
      ; framing
      ; chunk_remaining = 0
      ; chunk_started = false
      ; done_ = false
      ; bytes = 0
      ; max_bytes = t.max_body_bytes
      ; max_headers = t.max_header_bytes
      }
  in
  let reader =
    Eio.Buf_read.of_flow
      (Entity.flow entity)
      ~initial_size:(Int.min 4096 t.max_frame_bytes)
      ~max_size:t.max_frame_bytes
  in
  let origin =
    match
      Wire.Origin.create
        ~provider:profile.id
        ~account:profile.account
        ~endpoint:profile.endpoint
    with
    | Ok origin -> origin
    | Error _ -> assert false
  in
  let parser =
    Codec.Stream.create ~max_frame_bytes:t.max_frame_bytes origin |> Or_error.ok_exn
  in
  let rec loop () =
    let line =
      io (fun () ->
        try Some (Eio.Buf_read.line reader) with
        | End_of_file -> None)
    in
    match line with
    | None ->
      (match Codec.Stream.finish parser with
       | Ok outcome -> Terminal.Provider outcome
       | Error _ -> transport_failure Protocol)
    | Some line ->
      (match Codec.Stream.feed_line parser line with
       | Error _ -> transport_failure Protocol
       | Ok None -> loop ()
       | Ok (Some update) ->
         if Wire.Tracker.equal_disposition update.disposition Duplicate
         then loop ()
         else (
           match Wire.Event.view update.event with
           | Terminal _ | Error _ ->
             if not (List.is_empty update.newly_finalized)
             then (
               published := true;
               on_event (Event.Finalized update.newly_finalized));
             (match Codec.Stream.finish parser with
              | Ok outcome -> Terminal.Provider outcome
              | Error _ -> transport_failure Protocol)
           | Response _
           | Item_added _
           | Item_done _
           | Part_added _
           | Part_done _
           | Delta _
           | Text_done _
           | Annotation_added _
           | Unknown _ ->
             published := true;
             on_event (Event.Update update);
             loop ()))
  in
  loop ()
;;

let run t ~auth ~prepared ~on_event =
  let authenticated = ref false in
  let submitted = ref false in
  let published = ref false in
  let publish event =
    try on_event event with
    | Eio.Cancel.Cancelled _ as ex -> raise ex
    | ex ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      raise (Consumer_failure (ex, backtrace))
  in
  let failed reason =
    Terminal.Failed
      { delivery =
          (if !published
           then Response_started
           else if !submitted
           then Possibly_submitted
           else Definitely_not_submitted)
      ; reason
      }
  in
  let result =
    try
      t.with_timeout (fun () ->
        Eio.Switch.run (fun sw ->
          match auth ~sw (Prepared.profile prepared) with
          | Error error -> Error error
          | Ok lease ->
            authenticated := true;
            Ok (dispatch t ~sw ~lease ~prepared ~on_event:publish ~published ~submitted)))
    with
    | Consumer_failure (ex, backtrace) ->
      Stdlib.Printexc.raise_with_backtrace ex backtrace
    | Eio.Time.Timeout ->
      if !authenticated then Ok (failed Timeout) else Error Auth.Timed_out
    | Transport_failure reason -> Ok (failed reason)
  in
  match result with
  | Error _ -> result
  | Ok outcome ->
    on_event (Event.Terminal outcome);
    Ok outcome
;;
