open Core

type limits =
  { max_request_bytes : int
  ; max_response_bytes : int
  ; max_requests : int
  }

let default_limits =
  { max_request_bytes = 16 * 1024 * 1024
  ; max_response_bytes = 1024 * 1024
  ; max_requests = 128
  }
;;

type error =
  | Denied
  | Request_too_large
  | Response_too_large
  | Too_many_requests
  | Invalid_response_frame
  | Handler_failed
[@@deriving equal, sexp]

let error_to_string = function
  | Denied -> "request channel authority expired or was denied"
  | Request_too_large -> "request channel input exceeds its frame limit"
  | Response_too_large -> "request channel output exceeds its frame limit"
  | Too_many_requests -> "request channel count limit reached"
  | Invalid_response_frame -> "request channel handler returned an invalid frame"
  | Handler_failed -> "request channel handler failed"
;;

type t =
  { limits : limits
  ; check : unit -> bool
  ; handle : string -> string
  }

let create ~limits ~check ~handle =
  match
    limits.max_request_bytes > 0
    && limits.max_request_bytes < Int.max_value
    && limits.max_response_bytes > 0
    && limits.max_response_bytes < Int.max_value
    && limits.max_requests > 0
  with
  | true -> Ok { limits; check; handle }
  | false -> Error "request channel limits must be positive and leave room for framing"
;;

let request_fd = 3
let response_fd = 4

let serve t ~source ~sink ~check ~on_activity =
  let reader = Eio.Buf_read.of_flow source ~max_size:(t.limits.max_request_bytes + 1) in
  let rec loop count =
    match Eio.Buf_read.line reader with
    | exception End_of_file -> Ok ()
    | request ->
      let open Result.Let_syntax in
      let%bind () =
        match String.length request <= t.limits.max_request_bytes with
        | true -> Ok ()
        | false -> Error Request_too_large
      in
      let%bind () =
        match count < t.limits.max_requests with
        | true -> Ok ()
        | false -> Error Too_many_requests
      in
      let%bind () =
        match check () && t.check () with
        | true -> Ok ()
        | false -> Error Denied
      in
      on_activity ();
      let response = t.handle request in
      let%bind () =
        match check () && t.check () with
        | true -> Ok ()
        | false -> Error Denied
      in
      let%bind () =
        match String.length response <= t.limits.max_response_bytes with
        | true -> Ok ()
        | false -> Error Response_too_large
      in
      let%bind () =
        match String.contains response '\n' || String.contains response '\r' with
        | true -> Error Invalid_response_frame
        | false -> Ok ()
      in
      Eio.Flow.copy_string (response ^ "\n") sink;
      on_activity ();
      loop (count + 1)
  in
  try loop 0 with
  | Eio.Buf_read.Buffer_limit_exceeded -> Error Request_too_large
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> Error Handler_failed
;;

module Client = struct
  let exchange ~limits request =
    match
      String.length request <= limits.max_request_bytes
      && (not (String.contains request '\n' || String.contains request '\r'))
      && limits.max_response_bytes > 0
      && limits.max_response_bytes < Int.max_value
    with
    | false -> Error "invalid or oversized request frame"
    | true ->
      (try
         let request_pipe = Core_unix.File_descr.of_int request_fd in
         let response_pipe = Core_unix.File_descr.of_int response_fd in
         match
           (Core_unix.fstat request_pipe).st_kind, (Core_unix.fstat response_pipe).st_kind
         with
         | S_FIFO, S_FIFO ->
           let frame = Bytes.of_string (request ^ "\n") in
           let rec write position =
             match position = Bytes.length frame with
             | true -> ()
             | false ->
               let count =
                 Core_unix.single_write
                   ~restart:true
                   ~pos:position
                   ~len:(Bytes.length frame - position)
                   request_pipe
                   ~buf:frame
               in
               (match count with
                | 0 -> failwith "closed request pipe"
                | _ -> write (position + count))
           in
           write 0;
           let buffer = Buffer.create (Int.min limits.max_response_bytes 4096) in
           let chunk = Bytes.create 8192 in
           let rec read () =
             let count =
               Core_unix.read
                 ~restart:true
                 ~len:
                   (Int.min 8192 (limits.max_response_bytes - Buffer.length buffer + 1))
                 response_pipe
                 ~buf:chunk
             in
             match count with
             | 0 -> Error "request channel closed before its response"
             | _ ->
               let data = Stdlib.Bytes.sub_string chunk 0 count in
               (match String.index data '\n' with
                | Some index
                  when index + 1 = count
                       && Buffer.length buffer + index <= limits.max_response_bytes ->
                  Buffer.add_substring buffer data ~pos:0 ~len:index;
                  Ok (Buffer.contents buffer)
                | Some _ -> Error "unexpected request channel response framing"
                | None when Buffer.length buffer + count <= limits.max_response_bytes ->
                  Buffer.add_string buffer data;
                  read ()
                | None -> Error "request channel response exceeds its frame limit")
           in
           read ()
         | _ -> Error "no inherited request channel pipes"
       with
       | _ -> Error "inherited request channel is unavailable")
  ;;
end
