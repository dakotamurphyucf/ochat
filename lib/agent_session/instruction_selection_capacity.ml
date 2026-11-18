open! Core
module D = Document_schema
module P = Agent_protocol

let limits =
  D.Limits.create
    ~max_bytes:(8 * 1024 * 1024)
    ~max_depth:80
    ~max_fields:100000
    ~max_nodes:200000
  |> function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

(* A fresh Clear receipt has seven scalar fields and its two-field wrapper.
   Principal and session IDs have <=96 ASCII bytes, key <=256, SHA64, canonical
   timestamp <=30, and generation/revision <=19 digits. The wrapper identity
   repeats only principal/key. The actual allowed ASCII encodings, delimiter
   escapes and fixed JSON syntax total <2048 bytes, 16 fields and 32 nodes.
   The 4096/64/128 reserve also covers <80 bytes of decimal growth in selection
   and host revision/event/transaction counters. Clear needs no new capture,
   unknown field or external mutable input. Existing active references shrink. *)
let bytes = 4096
let fields = 64
let nodes = 128

let remaining_limits ~active limits =
  if not active
  then Ok limits
  else
    D.Limits.create
      ~max_bytes:(D.Limits.max_bytes limits - bytes)
      ~max_depth:(D.Limits.max_depth limits)
      ~max_fields:(D.Limits.max_fields limits - fields)
      ~max_nodes:(D.Limits.max_nodes limits - nodes)
;;

let validate ~active raw ~limits =
  let open Result.Let_syntax in
  let%bind usable =
    remaining_limits ~active limits
    |> Result.map_error ~f:(fun _ ->
      P.Error.create
        Resource_limit
        ~message:"host lacks instruction Clear reserve"
        ~retryable:false
        ())
  in
  D.Json.validate_and_measure ~limits:usable raw
  |> Result.map ~f:(fun _ -> ())
  |> Result.map_error ~f:(fun error ->
    P.Error.create
      Resource_limit
      ~message:(Sexp.to_string_hum (D.Error.sexp_of_t error))
      ~retryable:false
      ())
;;
