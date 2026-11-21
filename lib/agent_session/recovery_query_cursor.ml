open! Core
module P = Agent_protocol
module AEAD = Mirage_crypto.Chacha20
module Payload = Recovery_history_query.Continuation
module X = Persistence_codec

type t = AEAD.key

let nonce_bytes = 12
let tag_bytes = 16
let ttl = Mtime.Span.of_uint64_ns 300_000_000_000L
let create () = AEAD.of_secret (Mirage_crypto_rng.generate 32)
let aad audience = "ochat.recovery-history.cursor.v1\000" ^ audience

let expired () =
  P.Error.create
    Cursor_expired
    ~message:"recovery cursor expired; request the first page again"
    ~retryable:false
    ~data:(`Object [ "snapshot_required", `True ])
    ()
;;

let seal t ~now ~audience payload =
  let open Result.Let_syntax in
  let%bind expires = Result.of_option (Mtime.add_span now ttl) ~error:(expired ()) in
  let plaintext =
    `Object
      [ "expires", X.int64_json (Mtime.to_uint64_ns expires)
      ; "page", Payload.to_json payload
      ]
    |> Jsonaf.to_string
  in
  let nonce = Mirage_crypto_rng.generate nonce_bytes in
  let ciphertext =
    AEAD.authenticate_encrypt ~key:t ~nonce ~adata:(aad audience) plaintext
  in
  let encoded =
    Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet (nonce ^ ciphertext)
  in
  P.Page.Cursor.of_string encoded |> Result.map ~f:P.Page.Cursor.to_string
;;

let read t ~now ~audience cursor =
  let open Result.Let_syntax in
  let reject = expired () in
  let%bind raw =
    Base64.decode
      ~pad:false
      ~alphabet:Base64.uri_safe_alphabet
      (P.Page.Cursor.to_string cursor)
    |> Result.map_error ~f:(fun _ -> reject)
  in
  if String.length raw <= nonce_bytes + tag_bytes
  then Error reject
  else
    (let nonce = String.sub raw ~pos:0 ~len:nonce_bytes in
     let ciphertext = String.drop_prefix raw nonce_bytes in
     let%bind plaintext =
       AEAD.authenticate_decrypt ~key:t ~nonce ~adata:(aad audience) ciphertext
       |> Result.of_option ~error:reject
     in
     (* Only this module can produce an authenticated plaintext. Decode it
       strictly as the current purpose's bounded schema. *)
     let%bind fields =
       P.Recovery_wire.fields (Jsonaf.of_string plaintext) [ "expires"; "page" ]
     in
     let%bind expires = X.required fields "expires" X.int64 in
     if Mtime.compare now (Mtime.of_uint64_ns expires) >= 0
     then Error reject
     else X.required fields "page" Payload.of_json)
    |> Result.map_error ~f:(fun _ -> reject)
;;
