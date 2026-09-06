open Core
module Res = Openai.Responses
module Retry = Chat_response.Response_loop.For_testing

let parsing_error () = Res.Response_parsing_error (`Object [], Failure "invalid response")

let stream_parsing_error () =
  Res.Response_stream_parsing_error (`Object [], Failure "invalid stream response")
;;

let%expect_test "response parsing retries use progressively longer delays" =
  let calls = ref 0 in
  let delays = ref [] in
  let result =
    Retry.retry_request
      ~sleep:(fun delay -> delays := delay :: !delays)
      ~f:(fun () ->
        Int.incr calls;
        if !calls <= 3 then raise (parsing_error ()) else "ok")
  in
  printf
    "result=%s calls=%d delays=%s\n"
    result
    !calls
    (Sexp.to_string_hum ([%sexp_of: float list] (List.rev !delays)));
  [%expect {| result=ok calls=4 delays=(1 2 3) |}]
;;

let%expect_test "stream response parsing errors are retried" =
  let calls = ref 0 in
  let delays = ref [] in
  let result =
    Retry.retry_request
      ~sleep:(fun delay -> delays := delay :: !delays)
      ~f:(fun () ->
        Int.incr calls;
        if !calls = 1 then raise (stream_parsing_error ()) else "ok")
  in
  printf
    "result=%s calls=%d delays=%s\n"
    result
    !calls
    (Sexp.to_string_hum ([%sexp_of: float list] (List.rev !delays)));
  [%expect {| result=ok calls=2 delays=(1) |}]
;;

let%expect_test "response parsing retries stop after five retries" =
  let calls = ref 0 in
  let delays = ref [] in
  let error =
    try
      ignore
        (Retry.retry_request
           ~sleep:(fun delay -> delays := delay :: !delays)
           ~f:(fun () ->
             Int.incr calls;
             raise (parsing_error ()))
         : Nothing.t);
      "no error"
    with
    | Failure message -> message
  in
  printf
    "calls=%d delays=%s\nerror=%s\n"
    !calls
    (Sexp.to_string_hum ([%sexp_of: float list] (List.rev !delays)))
    error;
  [%expect
    {|
    calls=6 delays=(1 2 3 4 5)
    error=OpenAI response parsing failed after 5 retries: (Failure "invalid response")
    |}]
;;

let%expect_test "unrelated errors are not retried" =
  let calls = ref 0 in
  let delays = ref 0 in
  let error =
    try
      ignore
        (Retry.retry_request
           ~sleep:(fun _ -> Int.incr delays)
           ~f:(fun () ->
             Int.incr calls;
             failwith "permanent")
         : Nothing.t);
      "no error"
    with
    | Failure message -> message
  in
  printf "calls=%d delays=%d error=%s\n" !calls !delays error;
  [%expect {| calls=1 delays=0 error=permanent |}]
;;
