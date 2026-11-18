open! Core
module D = Document_schema

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let subject ~name ~character =
  let repeated = String.make 65536 character in
  let text =
    {|{"format":"ochat.document","schema_version":1,"kind":"parallel.|}
    ^ name
    ^ {|","payload":{"name":"|}
    ^ name
    ^ {|","text":"|}
    ^ repeated
    ^ {|\"line\nslash\\tail","escaped":"\u00e9\t","number":1.2300e+02,"nested":[{"id":"|}
    ^ name
    ^ {|","unknown":null}]},"future":{"kept":true}}|}
  in
  let expected =
    `Object
      [ "format", `String "ochat.document"
      ; "schema_version", `Number "1"
      ; "kind", `String ("parallel." ^ name)
      ; ( "payload"
        , `Object
            [ "name", `String name
            ; "text", `String (repeated ^ "\"line\nslash\\tail")
            ; "escaped", `String "é\t"
            ; "number", `Number "1.2300e+02"
            ; "nested", `Array [ `Object [ "id", `String name; "unknown", `Null ] ]
            ] )
      ; "future", `Object [ "kept", `True ]
      ]
  in
  text, expected
;;

let capture f =
  match f () with
  | value -> Ok value
  | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let reraise (exn, backtrace) = Exn.raise_with_original_backtrace exn backtrace

let%expect_test
    "concurrent native-domain document decodes preserve exact independent text"
  =
  let limits =
    D.Limits.create ~max_bytes:262144 ~max_depth:16 ~max_fields:64 ~max_nodes:128 |> ok
  in
  let left = subject ~name:"original" ~character:'A' in
  let right = subject ~name:"worker" ~character:'B' in
  let decode ~iterations ~name (text, expected) =
    for iteration = 1 to iterations do
      let actual = D.Document.decode ~limits text |> ok |> D.Document.json in
      if not (Jsonaf.exactly_equal expected actual)
      then
        raise_s
          [%sexp
            "concurrent decode changed exact contents", (name : string), (iteration : int)]
    done
  in
  (* The reference is constructed directly, including order/number spelling and
     escaped contents. Both calls must decode correctly, not merely avoid Error. *)
  List.iter
    [ "original", left; "worker", right ]
    ~f:(fun (name, subject) -> decode ~iterations:1 ~name subject);
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn env#clock 30. (fun () ->
      Eio.Switch.run (fun sw ->
        let ready, ready_u = Eio.Promise.create () in
        let start, start_u = Eio.Promise.create () in
        let joined, joined_u = Eio.Promise.create () in
        let release () =
          if Option.is_none (Eio.Promise.peek start) then Eio.Promise.resolve start_u ()
        in
        (* Resolve the only gate before Switch joins, including on assertions,
           cancellation or timeout. Every worker workload is also finite. *)
        Exn.protect ~finally:release ~f:(fun () ->
          Eio.Fiber.fork ~sw (fun () ->
            let outcome =
              capture (fun () ->
                Eio.Domain_manager.run env#domain_mgr (fun () ->
                  Eio.Promise.resolve ready_u ();
                  Eio.Promise.await start;
                  decode ~iterations:128 ~name:"worker" right))
            in
            (* Domain_manager.run has joined the actual native domain before
               its success or exception is reported to the parent. *)
            Eio.Promise.resolve joined_u outcome);
          Eio.Promise.await ready;
          release ();
          let original =
            capture (fun () -> decode ~iterations:128 ~name:"original" left)
          in
          let worker = Eio.Promise.await joined in
          match original, worker with
          | Ok (), Ok () -> ()
          | Error primary, _ -> reraise primary
          | Ok (), Error failure -> reraise failure))));
  print_endline "both native domains preserved complete exact document contents";
  [%expect {| both native domains preserved complete exact document contents |}]
;;
