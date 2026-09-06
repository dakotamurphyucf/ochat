open! Core

let io env scratch =
  let dir = Eio.Path.(Eio.Stdenv.fs env / scratch) in
  Io.save_doc ~dir "io.txt" "hello";
  Io.append_doc ~dir "io.txt" "\n";
  assert (String.equal (Io.load_doc ~dir "io.txt") "hello\n");
  Io.log ~dir ~file:"log.txt" "one";
  Io.log ~dir ~file:"log.txt" "two";
  assert (String.equal (Io.load_doc ~dir "log.txt") "onetwo");
  assert (String.is_prefix (Io.Base64.file_to_data_uri ~dir "io.txt") ~prefix:"data:");
  let permission = (Eio.Path.stat ~follow:true Eio.Path.(dir / "io.txt")).perm in
  assert (permission land 0o077 = 0)
;;

let pool env =
  Eio.Switch.run (fun sw ->
    let module Pool =
      Io.Task_pool (struct
        type input = string
        type output = string

        let dm = Eio.Stdenv.domain_mgr env
        let stream = Eio.Stream.create 0
        let sw = sw
        let handler = String.uppercase
      end)
    in
    Pool.spawn "docs";
    assert (String.equal (Pool.submit "abc") "ABC"))
;;

let caches () =
  let module Cache = Lru_cache.Make (Int) in
  let t = Cache.create ~max_size:2 () in
  Cache.set t ~key:1 ~data:"one";
  Cache.set t ~key:2 ~data:"two";
  assert (Option.equal String.equal (Cache.find t 1) (Some "one"));
  Cache.set t ~key:3 ~data:"three";
  assert (not (Cache.mem t 2));
  let module Ttl = Ttl_lru_cache.Make (String) in
  let t = Ttl.create ~max_size:2 () in
  Ttl.set_with_ttl t ~key:"expired" ~data:"old" ~ttl:(Time_ns.Span.of_sec (-1.));
  assert (Ttl.length t = 1);
  assert (List.length (Ttl.to_alist t) = 1);
  assert (Option.is_none (Ttl.find t "expired"));
  assert (Ttl.length t = 0);
  assert (Float.equal (Ttl.hit_rate t) 1.)
;;

let templates () =
  assert (
    String.equal
      (Template.render "{{x}}/{{ x }}/{{missing}}" [ "x", "yes" ])
      "yes/{{ x }}/{{missing}}");
  let module T =
    Template.Make_Template (struct
      type t = unit

      let to_key_value_pairs () = [ "x", "yes" ]
    end)
  in
  assert (String.equal (T.render (T.create "{{x}}/{{ x }}/{{missing}}") ()) "yes/yes/");
  assert (String.equal (Template.render "{{x}}" [ "x", "{{y}}"; "y", "later" ]) "later")
;;

let tokenizer () =
  let vocabulary =
    List.init 256 ~f:(fun code ->
      sprintf "%s %d" (Base64.encode_exn (String.of_char (Char.of_int_exn code))) code)
    |> String.concat ~sep:"\n"
  in
  let codec = Tikitoken.create_codec vocabulary in
  let text = "Hello, OCaml!" in
  let encoded = Tikitoken.encode ~codec ~text in
  let decoded = Tikitoken.decode ~codec ~encoded |> Bytes.to_string in
  assert (String.equal text decoded);
  assert (Bytes.length (Tikitoken.decode ~codec ~encoded:[ 999 ]) = 0)
;;

let vectors () =
  let docs =
    Array.init 21 ~f:(fun id ->
      { Vector_db.Vec.id = Int.to_string id
      ; len = 1
      ; vector = [| Float.of_int (21 - id); 1. |]
      })
  in
  let db = Vector_db.create_corpus docs in
  let embedding = Owl.Mat.of_array [| 1.; 0. |] 2 1 in
  assert (Array.equal Int.equal (Vector_db.query db embedding 1) [| 0 |]);
  let bm25 =
    Bm25.create
      (List.init 21 ~f:(fun id ->
         { Bm25.id; text = (if id = 20 then "needle" else "other") }))
  in
  assert (fst (List.hd_exn (Bm25.query bm25 ~text:"needle" ~k:1)) = 20);
  let hybrid = Vector_db.query_hybrid db ~bm25 ~beta:1. ~embedding ~text:"needle" ~k:1 in
  assert (not (Array.mem hybrid 20 ~equal:Int.equal))
;;

let sessions env scratch =
  let path = Eio.Path.(Eio.Stdenv.fs env / scratch / "session.bin") in
  let session = Session.create ~id:"docs" ~prompt_file:"example.chatmd" () in
  Session.Io.File.write path session;
  let loaded = Session_store.read_current_file path |> Or_error.ok_exn in
  assert (loaded.version = 5);
  assert (String.equal loaded.id "docs");
  Eio.Path.save ~create:(`Or_truncate 0o600) path "invalid snapshot";
  assert (Result.is_error (Session_store.read_current_file path));
  let _save : env:Eio_unix.Stdenv.base -> Session.t -> unit Or_error.t =
    Session_store.save
  in
  ()
;;

let run env scratch =
  io env scratch;
  pool env;
  caches ();
  templates ();
  tokenizer ();
  vectors ();
  sessions env scratch;
  Eio.Flow.copy_string
    "Library documentation behavior checks PASS\n"
    (Eio.Stdenv.stdout env)
;;
