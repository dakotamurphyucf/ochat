open! Core
open Admission_tests
module Store = Agent_store
module J = Store.Journal
module Initial = J.Initial_creation

let () = Mirage_crypto_rng_unix.use_default ()

let with_directory f =
  Eio_main.run (fun env ->
    let name = "och234-" ^ P.Id.Transaction.(create () |> to_string) in
    let root = Eio.Path.(env#fs / "/tmp" / name) in
    Eio.Path.mkdir ~perm:0o700 root;
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
      (fun () -> f env root))
;;

let payload =
  actual_transaction (Receipt_tests.created_state ()) |> Store.Transaction.encode
;;

let framed flags payload =
  Store.Frame.encode ~max_payload_length:(String.length payload + 100) ~flags payload
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Store.Frame.sexp_of_error error))
  |> Result.ok_or_failwith
;;

let reader env root ?(bytes = 1_000_000) () =
  Store.Retention_reader.create
    ~env
    ~root:(Eio.Path.native_exn root)
    ~max_entries:100
    ~max_bytes:bytes
  |> store_ok
;;

let first = Store.Journal_segment.Id.first
let next = Store.Journal_segment.Id.next first |> store_ok

type phase =
  | Created
  | Partial_seal
  | Sealed
  | Successor_created
  | Current_published

let create_interrupted_phase env root phase =
  let directory = Eio.Path.native_exn root in
  let first_segment =
    Store.Journal_segment.create_exclusive ~env ~directory ~id:first |> store_ok
  in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(root / "CURRENT")
    (Store.Journal_segment.Id.filename first ^ "\n");
  ignore
    (Store.Journal_segment.append
       ~env
       ~durability:Flush
       first_segment
       ~frame:(framed 0 payload)
     |> store_ok
     : int64 * int64);
  (match phase with
   | Created -> ()
   | Partial_seal | Sealed | Successor_created | Current_published ->
     let seal = framed 1 "segment sealed" in
     let seal =
       match phase with
       | Partial_seal -> String.prefix seal (String.length seal - 1)
       | Created | Sealed | Successor_created | Current_published -> seal
     in
     ignore
       (Store.Journal_segment.append ~env ~durability:Flush first_segment ~frame:seal
        |> store_ok
        : int64 * int64));
  (match phase with
   | Successor_created | Current_published ->
     ignore
       (Store.Journal_segment.create_exclusive ~env ~directory ~id:next |> store_ok
        : Store.Journal_segment.t)
   | Created | Partial_seal | Sealed -> ());
  match phase with
  | Current_published ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(root / "CURRENT")
      (Store.Journal_segment.Id.filename next ^ "\n")
  | Created | Partial_seal | Sealed | Successor_created -> ()
;;

let create_phase env root = function
  | Current_published ->
    let journal =
      J.create
        ~env
        ~directory:(Eio.Path.native_exn root)
        ~max_payload_length:100_000
        ~max_segment_bytes:1_000_000L
        ~max_segment_frames:1
      |> store_ok
    in
    ignore
      (J.append journal ~durability:Flush ~flags:0 ~payload |> store_ok : J.append_result)
  | (Created | Partial_seal | Sealed | Successor_created) as phase ->
    create_interrupted_phase env root phase
;;

let inspect env root checkpoint =
  Initial.inspect (reader env root ()) ~env ~max_payload_length:100_000 ~checkpoint
  |> store_ok
  |> Option.value_exn
;;

let%test_unit
    "actual append and interrupted rotation phases preserve one Created identity"
  =
  List.iter
    [ Created; Partial_seal; Sealed; Successor_created; Current_published ]
    ~f:(fun phase ->
      with_directory (fun env root ->
        create_phase env root phase;
        let proof = inspect env root Fn.ignore in
        assert (String.equal (Store.Frame.payload (Initial.entry proof).frame) payload);
        (* Authenticate the original real Created before any owner repair. *)
        let transaction = Store.Transaction.decode payload |> store_ok in
        ignore
          (Session_seed_receipt.of_created transaction ~limits ~checkpoint:Fn.ignore
           |> store_ok
           : Session_seed_receipt.t);
        let journal =
          Initial.open_journal
            proof
            ~max_segment_bytes:1_000_000L
            ~max_segment_frames:100
            ~checkpoint:Fn.ignore
          |> store_ok
        in
        let expected_current =
          match phase with
          | Created | Partial_seal -> first
          | Sealed | Successor_created | Current_published -> next
        in
        assert (
          Store.Journal_segment.Id.equal (J.current_segment journal) expected_current);
        let scan = J.scan journal |> store_ok in
        assert (Option.is_none scan.crash_tail);
        let transactions =
          List.filter scan.entries ~f:(fun entry -> Store.Frame.flags entry.frame = 0)
        in
        assert (List.length transactions = 1);
        assert (
          String.equal (Store.Frame.payload (List.hd_exn transactions).frame) payload)))
;;

let%test_unit
    "prefix admission charges exact first frame and rejects later Pending transactions"
  =
  let header = Bytes.of_string (String.prefix (framed 0 "") Store.Frame.header_length) in
  let maximum = Int64.of_int Int.max_value in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let byte = Int64.((maximum lsr shift) land 0xffL) |> Int64.to_int_exn in
    Bytes.set header (12 + index) (Char.of_int_exn byte)
  done;
  assert (
    Result.is_error
      (Store.Frame.encoded_length_of_header
         ~max_payload_length:Int.max_value
         (Bytes.to_string header)));
  with_directory (fun env root ->
    create_phase env root Created;
    let frame_bytes = String.length (framed 0 payload) in
    let exact = reader env root ~bytes:frame_bytes () in
    let entry =
      J.initial_entry exact ~env ~max_payload_length:100_000 ~checkpoint:Fn.ignore
      |> store_ok
      |> Option.value_exn
    in
    assert (String.equal (Store.Frame.payload entry.frame) payload);
    assert (Store.Retention_reader.remaining_bytes exact = 0);
    assert (
      Result.is_error
        (J.initial_entry
           (reader env root ~bytes:(frame_bytes - 1) ())
           ~env
           ~max_payload_length:100_000
           ~checkpoint:Fn.ignore));
    let segment =
      Store.Journal_segment.open_existing
        ~env
        ~directory:(Eio.Path.native_exn root)
        ~id:first
      |> store_ok
    in
    ignore
      (Store.Journal_segment.append
         ~env
         ~durability:Flush
         segment
         ~frame:(framed 0 payload)
       |> store_ok
       : int64 * int64);
    assert (
      Option.is_some
        (J.initial_entry
           (reader env root ~bytes:frame_bytes ())
           ~env
           ~max_payload_length:100_000
           ~checkpoint:Fn.ignore
         |> store_ok));
    assert (
      Result.is_error
        (Initial.inspect
           (reader env root ())
           ~env
           ~max_payload_length:100_000
           ~checkpoint:Fn.ignore)))
;;

let%test_unit
    "every observed initial repair checkpoint preserves original exception and journal"
  =
  with_directory (fun env root ->
    create_phase env root Partial_seal;
    let original =
      Eio.Path.load Eio.Path.(root / Store.Journal_segment.Id.filename first)
    in
    let polls = ref 0 in
    let proof = inspect env root Fn.ignore in
    ignore
      (Initial.open_journal
         proof
         ~max_segment_bytes:1_000_000L
         ~max_segment_frames:100
         ~checkpoint:(fun () -> Int.incr polls)
       |> store_ok
       : J.t);
    assert (!polls > 0);
    for ordinal = 1 to !polls do
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(root / Store.Journal_segment.Id.filename first)
        original;
      let proof = inspect env root Fn.ignore in
      let calls = ref 0 in
      let injected = Failure (sprintf "initial repair checkpoint %d" ordinal) in
      (match
         Initial.open_journal
           proof
           ~max_segment_bytes:1_000_000L
           ~max_segment_frames:100
           ~checkpoint:(fun () ->
             Int.incr calls;
             if !calls = ordinal then raise injected)
       with
       | _ -> assert false
       | exception exn -> assert (phys_equal injected exn));
      assert (
        String.equal
          original
          (Eio.Path.load Eio.Path.(root / Store.Journal_segment.Id.filename first)));
      ignore (inspect env root Fn.ignore : Initial.t)
    done)
;;
