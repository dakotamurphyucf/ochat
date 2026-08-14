open Core

let ok_exn = function
  | Ok value -> value
  | Error error -> failwith error
;;

let reasoning id : Openai.Responses.Item.t =
  Openai.Responses.Item.Reasoning { summary = []; _type = "reasoning"; id; status = None }
;;

let%expect_test "allocation, namespaces, payload replacement, and validation" =
  let allocator =
    History_entry.Allocator.create ~namespace:"session:a" ~next_sequence:0 |> ok_exn
  in
  let other =
    History_entry.Allocator.create ~namespace:"session:b" ~next_sequence:0 |> ok_exn
  in
  let first = History_entry.create ~allocator (reasoning "provider") |> ok_exn in
  let second = History_entry.create ~allocator (reasoning "provider") |> ok_exn in
  let third = History_entry.create ~allocator:other (reasoning "provider") |> ok_exn in
  let replaced = History_entry.with_item first (reasoning "replacement") in
  print_s
    [%sexp
      { first = (History_entry.Id.to_string (History_entry.id first) : string)
      ; second = (History_entry.Id.to_string (History_entry.id second) : string)
      ; third = (History_entry.Id.to_string (History_entry.id third) : string)
      ; replacement_preserved_id =
          (History_entry.Id.equal (History_entry.id first) (History_entry.id replaced)
           : bool)
      ; next_sequence = (History_entry.Allocator.next_sequence allocator : int)
      ; valid =
          (Result.is_ok (History_entry.validate ~allocator [ first; second; third ])
           : bool)
      ; duplicate_rejected =
          (Result.is_error (History_entry.validate ~allocator [ first; first ]) : bool)
      }];
  [%expect
    {|
    ((first 9:session:a:0) (second 9:session:a:1) (third 9:session:b:0)
     (replacement_preserved_id true) (next_sequence 2) (valid true)
     (duplicate_rejected true))
    |}]
;;

let%expect_test "ID codecs and restored allocator high-water mark" =
  let id = History_entry.Id.create ~namespace:"a:b" ~sequence:42 |> ok_exn in
  let encoded = History_entry.Id.to_string id in
  let decoded = History_entry.Id.of_string encoded |> ok_exn in
  let sexp_round_trip =
    History_entry.Id.t_of_sexp (History_entry.Id.sexp_of_t id)
    |> History_entry.Id.equal id
  in
  let restored =
    History_entry.Allocator.create ~namespace:"a:b" ~next_sequence:43 |> ok_exn
  in
  let next = History_entry.Allocator.allocate restored |> ok_exn in
  let bin_round_trip =
    let size = History_entry.Id.bin_size_t id in
    let buffer = Bigstring.create size in
    let end_pos = History_entry.Id.bin_write_t buffer ~pos:0 id in
    let pos_ref = ref 0 in
    let decoded = History_entry.Id.bin_read_t buffer ~pos_ref in
    end_pos = size && !pos_ref = size && History_entry.Id.equal id decoded
  in
  print_s
    [%sexp
      { encoded : string
      ; string_round_trip = (History_entry.Id.equal id decoded : bool)
      ; sexp_round_trip : bool
      ; bin_round_trip : bool
      ; restored_next = (History_entry.Id.to_string next : string)
      }];
  [%expect
    {|
    ((encoded 3:a:b:42) (string_round_trip true) (sexp_round_trip true)
     (bin_round_trip true) (restored_next 3:a:b:43))
    |}]
;;

let%expect_test "invalid codecs and failed reservations do not advance" =
  let allocator =
    History_entry.Allocator.create ~namespace:"limits" ~next_sequence:Int.max_value
    |> ok_exn
  in
  let invalid_strings = [ ""; "0::0"; "1:a:-1"; "01:a:0"; "1:a:00"; "1:a:0:trailing" ] in
  let malformed_rejected =
    List.for_all invalid_strings ~f:(fun encoded ->
      Result.is_error (History_entry.Id.of_string encoded))
  in
  let invalid_sexp_rejected =
    Option.is_none
      (Option.try_with (fun () -> History_entry.Id.t_of_sexp (Sexp.Atom "0::0")))
  in
  let json_round_trip =
    let id = History_entry.Id.create ~namespace:"json" ~sequence:3 |> ok_exn in
    History_entry.Id.t_of_jsonaf (History_entry.Id.jsonaf_of_t id)
    |> History_entry.Id.equal id
  in
  let invalid_json_rejected =
    Option.is_none
      (Option.try_with (fun () -> History_entry.Id.t_of_jsonaf (`String "0::0")))
  in
  let invalid_bin_rejected =
    let encoded = "0::0" in
    let size = Bin_prot.Size.bin_size_string encoded in
    let buffer = Bigstring.create size in
    let (_ : int) = Bin_prot.Write.bin_write_string buffer ~pos:0 encoded in
    let pos_ref = ref 0 in
    Option.is_none
      (Option.try_with (fun () -> History_entry.Id.bin_read_t buffer ~pos_ref))
  in
  let before = History_entry.Allocator.next_sequence allocator in
  let exhausted = Result.is_error (History_entry.Allocator.allocate allocator) in
  let negative =
    Result.is_error (History_entry.Allocator.reserve allocator ~count:(-1))
  in
  let unchanged = Int.equal before (History_entry.Allocator.next_sequence allocator) in
  print_s
    [%sexp
      { malformed_rejected : bool
      ; invalid_sexp_rejected : bool
      ; json_round_trip : bool
      ; invalid_json_rejected : bool
      ; invalid_bin_rejected : bool
      ; exhausted : bool
      ; negative : bool
      ; unchanged : bool
      }];
  [%expect
    {|
    ((malformed_rejected true) (invalid_sexp_rejected true)
     (json_round_trip true) (invalid_json_rejected true)
     (invalid_bin_rejected true) (exhausted true) (negative true)
     (unchanged true))
    |}]
;;

let%expect_test "batch and collection high-water invariants" =
  let allocator =
    History_entry.Allocator.create ~namespace:"batch" ~next_sequence:5 |> ok_exn
  in
  let batch = History_entry.Allocator.reserve allocator ~count:3 |> ok_exn in
  let sequences = List.map batch ~f:History_entry.Id.sequence in
  let item = reasoning "provider" in
  let at_watermark =
    History_entry.Id.create ~namespace:"batch" ~sequence:8
    |> ok_exn
    |> fun id -> History_entry.create_with_id ~id item
  in
  let foreign =
    History_entry.Id.create ~namespace:"foreign" ~sequence:Int.max_value
    |> ok_exn
    |> fun id -> History_entry.create_with_id ~id item
  in
  print_s
    [%sexp
      { sequences : int list
      ; matching_rejected =
          (Result.is_error (History_entry.validate ~allocator [ at_watermark ]) : bool)
      ; foreign_accepted =
          (Result.is_ok (History_entry.validate ~allocator [ foreign ]) : bool)
      }];
  [%expect
    {|
    ((sequences (5 6 7)) (matching_rejected true) (foreign_accepted true))
    |}]
;;

let%expect_test "identical call IDs remain distinct history occurrences" =
  let allocator =
    History_entry.Allocator.create ~namespace:"calls" ~next_sequence:0 |> ok_exn
  in
  let call : Openai.Responses.Item.t =
    Function_call
      { arguments = "{}"
      ; call_id = "same-call"
      ; name = "tool"
      ; _type = "function_call"
      ; id = Some "same-provider-id"
      ; status = None
      }
  in
  let first = History_entry.create ~allocator call |> ok_exn in
  let second = History_entry.create ~allocator call |> ok_exn in
  print_s
    [%sexp
      (not (History_entry.Id.equal (History_entry.id first) (History_entry.id second))
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "concurrent batch allocation is unique" =
  let allocator =
    History_entry.Allocator.create ~namespace:"parallel" ~next_sequence:0 |> ok_exn
  in
  let domains =
    List.init 4 ~f:(fun _ ->
      Domain.spawn (fun () ->
        History_entry.Allocator.reserve allocator ~count:250 |> ok_exn))
  in
  let ids = List.concat_map domains ~f:Domain.join in
  let unique = Hash_set.of_list (module History_entry.Id) ids in
  print_s
    [%sexp
      { allocated = (List.length ids : int)
      ; unique = (Hash_set.length unique : int)
      ; next_sequence = (History_entry.Allocator.next_sequence allocator : int)
      }];
  [%expect {| ((allocated 1000) (unique 1000) (next_sequence 1000)) |}]
;;
