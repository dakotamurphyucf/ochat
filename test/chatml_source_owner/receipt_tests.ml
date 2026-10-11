open Core
open Support

let%test_unit
    "bounded original intent is metadata, with exact spelling and independent source \
     bytes"
  =
  let id = Agent_protocol.Id.Prompt_definition.of_string "prd_receipt" |> ok in
  let create text cap checkpoint =
    Receipt.create ~definition_id:id ~root_file:text ~max_path_bytes:cap ~checkpoint
  in
  let first = create "/root/prompt.chatmd" 19 ignore |> ok in
  let same = Receipt.of_json (Receipt.to_json first) |> ok in
  assert (Receipt.equal first same);
  assert (String.equal (Receipt.fingerprint first) (Receipt.fingerprint same));
  List.iter [ "relative.chatmd"; "/bad\000path" ] ~f:(fun text ->
    assert (Result.is_error (create text 4096 ignore)));
  assert (Result.is_error (create "/root/prompt.chatmd" 18 ignore));
  let changed = create "/root/./prompt.chatmd" 4096 ignore |> ok in
  assert (not (Receipt.equal first changed));
  let polls = ref 0 in
  ignore
    (create ("/" ^ String.make 2048 'a') 4096 (fun () -> incr polls) |> ok : Receipt.t);
  assert (!polls > 0);
  for stop = 1 to !polls do
    let token = Failure "receipt checkpoint" in
    let visited = ref 0 in
    match
      create
        ("/" ^ String.make 2048 'a')
        4096
        (fun () ->
           incr visited;
           if !visited = stop then raise token)
    with
    | exception exn -> assert (phys_equal exn token)
    | Ok _ | Error _ -> assert false
  done
;;
