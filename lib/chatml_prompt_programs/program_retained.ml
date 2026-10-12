open Core
module Captured = Chatml_source
module Inventory = Program_inventory
module Revision = Program_source_revision

module Program = struct
  type input =
    | Provided
    | Selected of Captured.t

  type t =
    { key : string
    ; binding : Program_binding.t
    ; compiler_contract : Captured.Digest.t
    ; input : input
    }

  let key t = t.key
  let binding t = t.binding
  let compiler_contract t = t.compiler_contract
  let input t = t.input
end

type t = Program.t list

module Error = struct
  type t =
    | Invalid_key
    | Invalid_work_limit
    | Work_limit_exceeded
    | Missing_key of string
    | Ambiguous_key of string
    | Provided_key of string
    | Invalid_inventory of string
end

let programs t = t

let with_budget ~max_work_steps ~checkpoint ~f =
  let module Abort = struct
    exception Stop of Error.t
  end
  in
  if max_work_steps <= 0
  then Error Error.Invalid_work_limit
  else (
    let remaining = ref max_work_steps in
    let poll () =
      checkpoint ();
      if !remaining <= 0 then raise (Abort.Stop Error.Work_limit_exceeded);
      decr remaining
    in
    try f poll with
    | Abort.Stop error -> Error error)
;;

let bytes_equal left right ~poll =
  poll ();
  if String.length left <> String.length right
  then false
  else (
    let offset = ref 0 in
    let same = ref true in
    while !same && !offset < String.length left do
      poll ();
      let stop = !offset + Int.min 4096 (String.length left - !offset) in
      while !same && !offset < stop do
        same := Char.equal left.[!offset] right.[!offset];
        incr offset
      done
    done;
    !same)
;;

let same_revision expected source ~poll =
  let expected_id = Revision.id expected in
  let actual_id = Captured.id source in
  bytes_equal
    (Captured.Id.package_namespace expected_id)
    (Captured.Id.package_namespace actual_id)
    ~poll
  && bytes_equal
       (Captured.Id.relative_path expected_id)
       (Captured.Id.relative_path actual_id)
       ~poll
  && Captured.Digest.equal (Revision.digest expected) (Captured.digest source)
  && Int.equal (Revision.byte_length expected) (Captured.byte_length source)
;;

let find_lock sources expected ~poll =
  let open Result.Let_syntax in
  let%bind found =
    List.fold_result sources ~init:None ~f:(fun found source ->
      poll ();
      if not (same_revision expected source ~poll)
      then Ok found
      else (
        match found with
        | None -> Ok (Some source)
        | Some _ -> Error (Error.Invalid_inventory "Ambiguous selected lock source")))
  in
  match found with
  | None -> Error (Error.Invalid_inventory "Missing selected lock source")
  | Some source -> Ok source
;;

let create inventory ~max_work_steps ~checkpoint =
  with_budget ~max_work_steps ~checkpoint ~f:(fun poll ->
    poll ();
    let open Result.Let_syntax in
    let%map reversed =
      List.fold_result inventory.Inventory.programs ~init:[] ~f:(fun reversed record ->
        poll ();
        let%map projected_input =
          match record.Inventory.Program.kind with
          | Provided _ -> Ok Program.Provided
          | Selected expected ->
            let%map source = find_lock inventory.sources expected ~poll in
            Program.Selected source
        in
        Program.
          { key = record.key
          ; binding = record.binding
          ; compiler_contract = record.compiler_contract
          ; input = projected_input
          }
        :: reversed)
    in
    (* Charge the final copy as well as metadata/source visits. *)
    List.fold reversed ~init:[] ~f:(fun acc program ->
      poll ();
      program :: acc))
;;

let valid_key key =
  String.length key = 129
  && Char.equal key.[64] ':'
  && String.for_alli key ~f:(fun index character ->
    index = 64
    || Char.(character >= '0' && character <= '9')
    || Char.(character >= 'a' && character <= 'f'))
;;

let selected_lock t ~key ~max_work_steps ~checkpoint =
  if not (valid_key key)
  then Error Error.Invalid_key
  else
    with_budget ~max_work_steps ~checkpoint ~f:(fun poll ->
      poll ();
      let open Result.Let_syntax in
      let%bind found =
        List.fold_result t ~init:None ~f:(fun found program ->
          poll ();
          if not (bytes_equal program.Program.key key ~poll)
          then Ok found
          else (
            match found with
            | None -> Ok (Some program)
            | Some _ -> Error (Error.Ambiguous_key key)))
      in
      match found with
      | None -> Error (Error.Missing_key key)
      | Some program ->
        (match program.input with
         | Provided -> Error (Error.Provided_key key)
         | Selected source -> Ok source))
;;
