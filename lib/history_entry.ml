open! Core

module Id = struct
  type t =
    { namespace : string
    ; sequence : int
    }
  [@@deriving compare, hash]

  let validate_namespace namespace =
    if String.is_empty namespace
    then Error "history ID namespace must be nonempty"
    else Ok ()
  ;;

  let create ~namespace ~sequence =
    let open Result.Let_syntax in
    let%map () = validate_namespace namespace
    and () =
      if sequence < 0 then Error "history ID sequence must be nonnegative" else Ok ()
    in
    { namespace; sequence }
  ;;

  let namespace t = t.namespace
  let sequence t = t.sequence
  let equal a b = compare a b = 0
  let to_string t = sprintf "%d:%s:%d" (String.length t.namespace) t.namespace t.sequence

  let parse_int value =
    match Option.try_with (fun () -> Int.of_string value) with
    | Some value -> Ok value
    | None -> Error "history ID contains an invalid integer"
  ;;

  let of_string encoded =
    let open Result.Let_syntax in
    match String.lsplit2 encoded ~on:':' with
    | None -> Error "history ID is missing its namespace length"
    | Some (length_text, rest) ->
      let%bind namespace_length = parse_int length_text in
      if namespace_length < 0 || String.length rest <= namespace_length
      then Error "history ID has an invalid namespace length"
      else if not (Char.equal rest.[namespace_length] ':')
      then Error "history ID is missing its sequence separator"
      else (
        let namespace = String.sub rest ~pos:0 ~len:namespace_length in
        let sequence_text = String.drop_prefix rest (namespace_length + 1) in
        let%bind sequence = parse_int sequence_text in
        let%bind id = create ~namespace ~sequence in
        if String.equal encoded (to_string id)
        then Ok id
        else Error "history ID is not canonically encoded")
  ;;

  let invalid_encoded error = failwith ("invalid encoded history ID: " ^ error)

  let of_string_exn encoded =
    match of_string encoded with
    | Ok id -> id
    | Error error -> invalid_encoded error
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)

  let t_of_sexp = function
    | Sexp.Atom encoded -> of_string_exn encoded
    | sexp ->
      Sexplib.Conv.of_sexp_error
        "History_entry.Id.t must be a canonical encoded string"
        sexp
  ;;

  let jsonaf_of_t t = `String (to_string t)

  let t_of_jsonaf = function
    | `String encoded -> of_string_exn encoded
    | _ -> failwith "History_entry.Id.t must be a JSON string"
  ;;

  let bin_shape_t = Bin_prot.Shape.bin_shape_string
  let bin_size_t t = Bin_prot.Size.bin_size_string (to_string t)

  let bin_write_t buffer ~pos t =
    Bin_prot.Write.bin_write_string buffer ~pos (to_string t)
  ;;

  let bin_read_t buffer ~pos_ref =
    Bin_prot.Read.bin_read_string buffer ~pos_ref |> of_string_exn
  ;;

  let __bin_read_t__ buffer ~pos_ref _length = bin_read_t buffer ~pos_ref

  let bin_writer_t : t Bin_prot.Type_class.writer =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let bin_reader_t : t Bin_prot.Type_class.reader =
    { read = bin_read_t; vtag_read = __bin_read_t__ }
  ;;

  let bin_t : t Bin_prot.Type_class.t =
    { writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
  ;;
end

module Allocator = struct
  type t =
    { namespace : string
    ; next_sequence : int Atomic.t
    }

  let create ~namespace ~next_sequence =
    let open Result.Let_syntax in
    let%map (_ : Id.t) = Id.create ~namespace ~sequence:next_sequence in
    { namespace; next_sequence = Atomic.make next_sequence }
  ;;

  let namespace t = t.namespace
  let next_sequence t = Atomic.get t.next_sequence

  let rec reserve t ~count =
    if count < 0
    then Error "history ID reservation count must be nonnegative"
    else (
      let next_sequence = Atomic.get t.next_sequence in
      if count > Int.max_value - next_sequence
      then Error "history ID sequence exhausted"
      else (
        let ids =
          List.init count ~f:(fun offset ->
            { Id.namespace = t.namespace; sequence = next_sequence + offset })
        in
        if Atomic.compare_and_set t.next_sequence next_sequence (next_sequence + count)
        then Ok ids
        else reserve t ~count))
  ;;

  let allocate t =
    Result.bind (reserve t ~count:1) ~f:(function
      | [ id ] -> Ok id
      | _ -> Error "history ID allocator violated its reservation invariant")
  ;;
end

type t =
  { id : Id.t
  ; item : Openai.Responses.Item.t
  }
[@@deriving bin_io, sexp]

let create ~allocator item =
  Result.map (Allocator.allocate allocator) ~f:(fun id -> { id; item })
;;

let create_with_id ~id item = { id; item }
let id t = t.id
let item t = t.item
let with_item t item = { t with item }
let items entries = List.map entries ~f:item

let validate ~allocator entries =
  let ids = Hash_set.create (module Id) in
  let next_sequence = Allocator.next_sequence allocator in
  List.fold_result entries ~init:() ~f:(fun () entry ->
    if String.is_empty (Id.namespace entry.id) || Id.sequence entry.id < 0
    then Error "history contains an invalid entry ID"
    else if Hash_set.mem ids entry.id
    then Error "history contains a duplicate entry ID"
    else (
      Hash_set.add ids entry.id;
      if
        String.equal (Id.namespace entry.id) (Allocator.namespace allocator)
        && Id.sequence entry.id >= next_sequence
      then Error "history entry sequence is not below the allocator high-water mark"
      else Ok ()))
;;
