open! Core

module Budget = struct
  type t =
    { max_entries : int
    ; max_original_bytes : int
    }

  let create ~max_entries ~max_original_bytes =
    if max_entries < 0 || max_original_bytes < 0
    then
      Error
        (Store_error.Document
           (Document_schema.Error.Invalid_configuration
              "restoration cache allowances must be nonnegative"))
    else Ok { max_entries; max_original_bytes }
  ;;

  let default = { max_entries = 128; max_original_bytes = 16 * 1024 * 1024 }
end

module Snapshot_entry = struct
  type 'state t =
    { original : Document_record.t
    ; filename : string
    ; covered : int64
    ; installed : Snapshot.installed
    ; initial_state : 'state
    }
end

module Entry = struct
  type 'state t =
    | Snapshot of 'state Snapshot_entry.t
    | Transaction of
        { original : Document_record.t
        ; value : Transaction.t
        }
end

type 'state t =
  { budget : Budget.t
  ; entries : 'state Entry.t list String.Map.t
  }

let empty ~budget = { budget; entries = String.Map.empty }

let same_original left right =
  Int.equal (Document_record.flags left) (Document_record.flags right)
  && String.equal
       (Document_record.stored_digest left)
       (Document_record.stored_digest right)
  && String.equal (Document_record.stored_bytes left) (Document_record.stored_bytes right)
;;

let find_snapshot t ~filename ~covered ~original =
  Map.find t.entries (Document_record.stored_digest original)
  |> Option.bind ~f:(fun entries ->
    List.find_map entries ~f:(function
      | Entry.Snapshot cached
        when String.equal cached.filename filename
             && Int64.equal cached.covered covered
             && same_original cached.original original ->
        Some (cached.installed, cached.initial_state)
      | Snapshot _ | Transaction _ -> None))
;;

let find_transaction t ~original =
  Map.find t.entries (Document_record.stored_digest original)
  |> Option.bind ~f:(fun entries ->
    List.find_map entries ~f:(function
      | Entry.Transaction cached when same_original cached.original original ->
        Some cached.value
      | Snapshot _ | Transaction _ -> None))
;;

module Selection = struct
  type 'state t =
    { remaining_entries : int
    ; remaining_bytes : int
    ; entries : 'state Entry.t list String.Map.t
    }

  let add t original entry =
    let bytes = String.length (Document_record.stored_bytes original) in
    if t.remaining_entries > 0 && bytes <= t.remaining_bytes
    then
      { remaining_entries = t.remaining_entries - 1
      ; remaining_bytes = t.remaining_bytes - bytes
      ; entries =
          Map.update t.entries (Document_record.stored_digest original) ~f:(function
            | None -> [ entry ]
            | Some entries -> entry :: entries)
      }
    else t
  ;;
end

let replace t ~snapshots ~transactions =
  let selected =
    List.fold
      snapshots
      ~init:
        { Selection.remaining_entries = t.budget.max_entries
        ; remaining_bytes = t.budget.max_original_bytes
        ; entries = String.Map.empty
        }
      ~f:(fun selected (filename, installed, initial_state) ->
        let original =
          Snapshot.Stored.record (Snapshot.stored installed.Snapshot.snapshot)
        in
        let entry =
          Entry.Snapshot
            { original
            ; filename
            ; covered = installed.snapshot.transaction_sequence
            ; installed
            ; initial_state
            }
        in
        Selection.add selected original entry)
  in
  let selected =
    List.fold (List.rev transactions) ~init:selected ~f:(fun selected transaction ->
      let original = Transaction.Stored.record (Transaction.stored transaction) in
      Selection.add
        selected
        original
        (Entry.Transaction { original; value = transaction }))
  in
  { budget = t.budget; entries = selected.entries }
;;
