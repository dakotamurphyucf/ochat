open Core
module Attempt = Project_prepare_attempt
module Accounting = Project_prepare_accounting
module Captured = Chatml_source
module Entry = Project_prepare_request.Entry
module Build = Chatml_project_build
module Catalog = Chatml_catalog
module Error = Project_prepare_error

let compare_contents attempt first second =
  let open Result.Let_syntax in
  let first = Captured.contents first in
  let second = Captured.contents second in
  if phys_equal first second
  then Ok true
  else (
    let bytes = String.length first in
    let%map () = Attempt.charge attempt { Accounting.zero with work_steps = bytes } in
    let offset = ref 0 in
    let equal = ref true in
    while !equal && !offset < bytes do
      Attempt.poll attempt;
      let stop = !offset + Int.min 4096 (bytes - !offset) in
      while !equal && !offset < stop do
        equal := Char.equal first.[!offset] second.[!offset];
        incr offset
      done
    done;
    !equal)
;;

let find_member attempt root member =
  let member = Source.Portable_path.to_string member in
  (* Catalog admission guarantees every source belongs to its effective package
     namespace. Compare the bounded member labels, not unbounded whole IDs. *)
  let rec find = function
    | [] -> Ok None
    | source :: rest ->
      let path = Captured.Id.relative_path (Captured.id source) in
      let open Result.Let_syntax in
      let%bind () =
        Attempt.charge
          attempt
          { Accounting.zero with work_steps = 1 + String.length path }
      in
      if String.equal member path then Ok (Some source) else find rest
  in
  find (Catalog.sources root)
;;

let resolve attempt ~root entry =
  Attempt.poll attempt;
  match entry with
  | Entry.Declared -> Ok Build.Entry.Declared
  | Captured_fragment source | Captured_file { source; root_member = None } ->
    Ok (Build.Entry.Captured source)
  | Captured_file { source; root_member = Some member } ->
    let open Result.Let_syntax in
    let%bind found = find_member attempt root member in
    (match found with
     | None -> Ok (Build.Entry.Captured source)
     | Some actual ->
       let%bind equal =
         if
           Int.equal (Captured.byte_length source) (Captured.byte_length actual)
           && Captured.Digest.equal (Captured.digest source) (Captured.digest actual)
         then compare_contents attempt source actual
         else Ok false
       in
       if equal
       then Ok (Build.Entry.Captured actual)
       else
         Attempt.reject
           attempt
           (Error.create
              ~declared_at:(Chatml_project.declared_at (Catalog.project root))
              (Entry_revision_conflict (Captured.id actual))))
;;
