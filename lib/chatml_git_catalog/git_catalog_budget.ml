open Core
open Git_catalog_model

type t =
  { repository : Chatml_git_host.Repository.t
  ; limits : Limits.t
  ; user_checkpoint : unit -> unit
  ; stop : Error.t -> unit
  ; mutable declared_at : Project.Declared_at.t option
  ; mutable object_count : int
  ; mutable object_bytes : int
  ; mutable tree_entries : int
  ; mutable source_count : int
  ; mutable source_bytes : int
  ; mutable work_steps : int
  }

let limits t = t.limits
let context t declared_at = t.declared_at <- declared_at
let error t problem = Error.create ?declared_at:t.declared_at problem
let limited t limit = error t (Limit_exceeded limit)

let checkpoint t =
  (match Chatml_git_host.Repository.checkpoint t.repository with
   | Error failure -> t.stop (error t (Host failure))
   | Ok () -> ());
  if t.work_steps >= t.limits.max_work_steps then t.stop (limited t Work_steps);
  t.work_steps <- t.work_steps + 1;
  t.user_checkpoint ()
;;

let object_allowance t =
  Int.min t.limits.max_object_bytes (t.limits.max_total_object_bytes - t.object_bytes)
;;

let equal_string t first second =
  if String.length first <> String.length second
  then false
  else (
    let rec loop offset =
      if offset = String.length first
      then true
      else (
        if offset mod 4096 = 0 then checkpoint t;
        Char.equal first.[offset] second.[offset] && loop (offset + 1))
    in
    loop 0)
;;

let admit_object t =
  checkpoint t;
  if t.object_count >= t.limits.max_objects
  then Error (limited t Object_count)
  else (
    t.object_count <- t.object_count + 1;
    Ok ())
;;

let admit_body t bytes =
  checkpoint t;
  if bytes < 0 then t.stop (error t Accounting_overflow);
  if bytes > t.limits.max_object_bytes then t.stop (limited t Object_bytes);
  if bytes > t.limits.max_total_object_bytes - t.object_bytes
  then t.stop (limited t Total_object_bytes);
  t.object_bytes <- t.object_bytes + bytes
;;

let tree_allowance t = t.limits.max_tree_entries - t.tree_entries

let admit_tree t count =
  if count < 0
  then Error (error t Accounting_overflow)
  else if count > tree_allowance t
  then Error (limited t Tree_entries)
  else (
    t.tree_entries <- t.tree_entries + count;
    Ok ())
;;

let admit_source t bytes =
  checkpoint t;
  if bytes < 0
  then Error (error t Accounting_overflow)
  else if bytes > t.limits.max_source_bytes
  then Error (limited t Source_bytes)
  else if t.source_count >= t.limits.max_sources
  then Error (limited t Source_count)
  else if bytes > t.limits.max_total_source_bytes - t.source_bytes
  then Error (limited t Total_source_bytes)
  else (
    t.source_count <- t.source_count + 1;
    t.source_bytes <- t.source_bytes + bytes;
    Ok ())
;;

let snapshot t =
  { Accounting.object_count = t.object_count
  ; object_bytes = t.object_bytes
  ; tree_entries = t.tree_entries
  ; source_count = t.source_count
  ; source_bytes = t.source_bytes
  ; work_steps = t.work_steps
  }
;;

let with_budget repository ~limits ~checkpoint:user_checkpoint ~f =
  let module Escape = struct
    exception Expected of Error.t
  end
  in
  let t =
    { repository
    ; limits
    ; user_checkpoint
    ; stop = (fun error -> raise (Escape.Expected error))
    ; declared_at = None
    ; object_count = 0
    ; object_bytes = 0
    ; tree_entries = 0
    ; source_count = 0
    ; source_bytes = 0
    ; work_steps = 0
    }
  in
  try
    let open Result.Let_syntax in
    let%map result = f t in
    checkpoint t;
    result, snapshot t
  with
  | Escape.Expected error -> Error error
;;
