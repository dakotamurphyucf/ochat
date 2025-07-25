open Core

(** Path autocompletion engine – **blocking prototype**.

    This implementation is intentionally minimal so that command-mode can be
    developed and unit-tested before the fully-asynchronous refactor lands.
    The core idea is explained in {!file:lib/chat_tui/path_completion.md}: keep
    one cached, alphabetically *sorted* array per directory and perform two
    binary searches per keystroke to locate the slice that shares the current
    prefix.

    Key points:
    • Uses {!Eio.Path.read_dir} for non-blocking I/O but otherwise blocks the
      caller while reading the directory once every TTL (2 s).
    • Keeps **zero** per-prefix state – the only mutable state is a tiny
      per-directory cache and the last result stored in the [t] context so the
      user can cycle through matches.
    • Suitable for directories containing a few tens-of-thousands of entries
      (worst-case cost: *O(N log N)* on cache miss, *O(log N + K)* on cache
      hit).

    See the interface file {!Path_completion} for the public API. *)

module Dir_cache = struct
  type entry =
    { names : string array (* sorted *)
    ; ts : float (* last refresh *)
    }

  let ttl = 2.0
  let table : entry String.Table.t = String.Table.create ()

  let refresh ~fs dir : entry =
    (* Read directory using Eio non-blocking API. *)
    let names =
      try
        Eio.Path.read_dir Eio.Path.(fs / dir)
        |> List.filter ~f:(fun n -> not (String.equal n "." || String.equal n ".."))
        |> Array.of_list
      with
      | _ -> [||]
    in
    Array.sort names ~compare:String.compare;
    let entry = { names; ts = Core_unix.gettimeofday () } in
    Hashtbl.set table ~key:dir ~data:entry;
    entry
  ;;

  let get ~fs dir : entry =
    let now = Core_unix.gettimeofday () in
    match Hashtbl.find table dir with
    | Some e when Float.(now - e.ts < ttl) -> e
    | _ -> refresh ~fs dir
  ;;
end

type t =
  { mutable last : string list
  ; mutable idx : int
  }

let create () = { last = []; idx = 0 }

(* lower_bound binary search on [arr] between indices [lo, hi) *)
let lower_bound arr ~lo ~hi key =
  let rec aux lo hi =
    if lo >= hi
    then lo
    else (
      let mid = (lo + hi) lsr 1 in
      if String.compare arr.(mid) key < 0 then aux (mid + 1) hi else aux lo mid)
  in
  aux lo hi
;;

let hi_token prefix = prefix ^ "\255"

let suggestions (t : t) ~fs ~(cwd : string) ~(prefix : string) : string list =
  let dir_part, frag =
    match String.rsplit2 prefix ~on:'/' with
    | None -> "", prefix
    | Some (d, f) -> d, f
  in
  let dir_abs =
    if String.is_empty dir_part
    then cwd
    else if Filename.is_relative dir_part
    then Filename.concat cwd dir_part
    else dir_part
  in
  let entry = Dir_cache.get ~fs dir_abs in
  let lo = lower_bound entry.names ~lo:0 ~hi:(Array.length entry.names) frag in
  let hi = lower_bound entry.names ~lo ~hi:(Array.length entry.names) (hi_token frag) in
  let matches =
    Array.sub entry.names ~pos:lo ~len:(hi - lo)
    |> Array.to_list
    |> fun l -> List.take l 25
  in
  t.last <- matches;
  t.idx <- 0;
  matches
;;

let next (t : t) ~(dir : [ `Fwd | `Back ]) : string option =
  match t.last with
  | [] -> None
  | lst ->
    let len = List.length lst in
    t.idx
    <- (match dir with
        | `Fwd -> (t.idx + 1) mod len
        | `Back -> (t.idx - 1 + len) mod len);
    List.nth lst t.idx
;;

let reset (t : t) =
  t.last <- [];
  t.idx <- 0
;;
