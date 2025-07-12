Below is the smallest-possible “real” algorithm you need for run-time path completion once you already have the set of names that live in the *current* directory.

Why this one?
• It keeps zero per-prefix state.  
• Construction cost is just “sort the directory once”—fast enough to redo when the dir changes.  
• Per-keystroke work is O(log N + K) (N = entries in dir, K = number you show).  
• It can be made *incremental* (reuse the range from the previous keystroke) so even the log N virtually disappears while the user is typing.

Algorithm: “two-bound binary search on a sorted list”

Pre-step (whenever the directory contents may have changed)
1. Read all names in the directory into an array `names`.
2. Sort that array lexicographically.

Lookup step (every time the user types / erases a character)
Given `prefix`:
a) `lo = lower_bound(names, prefix)`  
   – first index whose value is ≥ `prefix`.

b) Construct the smallest string that is strictly *after* every string that begins with the prefix.  
   An easy trick: `hi_token = prefix + U+FFFF` (or any sentinel larger than all valid path chars).

c) `hi = lower_bound(names, hi_token)`

d) All matches are in the slice `names[lo : hi]`.  
   Return the first K of them, decorate with “/” for dirs, etc.

Pseudocode

function complete(sorted names[], string prefix, int k):
    lo = lower_bound(names, prefix)
    hi = lower_bound(names, prefix + '\uFFFF')
    return names[lo : min(hi, lo + k)]

`lower_bound` is the classic binary search that runs in
log₂N comparisons.

Making it incremental

While the user is still inside the same directory you can avoid rescanning
the whole array:

• Keep `(lo_prev, hi_prev)` from the previous keystroke.  
• The new prefix is either `prefix_prev + c` (user typed) or `prefix_prev[:-1]` (user deleted).

Case “typed one more char”  
 Run both binary searches **only inside** `names[lo_prev : hi_prev]` instead of the whole array.  
 That shrinks the log factor to log₂(width_of_previous_slice), usually a handful of steps.

Case “deleted one char”  
 Look one level up in the history stack of (lo, hi) slices you recorded earlier.

Handling dynamic directories

1. Cache the sorted list together with the directory’s mtime / change-id.  
2. On each completion request, if the cached mtime ≠ current mtime, rebuild the cache (O(N log N), still cheap for a few thousand names).  
   You can also subscribe to OS file-watchers (inotify / FSEvents / ReadDirectoryChangesW) and rebuild on change events.

Complexities

• Build (when dir changes): O(N log N) time, O(N) memory  
• Query (each keystroke):  
 – cold: O(log N + K)  
 – warm / incremental: O(log Δ + K) where Δ ≤ previous slice width  
 – scan fallback (tiny dirs): O(N · |prefix|) is fine too.

Why not tries/FSTs/… ?

For path completion you only ever look at *one* directory at a time, and those rarely exceed tens of thousands of entries.  The sort-&-binary-search approach is faster to build, uses less memory, and is easier to keep in sync than a trie or minimal DFA, yet it already delivers sub-millisecond latencies.

Drop-in reference implementation (Python 3)

import os, bisect, time, pathlib
from functools import lru_cache

MAX_RESULTS = 25
CACHE_TTL   = 2.0            # seconds – trivial invalidation

@lru_cache(maxsize=1024)
def _dir_cache(path):
    names = [e.name for e in os.scandir(path)]
    names.sort()
    return time.time(), names           # (timestamp, sorted list)

def _fresh_names(path):
    ts, names = _dir_cache(path)
    if time.time() - ts > CACHE_TTL:
        _dir_cache.cache_pop(path)
        ts, names = _dir_cache(path)
    return names

def complete_path(user_input, k=MAX_RESULTS):
    p = pathlib.Path(user_input).expanduser()
    dir_ = p.parent if p.name else p
    frag = p.name

    if not dir_.exists():
        return []

    names = _fresh_names(dir_)

    lo = bisect.bisect_left(names, frag)
    hi = bisect.bisect_left(names, frag + '\uffff')
    matches = names[lo:hi][:k]

    out = []
    for n in matches:
        full = dir_ / n
        out.append(str(full) + ('/' if full.is_dir() else ''))
    return out

The same idea drops into any language with a binary-search utility or a few lines of handwritten code.

Algorithm:  “Sorted-list + two-bound binary search”

Goal  
Given a partially typed path (for example “/usr/loc”), return the set of file-system entries in the *current directory* (“/usr”) whose names start with the fragment (“loc”), preferably capped to the first K suggestions.  
Because you only ever look inside one directory, the data you need to search is tiny (usually 1 – 10 000 names) and can be rebuilt on-demand.  The fastest and simplest approach is therefore:

1. Keep one cached, alphabetically **sorted array** of the directory’s names.  
2. For each query, use **two binary searches** to locate the contiguous slice that shares the requested prefix.  
3. Return that slice (or its first K elements).

The whole algorithm is O(N log N) to *build* when the directory changes, and O(log N + K) to *query*—well below human latency.

--------------------------------------------------------------------
Step-by-step instructions (language-agnostic)
--------------------------------------------------------------------

DATA STRUCTURES  
DS1  DirectoryCacheEntry  
      – names[]      : array of strings, sorted  
      – timestamp    : last time the list was refreshed  
      – (optional) dir_id/mtime : value from the OS that changes when the directory mutates  
      – (optional) history_stack: list of (lo, hi) slices for incremental typing

DS2  Cache  
      – map: directory-path  →  DirectoryCacheEntry  
      – max_size, eviction_policy (LRU is typical)

-----------------------------------------------------
FUNCTION  build_or_refresh(dir_path)
-----------------------------------------------------
Input : absolute directory path  
Output: DirectoryCacheEntry (fresh or reused)

1. If dir_path not in Cache:  go to step 4.  
2. entry ← Cache[dir_path]  
3. If entry is still fresh (timestamp younger than TTL *and* dir_id unchanged): return entry.  
4. Read directory entries with **Eio.Path.read_dir** (non-blocking).  Ignore “.” and “..”.  
5. names[] ← list of entry names; sort lexicographically.  
6. entry ← DirectoryCacheEntry{ names, now(), dir_id }.  
7. Cache[dir_path] ← entry  (insert or overwrite).  
8. return entry.

-----------------------------------------------------
FUNCTION  lower_bound(sorted_array, key)
-----------------------------------------------------
Classic binary search: smallest index i such that sorted_array[i] ≥ key.  
(Implement once; reuse everywhere.)

-----------------------------------------------------
FUNCTION  complete(user_input, K)
-----------------------------------------------------
Input : user_input (string typed so far), K (max suggestions)  
Output: list of at most K completion strings

1. Parse user_input into  
      dir_part   = text up to last “/” (or “\” on Windows; include “~” expansion)  
      fragment   = text after that separator (may be empty).  
   Examples:  
      “/home/al”  → dir_part = “/home”,  fragment = “al”  
      “/etc/”     → dir_part = “/etc/”, fragment = “”    (user just typed the slash)

2. If dir_part does not exist on disk: return [].

3. entry ← build_or_refresh(dir_part)  // see above  
   names[] ← entry.names

   • (Tiny optimisation)  If this is *not* the first keystroke and the previous
     query was in the same directory, retrieve the previous (lo, hi) slice from
     entry.history_stack to narrow the search range.  Otherwise search the
     entire array (0 … names.length).

4. Compute search bounds in the chosen range:  
      lo = lower_bound(names, fragment)  
      hi = lower_bound(names, fragment + U+FFFF)  
   (U+FFFF is a convenient character that is lexicographically after any legal
    file-name character.)

5. slice = names[lo : hi]  
   If slice.length > K, truncate to first K.

6. For each name in slice, build the presentation string:  
      completion = dir_part + name + ( “/” if that entry is a directory )  
      add completion to result list.

7. Push (lo, hi) onto entry.history_stack so the next keystroke can reuse it.  
   If the user deletes a character, pop one level from the stack.

8. Return result list.

--------------------------------------------------------------------
Incremental typing optimisation (optional but cheap)
--------------------------------------------------------------------
• When the user *adds* one character, the new slice is always contained inside the previous one, so re-run the two binary searches inside [lo_prev : hi_prev] instead of [0 : N].  
• When the user *deletes* one character, pop the previous slice off the stack instead of searching again.

--------------------------------------------------------------------
Cache invalidation strategies
--------------------------------------------------------------------
Pick one or combine several:

1. Time-to-live (TTL): mark each entry stale after, say, 2 seconds.  
2. Compare stored dir_id / mtime with the current value on each request.  
3. Subscribe to OS file-watcher (inotify, FSEvents, ReadDirectoryChangesW) and purge cache entry on change event.  
4. Size-based LRU eviction to cap total memory.

--------------------------------------------------------------------
Complexities
--------------------------------------------------------------------
• Build/refresh:  O(N log N) time, O(N) memory per directory  
• Query (cold):   O(log N + K) comparisons  
• Query (incremental typing): O(log Δ + K) where Δ ≤ previous slice width  
• Query (tiny dirs, say N ≤ 32): linear scan is often faster—micro-optimise only if measured.

--------------------------------------------------------------------
Edge cases to cover
--------------------------------------------------------------------
1. Empty fragment (“/etc/”): return *all* directory entries (maybe limited to K).  
2. No matches: empty list.  
3. Very deep paths or tilde expansion (“~/Docu”): normalise to an absolute path before lookup.  
4. Case-insensitive file systems: apply a uniform case transform both to names[] and to fragment, but keep the original spelling for display.  
5. Names containing newlines or unusual Unicode: treat as opaque strings; the U+FFFF sentinel trick still works.

--------------------------------------------------------------------
Why this algorithm is preferable here
--------------------------------------------------------------------
• Build cost is proportional only to the *current* directory size; no global index.  
• Query latency is dominated by two log₂(N) searches—microseconds for typical N.  
• Memory usage is minimal (just the cached arrays).  
• Works equally well for local disks, network shares, or virtual file systems.  
• Simple enough to re-implement in any language in ≈ 50 lines.

Follow these steps and you will have an autocomplete engine that is fast, memory-light, and always up to date with the file system, without any pre-computed global index.



### Ocaml Implementation
Below is a self-contained implementation of the “sorted-list + two-bound binary search” completion algorithm in OCaml, written in Jane-Street style and using Core.  It exposes a single entry‐point

    val Path_complete.complete : ?max_results:int -> fs:Eio.Path.t -> string -> string list

which turns the user’s partially typed path into at most max_results (default 25) candidate completions.  The code keeps a small, TTL-based, per-directory cache **built with Eio’s non-blocking FS API** so it can happily run inside the TUI’s cooperative scheduler.

--------------------------------------------------------------------
file: path_complete.ml
--------------------------------------------------------------------
```ocaml
open Core

(*------------------------------------------------------------------*)
(*  A single directory cache entry                                  *)
(*------------------------------------------------------------------*)
module Cache_entry = struct
  type t =
    { names     : string array       (* sorted lexicographically   *)
    ; timestamp : Time_ns.t          (* last refresh time          *)
    }

  let create (names : string array) : t =
    Array.sort names ~compare:String.compare;
    { names; timestamp = Time_ns.now () }
end

(*------------------------------------------------------------------*)
(*  Global (LRU-bounded) cache                                      *)
(*------------------------------------------------------------------*)
module Cache = struct
  let ttl      = Time_ns.Span.of_sec 2.
  let max_size = 1024

  (* Global LRU table: dir_path → cached & sorted names *)
  let table : Cache_entry.t String.Table.t = String.Table.create ()

  let%inline fresh entry =
    Time_ns.(Span.( < ) (diff (now ()) entry.Cache_entry.timestamp) ttl)

  let evict_if_necessary () =
    if Hashtbl.length table > max_size then (
      let by_age =
        Hashtbl.to_alist table
        |> List.sort ~compare:(fun (_, a) (_, b) ->
             Time_ns.compare a.Cache_entry.timestamp b.timestamp)
      in
      by_age
      |> List.take ~n:(List.length by_age / 2)
      |> List.iter ~f:(fun (path, _) -> Hashtbl.remove table path))

  (* Read and sort directory names using Eio's non-blocking FS API.            *)
  (* [dir] must come from the surrounding Eio environment (e.g. Eio.Path.cwd). *)
  let refresh_from_disk dir_path ~fs =
    let names =
      try
        Io.directory ~dir:fs dir_path |> Array.of_list
      with _ -> [||]
    in
    let entry = Cache_entry.create names in
    Hashtbl.set table ~key:dir_path ~data:entry;
    evict_if_necessary (); entry

  let get dir_path ~fs =
    match Hashtbl.find table dir_path with
    | Some entry when fresh entry -> entry
    | _ -> refresh_from_disk dir_path ~fs
end

(*------------------------------------------------------------------*)
(*  Lower-bound binary search on a string array                     *)
(*------------------------------------------------------------------*)
let lower_bound (arr : string array) ~(lo : int) ~(hi : int) (key : string)
  : int =
  let rec loop lo hi =
    if lo >= hi
    then lo
    else (
      let mid = (lo + hi) / 2 in
      if String.compare arr.(mid) key < 0
      then loop (mid + 1) hi
      else loop lo mid)
  in
  loop lo hi

(* Sentinel that compares after any valid path character *)
let hi_sentinel = "\255"

(*------------------------------------------------------------------*)
(*  Public API                                                      *)
(*------------------------------------------------------------------*)
let complete ?(max_results = 25) ~fs (user_input : string) : string list =
  (* Expand a leading “~/” to $HOME.  (Core doesn’t provide this.) *)
  let expand_user path =
    match String.chop_prefix path ~prefix:"~/" with
    | None -> if String.equal path "~" then Sys.home_directory () else path
    | Some rest -> Filename.concat (Sys.home_directory ()) rest
  in
  let user_input = expand_user user_input in

  (* Split into dir part and fragment *)
  let dir_part, fragment =
    match String.rsplit2 user_input ~on:'/' with
    | None -> ("."      , user_input)
    | Some ("" , frag) -> ("/"     , frag     ) (* input like "/frag" *)
    | Some (dir, frag) -> (dir     , frag     )
  in
  (* Bail out early if the directory does not exist *)
  (match Eio.Path.(exists fs dir_part) with
   | true -> ()
   | false -> [])
  |> function
  | exception _ -> []
  | () ->
    let { Cache_entry.names; _ } = Cache.get dir_part ~fs in
    let n = Array.length names in
    let lo = lower_bound names ~lo:0 ~hi:n fragment in
    let hi =
      let hi_token = fragment ^ hi_sentinel in
      lower_bound names ~lo:lo ~hi:n hi_token
    in
    let count = Int.min max_results (hi - lo) in
    List.init count ~f:(fun i -> names.(lo + i))
    |> List.filter_map ~f:(fun name ->
         let full = Filename.concat dir_part name in
         let suffix =
           match Eio.Path.(is_directory (fs / full)) with
           | true  -> "/"
           | false -> ""
         in
         Some (full ^ suffix))
```
