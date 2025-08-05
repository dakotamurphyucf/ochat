(** OCaml port of OpenAI’s reference *apply_patch* helper.

    This implementation understands the “Ochat diff” patch syntax
    discussed in the
    {{:https://cookbook.openai.com/examples/gpt4-1_prompting_guide#reference-implementation-apply_patchpy}
    prompting-guide} and can apply multi-file edits against an
    arbitrary workspace.  The module is intentionally IO-free – all
    reads and writes happen through the callback functions supplied to
    {!val:process_patch}.  See {!file:apply_patch.mli} for the public
    interface and {!file:apply_patch.doc.md} for an extended
    discussion and examples.

    The rest of this file contains the mechanics: Unicode
    canonisation, fuzzy context matching, and a small patch/commit
    DSL.  These helpers stay undocumented on purpose – they are
    implementation details and may change without notice. *)

open Core

(* Re-export structured error module for external consumers who open
   [Apply_patch].  This avoids the need to depend on the internal
   wrapper details of the library. *)
module Apply_patch_error = Apply_patch_error

let error_to_string = Apply_patch_error.to_string

[@@@warning "-20-26-32-69"]

(* ──────────────────────────────────────────────────────────────────────────
		   Constants (same strings as in the TypeScript reference)
		   ──────────────────────────────────────────────────────────────────────────*)
let patch_prefix = "*** Begin Patch\n"
let patch_suffix = "\n*** End Patch"
let add_file_prefix = "*** Add File: "
let delete_file_prefix = "*** Delete File: "
let update_file_prefix = "*** Update File: "
let move_file_to_prefix = "*** Move to: "
let end_of_file_prefix = "*** End of File"
let hunk_add_line_prefix = '+'

(* ──────────────────────────────────────────────────────────────────────────
		   Domain types
		   ──────────────────────────────────────────────────────────────────────────*)
type action_kind =
  | Add
  | Delete
  | Update

type file_change =
  { kind : action_kind
  ; old_text : string option
  ; new_text : string option
  ; move_path : string option
  }

type commit = { changes : file_change String.Map.t }

(* Patch-level structures *)
type chunk =
  { mutable orig_index : int
  ; del_lines : string list
  ; ins_lines : string list
  }

type patch_action =
  { kind : action_kind
  ; new_file : string option
  ; chunks : chunk list
  ; move_path : string option
  }

type patch = { actions : patch_action String.Map.t }

(* Use the new structured error type *)
exception Diff_error = Apply_patch_error.Diff_error

(* ──────────────────────────────────────────────────────────────────────────
		   1.  Unicode canonicalisation helpers
		   ──────────────────────────────────────────────────────────────────────────*)
module Canon = struct
  (* Punctuation look-alikes → ASCII *)
  let table =
    let groups =
      [ [ 0x002D; 0x2010; 0x2011; 0x2012; 0x2013; 0x2014; 0x2212 ], 0x002D (* dashes *)
      ; [ 0x0022; 0x201C; 0x201D; 0x201E; 0x00AB; 0x00BB ], 0x0022 (* quotes *)
      ; [ 0x0027; 0x2018; 0x2019; 0x201B ], 0x0027 (* apostr *)
      ; [ 0x00A0; 0x202F ], 0x0020 (* space  *)
      ]
    in
    groups
    |> List.concat_map ~f:(fun (lst, dst) ->
      List.map lst ~f:(fun cp -> Uchar.of_scalar_exn cp, Uchar.of_scalar_exn dst))
    |> Map.of_alist_exn (module Uchar)
  ;;

  let substitute u = Map.find table u |> Option.value ~default:u

  let normalise (s : string) : string =
    let s = Uunf_string.normalize_utf_8 `NFC s in
    let buf = Buffer.create (String.length s) in
    let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
    let rec loop () =
      match Uutf.decode dec with
      | `Uchar u ->
        Uutf.Buffer.add_utf_8 buf (substitute u);
        (* fixed helper *)
        loop ()
      | `End -> ()
      | `Await -> loop ()
      | `Malformed _ -> loop ()
    in
    loop ();
    Buffer.contents buf
  ;;
end

(* ──────────────────────────────────────────────────────────────────────────
		   2.  Small helpers
		   ──────────────────────────────────────────────────────────────────────────*)
let slc xs i len = if len <= 0 then [] else List.sub xs ~pos:i ~len
let join = String.concat ~sep:"\n"

(* ──────────────────────────────────────────────────────────────────────────
		   3.  Context search  (fuzzing logic)
		   ──────────────────────────────────────────────────────────────────────────*)
let find_context_core ~lines ~context ~start : int * int =
  let canon = Canon.normalise in
  let ctx_len = List.length context in
  if ctx_len = 0
  then start, 0
  else (
    let test prep fuzz =
      let ctx_canon = canon (join (List.map context ~f:(fun s -> prep s))) in
      let rec scan i =
        if i > List.length lines - ctx_len
        then None
        else (
          let seg = canon (join (List.map (slc lines i ctx_len) ~f:(fun s -> prep s))) in
          if String.equal seg ctx_canon then Some (i, fuzz) else scan (i + 1))
      in
      scan
    in
    let rec try_passes = function
      | [] -> -1, 0
      | (prep, fuzz) :: tl ->
        (match test prep fuzz start with
         | Some res -> res
         | None -> try_passes tl)
    in
    try_passes
      [ Fn.id, 0; (fun str -> String.rstrip str), 1; (fun str -> String.strip str), 100 ])
;;

let find_context ~lines ~context ~start ~eof =
  if eof
  then (
    let tail_start = Int.max 0 (List.length lines - List.length context) in
    match find_context_core ~lines ~context ~start:tail_start with
    | -1, _ ->
      let i, f = find_context_core ~lines ~context ~start in
      i, f + 10_000
    | res -> res)
  else find_context_core ~lines ~context ~start
;;

(* ──────────────────────────────────────────────────────────────────────────
		   4.  Chunk extractor (peek_next_section)
		   ──────────────────────────────────────────────────────────────────────────*)
let peek_next_section (lines : string array) ~(idx0 : int)
  : string list * chunk list * int * bool
  =
  let idx = ref idx0 in
  let old = ref [] in
  let del_buf = ref [] in
  let ins_buf = ref [] in
  let chunks = ref [] in
  let mode = ref `Keep in
  let push_chunk () =
    if not (List.is_empty !del_buf && List.is_empty !ins_buf)
    then (
      let ch =
        { orig_index = List.length !old - List.length !del_buf
        ; del_lines = List.rev !del_buf
        ; ins_lines = List.rev !ins_buf
        }
      in
      chunks := ch :: !chunks;
      del_buf := [];
      ins_buf := [] (* reset correctly *))
  in
  let boundary s =
    List.exists
      [ "@@"
      ; patch_suffix
      ; update_file_prefix
      ; delete_file_prefix
      ; add_file_prefix
      ; end_of_file_prefix
      ]
      ~f:(fun p -> String.is_prefix s ~prefix:(String.strip p))
  in
  while !idx < Array.length lines && not (boundary lines.(!idx)) do
    let s = lines.(!idx) in
    if String.is_prefix s ~prefix:"***"
    then raise (Diff_error (Apply_patch_error.Syntax_error { line = !idx; text = s }));
    incr idx;
    let prev = !mode in
    let s, m =
      match String.get s 0 with
      | c when Char.equal c hunk_add_line_prefix -> s, `Add
      | '-' -> s, `Del
      | ' ' -> s, `Keep
      | _ -> " " ^ s, `Keep
    in
    mode := m;
    (* --- handle the case of missing space in front of a line -------- *)
    (* tolerate missing space *)
    (match !mode, prev with
     | `Keep, `Del | `Keep, `Add -> push_chunk ()
     | _ -> ());
    let line =
      match !mode with
      | `Keep -> String.drop_prefix s 1
      | _ -> String.drop_prefix s 1
    in
    match !mode with
    | `Del ->
      del_buf := line :: !del_buf;
      old := line :: !old
    | `Add -> ins_buf := line :: !ins_buf
    | `Keep -> old := line :: !old
  done;
  push_chunk ();
  (* If we're positioned on a chunk delimiter "@@" advance the index so that
     callers always observe forward progress.  This also treats the trailing
     "@@" that closes a chunk as belonging to that chunk rather than the next
     one. *)
  if !idx < Array.length lines && String.equal lines.(!idx) "@@" then incr idx;
  let eof = !idx < Array.length lines && String.equal lines.(!idx) end_of_file_prefix in
  if eof then incr idx;
  List.rev !old, List.rev !chunks, !idx, eof
;;

(* ──────────────────────────────────────────────────────────────────────────
		   5.  Patch parser
		   ──────────────────────────────────────────────────────────────────────────*)
module Parser = struct
  type t =
    { current : string String.Map.t
    ; lines : string array
    ; mutable i : int
    ; mutable fuzz : int
    ; mutable patch : patch
    }

  let make ~current ~lines =
    { current; lines; i = 0; fuzz = 0; patch = { actions = String.Map.empty } }
  ;;

  let at_end ?prefixes t =
    t.i >= Array.length t.lines
    || Option.value_map
         prefixes
         ~default:false
         ~f:
           (List.exists ~f:(fun p ->
              String.is_prefix t.lines.(t.i) ~prefix:(String.strip p)))
  ;;

  let read ?(pref = "") t =
    if t.i >= Array.length t.lines
    then ""
    else (
      let l = t.lines.(t.i) in
      if String.is_prefix l ~prefix:pref
      then (
        t.i <- t.i + 1;
        String.drop_prefix l (String.length pref))
      else "")
  ;;

  (* -- UPDATE file ----------------------------------------------------- *)
  let parse_update_file t ~(path : string) ~(orig_text : string) : patch_action =
    let file_lines = String.split_lines orig_text in
    let idx = ref 0 in
    let chunks_acc = ref [] in
    while
      not
        (at_end
           t
           ~prefixes:
             [ patch_suffix
             ; update_file_prefix
             ; delete_file_prefix
             ; add_file_prefix
             ; end_of_file_prefix
             ])
    do
      let def_str = read ~pref:"@@ " t in
      let section =
        if String.equal t.lines.(t.i) "@@"
        then (
          t.i <- t.i + 1;
          true)
        else false
      in
      if String.(is_empty def_str) && (not section) && !idx <> 0
      then
        raise
          (Diff_error
             (Apply_patch_error.Syntax_error { line = t.i; text = t.lines.(t.i) }));
      (* --- search for def_str in original (canonical) ---------------- *)
      if not (String.is_empty (String.strip def_str))
      then (
        let canon = Canon.normalise in
        let rec search i f =
          if i >= List.length file_lines
          then None
          else if String.equal (f (canon (List.nth_exn file_lines i))) (f (canon def_str))
          then Some (i + 1)
          else search (i + 1) f
        in
        match search !idx Fn.id with
        | Some j -> idx := j
        | None ->
          (match search !idx String.strip with
           | Some j ->
             idx := j;
             t.fuzz <- t.fuzz + 1
           | None -> ()));
      let ctx, chunks, next_i, eof = peek_next_section t.lines ~idx0:t.i in
      (* [peek_next_section] must always advance; otherwise we would loop
         forever.  The helper now guarantees this property, so hitting this
         branch indicates a logic bug or a malformed patch. *)
      if Int.(next_i <= t.i)
      then
        raise
          (Diff_error
             (Apply_patch_error.Syntax_error
                { line = t.i; text = "Empty or malformed section in patch" }));
      let new_idx, fuzz_score =
        find_context ~lines:file_lines ~context:ctx ~start:!idx ~eof
      in
      if new_idx = -1
      then (
        let ctx_txt = join ctx in
        let msg =
          if eof
          then sprintf "Invalid EOF Context %d:\n%s" !idx ctx_txt
          else sprintf "Invalid Context %d:\n%s" !idx ctx_txt
        in
        let expected_ctx = ctx in
        (* Build a small snippet (±3 lines) around the position where the
           context was expected.  The reference index is [!idx]. *)
        let snippet =
          let before = 3 in
          let after = 3 in
          let ctx_len = List.length ctx in
          let start_line = Int.max 0 !idx in
          let window_start = Int.max 0 (start_line - before) in
          let window_end =
            Int.min (List.length file_lines) (start_line + ctx_len + after)
          in
          slc file_lines window_start (window_end - window_start)
        in
        raise
          (Diff_error
             (Apply_patch_error.Context_mismatch
                { path; expected = expected_ctx; fuzz = fuzz_score; snippet })));
      t.fuzz <- t.fuzz + fuzz_score;
      List.iter chunks ~f:(fun ch -> ch.orig_index <- ch.orig_index + new_idx);
      chunks_acc := !chunks_acc @ chunks;
      idx := new_idx + List.length ctx;
      t.i <- next_i
    done;
    { kind = Update; new_file = None; chunks = !chunks_acc; move_path = None }
  ;;

  (* -- ADD file -------------------------------------------------------- *)
  let parse_add_file t : patch_action =
    let rec collect acc =
      if
        at_end
          t
          ~prefixes:
            [ patch_suffix; update_file_prefix; delete_file_prefix; add_file_prefix ]
      then List.rev acc
      else (
        let s = read t in
        if Char.(s.[0] <> hunk_add_line_prefix)
        then raise (Diff_error (Apply_patch_error.Syntax_error { line = t.i; text = s }))
        else collect (String.drop_prefix s 1 :: acc))
    in
    let lines = collect [] in
    { kind = Add
    ; new_file = Some (String.concat_lines lines)
    ; chunks = []
    ; move_path = None
    }
  ;;

  (* -- top-level ------------------------------------------------------- *)
  let parse t =
    while not (at_end t ~prefixes:[ patch_suffix ]) do
      match read ~pref:update_file_prefix t with
      | path when not (String.is_empty path) ->
        if not (Map.mem t.current path)
        then
          raise (Diff_error (Apply_patch_error.Missing_file { path; action = `Update }));
        let move_to = read ~pref:move_file_to_prefix t in
        let upd = parse_update_file t ~path ~orig_text:(Map.find_exn t.current path) in
        let upd =
          { upd with
            move_path = (if String.is_empty move_to then None else Some move_to)
          }
        in
        t.patch <- { actions = Map.set t.patch.actions ~key:path ~data:upd }
      | _ ->
        (match read ~pref:delete_file_prefix t with
         | path when not (String.is_empty path) ->
           if not (Map.mem t.current path)
           then
             raise
               (Diff_error (Apply_patch_error.Missing_file { path; action = `Delete }));
           t.patch
           <- { actions =
                  Map.set
                    t.patch.actions
                    ~key:path
                    ~data:
                      { kind = Delete; new_file = None; chunks = []; move_path = None }
              }
         | _ ->
           (match read ~pref:add_file_prefix t with
            | path when not (String.is_empty path) ->
              if Map.mem t.current path
              then raise (Diff_error (Apply_patch_error.File_exists { path }));
              let add = parse_add_file t in
              t.patch <- { actions = Map.set t.patch.actions ~key:path ~data:add }
            | _ ->
              raise
                (Diff_error
                   (Apply_patch_error.Syntax_error { line = t.i; text = t.lines.(t.i) }))))
    done;
    if not (String.is_prefix t.lines.(t.i) ~prefix:(String.strip patch_suffix))
    then
      raise
        (Diff_error
           (Apply_patch_error.Syntax_error { line = t.i; text = "Missing End Patch" }));
    t.i <- t.i + 1
  ;;
end

(* ──────────────────────────────────────────────────────────────────────────
		   6.  High-level helpers
		   ──────────────────────────────────────────────────────────────────────────*)
let text_to_patch (text : string) (orig : string String.Map.t) : patch * int =
  let lines = String.rstrip text |> String.split_lines |> Array.of_list in
  if
    Array.length lines < 2
    || (not (String.is_prefix lines.(0) ~prefix:(String.strip patch_prefix)))
    || not (String.equal lines.(Array.length lines - 1) (String.strip patch_suffix))
  then
    raise
      (Diff_error
         (Apply_patch_error.Syntax_error { line = 0; text = "Invalid patch text" }));
  (* sequence separator *)
  let p = Parser.make ~current:orig ~lines in
  p.i <- 1;
  Parser.parse p;
  p.patch, p.fuzz
;;

let identify_files_needed text =
  String.rstrip text
  |> String.split_lines
  |> List.filter_map ~f:(fun l ->
    if String.is_prefix l ~prefix:update_file_prefix
    then Some (String.drop_prefix l (String.length update_file_prefix))
    else if String.is_prefix l ~prefix:delete_file_prefix
    then Some (String.drop_prefix l (String.length delete_file_prefix))
    else None)
  |> Set.of_list (module String)
  |> Set.to_list
;;

let identify_files_added text =
  String.rstrip text
  |> String.split_lines
  |> List.filter_map ~f:(fun l ->
    if String.is_prefix l ~prefix:add_file_prefix
    then Some (String.drop_prefix l (String.length add_file_prefix))
    else None)
;;

let list_slice xs ~pos ~len = if len <= 0 then [] else List.sub xs ~pos ~len

let updated_file ~orig_text (act : patch_action) ~path =
  let orig = String.split_lines orig_text in
  let dest, idx =
    List.fold act.chunks ~init:([], 0) ~f:(fun (acc, i0) ch ->
      if ch.orig_index > List.length orig
      then
        raise
          (Diff_error
             (Apply_patch_error.Bounds_error
                { path; index = ch.orig_index; len = List.length orig }));
      if i0 > ch.orig_index
      then
        raise
          (Diff_error
             (Apply_patch_error.Bounds_error { path; index = i0; len = ch.orig_index }));
      let keep = slc orig i0 (ch.orig_index - i0) in
      acc @ keep @ ch.ins_lines, ch.orig_index + List.length ch.del_lines)
  in
  let rest = slc orig idx (List.length orig - idx) in
  String.concat_lines (dest @ rest)
;;

let patch_to_commit (p : patch) (orig : string String.Map.t) : commit =
  let changes =
    Map.fold p.actions ~init:String.Map.empty ~f:(fun ~key:path ~data acc ->
      match data.kind with
      | Delete ->
        Map.set
          acc
          ~key:path
          ~data:
            { kind = Delete
            ; old_text = Map.find orig path
            ; new_text = None
            ; move_path = None
            }
      | Add ->
        Map.set
          acc
          ~key:path
          ~data:
            { kind = Add; old_text = None; new_text = data.new_file; move_path = None }
      | Update ->
        let new_text = updated_file ~orig_text:(Map.find_exn orig path) data ~path in
        Map.set
          acc
          ~key:path
          ~data:
            { kind = Update
            ; old_text = Map.find orig path
            ; new_text = Some new_text
            ; move_path = data.move_path
            })
  in
  { changes }
;;

let load_files paths ~open_fn =
  List.fold paths ~init:String.Map.empty ~f:(fun acc p ->
    try Map.set acc ~key:p ~data:(open_fn p) with
    | _ ->
      raise (Diff_error (Apply_patch_error.Missing_file { path = p; action = `Update })))
;;

let apply_commit (c : commit) ~write_fn ~remove_fn =
  Map.iteri c.changes ~f:(fun ~key:path ~data ->
    match data.kind with
    | Delete -> remove_fn path
    | Add -> write_fn path (Option.value_exn data.new_text)
    | Update ->
      (match data.move_path with
       | Some dst ->
         write_fn dst (Option.value_exn data.new_text);
         remove_fn path
       | None -> write_fn path (Option.value_exn data.new_text)))
;;

(* Success snippet generation *)

let rec common_prefix_len l1 l2 idx =
  match l1, l2 with
  | h1 :: t1, h2 :: t2 when String.equal h1 h2 -> common_prefix_len t1 t2 (idx + 1)
  | _ -> idx
;;

let rec common_suffix_len l1 l2 count =
  match l1, l2 with
  | [], _ | _, [] -> count
  | _ ->
    (match List.last l1, List.last l2 with
     | Some h1, Some h2 when String.equal h1 h2 ->
       common_suffix_len (List.drop_last_exn l1) (List.drop_last_exn l2) (count + 1)
     | _ -> count)
;;

let generate_snippets (commit : commit) ~(_orig : string String.Map.t)
  : (string * string) list
  =
  let context = 0 in
  Map.fold commit.changes ~init:[] ~f:(fun ~key:path ~data acc ->
    let snippet =
      match data.kind with
      | Add ->
        let lines = Option.value_exn data.new_text |> String.split_lines in
        lines
        |> List.mapi ~f:(fun idx line -> Printf.sprintf "%4d | +%s" (idx + 1) line)
        |> String.concat ~sep:"\n"
      | Delete ->
        let lines = Option.value_exn data.old_text |> String.split_lines in
        lines
        |> List.mapi ~f:(fun idx line -> Printf.sprintf "%4d | -%s" (idx + 1) line)
        |> String.concat ~sep:"\n"
      | Update ->
        let old_lines = Option.value_exn data.old_text |> String.split_lines in
        let new_lines = Option.value_exn data.new_text |> String.split_lines in
        let prefix_len = common_prefix_len old_lines new_lines 0 in
        let suffix_len =
          common_suffix_len
            (List.drop old_lines prefix_len)
            (List.drop new_lines prefix_len)
            0
        in
        let first_changed = prefix_len in
        let last_changed_new = List.length new_lines - suffix_len - 1 in
        let start_line = Int.max 0 (first_changed - context) in
        let end_line = Int.min (List.length new_lines - 1) (last_changed_new + context) in
        List.init (end_line - start_line + 1) ~f:(fun i -> i + start_line)
        |> List.map ~f:(fun idx ->
          let line = List.nth_exn new_lines idx in
          let prefix_char =
            if idx >= first_changed && idx <= last_changed_new then '+' else ' '
          in
          Printf.sprintf "%4d | %c%s" (idx + 1) prefix_char line)
        |> String.concat ~sep:"\n"
    in
    (path, snippet) :: acc)
  |> List.rev
;;

(** [process_patch ~text ~open_fn ~write_fn ~remove_fn] interprets and
    applies the patch [text].  All file-system interactions are
    delegated to the three callbacks:

    – [open_fn path]   must return the current contents of [path] when
      the patch references the file.

    – [write_fn path contents] receives the complete new contents for
      every added or updated file as well as the destination of a
      rename.

    – [remove_fn path] is invoked for deleted files and for the source
      of a rename.

    The function validates that [text] starts with {!val:patch_prefix}
    and ends with {!val:patch_suffix}.  On success it returns
    {['Done!']}.  If the patch is malformed or inconsistent with the
    workspace a {!exception:Diff_error} is raised. *)

let process_patch
      ~(text : string)
      ~(open_fn : string -> string)
      ~(write_fn : string -> string -> unit)
      ~(remove_fn : string -> unit)
  : string * (string * string) list
  =
  if not (String.is_prefix text ~prefix:patch_prefix)
  then
    raise
      (Diff_error
         (Apply_patch_error.Syntax_error
            { line = 0; text = "Patch must start with *** Begin Patch\\n" }));
  let needed = identify_files_needed text in
  let orig = load_files needed ~open_fn in
  let patch, _ = text_to_patch text orig in
  let commit = patch_to_commit patch orig in
  let snippets = generate_snippets commit ~_orig:orig in
  apply_commit commit ~write_fn ~remove_fn;
  "Done!", snippets
;;
