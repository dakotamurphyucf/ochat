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

(* Debug tracing (opt-in) ------------------------------------------------------ *)

let debug_enabled : bool ref = ref false
let set_debug (enabled : bool) : unit = debug_enabled := enabled

let debug_log fmt =
  if !debug_enabled
  then Printf.ksprintf (fun s -> prerr_endline ("[apply_patch] " ^ s)) fmt
  else Printf.ksprintf (fun _ -> ()) fmt
;;

(* ──────────────────────────────────────────────────────────────────────────
		   Constants (same strings as in the TypeScript reference)
		   ──────────────────────────────────────────────────────────────────────────*)
let patch_prefix = "*** Begin Patch\n"
let patch_suffix = "\n*** End Patch"
let end_patch_line = "*** End Patch"
let add_file_prefix = "*** Add File: "
let delete_file_prefix = "*** Delete File: "
let update_file_prefix = "*** Update File: "
let move_file_to_prefix = "*** Move to: "
let end_of_file_prefix = "*** End of File"
let section_break_line = "***"
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
      ; end_patch_line
      ; section_break_line
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
      (* Lenient continuation heuristic:
         If a model wraps a line and drops the leading diff prefix, treat it as a
         continuation of the previous add/delete run when applicable. Otherwise
         treat it as context. *)
      | _ ->
        let m =
          match prev with
          | `Add -> `Add
          | `Del -> `Del
          | `Keep -> `Keep
        in
        " " ^ s, m
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
  (* Do NOT consume a bare "@@" delimiter here.
     The update-file loop expects "@@" to be present so it can delimit hunks.
     Consuming it inside [peek_next_section] causes patches with single "@@" separators
     between hunks to desync, requiring a double "@@" to work. *)
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
             [ end_patch_line
             ; section_break_line
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
      (* Lenient no-progress recovery:
         If [peek_next_section] does not advance, consume one line so we never hang.
         This can happen with malformed patches (e.g. unexpected terminators or
         wrapped lines) and we prefer recovery over infinite loops. *)
      if Int.(next_i <= t.i)
      then
        if t.i < Array.length t.lines
        then t.i <- t.i + 1
        else
          raise
            (Diff_error
               (Apply_patch_error.Syntax_error
                  { line = t.i; text = "Empty or malformed section in patch" }));
      let new_idx, fuzz_score =
        find_context ~lines:file_lines ~context:ctx ~start:!idx ~eof
      in
      if new_idx = -1
      then (
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
            [ end_patch_line; update_file_prefix; delete_file_prefix; add_file_prefix ]
      then List.rev acc
      else (
        let s = read t in
        (* Harden against empty lines to avoid [s.[0]] bounds errors. *)
        if String.is_empty s
        then raise (Diff_error (Apply_patch_error.Syntax_error { line = t.i; text = s }))
        else if Char.(s.[0] <> hunk_add_line_prefix)
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
    while not (at_end t ~prefixes:[ end_patch_line ]) do
      (* Accept optional "***" section break lines between sections. *)
      if String.equal t.lines.(t.i) section_break_line
      then t.i <- t.i + 1
      else (
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
                     (Apply_patch_error.Syntax_error { line = t.i; text = t.lines.(t.i) })))))
    done;
    if not (String.is_prefix t.lines.(t.i) ~prefix:end_patch_line)
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

type diff_op =
  | Context_line of
      { old_line_number : int
      ; new_line_number : int
      ; text : string
      }
  | Added_line of
      { new_line_number : int
      ; text : string
      }
  | Deleted_line of
      { old_line_number : int
      ; text : string
      }

type occurrence =
  { count : int
  ; index : int
  }

let format_line_number = function
  | Some line_number -> Printf.sprintf "%4d" line_number
  | None -> "   -"
;;

let format_diff_line = function
  | Context_line { old_line_number; new_line_number; text } ->
    Printf.sprintf
      "o:%s n:%s |  %s"
      (format_line_number (Some old_line_number))
      (format_line_number (Some new_line_number))
      text
  | Added_line { new_line_number; text } ->
    Printf.sprintf
      "o:%s n:%s | +%s"
      (format_line_number None)
      (format_line_number (Some new_line_number))
      text
  | Deleted_line { old_line_number; text } ->
    Printf.sprintf
      "o:%s n:%s | -%s"
      (format_line_number (Some old_line_number))
      (format_line_number None)
      text
;;

let diff_table old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop =
  let old_count = old_stop - old_start in
  let new_count = new_stop - new_start in
  let table = Array.make_matrix ~dimx:(old_count + 1) ~dimy:(new_count + 1) 0 in
  for old_idx = old_count - 1 downto 0 do
    for new_idx = new_count - 1 downto 0 do
      table.(old_idx).(new_idx)
      <- (if String.equal old_lines.(old_start + old_idx) new_lines.(new_start + new_idx)
          then 1 + table.(old_idx + 1).(new_idx + 1)
          else Int.max table.(old_idx + 1).(new_idx) table.(old_idx).(new_idx + 1))
    done
  done;
  table
;;

let rec lcs_diff_ops
          old_lines
          new_lines
          table
          ~old_start
          ~old_stop
          ~new_start
          ~new_stop
          ~old_idx
          ~new_idx
          acc
  =
  let old_count = old_stop - old_start in
  let new_count = new_stop - new_start in
  if old_idx = old_count && new_idx = new_count
  then List.rev acc
  else if old_idx = old_count
  then
    lcs_diff_ops
      old_lines
      new_lines
      table
      ~old_start
      ~old_stop
      ~new_start
      ~new_stop
      ~old_idx
      ~new_idx:(new_idx + 1)
      (Added_line
         { new_line_number = new_start + new_idx + 1
         ; text = new_lines.(new_start + new_idx)
         }
       :: acc)
  else if new_idx = new_count
  then
    lcs_diff_ops
      old_lines
      new_lines
      table
      ~old_start
      ~old_stop
      ~new_start
      ~new_stop
      ~old_idx:(old_idx + 1)
      ~new_idx
      (Deleted_line
         { old_line_number = old_start + old_idx + 1
         ; text = old_lines.(old_start + old_idx)
         }
       :: acc)
  else if String.equal old_lines.(old_start + old_idx) new_lines.(new_start + new_idx)
  then
    lcs_diff_ops
      old_lines
      new_lines
      table
      ~old_start
      ~old_stop
      ~new_start
      ~new_stop
      ~old_idx:(old_idx + 1)
      ~new_idx:(new_idx + 1)
      (Context_line
         { old_line_number = old_start + old_idx + 1
         ; new_line_number = new_start + new_idx + 1
         ; text = new_lines.(new_start + new_idx)
         }
       :: acc)
  else if table.(old_idx + 1).(new_idx) >= table.(old_idx).(new_idx + 1)
  then
    lcs_diff_ops
      old_lines
      new_lines
      table
      ~old_start
      ~old_stop
      ~new_start
      ~new_stop
      ~old_idx:(old_idx + 1)
      ~new_idx
      (Deleted_line
         { old_line_number = old_start + old_idx + 1
         ; text = old_lines.(old_start + old_idx)
         }
       :: acc)
  else
    lcs_diff_ops
      old_lines
      new_lines
      table
      ~old_start
      ~old_stop
      ~new_start
      ~new_stop
      ~old_idx
      ~new_idx:(new_idx + 1)
      (Added_line
         { new_line_number = new_start + new_idx + 1
         ; text = new_lines.(new_start + new_idx)
         }
       :: acc)
;;

let lcs_diff_range old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop =
  let table = diff_table old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop in
  lcs_diff_ops
    old_lines
    new_lines
    table
    ~old_start
    ~old_stop
    ~new_start
    ~new_stop
    ~old_idx:0
    ~new_idx:0
    []
;;

let addition_preview_limit = 20
let replacement_marker_line = "o:   - n:   - | ~ replaced by ~"

let separator_line unchanged_count =
  let suffix = if unchanged_count = 1 then "line" else "lines" in
  Printf.sprintf
    "o:%s n:%s | ... %d unchanged %s ..."
    "   -"
    "   -"
    unchanged_count
    suffix
;;

let hunk_label_line index total = Printf.sprintf "[hunk %d/%d]" index total
;;

let count_description count noun =
  let suffix = if count = 1 then noun else noun ^ "s" in
  Printf.sprintf "%d %s" count suffix
;;

let max_scope_anchors = 2

type anchor_kind =
  | Container
  | Declaration
  | Section
;;

let container_anchor_prefixes =
  [ "module "
  ; "class "
  ; "interface "
  ; "struct "
  ; "trait "
  ; "impl "
  ; "namespace "
  ; "package "
  ; "protocol "
  ; "enum "
  ; "record "
  ]
;;

let declaration_anchor_prefixes =
  [ "let "
  ; "let%"
  ; "and "
  ; "type "
  ; "exception "
  ; "val "
  ; "def "
  ; "fn "
  ; "func "
  ; "function "
  ; "method "
  ; "sub "
  ; "proc "
  ]
;;

let section_anchor_prefixes = [ "# "; "## "; "### "; "#### " ]

let anchor_modifiers =
  [ "export "
  ; "default "
  ; "public "
  ; "private "
  ; "protected "
  ; "static "
  ; "async "
  ; "abstract "
  ; "final "
  ; "virtual "
  ; "inline "
  ; "extern "
  ; "constexpr "
  ; "pub "
  ; "override "
  ; "sealed "
  ; "internal "
  ; "open "
  ]
;;

let control_flow_prefixes =
  [ "if "
  ; "for "
  ; "while "
  ; "switch "
  ; "catch "
  ; "else"
  ; "do "
  ; "try"
  ; "match "
  ; "with "
  ; "when "
  ]
;;

let starts_with_any prefixes text =
  List.exists prefixes ~f:(fun prefix -> String.is_prefix text ~prefix)
;;

let count_leading_whitespace line =
  String.take_while line ~f:Char.is_whitespace |> String.length
;;

let rec strip_anchor_modifiers text =
  match List.find anchor_modifiers ~f:(fun prefix -> String.is_prefix text ~prefix) with
  | None -> text
  | Some prefix ->
    String.drop_prefix text (String.length prefix) |> String.lstrip |> strip_anchor_modifiers
;;

let is_function_like_signature text =
  let stripped = String.rstrip text in
  String.is_substring stripped ~substring:"("
  && String.is_substring stripped ~substring:")"
  && (String.is_suffix stripped ~suffix:"{"
      || String.is_suffix stripped ~suffix:":"
      || String.is_suffix stripped ~suffix:"=>")
  && not (starts_with_any control_flow_prefixes stripped)
;;

let classify_anchor_line line =
  let stripped = String.strip line in
  let normalized = strip_anchor_modifiers stripped in
  if String.is_empty stripped
  then None
  else if starts_with_any section_anchor_prefixes stripped
  then Some (Section, stripped)
  else if starts_with_any container_anchor_prefixes normalized
  then Some (Container, stripped)
  else if starts_with_any declaration_anchor_prefixes normalized || is_function_like_signature normalized
  then Some (Declaration, stripped)
  else None
;;

let rec collect_scope_anchors lines index anchors current_indent =
  if index < 0 || List.length anchors = max_scope_anchors
  then anchors
  else (
    match classify_anchor_line lines.(index) with
    | None -> collect_scope_anchors lines (index - 1) anchors current_indent
    | Some (kind, text) ->
      let indent = count_leading_whitespace lines.(index) in
      let should_take =
        List.is_empty anchors
        || indent < current_indent
        ||
        match kind with
        | Container | Section -> true
        | Declaration -> false
      in
      if should_take && not (List.mem anchors text ~equal:String.equal)
      then
        collect_scope_anchors
          lines
          (index - 1)
          (text :: anchors)
          (Int.min current_indent indent)
      else collect_scope_anchors lines (index - 1) anchors current_indent)
;;

let anchor_lines texts =
  List.mapi texts ~f:(fun idx text -> Printf.sprintf "@ scope[%d]: %s" (idx + 1) text)
;;

let line_number_of_diff_op = function
  | Context_line { new_line_number; _ } -> new_line_number
  | Added_line { new_line_number; _ } -> new_line_number
  | Deleted_line { old_line_number; _ } -> old_line_number
;;

let is_change = function
  | Context_line _ -> false
  | Added_line _ | Deleted_line _ -> true
;;

let update_occurrence occurrences line index =
  let data =
    match Map.find occurrences line with
    | None -> { count = 1; index }
    | Some data -> { count = data.count + 1; index = data.index }
  in
  Map.set occurrences ~key:line ~data
;;

let occurrences lines ~start ~stop =
  let occurrences = ref String.Map.empty in
  for index = start to stop - 1 do
    occurrences := update_occurrence !occurrences lines.(index) index
  done;
  !occurrences
;;

let unique_matching_pairs old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop =
  let old_occurrences = occurrences old_lines ~start:old_start ~stop:old_stop in
  let new_occurrences = occurrences new_lines ~start:new_start ~stop:new_stop in
  Map.fold old_occurrences ~init:[] ~f:(fun ~key ~data acc ->
    match data, Map.find new_occurrences key with
    | { count = 1; index = old_index }, Some { count = 1; index = new_index } ->
      (old_index, new_index) :: acc
    | _ -> acc)
  |> List.sort ~compare:(fun (old_index1, _) (old_index2, _) ->
    Int.compare old_index1 old_index2)
;;

let longest_increasing_pairs pairs =
  let pairs = Array.of_list pairs in
  let pair_count = Array.length pairs in
  if pair_count = 0
  then []
  else (
    let lengths = Array.create ~len:pair_count 1 in
    let previous = Array.create ~len:pair_count None in
    for index = 0 to pair_count - 1 do
      for previous_index = 0 to index - 1 do
        let _, previous_new_index = pairs.(previous_index) in
        let _, new_index = pairs.(index) in
        if
          previous_new_index < new_index && lengths.(previous_index) + 1 > lengths.(index)
        then (
          lengths.(index) <- lengths.(previous_index) + 1;
          previous.(index) <- Some previous_index)
      done
    done;
    let best_index =
      Array.foldi lengths ~init:0 ~f:(fun index best_index length ->
        if length > lengths.(best_index) then index else best_index)
    in
    let rec backtrack index acc =
      match previous.(index) with
      | None -> pairs.(index) :: acc
      | Some previous_index -> backtrack previous_index (pairs.(index) :: acc)
    in
    backtrack best_index [])
;;

let common_prefix_length old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop =
  let limit = Int.min (old_stop - old_start) (new_stop - new_start) in
  let rec loop offset =
    if offset = limit
    then offset
    else if String.equal old_lines.(old_start + offset) new_lines.(new_start + offset)
    then loop (offset + 1)
    else offset
  in
  loop 0
;;

let common_suffix_length old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop =
  let limit = Int.min (old_stop - old_start) (new_stop - new_start) in
  let rec loop offset =
    if offset = limit
    then offset
    else if
      String.equal old_lines.(old_stop - offset - 1) new_lines.(new_stop - offset - 1)
    then loop (offset + 1)
    else offset
  in
  loop 0
;;

let context_range old_lines new_lines ~old_start ~new_start ~length =
  List.init length ~f:(fun offset ->
    Context_line
      { old_line_number = old_start + offset + 1
      ; new_line_number = new_start + offset + 1
      ; text = new_lines.(new_start + offset)
      })
;;

let added_range new_lines ~new_start ~new_stop =
  List.init (new_stop - new_start) ~f:(fun offset ->
    Added_line
      { new_line_number = new_start + offset + 1; text = new_lines.(new_start + offset) })
;;

let deleted_range old_lines ~old_start ~old_stop =
  List.init (old_stop - old_start) ~f:(fun offset ->
    Deleted_line
      { old_line_number = old_start + offset + 1; text = old_lines.(old_start + offset) })
;;

let rec patience_diff_range old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop =
  if old_start = old_stop
  then added_range new_lines ~new_start ~new_stop
  else if new_start = new_stop
  then deleted_range old_lines ~old_start ~old_stop
  else (
    let prefix_length =
      common_prefix_length old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop
    in
    let old_start = old_start + prefix_length in
    let new_start = new_start + prefix_length in
    let suffix_length =
      common_suffix_length old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop
    in
    let old_stop = old_stop - suffix_length in
    let new_stop = new_stop - suffix_length in
    let prefix =
      context_range
        old_lines
        new_lines
        ~old_start:(old_start - prefix_length)
        ~new_start:(new_start - prefix_length)
        ~length:prefix_length
    in
    let suffix =
      context_range
        old_lines
        new_lines
        ~old_start:old_stop
        ~new_start:new_stop
        ~length:suffix_length
    in
    let middle =
      if old_start = old_stop
      then added_range new_lines ~new_start ~new_stop
      else if new_start = new_stop
      then deleted_range old_lines ~old_start ~old_stop
      else (
        let anchors =
          unique_matching_pairs
            old_lines
            new_lines
            ~old_start
            ~old_stop
            ~new_start
            ~new_stop
          |> longest_increasing_pairs
        in
        if List.is_empty anchors
        then lcs_diff_range old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop
        else
          patience_diff_with_anchors
            old_lines
            new_lines
            ~old_stop
            ~new_stop
            anchors
            ~old_start
            ~new_start)
    in
    List.concat [ prefix; middle; suffix ])

and patience_diff_with_anchors
      old_lines
      new_lines
      ~old_stop
      ~new_stop
      anchors
      ~old_start
      ~new_start
  =
  match anchors with
  | [] ->
    patience_diff_range old_lines new_lines ~old_start ~old_stop ~new_start ~new_stop
  | (old_anchor, new_anchor) :: rest ->
    let before =
      patience_diff_range
        old_lines
        new_lines
        ~old_start
        ~old_stop:old_anchor
        ~new_start
        ~new_stop:new_anchor
    in
    let anchor =
      Context_line
        { old_line_number = old_anchor + 1
        ; new_line_number = new_anchor + 1
        ; text = new_lines.(new_anchor)
        }
    in
    let after =
      patience_diff_with_anchors
        old_lines
        new_lines
        ~old_stop
        ~new_stop
        rest
        ~old_start:(old_anchor + 1)
        ~new_start:(new_anchor + 1)
    in
    List.concat [ before; [ anchor ]; after ]
;;

let hunk_ranges ops ~context_lines =
  let changed_indexes =
    Array.foldi ops ~init:[] ~f:(fun idx acc op ->
      if is_change op then idx :: acc else acc)
    |> List.rev
  in
  let last_index = Array.length ops - 1 in
  let rec loop ranges current = function
    | [] ->
      (match current with
       | Some current -> List.rev (current :: ranges)
       | None -> List.rev ranges)
    | idx :: rest ->
      let next_range =
        Int.max 0 (idx - context_lines), Int.min last_index (idx + context_lines)
      in
      (match current with
       | None -> loop ranges (Some next_range) rest
       | Some (start_idx, end_idx) ->
         let next_start, next_end = next_range in
         if next_start <= end_idx + 1
         then loop ranges (Some (start_idx, Int.max end_idx next_end)) rest
         else loop ((start_idx, end_idx) :: ranges) (Some next_range) rest)
  in
  loop [] None changed_indexes
;;

let unchanged_count_between ops ~prev_end ~next_start =
  let count = ref 0 in
  for idx = prev_end + 1 to next_start - 1 do
    match ops.(idx) with
    | Context_line _ -> count := !count + 1
    | Added_line _ | Deleted_line _ -> ()
  done;
  !count
;;

let range_ops ops (start_idx, end_idx) =
  List.init (end_idx - start_idx + 1) ~f:(fun offset -> ops.(start_idx + offset))
;;

let range_anchor ops (start_idx, end_idx) ~old_lines ~new_lines =
  let rec loop index =
    if index > end_idx
    then []
    else (
      match ops.(index) with
      | Context_line _ -> loop (index + 1)
      | Added_line { new_line_number; _ } ->
        collect_scope_anchors new_lines (new_line_number - 1) [] Int.max_value
      | Deleted_line { old_line_number; _ } ->
        collect_scope_anchors old_lines (old_line_number - 1) [] Int.max_value)
  in
  loop start_idx
;;

let render_hunk_lines lines =
  let rec loop saw_deletion = function
    | [] -> []
    | (Added_line _ as line) :: rest when saw_deletion ->
      replacement_marker_line :: format_diff_line line :: loop false rest
    | (Deleted_line _ as line) :: rest -> format_diff_line line :: loop true rest
    | line :: rest -> format_diff_line line :: loop false rest
  in
  loop false lines
;;

let render_range ops range ~old_lines ~new_lines =
  let anchor = range_anchor ops range ~old_lines ~new_lines |> anchor_lines in
  let lines = range_ops ops range |> render_hunk_lines in
  List.append anchor lines
;;

let update_summary_line (data : file_change) lines ~hunk_count =
  let insertions =
    List.count lines ~f:(function
      | Added_line _ -> true
      | _ -> false)
  in
  let deletions =
    List.count lines ~f:(function
      | Deleted_line _ -> true
      | _ -> false)
  in
  let label = if Option.is_some data.move_path then "Move" else "Update" in
  Printf.sprintf
    "%s of file successful. %s, %s, %s."
    label
    (count_description insertions "insertion")
    (count_description deletions "deletion")
    (count_description hunk_count "hunk")
;;

let format_changed_lines (data : file_change) lines ~old_lines ~new_lines =
  let ops = Array.of_list lines in
  let ranges = hunk_ranges ops ~context_lines:1 in
  let summary = update_summary_line data lines ~hunk_count:(List.length ranges) in
  let total_hunks = List.length ranges in
  let rec loop previous_range hunk_index acc = function
    | [] -> List.rev acc
    | range :: rest ->
      let acc =
        match previous_range with
        | Some (_, prev_end) ->
          let next_start, _ = range in
          let omitted = unchanged_count_between ops ~prev_end ~next_start in
          if omitted > 0 then separator_line omitted :: acc else acc
        | None -> acc
      in
      let rendered_range = render_range ops range ~old_lines ~new_lines in
      let acc =
        List.rev_append
          (hunk_label_line hunk_index total_hunks :: rendered_range)
          acc
      in
      loop (Some range) (hunk_index + 1) acc rest
  in
  summary :: loop None 1 [] ranges |> fun lines -> String.concat ~sep:"\n" lines
;;

let add_snippet data =
  let lines = Option.value_exn data.new_text |> String.split_lines in
  if List.length lines > addition_preview_limit
  then "Addition of file successful."
  else (
    let preview =
      List.mapi lines ~f:(fun idx line ->
        format_diff_line (Added_line { new_line_number = idx + 1; text = line }))
      |> String.concat ~sep:"\n"
    in
    String.concat ~sep:"\n" [ "Addition of file successful."; preview ])
;;

let update_snippet data =
  let old_lines = Option.value_exn data.old_text |> String.split_lines |> Array.of_list in
  let new_lines = Option.value_exn data.new_text |> String.split_lines |> Array.of_list in
  let diff_ops =
    patience_diff_range
      old_lines
      new_lines
      ~old_start:0
      ~old_stop:(Array.length old_lines)
      ~new_start:0
      ~new_stop:(Array.length new_lines)
  in
  match List.exists diff_ops ~f:is_change with
  | false ->
    if Option.is_some data.move_path
    then "Move of file successful."
    else "Update of file successful."
  | true -> format_changed_lines data diff_ops ~old_lines ~new_lines
;;

let generate_snippets (commit : commit) ~(_orig : string String.Map.t)
  : (string * string) list
  =
  Map.fold commit.changes ~init:[] ~f:(fun ~key:path ~data acc ->
    let snippet =
      match data.kind with
      | Add -> add_snippet data
      | Delete -> "Deletion of file successful."
      | Update -> update_snippet data
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
