open Core

module Error = struct
  type t =
    | Input_limit
    | Output_limit
    | Work_limit
    | Node_limit
    | Depth_limit
    | Invalid_html
  [@@deriving equal, sexp_of]
end

module Limits = struct
  type t =
    { input : int
    ; output : int
    ; work : int
    ; nodes : int
    ; depth : int
    }

  let create ~max_input_bytes ~max_output_bytes ~max_work_steps ~max_nodes ~max_depth =
    if
      max_input_bytes < 0
      || max_output_bytes < 0
      || max_work_steps < 0
      || max_nodes < 0
      || max_depth < 0
    then Or_error.error_string "conversion limits must be nonnegative"
    else
      Ok
        { input = max_input_bytes
        ; output = max_output_bytes
        ; work = max_work_steps
        ; nodes = max_nodes
        ; depth = max_depth
        }
  ;;
end

module Accounting = struct
  type t =
    { input : int
    ; output : int
    ; work : int
    ; nodes : int
    ; depth : int
    }

  let input_bytes t = t.input
  let output_bytes t = t.output
  let work_steps t = t.work
  let nodes t = t.nodes
  let depth t = t.depth
end

type t =
  { limits : Limits.t option
  ; checkpoint : unit -> unit
  ; mutable since_poll : int
  ; mutable used_work : int
  ; mutable used_nodes : int
  ; mutable current_depth : int
  ; mutable used_depth : int
  ; mutable used_output : int
  }

exception Rejected of t * Error.t

let legacy () =
  { limits = None
  ; checkpoint = ignore
  ; since_poll = 0
  ; used_work = 0
  ; used_nodes = 0
  ; current_depth = 0
  ; used_depth = 0
  ; used_output = 0
  }
;;

let is_bounded t = Option.is_some t.limits
let poll t = t.checkpoint ()
let reject t e = raise (Rejected (t, e))

let add t used count limit error =
  if count < 0 || count > limit - used then reject t error;
  used + count
;;

let work t count =
  match t.limits with
  | None -> ()
  | Some limits ->
    t.used_work <- add t t.used_work count limits.work Work_limit;
    if count >= 4096 - t.since_poll
    then (
      t.since_poll <- 0;
      poll t)
    else t.since_poll <- t.since_poll + count
;;

let nodes t count =
  match t.limits with
  | None -> ()
  | Some limits ->
    t.used_nodes <- add t t.used_nodes count limits.nodes Node_limit;
    work t count
;;

let node t = nodes t 1

let depth t count =
  match t.limits with
  | None -> ()
  | Some limits ->
    if count > limits.depth then reject t Depth_limit;
    t.used_depth <- Int.max t.used_depth count
;;

let output_bytes t count =
  match t.limits with
  | None -> ()
  | Some limits ->
    if count > limits.output then reject t Output_limit;
    t.used_output <- count
;;

let reject_html t = reject t Invalid_html

let run limits ~input_bytes ~checkpoint ~f =
  if input_bytes > limits.Limits.input
  then Error Error.Input_limit
  else
    let exception
      (* Each invocation transports only its own borrowed callback failures past
       known parser handlers, including identically named exceptions. *)
      Callback_raised of exn * Stdlib.Printexc.raw_backtrace
    in
    let guarded_checkpoint () =
      try checkpoint () with
      | exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        raise (Callback_raised (exn, backtrace))
    in
    let t =
      { limits = Some limits
      ; checkpoint = guarded_checkpoint
      ; since_poll = 0
      ; used_work = 0
      ; used_nodes = 0
      ; current_depth = 0
      ; used_depth = 0
      ; used_output = 0
      }
    in
    try
      poll t;
      let value = f t in
      poll t;
      let accounting : Accounting.t =
        { input = input_bytes
        ; output = t.used_output
        ; work = t.used_work
        ; nodes = t.used_nodes
        ; depth = t.used_depth
        }
      in
      Ok (value, accounting)
    with
    | Rejected (owner, e) when phys_equal owner t -> Error e
    | Callback_raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let scan t s =
  let rec loop offset =
    if offset < String.length s
    then (
      let count = Int.min 4096 (String.length s - offset) in
      work t count;
      loop (offset + count))
  in
  loop 0
;;

let map t xs ~f =
  List.map xs ~f:(fun x ->
    nodes t 4;
    f x)
;;

let mapi t xs ~f =
  List.mapi xs ~f:(fun i x ->
    nodes t 4;
    f i x)
;;

let filter t xs ~f =
  List.filter xs ~f:(fun x ->
    nodes t 4;
    f x)
;;

let filter_map t xs ~f =
  List.filter_map xs ~f:(fun x ->
    nodes t 4;
    f x)
;;

let concat_map t xs ~f =
  List.concat_map xs ~f:(fun x ->
    nodes t 4;
    let ys = f x in
    List.iter ys ~f:(fun _ -> nodes t 2);
    ys)
;;

let init t n ~f =
  List.init n ~f:(fun i ->
    node t;
    f i)
;;

let concat t ~sep xs =
  scan t sep;
  List.iteri xs ~f:(fun i s ->
    if i > 0 then scan t sep;
    scan t s);
  String.concat ~sep xs
;;

let strip t s =
  scan t s;
  String.strip s
;;

let lstrip t s =
  scan t s;
  String.lstrip s
;;

let rstrip t s =
  scan t s;
  String.rstrip s
;;

let lowercase t s =
  scan t s;
  String.lowercase s
;;

let split t s ~on =
  scan t s;
  nodes t (String.length s + 1);
  String.split s ~on
;;

let split_lines t s =
  scan t s;
  nodes t (String.length s + 1);
  String.split_lines s
;;

let soup_nodes t sequence =
  Soup.fold
    (fun acc n ->
       nodes t 2;
       n :: acc)
    []
    sequence
  |> List.rev
;;

let soup_attribute t name element =
  Soup.fold_attributes
    (fun found candidate value ->
       work t 1;
       scan t candidate;
       match found with
       | Some _ -> found
       | None ->
         if String.equal name candidate
         then (
           node t;
           Some value)
         else None)
    None
    element
;;

let soup_delete t node =
  (* Soup.delete filters the actual parent list, including document parents.
     Siblings exclude the removed node and identify every copied list cell. *)
  Soup.iter (fun _ -> nodes t 1) (Soup.siblings node);
  work t 1;
  Soup.delete node
;;

let soup_texts t root =
  let one acc n =
    work t 1;
    if Soup.is_text n
    then (
      match Soup.leaf_text n with
      | None -> acc
      | Some s ->
        scan t s;
        nodes t 2;
        s :: acc)
    else acc
  in
  if Soup.is_text root
  then one [] root |> List.rev
  else Soup.fold one [] (Soup.descendants root) |> List.rev
;;

let reject_output t = reject t Output_limit

let enter t =
  if is_bounded t
  then (
    work t 1;
    depth t (t.current_depth + 1);
    t.current_depth <- t.current_depth + 1)
;;

let leave t = if is_bounded t then t.current_depth <- t.current_depth - 1

let is_substring t s ~substring =
  scan t s;
  String.is_substring s ~substring
;;

let iter t xs ~f =
  List.iter xs ~f:(fun x ->
    work t 1;
    f x)
;;

let for_all t xs ~f =
  List.for_all xs ~f:(fun x ->
    work t 1;
    f x)
;;

let exists t xs ~f =
  List.exists xs ~f:(fun x ->
    work t 1;
    f x)
;;

let find t xs ~f =
  List.find xs ~f:(fun x ->
    work t 1;
    f x)
;;

let find_map t xs ~f =
  List.find_map xs ~f:(fun x ->
    work t 1;
    f x)
;;

let count t xs ~f =
  List.count xs ~f:(fun x ->
    work t 1;
    f x)
;;

let length t xs =
  List.fold xs ~init:0 ~f:(fun n _ ->
    work t 1;
    n + 1)
;;

let rev t xs =
  iter t xs ~f:(fun _ -> node t);
  List.rev xs
;;

let rev_append t xs ys =
  iter t xs ~f:(fun _ -> node t);
  List.rev_append xs ys
;;

let append t xs ys =
  iter t xs ~f:(fun _ -> node t);
  List.append xs ys
;;

let zip_exn t xs ys =
  iter t xs ~f:(fun _ -> nodes t 2);
  List.zip_exn xs ys
;;
