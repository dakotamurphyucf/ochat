open Core
module Local = Chatml_local_source
module Locator = Chatml_local_locator

module Limits = struct
  type t =
    { locator_bytes : int
    ; locator_components : int
    ; anchors : int
    ; retained_roots : int
    ; work_steps : int
    }

  let locator_bytes t = t.locator_bytes
  let locator_components t = t.locator_components
  let anchors t = t.anchors
  let retained_roots t = t.retained_roots
  let work_steps t = t.work_steps

  let create
        ~max_locator_bytes
        ~max_locator_components
        ~max_anchors
        ~max_retained_roots
        ~max_work_steps
    =
    if
      max_locator_bytes <= 0
      || max_locator_components < 0
      || max_anchors < 0
      || max_retained_roots < 0
      || max_work_steps < 0
    then Or_error.error_string "invalid local root limit"
    else
      Ok
        { locator_bytes = max_locator_bytes
        ; locator_components = max_locator_components
        ; anchors = max_anchors
        ; retained_roots = max_retained_roots
        ; work_steps = max_work_steps
        }
  ;;
end

module Error = struct
  type code =
    | Invalid_locator
    | Invalid_anchor
    | Duplicate_anchor
    | Outside_grants
    | Foreign_cursor
    | Busy
    | Closed
    | Limit_exceeded
    | Accounting_overflow
    | Filesystem
  [@@deriving equal, sexp_of]

  type t =
    { code : code
    ; filesystem : Local.Error.t option
    ; locator_prefix : string option
    }

  let code t = t.code
  let filesystem t = t.filesystem
  let locator_prefix t = t.locator_prefix

  let create ?locator ?filesystem code =
    { code
    ; filesystem
    ; locator_prefix = Option.map locator ~f:(fun s -> String.prefix s 256)
    }
  ;;
end

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ()
;;

let failure ?locator code = Error (Error.create ?locator code)

let sum left right =
  if left < 0 || right < 0 || right > Int.max_value - left
  then failure Accounting_overflow
  else Ok (left + right)
;;

let product left right =
  if left < 0 || right < 0 || (left > 0 && right > Int.max_value / left)
  then failure Accounting_overflow
  else Ok (left * right)
;;

module Anchor = struct
  type source =
    | Directory of [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
    | Retained of Local.t

  type t =
    { source : source
    ; prefix : Locator.t
    ; max_path_components : int
    }

  let admit source ~host_prefix ~max_path_components ~limits ~checkpoint =
    if
      max_path_components <= 0
      || String.is_empty host_prefix
      || not (Char.equal host_prefix.[0] '/')
    then failure ~locator:host_prefix Invalid_anchor
    else
      Locator.normalize
        host_prefix
        ~base:None
        ~max_bytes:limits.Limits.locator_bytes
        ~max_components:limits.locator_components
        ~checkpoint:(fun () -> poll checkpoint)
      |> Result.map_error ~f:(fun _ -> Error.create ~locator:host_prefix Invalid_anchor)
      |> Result.map ~f:(fun prefix -> { source; prefix; max_path_components })
  ;;

  let create root ~host_prefix ~max_path_components ~limits ~checkpoint =
    let _, path = root in
    if not (String.is_empty path)
    then failure ~locator:host_prefix Invalid_anchor
    else admit (Directory root) ~host_prefix ~max_path_components ~limits ~checkpoint
  ;;

  let of_local owner ~host_prefix ~limits ~checkpoint =
    admit
      (Retained owner)
      ~host_prefix
      ~max_path_components:(Local.path_component_limit owner)
      ~limits
      ~checkpoint
  ;;

  let may_override t ~original_prefix ~locator ~limits ~checkpoint =
    let normalize spelling =
      Locator.normalize
        spelling
        ~base:None
        ~max_bytes:limits.Limits.locator_bytes
        ~max_components:limits.locator_components
        ~checkpoint
      |> Result.map_error ~f:(function
        | Locator.Invalid -> Error.create Invalid_locator
        | Limit_exceeded -> Error.create Limit_exceeded)
    in
    let open Result.Let_syntax in
    let%bind original = normalize original_prefix in
    let%bind target = normalize locator in
    checkpoint ();
    Ok
      (Locator.component_count t.prefix >= Locator.component_count original
       && Option.is_some (Locator.suffix target ~prefix:t.prefix ~checkpoint))
  ;;
end

type lifecycle =
  { mutable live : bool
  ; mutable busy : bool
  }

module Cursor = struct
  type t =
    { lifecycle : lifecycle
    ; location : Locator.t
    ; owner : Local.t
    ; identity : Local.Directory_identity.t
    }

  let owner t = t.owner
  let identity t = t.identity

  (* The resolver admits exactly one cursor per normalized route in its cache.
     Physical identity here deliberately denotes that affine route, not files. *)
  let equal_route = phys_equal
end

module Accounting = struct
  type t =
    { retained_roots : int
    ; work_steps : int
    }

  let retained_roots t = t.retained_roots
  let work_steps t = t.work_steps
end

type t =
  { limits : Limits.t
  ; lifecycle : lifecycle
  ; sw : Eio.Switch.t
  ; mutable anchors : Cursor.t list
  ; mutable routes : Cursor.t String.Map.t
  ; mutable retained_roots : int
  ; mutable work_steps : int
  ; mutable anchor_bytes : int
  }

let accounting (t : t) =
  Accounting.{ retained_roots = t.retained_roots; work_steps = t.work_steps }
;;

let reserve t units =
  if units < 0
  then failure Accounting_overflow
  else if units > t.limits.work_steps - t.work_steps
  then failure Limit_exceeded
  else (
    t.work_steps <- t.work_steps + units;
    Ok ())
;;

let reserve_scaled t bytes factor extra =
  let open Result.Let_syntax in
  let%bind units = product bytes factor in
  let%bind units = sum units extra in
  reserve t units
;;

let filesystem ?locator result =
  Result.map_error result ~f:(fun filesystem ->
    Error.create ?locator ~filesystem Filesystem)
;;

let root_slot t =
  if t.retained_roots >= t.limits.retained_roots then failure Limit_exceeded else Ok ()
;;

let admit_anchors t anchors ~checkpoint =
  let open Result.Let_syntax in
  let%map _, _ =
    List.fold_result
      anchors
      ~init:(0, String.Set.empty)
      ~f:(fun (count, prefixes) anchor ->
        if count >= t.limits.anchors
        then failure Limit_exceeded
        else (
          let bytes = Locator.byte_length anchor.Anchor.prefix in
          if
            bytes > t.limits.locator_bytes
            || Locator.component_count anchor.prefix > t.limits.locator_components
          then failure Limit_exceeded
          else (
            let%bind cost = sum bytes 1 in
            let%bind factor = product count 4 in
            let%bind factor = sum factor 8 in
            let%bind cost = product cost factor in
            let%bind () = reserve t cost in
            poll checkpoint;
            let key = Locator.key anchor.prefix in
            if Set.mem prefixes key
            then failure ~locator:key Duplicate_anchor
            else (
              let%map total = sum t.anchor_bytes bytes in
              t.anchor_bytes <- total;
              count + 1, Set.add prefixes key))))
  in
  ()
;;

let acquire_anchors t anchors ~checkpoint =
  let open Result.Let_syntax in
  let%map reversed =
    List.fold_result anchors ~init:[] ~f:(fun acquired (anchor : Anchor.t) ->
      let%bind () = root_slot t in
      let%bind () = reserve t 16 in
      poll checkpoint;
      let%bind owner =
        (match anchor.Anchor.source with
         | Directory root ->
           Local.open_root
             root
             ~sw:t.sw
             ~max_path_components:anchor.max_path_components
             ~checkpoint
         | Retained parent ->
           let directory =
             Local.Directory_path.create [] ~max_bytes:0 ~max_components:0 ~checkpoint
             |> Or_error.ok_exn
           in
           Local.open_subroot parent ~sw:t.sw ~directory ~checkpoint)
        |> filesystem ~locator:(Locator.key anchor.prefix)
      in
      t.retained_roots <- t.retained_roots + 1;
      let%bind identity = Local.identity owner ~checkpoint |> filesystem in
      let cursor =
        { Cursor.lifecycle = t.lifecycle; location = anchor.prefix; owner; identity }
      in
      t.routes <- Map.set t.routes ~key:(Locator.key anchor.prefix) ~data:cursor;
      Ok (cursor :: acquired))
  in
  t.anchors <- List.rev reversed
;;

let with_anchors anchors ~limits ~checkpoint ~f =
  Eio.Switch.run (fun sw ->
    let lifecycle = { live = true; busy = false } in
    let t =
      { limits
      ; lifecycle
      ; sw
      ; anchors = []
      ; routes = String.Map.empty
      ; retained_roots = 0
      ; work_steps = 0
      ; anchor_bytes = 0
      }
    in
    Fun.protect
      ~finally:(fun () -> lifecycle.live <- false)
      (fun () ->
         match admit_anchors t anchors ~checkpoint with
         | Error error -> Error (`Roots error)
         | Ok () ->
           (match acquire_anchors t anchors ~checkpoint with
            | Error error -> Error (`Roots error)
            | Ok () -> Result.map_error (f t) ~f:(fun error -> `Callback error))))
;;

let choose_anchor t location ~checkpoint =
  List.fold t.anchors ~init:None ~f:(fun selected cursor ->
    poll checkpoint;
    match
      Locator.suffix location ~prefix:cursor.Cursor.location ~checkpoint:(fun () ->
        poll checkpoint)
    with
    | None -> selected
    | Some suffix ->
      (match selected with
       | None -> Some (cursor, suffix)
       | Some (previous, _) ->
         if
           Locator.component_count cursor.location
           > Locator.component_count previous.Cursor.location
         then Some (cursor, suffix)
         else selected))
;;

let lookup_work t location =
  let rec height count depth =
    if count = 0 then depth else height (count / 2) (depth + 1)
  in
  let open Result.Let_syntax in
  let%bind anchor_cost = product t.anchor_bytes 4 in
  let%bind key_cost =
    product (Locator.byte_length location) ((4 * height (Map.length t.routes) 0) + 8)
  in
  let%bind units = sum anchor_cost key_cost in
  let%bind visits = sum (List.length t.anchors) 1 in
  let%bind units = sum units visits in
  reserve t units
;;

let resolve_open t ~base ~locator ~checkpoint =
  let open Result.Let_syntax in
  let%bind base =
    match base with
    | None -> Ok None
    | Some cursor ->
      if not (phys_equal cursor.Cursor.lifecycle t.lifecycle)
      then failure ~locator Foreign_cursor
      else Ok (Some cursor.location)
  in
  if String.length locator > t.limits.locator_bytes
  then failure ~locator Limit_exceeded
  else (
    let%bind bytes =
      sum
        (String.length locator)
        (Option.value_map base ~default:0 ~f:Locator.byte_length)
    in
    let%bind () = reserve_scaled t bytes 8 8 in
    let%bind location =
      Locator.normalize
        locator
        ~base
        ~max_bytes:t.limits.locator_bytes
        ~max_components:t.limits.locator_components
        ~checkpoint:(fun () -> poll checkpoint)
      |> Result.map_error ~f:(function
        | Invalid -> Error.create ~locator Invalid_locator
        | Limit_exceeded -> Error.create ~locator Limit_exceeded)
    in
    let%bind () = lookup_work t location in
    match choose_anchor t location ~checkpoint with
    | None -> failure ~locator Outside_grants
    | Some (anchor, suffix) ->
      (* Nested grants take precedence even if a route was previously acquired
         through a broader one. Exact canonical target identifies the selected
         longest-prefix grant in this immutable admitted anchor set. *)
      (match Map.find t.routes (Locator.key location) with
       | Some cursor -> Ok cursor
       | None ->
         let%bind () = root_slot t in
         let%bind depth_cost = product (List.length suffix) 8 in
         let%bind overhead = sum depth_cost 16 in
         let%bind () = reserve_scaled t (Locator.byte_length location) 4 overhead in
         let%bind directory =
           Local.Directory_path.create
             suffix
             ~max_bytes:t.limits.locator_bytes
             ~max_components:t.limits.locator_components
             ~checkpoint:(fun () -> poll checkpoint)
           |> Result.map_error ~f:(fun _ -> Error.create ~locator Invalid_locator)
         in
         let%bind owner =
           Local.open_subroot anchor.owner ~sw:t.sw ~directory ~checkpoint
           |> filesystem ~locator
         in
         t.retained_roots <- t.retained_roots + 1;
         let%bind identity = Local.identity owner ~checkpoint |> filesystem ~locator in
         let cursor = { Cursor.lifecycle = t.lifecycle; location; owner; identity } in
         t.routes <- Map.set t.routes ~key:(Locator.key location) ~data:cursor;
         Ok cursor))
;;

let resolve t ~base ~locator ~checkpoint =
  if not t.lifecycle.live
  then failure ~locator Closed
  else if t.lifecycle.busy
  then failure ~locator Busy
  else (
    t.lifecycle.busy <- true;
    Fun.protect
      ~finally:(fun () -> t.lifecycle.busy <- false)
      (fun () -> resolve_open t ~base ~locator ~checkpoint))
;;
