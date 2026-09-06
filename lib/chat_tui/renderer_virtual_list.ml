open Core

module Geometry = struct
  type t =
    { mutable heights : int array
    ; mutable prefix : int array
    ; mutable exact : bool array
    ; mutable generation : int
    }

  module Snapshot = struct
    type t =
      { heights : int array
      ; prefix : int array
      ; exact : bool array
      ; generation : int
      }

    let generation t = t.generation
    let length t = Array.length t.heights
    let heights t = Array.copy t.heights
    let exactness t = Array.copy t.exact
    let prefix t = Array.copy t.prefix
    let total_height t = t.prefix.(Array.length t.prefix - 1)

    let height t ~index =
      if index < 0 || index >= length t then None else Some t.heights.(index)
    ;;

    let is_exact t ~index = index >= 0 && index < length t && t.exact.(index)

    let item_start t ~index =
      if index < 0 || index >= length t then None else Some t.prefix.(index)
    ;;

    let range_is_exact t ~first ~past =
      if first < 0 || past < first || past > length t
      then invalid_arg "Renderer_virtual_list.Geometry.Snapshot.range_is_exact";
      let rec loop index = index >= past || (t.exact.(index) && loop (index + 1)) in
      loop first
    ;;

    let exact_prefix_length t =
      let rec loop index =
        if index >= length t || not t.exact.(index) then index else loop (index + 1)
      in
      loop 0
    ;;
  end

  type batch_result =
    | Applied
    | Stale_generation
  [@@deriving sexp_of]

  let create () = { heights = [||]; prefix = [| 0 |]; exact = [||]; generation = 0 }

  let clear t =
    t.heights <- [||];
    t.prefix <- [| 0 |];
    t.exact <- [||];
    t.generation <- t.generation + 1
  ;;

  let generation t = t.generation
  let length t = Array.length t.heights
  let heights t = t.heights
  let prefix t = t.prefix
  let total_height t = t.prefix.(Array.length t.prefix - 1)

  let shape_matches t ~length =
    Array.length t.heights = length && Array.length t.prefix = length + 1
  ;;

  let add_height_exn total height =
    if height < 0 || total > Int.max_value - height
    then invalid_arg "Renderer_virtual_list.Geometry: invalid cumulative height";
    total + height
  ;;

  let prefix_of_heights heights =
    let prefix = Array.create ~len:(Array.length heights + 1) 0 in
    Array.iteri heights ~f:(fun index height ->
      prefix.(index + 1) <- add_height_exn prefix.(index) height);
    prefix
  ;;

  let snapshot t =
    { Snapshot.heights = Array.copy t.heights
    ; prefix = Array.copy t.prefix
    ; exact = Array.copy t.exact
    ; generation = t.generation
    }
  ;;

  let rebuild t ~length ~height_at_index =
    let heights =
      Array.init length ~f:(fun index ->
        let height = height_at_index index in
        if height < 0
        then invalid_arg "Renderer_virtual_list.Geometry.rebuild: negative height";
        height)
    in
    let prefix = prefix_of_heights heights in
    t.heights <- heights;
    t.prefix <- prefix;
    t.exact <- Array.create ~len:length true;
    t.generation <- t.generation + 1
  ;;

  let initialize_estimated t ~length ~estimated_height_at_index =
    let heights =
      Array.init length ~f:(fun index ->
        let height = estimated_height_at_index index in
        if height < 0
        then
          invalid_arg
            "Renderer_virtual_list.Geometry.initialize_estimated: negative height";
        height)
    in
    let prefix = prefix_of_heights heights in
    t.heights <- heights;
    t.prefix <- prefix;
    t.exact <- Array.create ~len:length false;
    t.generation <- t.generation + 1
  ;;

  let extend_estimated t ~length:new_length ~estimated_height_at_index =
    let old_length = length t in
    if new_length < old_length
    then invalid_arg "Renderer_virtual_list.Geometry.extend_estimated: shrinking";
    if new_length > old_length
    then (
      let heights =
        Array.init new_length ~f:(fun index ->
          if index < old_length
          then t.heights.(index)
          else (
            let height = estimated_height_at_index index in
            if height < 0
            then
              invalid_arg
                "Renderer_virtual_list.Geometry.extend_estimated: negative height";
            height))
      in
      let exact =
        Array.init new_length ~f:(fun index -> index < old_length && t.exact.(index))
      in
      let prefix = prefix_of_heights heights in
      t.heights <- heights;
      t.prefix <- prefix;
      t.exact <- exact;
      t.generation <- t.generation + 1)
  ;;

  let reconcile_prefix t ~preserved_length ~length:new_length ~estimated_height_at_index =
    let old_length = length t in
    let preserved_length =
      Int.max 0 (Int.min preserved_length (Int.min old_length new_length))
    in
    let heights =
      Array.init new_length ~f:(fun index ->
        if index < preserved_length
        then t.heights.(index)
        else (
          let height = estimated_height_at_index index in
          if height < 0
          then
            invalid_arg "Renderer_virtual_list.Geometry.reconcile_prefix: negative height";
          height))
    in
    let exact =
      Array.init new_length ~f:(fun index -> index < preserved_length && t.exact.(index))
    in
    let prefix = prefix_of_heights heights in
    t.heights <- heights;
    t.prefix <- prefix;
    t.exact <- exact;
    t.generation <- t.generation + 1
  ;;

  let has_valid_prefix ~heights ~prefix =
    Array.length prefix = Array.length heights + 1
    && Int.equal prefix.(0) 0
    && Array.for_all heights ~f:(fun height -> height >= 0)
    && Array.for_alli heights ~f:(fun index height ->
      prefix.(index) <= Int.max_value - height
      && Int.equal prefix.(index + 1) (prefix.(index) + height))
  ;;

  let replace t ~heights ~prefix =
    if not (has_valid_prefix ~heights ~prefix)
    then invalid_arg "Renderer_virtual_list.Geometry.replace: invalid prefix sums";
    t.heights <- heights;
    t.prefix <- prefix;
    t.exact <- Array.create ~len:(Array.length heights) true;
    t.generation <- t.generation + 1
  ;;

  let replace_partial t ~heights ~prefix ~exact =
    if
      (not (has_valid_prefix ~heights ~prefix))
      || not (Int.equal (Array.length exact) (Array.length heights))
    then invalid_arg "Renderer_virtual_list.Geometry.replace_partial: invalid geometry";
    t.heights <- heights;
    t.prefix <- prefix;
    t.exact <- exact;
    t.generation <- t.generation + 1
  ;;

  let update_height t ~index ~height =
    if index < 0 || index >= length t
    then invalid_arg "Renderer_virtual_list.Geometry.update_height: invalid index";
    if height < 0
    then invalid_arg "Renderer_virtual_list.Geometry.update_height: negative height";
    let delta = height - t.heights.(index) in
    if not (Int.equal delta 0)
    then (
      t.heights.(index) <- height;
      for suffix = index + 1 to Array.length t.heights do
        t.prefix.(suffix) <- t.prefix.(suffix) + delta
      done)
  ;;

  let apply_exact_batch t ~expected_generation ~start_index ~heights:updates =
    if not (Int.equal expected_generation t.generation)
    then Stale_generation
    else (
      let past = start_index + Array.length updates in
      if start_index < 0 || past < start_index || past > length t
      then invalid_arg "Renderer_virtual_list.Geometry.apply_exact_batch: invalid range";
      if Array.is_empty updates
      then invalid_arg "Renderer_virtual_list.Geometry.apply_exact_batch: empty batch";
      if Array.exists updates ~f:(fun height -> height < 0)
      then invalid_arg "Renderer_virtual_list.Geometry.apply_exact_batch: negative height";
      let heights = Array.copy t.heights in
      let exact = Array.copy t.exact in
      Array.iteri updates ~f:(fun offset height ->
        let index = start_index + offset in
        heights.(index) <- height;
        exact.(index) <- true);
      let prefix = prefix_of_heights heights in
      t.heights <- heights;
      t.prefix <- prefix;
      t.exact <- exact;
      t.generation <- t.generation + 1;
      Applied)
  ;;

  let item_start t ~index =
    if index < 0 || index >= length t then None else Some t.prefix.(index)
  ;;

  let is_exact t ~index = index >= 0 && index < length t && t.exact.(index)

  let range_is_exact t ~first ~past =
    if first < 0 || past < first || past > length t
    then invalid_arg "Renderer_virtual_list.Geometry.range_is_exact";
    let rec loop index = index >= past || (t.exact.(index) && loop (index + 1)) in
    loop first
  ;;

  let exact_prefix_length t =
    let rec loop index =
      if index >= length t || not t.exact.(index) then index else loop (index + 1)
    in
    loop 0
  ;;

  let all_exact t = Array.for_all t.exact ~f:Fn.id

  let mark_all_estimated t =
    t.exact <- Array.create ~len:(length t) false;
    t.generation <- t.generation + 1
  ;;

  let mark_exact t ~index ~height =
    update_height t ~index ~height;
    t.exact.(index) <- true
  ;;

  let estimated_indices t =
    Array.filter_mapi t.exact ~f:(fun index exact -> if exact then None else Some index)
    |> Array.to_list
  ;;
end

module Viewport = struct
  type t =
    { scroll : int
    ; max_scroll : int
    ; first_visible : int option
    ; last_visible : int option
    ; top_spacer : int
    ; bottom_spacer : int
    }
  [@@deriving fields ~getters]

  let first_index_after prefix ~target ~is_after =
    let low = ref 0 in
    let high = ref (Array.length prefix) in
    while !low < !high do
      let middle = (!low + !high) lsr 1 in
      if is_after prefix.(middle) target then high := middle else low := middle + 1
    done;
    !low
  ;;

  let compute_with ~length ~total_height ~prefix ~requested_scroll ~height ~follow_bottom =
    let height = Int.max 0 height in
    let max_scroll = Int.max 0 (total_height - height) in
    let scroll =
      if follow_bottom
      then max_scroll
      else Int.max 0 (Int.min max_scroll requested_scroll)
    in
    if Int.equal length 0 || Int.equal height 0
    then
      { scroll
      ; max_scroll
      ; first_visible = None
      ; last_visible = None
      ; top_spacer = 0
      ; bottom_spacer = total_height
      }
    else (
      let first =
        first_index_after prefix ~target:scroll ~is_after:(fun value target ->
          value > target)
        - 1
        |> Int.max 0
      in
      let end_position = Int.min total_height (scroll + height) in
      let last =
        first_index_after prefix ~target:end_position ~is_after:(fun value target ->
          value >= target)
        - 1
        |> Int.max 0
        |> Int.min (length - 1)
      in
      { scroll
      ; max_scroll
      ; first_visible = Some first
      ; last_visible = Some last
      ; top_spacer = prefix.(first)
      ; bottom_spacer = total_height - prefix.(last + 1)
      })
  ;;

  let compute ~geometry ~requested_scroll ~height ~follow_bottom =
    compute_with
      ~length:(Geometry.length geometry)
      ~total_height:(Geometry.total_height geometry)
      ~prefix:(Geometry.prefix geometry)
      ~requested_scroll
      ~height
      ~follow_bottom
  ;;

  let compute_snapshot ~geometry ~requested_scroll ~height ~follow_bottom =
    compute_with
      ~length:(Geometry.Snapshot.length geometry)
      ~total_height:(Geometry.Snapshot.total_height geometry)
      ~prefix:(Geometry.Snapshot.prefix geometry)
      ~requested_scroll
      ~height
      ~follow_bottom
  ;;

  let visible_indices t =
    match t.first_visible, t.last_visible with
    | Some first, Some last ->
      List.init (Int.max 0 (last - first + 1)) ~f:(fun offset -> first + offset)
    | None, _ | _, None -> []
  ;;

  let estimated_visible_indices ~geometry t =
    visible_indices t
    |> List.filter ~f:(fun index -> not (Geometry.is_exact geometry ~index))
  ;;

  let is_exact ~geometry t =
    match t.first_visible, t.last_visible with
    | Some first, Some last -> Geometry.range_is_exact geometry ~first ~past:(last + 1)
    | None, _ | _, None -> true
  ;;

  let bottom_up_candidates ~geometry ~height ~overscan_rows =
    let target = Int.max 0 height + Int.max 0 overscan_rows in
    let heights = Geometry.heights geometry in
    let rec loop index covered acc =
      if index < 0 || covered >= target
      then List.rev acc
      else (
        let next_covered = covered + heights.(index) in
        let acc = if Geometry.is_exact geometry ~index then acc else index :: acc in
        loop (index - 1) next_covered acc)
    in
    loop (Geometry.length geometry - 1) 0 []
  ;;
end

module Anchor = struct
  type offset =
    | From_start of int
    | From_end of int

  type t =
    { index : int
    ; offset : offset
    ; screen_row : int
    }

  let index t = t.index

  let at_start ~index ~offset ~screen_row =
    { index; offset = From_start (Int.max 0 offset); screen_row = Int.max 0 screen_row }
  ;;

  let at_end ~index ~offset ~screen_row =
    { index; offset = From_end (Int.max 0 offset); screen_row = Int.max 0 screen_row }
  ;;

  let create ~geometry ~viewport ~screen_row =
    let absolute_row = Viewport.scroll viewport + Int.max 0 screen_row in
    let prefix = Geometry.prefix geometry in
    let rec find = function
      | [] -> None
      | index :: rest ->
        if absolute_row < prefix.(index + 1)
        then (
          let item_start = prefix.(index) in
          let intra_row = Int.max 0 (absolute_row - item_start) in
          let offset =
            if Geometry.is_exact geometry ~index
            then From_start intra_row
            else (
              let item_height = prefix.(index + 1) - item_start in
              From_end (Int.max 0 (item_height - 1 - intra_row)))
          in
          Some { index; offset; screen_row = Int.max 0 screen_row })
        else find rest
    in
    find (Viewport.visible_indices viewport)
  ;;

  let create_at_scroll ~geometry ~scroll =
    let viewport =
      Viewport.compute ~geometry ~requested_scroll:scroll ~height:1 ~follow_bottom:false
    in
    create ~geometry ~viewport ~screen_row:0
  ;;

  let remap_index t ~index = { t with index }

  let corrected_scroll t ~geometry =
    let corrected_scroll_with ~prefix ~length t =
      if t.index < 0 || t.index >= length
      then None
      else (
        let item_start = prefix.(t.index) in
        let item_height = prefix.(t.index + 1) - item_start in
        let max_intra_row = Int.max 0 (item_height - 1) in
        let intra_row =
          match t.offset with
          | From_start rows -> Int.min max_intra_row rows
          | From_end rows -> Int.max 0 (max_intra_row - rows)
        in
        Some (Int.max 0 (item_start + intra_row - t.screen_row)))
    in
    corrected_scroll_with
      ~prefix:(Geometry.prefix geometry)
      ~length:(Geometry.length geometry)
      t
  ;;

  let corrected_scroll_snapshot t ~geometry =
    let prefix = Geometry.Snapshot.prefix geometry in
    if t.index < 0 || t.index >= Geometry.Snapshot.length geometry
    then None
    else (
      let item_start = prefix.(t.index) in
      let item_height = prefix.(t.index + 1) - item_start in
      let max_intra_row = Int.max 0 (item_height - 1) in
      let intra_row =
        match t.offset with
        | From_start rows -> Int.min max_intra_row rows
        | From_end rows -> Int.max 0 (max_intra_row - rows)
      in
      Some (Int.max 0 (item_start + intra_row - t.screen_row)))
  ;;
end

let render ~viewport ~width ~image_at_index =
  let visible =
    Viewport.visible_indices viewport |> List.map ~f:image_at_index |> Notty.I.vcat
  in
  Notty.I.vcat
    [ Notty.I.void width (Viewport.top_spacer viewport)
    ; visible
    ; Notty.I.void width (Viewport.bottom_spacer viewport)
    ]
;;
