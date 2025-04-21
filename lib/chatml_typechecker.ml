open Core
open Chatml.Chatml_lang

(**********************************************************************)
(* WARNING: This is a prototype; please adapt & test thoroughly.      *)
(**********************************************************************)

(***************************************************************************)
(* 1) Our type representation, including row types and poly variants       *)
(***************************************************************************)

type poly_row_bound =
  | Exact (* e.g. `[ `Tag1(...) | `Tag2(...)]`     *)
  | AtLeast (* e.g. `[> `Tag1(...) ]` must contain at least these tags *)
  | AtMost (* e.g. `[< `Tag1(...) ]` must not contain anything else   *)

type record_bound =
  | RB_open (* “At least these fields” *)
  | RB_closed (* “Exactly these fields” *)

type row =
  | REmpty
  | RExtend of string * ttype * row
  | RVar of int

(*
   Polymorphic variants: we store 
		   - bound: whether it's exact, or can have more/fewer tags
		   - tags: known tags and their argument types
		   - leftover: reference to leftover row variable (storing more constraints).
*)
and poly_row =
  { bound : poly_row_bound
  ; tags : (string * ttype list) list
  ; leftover : int option
  }

and ttype =
  | TInt
  | TBool
  | TFloat
  | TString
  | TUnit
  | TVar of int
  | TFun of ttype list * ttype
  | TArray of ttype
  | TPolyVariant of poly_row
  | TRecord of row * record_bound
  | TModule of (string * scheme) list
  | TDynamic

and scheme = Scheme of int list * ttype

(***************************************************************************)
(* 2) Union-Find for ordinary type vars                                    *)
(***************************************************************************)

module TypeUF = struct
  type node =
    | Link of int
    | Root of ttype option

  let store = Hashtbl.create (module Int)

  let get_node (x : int) : node =
    match Hashtbl.find store x with
    | Some n -> n
    | None ->
      let n = Root None in
      Hashtbl.set store ~key:x ~data:n;
      n
  ;;

  let rec find (x : int) : int * ttype option =
    match get_node x with
    | Link y ->
      let r, topt = find y in
      if r <> y then Hashtbl.set store ~key:x ~data:(Link r);
      r, topt
    | Root t -> x, t
  ;;

  let set_root_type (x : int) (ty : ttype option) =
    let rx, _ = find x in
    Hashtbl.set store ~key:rx ~data:(Root ty)
  ;;

  let union (x : int) (y : int) =
    let rx, tx = find x in
    let ry, ty = find y in
    if rx <> ry
    then (
      Hashtbl.set store ~key:ry ~data:(Link rx);
      match tx, ty with
      | None, None -> ()
      | None, Some t -> set_root_type rx (Some t)
      | Some t, None -> set_root_type rx (Some t)
      | Some _, Some _ -> ())
  ;;
end

(***************************************************************************)
(* 3) Union-Find for record row vars                                       *)
(***************************************************************************)

module RowUF = struct
  type row_node =
    | RLink of int
    | RRoot of row option

  let store = Hashtbl.create (module Int)

  let get_node (rv : int) =
    match Hashtbl.find store rv with
    | Some n -> n
    | None ->
      let n = RRoot None in
      Hashtbl.set store ~key:rv ~data:n;
      n
  ;;

  let rec find (rv : int) : int * row option =
    match get_node rv with
    | RLink r2 ->
      let r, ropt = find r2 in
      if r <> r2 then Hashtbl.set store ~key:rv ~data:(RLink r);
      r, ropt
    | RRoot ropt -> rv, ropt
  ;;

  let set_root (rv : int) (ropt : row option) =
    let r, _ = find rv in
    Hashtbl.set store ~key:r ~data:(RRoot ropt)
  ;;

  let union (r1 : int) (r2 : int) =
    let root1, ro1 = find r1 in
    let root2, ro2 = find r2 in
    if root1 <> root2
    then (
      Hashtbl.set store ~key:root2 ~data:(RLink root1);
      match ro1, ro2 with
      | None, None -> ()
      | None, Some r -> set_root root1 (Some r)
      | Some r, None -> set_root root1 (Some r)
      | Some _, Some _ -> ())
  ;;
end

(***************************************************************************)
(* 4) Union-Find for leftover row vars in polymorphic variants             *)
(***************************************************************************)

module PolyVarUF = struct
  type pv_node =
    | PVLink of int
    | PVRoot of poly_row option

  let store = Hashtbl.create (module Int)

  let get_node (id : int) =
    match Hashtbl.find store id with
    | Some n -> n
    | None ->
      let n = PVRoot None in
      Hashtbl.set store ~key:id ~data:n;
      n
  ;;

  let rec find (id : int) : int * poly_row option =
    match get_node id with
    | PVLink next ->
      let r, ropt = find next in
      if r <> next then Hashtbl.set store ~key:id ~data:(PVLink r);
      r, ropt
    | PVRoot ropt -> id, ropt
  ;;

  let set_root (id : int) (ropt : poly_row option) =
    let r, _ = find id in
    Hashtbl.set store ~key:r ~data:(PVRoot ropt)
  ;;

  let union (id1 : int) (id2 : int) =
    let r1, ro1 = find id1 in
    let r2, ro2 = find id2 in
    if r1 <> r2
    then (
      Hashtbl.set store ~key:r2 ~data:(PVLink r1);
      match ro1, ro2 with
      | None, None -> ()
      | None, Some r -> set_root r1 (Some r)
      | Some r, None -> set_root r1 (Some r)
      | Some _, Some _ -> ())
  ;;
end

(***************************************************************************)
(* 5) Fresh generators for type vars, row vars, leftover row vars          *)
(***************************************************************************)

let next_tvar_id = ref 0

let fresh_tvar () =
  let id = !next_tvar_id in
  incr next_tvar_id;
  TVar id
;;

let next_rvar_id = ref 0

let fresh_rvar () =
  let id = !next_rvar_id in
  incr next_rvar_id;
  id
;;

let next_pvvar_id = ref 0

let fresh_pvvar () =
  let id = !next_pvvar_id in
  incr next_pvvar_id;
  id
;;

exception Type_error of string

(***************************************************************************)
(* 6) Expand to normal forms                                               *)
(***************************************************************************)

let rec expand (ty : ttype) : ttype =
  match ty with
  | TVar tv ->
    let rid, maybe_ty = TypeUF.find tv in
    (match maybe_ty with
     | None -> TVar rid
     | Some real_ty ->
       let e = expand real_ty in
       TypeUF.set_root_type rid (Some e);
       e)
  | TFun (args, ret) -> TFun (List.map ~f:expand args, expand ret)
  | TRecord (row, bound) -> TRecord (expand_row row, bound)
  | TPolyVariant pv -> TPolyVariant (expand_poly_row pv)
  | TArray t -> TArray (expand t)
  | TModule fields ->
    let upd =
      List.map fields ~f:(fun (nm, Scheme (bvs, body)) -> nm, Scheme (bvs, expand body))
    in
    TModule upd
  | TInt | TBool | TFloat | TString | TUnit | TDynamic -> ty

and expand_row (r : row) : row =
  match r with
  | REmpty -> REmpty
  | RExtend (lbl, fty, tail) -> RExtend (lbl, expand fty, expand_row tail)
  | RVar rv ->
    let root, ropt = RowUF.find rv in
    (match ropt with
     | None -> RVar root
     | Some actual ->
       let e = expand_row actual in
       RowUF.set_root root (Some e);
       e)

and expand_poly_row_with (visited_pvs : Int.Hash_set.t) (pv : poly_row) : poly_row =
  (* Expand each tag's argument types normally *)
  let expanded_tags =
    List.map pv.tags ~f:(fun (tag, tys) -> tag, List.map tys ~f:expand)
  in
  let bound = pv.bound in
  match pv.leftover with
  | None -> { bound; tags = expanded_tags; leftover = None }
  | Some leftover_id ->
    (* If we've already visited this leftover var, do NOT expand again *)
    if Hash_set.mem visited_pvs leftover_id
    then
      (* We must not recursively re-expand the same leftover.
		         So just keep it as-is, or you may set leftover=None if you prefer. *)
      { bound; tags = expanded_tags; leftover = Some leftover_id }
    else (
      (* Mark this leftover_id as visited *)
      Hash_set.add visited_pvs leftover_id;
      (* Proceed with normal union-find find logic *)
      let root, ropt = PolyVarUF.find leftover_id in
      match ropt with
      | None ->
        (* No known expanded shape yet, so keep leftover=Some root. *)
        { bound; tags = expanded_tags; leftover = Some root }
      | Some leftover_row ->
        (* Recursively expand that leftover row with the same visited set. *)
        let e = expand_poly_row_with visited_pvs leftover_row in
        (* Possibly unify the two bounds & tags.  For a simple approach,
		           merge them in the same style your unify_poly_variant_rows does. *)
        let merged_bound = unify_poly_bounds bound e.bound in
        let merged_tags = unify_poly_tags bound e.bound expanded_tags e.tags in
        { bound = merged_bound; tags = merged_tags; leftover = e.leftover })

(* Then, in your original expand_poly_row, just create a fresh visited set
		   and delegate to expand_poly_row_with. *)
and expand_poly_row (pv : poly_row) : poly_row =
  let visited_pvs = Int.Hash_set.create () in
  expand_poly_row_with visited_pvs pv

(* For a quick demonstration, unify two poly_row_bound in a naive way. *)
and unify_poly_bounds b1 b2 =
  match b1, b2 with
  | Exact, Exact -> Exact
  | Exact, AtLeast | AtLeast, Exact -> AtLeast
  | Exact, AtMost | AtMost, Exact -> AtMost
  | AtMost, AtMost -> AtMost
  | AtLeast, AtLeast -> AtLeast
  | AtMost, AtLeast | AtLeast, AtMost ->
    (* If one is AtMost and the other is AtLeast, that typically collapses to Exact 
		       in a more advanced system, but you might do checks. 
		       Here we just produce Exact.
    *)
    Exact

(* Merge tags from two expansions for demonstration. Real logic might do more checks. *)
and unify_poly_tags b1 b2 tagsA tagsB =
  (* simplistic union-of-tags approach: unify argument types if the same tag 
		     appears in both. If a tag is only on one side but "AtMost" side doesn't allow it,
		     that might error.
  *)
  let mapA =
    List.fold tagsA ~init:String.Map.empty ~f:(fun acc (tg, tys) ->
      Map.set acc ~key:tg ~data:tys)
  in
  let mapB =
    List.fold tagsB ~init:String.Map.empty ~f:(fun acc (tg, tys) ->
      Map.set acc ~key:tg ~data:tys)
  in
  let all_tags =
    Set.union (String.Set.of_list (Map.keys mapA)) (String.Set.of_list (Map.keys mapB))
  in
  let merged = ref [] in
  Set.iter all_tags ~f:(fun t ->
    let a = Map.find mapA t in
    let b = Map.find mapB t in
    match a, b with
    | Some tysA, Some tysB ->
      (* unify each argument type pair *)
      if List.length tysA <> List.length tysB
      then raise (Type_error (Printf.sprintf "Tag `%s` has mismatched arities" t));
      List.iter2_exn tysA tysB ~f:unify;
      let final_args = List.map tysA ~f:expand in
      merged := (t, final_args) :: !merged
    | Some tysA, None ->
      (* If the other side is AtMost, we might forbid new tags. *)
      (match b2 with
       | AtMost ->
         raise (Type_error (Printf.sprintf "Tag `%s` not allowed in AtMost variant" t))
       | _ -> merged := (t, List.map tysA ~f:expand) :: !merged)
    | None, Some tysB ->
      (match b1 with
       | AtMost ->
         raise (Type_error (Printf.sprintf "Tag `%s` not allowed in AtMost variant" t))
       | _ -> merged := (t, List.map tysB ~f:expand) :: !merged)
    | None, None -> ());
  List.rev !merged

(***************************************************************************)
(* 7) The main unify function                                              *)
(***************************************************************************)

and unify (t1 : ttype) (t2 : ttype) : unit =
  let t1' = expand t1 in
  let t2' = expand t2 in
  match t1', t2' with
  | TDynamic, _ -> ()
  | _, TDynamic -> ()
  | TInt, TInt | TBool, TBool | TFloat, TFloat | TString, TString | TUnit, TUnit -> ()
  | TVar x, TVar y -> TypeUF.union x y
  | TVar x, other ->
    print_endline "TVar x, other";
    TypeUF.set_root_type x (Some other)
  | other, TVar x ->
    print_endline "other, TVar x";
    TypeUF.set_root_type x (Some other)
  | TFun (a1, r1), TFun (a2, r2) ->
    if List.length a1 <> List.length a2 then raise (Type_error "Function arity mismatch");
    List.iter2_exn a1 a2 ~f:unify;
    unify r1 r2
  | TRecord (ra, boundA), TRecord (rb, boundB) ->
    (match t1 with
     | TVar x ->
       TypeUF.set_root_type x (Some (TRecord (ra, boundA)));
       print_endline "ra is tvar"
     | _ -> print_endline "ra is not tvar");
    (match t2 with
     | TVar x ->
       TypeUF.set_root_type x (Some (TRecord (ra, boundA)));
       print_endline "rb is tvar"
     | _ -> print_endline "rb is not tvar");
    let _merged_row, _merged_bound = unify_rows ra boundA rb boundB in
    (* unify_rows already updates row variables in its union-find store.
		       So we don't necessarily need more here, as unify() returns unit. *)
    ()
  | TPolyVariant pvA, TPolyVariant pvB -> unify_poly_variant_rows pvA pvB
  | TArray e1, TArray e2 -> unify e1 e2
  | TModule _, TModule _ ->
    raise (Type_error "Unifying modules directly is not supported in this example.")
  | TModule _, _ | _, TModule _ ->
    raise (Type_error "Cannot unify a module with a non-module.")
  | _ ->
    let msg = Printf.sprintf "Cannot unify %s with %s" (show_type t1') (show_type t2') in
    raise (Type_error msg)

and unify_poly_variant_rows (pvA : poly_row) (pvB : poly_row) : unit =
  (* We “expand” them again to be safe. Then unify them by merging everything. 
		     The final shape is forced back into each leftover if present. *)
  let eA = expand_poly_row pvA in
  let eB = expand_poly_row pvB in
  let final_bound = unify_poly_bounds eA.bound eB.bound in
  let merged_tags = unify_poly_tags eA.bound eB.bound eA.tags eB.tags in
  (* unify leftover row variables if both sides are open. *)
  let leftover =
    match eA.leftover, eB.leftover with
    | None, None -> None
    | Some la, None -> Some la
    | None, Some lb -> Some lb
    | Some la, Some lb ->
      PolyVarUF.union la lb;
      let root, _ropt = PolyVarUF.find la in
      Some root
  in
  (* Now we record the final shape into each leftover if needed. *)
  let final_poly = { bound = final_bound; tags = merged_tags; leftover } in
  (* store final_poly in leftover if leftover is Some. *)
  let store_in lf =
    let _, ropt = PolyVarUF.find lf in
    match ropt with
    | None -> PolyVarUF.set_root lf (Some final_poly)
    | Some _old -> PolyVarUF.set_root lf (Some final_poly)
  in
  (match leftover with
   | Some lf -> store_in lf
   | None -> ());
  ()

(** unify_rows:
		    Given (rowA, boundA) and (rowB, boundB),
		    produce a unified row shape that is consistent with both. *)
and unify_rows (rowA : row) (boundA : record_bound) (rowB : row) (boundB : record_bound)
  : row * record_bound
  =
  (* First, expand each row so we see its fully resolved shape. *)
  (* Expand each row so we see its fully resolved shape. *)
  let ea = expand_row rowA in
  let eb = expand_row rowB in
  print_endline "unify_rows";
  print_endline (show_type (TRecord (ea, boundA)));
  print_endline (show_type (TRecord (eb, boundB)));
  (* Convert each row to (Map<label,type> plus leftover row var). *)
  let mapA, leftoverA = row_to_map ea in
  let mapB, leftoverB = row_to_map eb in
  print_s [%message (leftoverA : int option)];
  print_s [%message (leftoverB : int option)];
  (* All keys present on either side. *)
  let all_keys =
    Set.union
      (Set.of_list (module String) (Map.keys mapA))
      (Set.of_list (module String) (Map.keys mapB))
  in
  (* We'll accumulate the merged label->type in a map. *)
  let merged_map = ref String.Map.empty in
  Set.iter all_keys ~f:(fun lbl ->
    print_endline lbl;
    print_endline "-----------------";
    let tyA_opt = Map.find mapA lbl in
    let tyB_opt = Map.find mapB lbl in
    match tyA_opt, tyB_opt with
    | Some tA, Some tB ->
      (* Both sides have this label -> unify. *)
      unify tA tB;
      merged_map := Map.set !merged_map ~key:lbl ~data:(expand tA);
      print_endline "Both sides have this label -> unify."
    | Some tA, None ->
      (* Label only on side A.  If side B is closed & leftoverB=None,
		         that's an error: B doesn't allow new fields. *)
      (match boundB, leftoverB with
       | RB_closed, None ->
         raise
           (Type_error
              (Printf.sprintf
                 "Extra field '%s' not allowed; record is closed on RHS."
                 lbl))
       | _ ->
         (* B is open, or leftoverB is Some, so we accept it. *)
         merged_map := Map.set !merged_map ~key:lbl ~data:(expand tA));
      print_endline
        "Label only on side A. B is open, or leftoverB is Some, so we accept it."
    | None, Some tB ->
      (* Label only on side B.  If side A is closed & leftoverA=None,
		         that's an error. *)
      (match boundA, leftoverA with
       | RB_closed, None ->
         raise
           (Type_error
              (Printf.sprintf
                 "Extra field '%s' not allowed; record is closed on LHS."
                 lbl))
       | _ -> merged_map := Map.set !merged_map ~key:lbl ~data:(expand tB));
      print_endline
        "Label only on side B. A is open, or leftoverA is Some, so we accept it."
    | None, None ->
      (* Shouldn't happen, since lbl is in all_keys. *)
      ());
  (* Decide how leftover row variables unify. If either side is open,
		     we remain open. If both are closed, but leftover is Some, we forcibly close it. *)
  let merged_bound =
    match boundA, boundB with
    | RB_open, _ -> RB_open
    | _, RB_open -> RB_open
    | RB_closed, RB_closed -> RB_closed
  in
  let bound_str =
    match merged_bound with
    | RB_open -> "open"
    | RB_closed -> "closed"
  in
  print_endline @@ "merged_bound: " ^ bound_str;
  (* For leftover row actual union, unify leftoverA and leftoverB if both exist.
		     If both sides are closed, leftover is None. If one is open with leftover Some,
		     we keep that leftover open. *)
  let leftover_merged =
    match leftoverA, leftoverB with
    | None, None ->
      (match merged_bound with
       | RB_open -> Some (fresh_rvar ())
       | _ -> None)
    | Some la, None ->
      (match merged_bound with
       | RB_open -> Some la
       | _ -> None)
    | None, Some lb ->
      (match merged_bound with
       | RB_open -> Some lb
       | _ -> None)
    | Some la, Some lb ->
      (* unify them so they become the same leftover var. *)
      RowUF.union la lb;
      let root, _ = RowUF.find la in
      (match merged_bound with
       | RB_open -> Some root
       | _ -> None)
  in
  print_s [%message "leftover_merged: " (leftover_merged : int option)];
  (* Build a final row out of merged_map plus leftover_merged. *)
  let final_row = row_of_map !merged_map leftover_merged in
  (* Optionally store final_row in rowA, rowB's rowvars so expansions see the updated shape. *)
  let store_final r =
    match expand_row r with
    | RVar rv ->
      print_endline "RVar";
      let root, _ = RowUF.find rv in
      RowUF.set_root root (Some final_row)
    | REmpty -> print_endline "Not RVar REmpty"
    | RExtend _ -> print_endline "Not RVar RExtend"
  in
  store_final rowA;
  store_final rowB;
  print_endline @@ "final_row: " ^ show_row final_row;
  final_row, merged_bound

(***************************************************************************)
(* Symmetrical row_to_map for records                                      *)
(***************************************************************************)

and row_to_map (r : row) : ttype String.Map.t * int option =
  match expand_row r with
  | REmpty -> String.Map.empty, None
  | RExtend (lbl, fty, tail) ->
    let m, leftover = row_to_map tail in
    (match Map.find m lbl with
     | Some existing ->
       unify existing fty;
       m, leftover
     | None -> Map.set m ~key:lbl ~data:fty, leftover)
  | RVar rv ->
    let root, ropt = RowUF.find rv in
    (match ropt with
     | None -> String.Map.empty, Some root
     | Some row' -> row_to_map row')

and row_of_map (m : ttype String.Map.t) (rv_opt : int option) : row =
  let sorted = Map.to_alist m in
  let rec build al =
    match al with
    | [] ->
      (match rv_opt with
       | None -> REmpty
       | Some rv -> RVar rv)
    | (lbl, fty) :: tl -> RExtend (lbl, fty, build tl)
  in
  build sorted

(***************************************************************************)
(* 8) show_type for printing                                               *)
(***************************************************************************)

and show_type (ty : ttype) : string =
  match expand ty with
  | TInt -> "int"
  | TBool -> "bool"
  | TFloat -> "float"
  | TString -> "string"
  | TUnit -> "unit"
  | TVar id ->
    let _, maybe = TypeUF.find id in
    (match maybe with
     | None -> Printf.sprintf "'a%d" id
     | Some real -> show_type real)
  | TFun (args, ret) ->
    let as_ = String.concat ~sep:" * " (List.map args ~f:show_type) in
    Printf.sprintf "(%s -> %s)" as_ (show_type ret)
  | TArray t -> Printf.sprintf "array(%s)" (show_type t)
  | TModule fields ->
    let fs =
      List.map fields ~f:(fun (nm, Scheme (_bvs, body)) -> nm ^ ":" ^ show_type body)
    in
    Printf.sprintf "Module{%s}" (String.concat ~sep:"; " fs)
  | TRecord (row, bound) ->
    let bound_str =
      match bound with
      | RB_open -> "open"
      | RB_closed -> "closed"
    in
    Printf.sprintf "{%s} %s" (show_row row) bound_str
  | TPolyVariant pv -> show_poly_variant pv
  | TDynamic -> "dynamic"

and show_row (r : row) : string =
  match expand_row r with
  | REmpty -> ""
  | RExtend (lbl, fty, REmpty) -> Printf.sprintf "%s:%s" lbl (show_type fty)
  | RExtend (lbl, fty, tail) ->
    let tail_str = show_row tail in
    if String.is_empty tail_str
    then Printf.sprintf "%s:%s" lbl (show_type fty)
    else Printf.sprintf "%s:%s; %s" lbl (show_type fty) tail_str
  | RVar rv ->
    let root, ropt = RowUF.find rv in
    (match ropt with
     | None -> Printf.sprintf "...(rvar%d)" root
     | Some row' -> show_row row')

and show_poly_variant (pv : poly_row) : string =
  let e = expand_poly_row pv in
  let tags_str =
    List.map e.tags ~f:(fun (tag, tys) ->
      if List.is_empty tys
      then Printf.sprintf "`%s" tag
      else (
        let st = String.concat ~sep:", " (List.map tys ~f:show_type) in
        Printf.sprintf "`%s(%s)" tag st))
    |> String.concat ~sep:" | "
  in
  let leftover_str =
    match e.leftover with
    | None -> ""
    | Some x -> Printf.sprintf "; leftover=%d" x
  in
  let bound_str =
    match e.bound with
    | Exact -> ""
    | AtLeast -> ">"
    | AtMost -> "<"
  in
  if String.is_empty tags_str
  then Printf.sprintf "[%s poly?%s]" bound_str leftover_str
  else Printf.sprintf "[%s %s%s]" bound_str tags_str leftover_str
;;

(***************************************************************************)
(* 9) Free tvars, schemes, instantiation, generalization                   *)
(***************************************************************************)

let rec free_tvars (ty : ttype) : Int.Set.t =
  match expand ty with
  | TInt | TBool | TFloat | TString | TUnit | TDynamic -> Int.Set.empty
  | TVar i -> Int.Set.singleton i
  | TFun (args, ret) ->
    let sets = List.map args ~f:free_tvars in
    let s = Set.union_list (module Int) sets in
    Set.union s (free_tvars ret)
  | TArray t -> free_tvars t
  | TPolyVariant pv ->
    let tvs_for_tags =
      List.map pv.tags ~f:(fun (_, tys) ->
        tys |> List.map ~f:free_tvars |> Set.union_list (module Int))
      |> Set.union_list (module Int)
    in
    tvs_for_tags
  | TRecord (r, _bound) -> free_row_tvars r
  | TModule fields ->
    List.fold fields ~init:Int.Set.empty ~f:(fun acc (_, Scheme (_bvs, body)) ->
      Set.union acc (free_tvars body))

and free_row_tvars (r : row) =
  match expand_row r with
  | REmpty -> Int.Set.empty
  | RExtend (_, fty, tail) -> Set.union (free_tvars fty) (free_row_tvars tail)
  | RVar _ -> Int.Set.empty
;;

let free_tvars_scheme (Scheme (bvs, body)) =
  let fv_body = free_tvars body in
  Set.diff fv_body (Int.Set.of_list bvs)
;;

let free_tvars_env (env : (string, scheme) Hashtbl.t) : Int.Set.t =
  Hashtbl.data env |> List.map ~f:free_tvars_scheme |> Set.union_list (module Int)
;;

let generalize (env : (string, scheme) Hashtbl.t) (ty : ttype) : scheme =
  let fv_env = free_tvars_env env in
  let fv_ty = free_tvars ty in
  let gen_vars = Set.diff fv_ty fv_env in
  Scheme (Set.to_list gen_vars, ty)
;;

let rec instantiate (Scheme (bvars, body)) : ttype =
  let mapping = Int.Table.create () in
  List.iter bvars ~f:(fun bv -> Hashtbl.set mapping ~key:bv ~data:(fresh_tvar ()));
  let rec repl ty =
    match expand ty with
    | TInt | TBool | TFloat | TString | TUnit | TDynamic -> ty
    | TVar tv ->
      (match Hashtbl.find mapping tv with
       | Some nty -> nty
       | None -> TVar tv)
    | TFun (a, r) -> TFun (List.map a ~f:repl, repl r)
    | TArray el -> TArray (repl el)
    | TRecord (row, bound) -> TRecord (repl_row row, bound)
    | TPolyVariant pv ->
      let new_tags = List.map pv.tags ~f:(fun (tg, tys) -> tg, List.map tys ~f:repl) in
      TPolyVariant { pv with tags = new_tags }
    | TModule fs ->
      let newfs =
        List.map fs ~f:(fun (nm, Scheme (bvs2, ft)) ->
          nm, Scheme ([], instantiate (Scheme (bvs2, ft))))
      in
      TModule newfs
  and repl_row r =
    match expand_row r with
    | REmpty -> REmpty
    | RExtend (lbl, fty, tail) -> RExtend (lbl, repl fty, repl_row tail)
    | RVar rv -> RVar rv
  in
  repl body
;;

(***************************************************************************)
(* 10) Expression & Pattern inference                                      *)
(***************************************************************************)

let lookup_env (env : (string, scheme) Hashtbl.t) (x : string) : ttype =
  match Hashtbl.find env x with
  | Some sc -> instantiate sc
  | None -> raise (Type_error (Printf.sprintf "Unbound variable '%s'" x))
;;

let add_to_env (env : (string, scheme) Hashtbl.t) (x : string) (ty : ttype) =
  Hashtbl.set env ~key:x ~data:(generalize env ty)
;;

let rec close_row (r : row) : row =
  match expand_row r with
  | REmpty -> REmpty
  | RExtend (lbl, fty, tail) ->
    (* Recursively close the tail, so we disallow further extension. *)
    RExtend (lbl, fty, close_row tail)
  | RVar rv ->
    let root, row_opt = RowUF.find rv in
    (match row_opt with
     | None ->
       (* No constraints yet, so force it to be exactly REmpty. *)
       RowUF.set_root root (Some REmpty);
       REmpty
     | Some actual_row ->
       (* Recursively close whatever that variable points to. *)
       let closed = close_row actual_row in
       RowUF.set_root root (Some closed);
       closed)
;;

let rec infer_expr (env : (string, scheme) Hashtbl.t) (e : expr) : ttype =
  match e with
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EFloat _ -> TFloat
  | EString _ -> TString
  | EVar x -> lookup_env env x
  | ELambda (params, body) ->
    let param_tys =
      List.map params ~f:(fun p ->
        let tv = fresh_tvar () in
        Hashtbl.set env ~key:p ~data:(Scheme ([], tv));
        tv)
    in
    let ret_ty = infer_expr env body in
    (* print_endline "Lambda params:"; *)
    List.iter param_tys ~f:(fun t -> print_endline (show_type t));
    (* print_endline "Lambda body:";
    print_endline (show_type ret_ty); *)
    TFun (param_tys, ret_ty)
  | EApp (fn_expr, arg_exprs) ->
    let fn_ty = infer_expr env fn_expr in
    print_endline "Function type:";
    print_endline @@ show_type fn_ty;
    let arg_tys = List.map arg_exprs ~f:(infer_expr env) in
    print_endline "Function args:";
    List.iter arg_tys ~f:(fun t -> print_endline (show_type t));
    let ret_ty = fresh_tvar () in
    unify fn_ty (TFun (arg_tys, ret_ty));
    (* print_endline "Function type:";
    print_endline (show_type fn_ty);
    print_endline "Argument types:"; *)
    print_endline "Function args2:";
    List.iteri arg_tys ~f:(fun i t ->
      print_endline @@ Int.to_string i;
      print_endline (show_type t));
    (* print_endline "Result type:";
    print_endline (show_type ret_ty); *)
    ret_ty
  | EIf (cond_expr, then_expr, else_expr) ->
    let cty = infer_expr env cond_expr in
    unify cty TBool;
    let t1 = infer_expr env then_expr in
    let t2 = infer_expr env else_expr in
    unify t1 t2;
    t1
  | EWhile (cond_expr, body_expr) ->
    unify (infer_expr env cond_expr) TBool;
    ignore (infer_expr env body_expr);
    TUnit
  | ESequence (e1, e2) ->
    print_endline "ESequence1";
    ignore (infer_expr env e1);
    print_endline "ESequence2";
    infer_expr env e2
  | ELetIn (x, rhs, body) ->
    let rhs_ty = infer_expr env rhs in
    add_to_env env x rhs_ty;
    let e = infer_expr env body in
    (* print_endline "Expression type Eletin:";
    print_endline (show_type rhs_ty);
    print_endline "Expression type Eletinbody:";
    print_endline (show_type e); *)
    e
  | ELetRec (bindings, body) ->
    List.iter bindings ~f:(fun (nm, _) ->
      Hashtbl.set env ~key:nm ~data:(Scheme ([], fresh_tvar ())));
    List.iter bindings ~f:(fun (nm, rhs_expr) ->
      let rhs_ty = infer_expr env rhs_expr in
      let (Scheme (_, tv)) = Hashtbl.find_exn env nm in
      unify rhs_ty tv);
    infer_expr env body
  | EMatch (scrut, cases) ->
    let s_ty = infer_expr env scrut in
    let result_ty = fresh_tvar () in
    List.iter cases ~f:(fun (pat, rhs) ->
      let localEnv = Hashtbl.copy env in
      let pat_ty = infer_pattern localEnv pat in
      unify s_ty pat_ty;
      let rt = infer_expr localEnv rhs in
      unify rt result_ty);
    result_ty
  | ERecord fields ->
    (* 1) Infer each field’s type, build a row *)
    let row =
      List.fold_right fields ~init:REmpty ~f:(fun (fld, fexpr) acc ->
        let fty = infer_expr env fexpr in
        RExtend (fld, fty, acc))
    in
    let expanded_row = expand_row row in
    (* Now *close* it. That means leftover row vars become REmpty, 
		       so no new fields can unify into it later. *)
    let closed_row = close_row expanded_row in
    TRecord (closed_row, RB_closed)
  | EFieldGet (obj_expr, field) ->
    let obj_ty = infer_expr env obj_expr in
    print_endline "FieldGet obj_ty:";
    print_endline @@ show_type obj_ty;
    let field_ty = fresh_tvar () in
    let leftover = fresh_rvar () in
    unify obj_ty (TRecord (RExtend (field, field_ty, RVar leftover), RB_open));
    field_ty
  | EFieldSet (obj_expr, field, new_val_expr) ->
    let obj_ty = infer_expr env obj_expr in
    print_endline "FieldSet obj_ty:";
    print_endline @@ show_type obj_ty;
    let val_ty = infer_expr env new_val_expr in
    let leftover = fresh_rvar () in
    unify obj_ty (TRecord (RExtend (field, val_ty, RVar leftover), RB_open));
    TUnit
  | EVariant (tag, exprs) ->
    let arg_tys = List.map exprs ~f:(infer_expr env) in
    let leftover = fresh_pvvar () in
    TPolyVariant { bound = AtLeast; tags = [ tag, arg_tys ]; leftover = Some leftover }
  | EArray elts ->
    (match elts with
     | [] -> TArray (fresh_tvar ())
     | hd :: tl ->
       let hd_ty = infer_expr env hd in
       List.iter tl ~f:(fun e2 -> unify (infer_expr env e2) hd_ty);
       TArray hd_ty)
  | EArrayGet (arr_expr, idx_expr) ->
    unify (infer_expr env idx_expr) TInt;
    let arr_ty = infer_expr env arr_expr in
    let elt_ty = fresh_tvar () in
    unify arr_ty (TArray elt_ty);
    elt_ty
  | EArraySet (arr_expr, idx_expr, v_expr) ->
    unify (infer_expr env idx_expr) TInt;
    let v_ty = infer_expr env v_expr in
    unify (infer_expr env arr_expr) (TArray v_ty);
    TUnit
  | ERef e1 ->
    ignore (infer_expr env e1);
    TUnit (* simplified: no real references in this example *)
  | ESetRef (ref_expr, val_expr) ->
    ignore (infer_expr env ref_expr);
    ignore (infer_expr env val_expr);
    TUnit
  | EDeref e1 ->
    ignore (infer_expr env e1);
    TUnit

and infer_pattern (env : (string, scheme) Hashtbl.t) (p : pattern) : ttype =
  match p with
  | PWildcard -> fresh_tvar ()
  | PVar x ->
    let tv = fresh_tvar () in
    Hashtbl.set env ~key:x ~data:(Scheme ([], tv));
    tv
  | PInt _ -> TInt
  | PBool _ -> TBool
  | PFloat _ -> TFloat
  | PString _ -> TString
  | PVariant (tag, subpats) ->
    let subtys = List.map subpats ~f:(infer_pattern env) in
    let leftover = fresh_pvvar () in
    TPolyVariant { bound = AtLeast; tags = [ tag, subtys ]; leftover = Some leftover }
;;

(***************************************************************************)
(* 11) Statements & Program                                                *)
(***************************************************************************)

let rec infer_stmt (env : (string, scheme) Hashtbl.t) (s : stmt) : unit =
  match s with
  | SLet (x, e) ->
    let t = infer_expr env e in
    print_endline ("Expression type slet " ^ x ^ ":");
    show_type t |> print_endline;
    add_to_env env x t
  | SLetRec bindings ->
    List.iter bindings ~f:(fun (nm, _) ->
      Hashtbl.set env ~key:nm ~data:(Scheme ([], fresh_tvar ())));
    List.iter bindings ~f:(fun (nm, rhs_expr) ->
      let rhs_ty = infer_expr env rhs_expr in
      let (Scheme (_, tv)) = Hashtbl.find_exn env nm in
      unify rhs_ty tv)
  | SModule (mname, stmts) ->
    let menv = Hashtbl.copy env in
    let keys_before = Set.of_list (module String) (Hashtbl.keys menv) in
    List.iter stmts ~f:(infer_stmt menv);
    let keys_after = Set.of_list (module String) (Hashtbl.keys menv) in
    let new_keys = Set.diff keys_after keys_before in
    let new_fields =
      Set.to_list new_keys |> List.map ~f:(fun k -> k, Hashtbl.find_exn menv k)
    in
    let mod_ty = TModule new_fields in
    Hashtbl.set env ~key:mname ~data:(Scheme ([], mod_ty))
  | SOpen _mname ->
    (* For brevity, omitted. You'd unify or copy names in real code. *)
    raise (Type_error "Open not implemented in this example.")
  | SExpr e ->
    let t = infer_expr env e in
    print_endline "Expression type:";
    show_type t |> print_endline;
    ignore t
;;

(***************************************************************************)
(* 12) Add builtins and entry-point                                        *)
(***************************************************************************)
let add_builtins (env : (string, scheme) Hashtbl.t) : unit =
  let alpha = fresh_tvar () in
  let print_ty = TFun ([ TArray alpha ], TUnit) in
  Hashtbl.set env ~key:"print" ~data:(generalize env print_ty);
  let to_string_ty = TFun ([ TDynamic ], TString) in
  Hashtbl.set env ~key:"to_string" ~data:(generalize env to_string_ty);
  let eq_arg = fresh_tvar () in
  let eq_ty = TFun ([ eq_arg; eq_arg ], TBool) in
  Hashtbl.set env ~key:"==" ~data:(generalize env eq_ty);
  let leq_arg = fresh_tvar () in
  let leq_ty = TFun ([ leq_arg; leq_arg ], TBool) in
  Hashtbl.set env ~key:"<=" ~data:(generalize env leq_ty);
  let geq_arg = fresh_tvar () in
  let geq_ty = TFun ([ geq_arg; geq_arg ], TBool) in
  Hashtbl.set env ~key:">=" ~data:(generalize env geq_ty);
  let less_arg = fresh_tvar () in
  let less_ty = TFun ([ less_arg; less_arg ], TBool) in
  Hashtbl.set env ~key:"<" ~data:(generalize env less_ty);
  let greater_arg = fresh_tvar () in
  let greater_ty = TFun ([ greater_arg; greater_arg ], TBool) in
  Hashtbl.set env ~key:">" ~data:(generalize env greater_ty);
  let alpha1 = fresh_tvar () in
  let plus_ty = TFun ([ alpha1; alpha1 ], alpha1) in
  Hashtbl.set env ~key:"+" ~data:(generalize env plus_ty);
  let alpha2 = fresh_tvar () in
  let minus_ty = TFun ([ alpha2; alpha2 ], alpha2) in
  Hashtbl.set env ~key:"-" ~data:(generalize env minus_ty);
  let alpha3 = fresh_tvar () in
  let times_ty = TFun ([ alpha3; alpha3 ], alpha3) in
  Hashtbl.set env ~key:"*" ~data:(generalize env times_ty);
  let alpha4 = fresh_tvar () in
  let div_ty = TFun ([ alpha4; alpha4 ], alpha4) in
  Hashtbl.set env ~key:"/" ~data:(generalize env div_ty);
  let sum_ty = TFun ([ TArray TInt ], TInt) in
  Hashtbl.set env ~key:"sum" ~data:(generalize env sum_ty);
  let len_arg = fresh_tvar () in
  let length_ty = TFun ([ TArray len_arg ], TInt) in
  Hashtbl.set env ~key:"length" ~data:(generalize env length_ty)
;;

let infer_program (prog : program) : unit =
  let env = Hashtbl.create (module String) in
  add_builtins env;
  try
    List.iter prog ~f:(infer_stmt env);
    Printf.printf "Type checking succeeded!\n"
  with
  | Type_error msg ->
    Printf.eprintf "Type error: %s\n" msg;
    exit 1
;;
