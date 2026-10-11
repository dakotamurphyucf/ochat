open Core
module Shape_kind = Chatml.Chatml_shape_kind
module Equality_requirement = Chatml.Chatml_equality_requirement

module Env = struct
  type 'a t = 'a String.Map.t

  let empty : 'a t = String.Map.empty
  let add k v m : 'a t = Map.set m ~key:k ~data:v
  let find m k = Map.find m k
  let find_exn m k = Map.find_exn m k
  let of_list l = List.fold l ~init:empty ~f:(fun acc (k, v) -> add k v acc)
  let merge m1 m2 = Map.merge_skewed m1 m2 ~combine:(fun ~key:_ v_left _v_right -> v_left)
  let is_empty = Map.is_empty
  let bindings m = Map.to_alist m
  let map m ~f = Map.map m ~f
  let iter m ~f = Map.iteri m ~f:(fun ~key ~data -> f key data)

  let choose m =
    match Map.min_elt m with
    | None -> raise_notrace (Invalid_argument "empty map")
    | Some (k, v) -> k, v
  ;;

  let exists m f = Map.existsi m ~f:(fun ~key ~data -> f key data)
  let fold m ~init ~f = Map.fold m ~init ~f:(fun ~key ~data acc -> f ~key ~data acc)
  let singleton = String.Map.singleton
end

(** --------------------------------------------------------------------- *)

(** 2. Types and type variables                                            *)

(** --------------------------------------------------------------------- *)

type name = string

type typ =
  | Fun of typ list * typ
  | Generic of
      { name : name
      ; equality : Equality_requirement.t option
      }
  | Var of tv ref
  | Con of string * typ list
  | Mu of name * typ
  | Rec_var of name
  | Ref of typ
  | Record of typ (* row *)
  | Module of typ (* exported value row *)
  | Field_receiver of typ * typ (* representation kind, accessible row *)
  | Shape_kind of Shape_kind.t
  | Variant of typ (* row with Variant_payload fields *)
  | Variant_payload of typ list (* constructor arguments, never a value type *)
  | Tuple of typ list
  | Array of typ
  | TInt
  | TFloat
  | Boolean
  | String
  | Row of typ Env.t * typ (* fields * tail *)
  | Empty_row
  | Unit

and tv =
  | Bound of typ
  | Free of
      { name : name
      ; level : int
      ; equality : Equality_requirement.t option
      }
