open Core

type t = float list -> float

let return_default = 0.0

let mean (xs : float list) : float =
  match xs with
  | [] -> return_default
  | _ -> List.sum (module Float) xs ~f:Fn.id /. Float.of_int (List.length xs)
;;

let median (xs : float list) : float =
  match xs with
  | [] -> return_default
  | [ _ ] as single -> List.hd_exn single
  | _ ->
    let sorted = List.sort xs ~compare:Float.compare in
    let n = List.length sorted in
    let mid = n / 2 in
    if Int.(n % 2 = 1)
    then List.nth_exn sorted mid
    else (
      let a = List.nth_exn sorted (mid - 1) in
      let b = List.nth_exn sorted mid in
      (a +. b) /. 2.)
;;

let trimmed_mean ~(trim : float) : t =
  if Float.(trim < 0.) || Float.(trim >= 0.5)
  then invalid_arg "Aggregator.trimmed_mean: trim must be in [0, 0.5).";
  fun xs ->
    match xs with
    | [] -> return_default
    | _ ->
      let sorted = List.sort xs ~compare:Float.compare in
      let n = List.length sorted in
      let k = Int.of_float (Float.of_int n *. trim) in
      let trimmed_front = List.drop sorted k in
      let trimmed_len = List.length trimmed_front in
      let trimmed =
        if k = 0
        then trimmed_front
        else if trimmed_len <= k
        then []
        else List.take trimmed_front (trimmed_len - k)
      in
      mean trimmed
;;

let weighted ~(weights : float list) : t =
  fun xs ->
  let len_scores = List.length xs in
  let len_weights = List.length weights in
  if len_scores = 0
  then return_default
  else if len_scores <> len_weights
  then mean xs
  else (
    let weighted_sum, total_w =
      List.fold2_exn xs weights ~init:(0.0, 0.0) ~f:(fun (acc, wacc) x w ->
        acc +. (x *. w), wacc +. w)
    in
    if Float.(total_w = 0.) then return_default else weighted_sum /. total_w)
;;

let min (xs : float list) : float =
  match xs with
  | [] -> return_default
  | _ -> List.fold xs ~init:Float.infinity ~f:Float.min
;;

let max (xs : float list) : float =
  match xs with
  | [] -> return_default
  | _ -> List.fold xs ~init:(-.Float.infinity) ~f:Float.max
;;
