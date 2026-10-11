open! Core
module P = Chatml_binding_pattern

type mismatch =
  { span : Source.span
  ; message : string
  }

let extract ?(checkpoint = fun () -> ()) pattern value =
  let rec collect result (pattern : P.t) value =
    checkpoint ();
    match pattern.value, value with
    | Name name, _ -> Ok ((name, value) :: result)
    | Wildcard, _ -> Ok result
    | Tuple patterns, Chatml_lang.VTuple values ->
      (match List.zip (Chatml_tuple.to_list patterns) (Chatml_tuple.to_list values) with
       | Unequal_lengths ->
         Error { span = pattern.span; message = "Tuple binding arity mismatch" }
       | Ok children ->
         List.fold_result children ~init:result ~f:(fun result (pattern, value) ->
           collect result pattern value))
    | Tuple _, _ ->
      Error { span = pattern.span; message = "Tuple binding requires a tuple value" }
  in
  Result.map (collect [] pattern value) ~f:List.rev
;;
