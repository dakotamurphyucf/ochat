open Core

let%test_unit "fresh strings retain their regex captures across collections" =
  let regex =
    Oniguruma.create
      "^(one)(two)[0-9]+$"
      Oniguruma.Options.none
      Oniguruma.Encoding.utf8
      (Oniguruma.Syntax.default ())
    |> Result.ok_or_failwith
  in
  let misses = ref 0
  and bad_captures = ref 0 in
  for index = 0 to 200_000 do
    let text = "onetwo" ^ Int.to_string index in
    match Oniguruma.match_ regex text 0 Oniguruma.Options.none with
    | None -> incr misses
    | Some region ->
      if
        Oniguruma.Region.capture_beg region 1 <> 0
        || Oniguruma.Region.capture_end region 1 <> 3
        || Oniguruma.Region.capture_beg region 2 <> 3
        || Oniguruma.Region.capture_end region 2 <> 6
      then incr bad_captures
  done;
  [%test_eq: int] 0 !misses;
  [%test_eq: int] 0 !bad_captures
;;
