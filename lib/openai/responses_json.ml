open! Core

let valid_number text =
  let length = String.length text in
  let rec digits position =
    if position < length && Char.is_digit text.[position]
    then digits (position + 1)
    else position
  in
  let start = if length > 0 && Char.equal text.[0] '-' then 1 else 0 in
  if start >= length
  then false
  else (
    let integer_end =
      if Char.equal text.[start] '0'
      then start + 1
      else if Char.(text.[start] >= '1' && text.[start] <= '9')
      then digits (start + 1)
      else start
    in
    if integer_end = start
    then false
    else (
      let fraction_end =
        if integer_end < length && Char.equal text.[integer_end] '.'
        then (
          let after = digits (integer_end + 1) in
          if after = integer_end + 1 then -1 else after)
        else integer_end
      in
      if fraction_end < 0
      then false
      else (
        let exponent_end =
          if
            fraction_end < length
            && (Char.equal text.[fraction_end] 'e' || Char.equal text.[fraction_end] 'E')
          then (
            let start = fraction_end + 1 in
            let start =
              if
                start < length
                && (Char.equal text.[start] '+' || Char.equal text.[start] '-')
              then start + 1
              else start
            in
            let after = digits start in
            if after = start then -1 else after)
          else fraction_end
        in
        exponent_end = length)))
;;
