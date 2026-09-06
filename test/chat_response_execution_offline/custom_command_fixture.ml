let write channel text =
  output_string channel text;
  flush channel
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; "delayed" ] ->
    write stdout "first";
    Unix.sleepf 0.05;
    write stderr "second";
    Unix.sleepf 0.05
  | [ _; "both" ] ->
    write stdout "out";
    write stderr "err"
  | [ _; "empty" ] -> ()
  | [ _; "utf8-split" ] ->
    write stdout "\xC3";
    Unix.sleepf 0.05;
    write stdout "\xA9"
  | [ _; "limit" ] ->
    write stdout (String.make 999_999 'a');
    write stdout "beyond"
  | [ _; "hang" ] -> Unix.sleepf 1.
  | _ -> failwith "unknown fixture mode"
;;
