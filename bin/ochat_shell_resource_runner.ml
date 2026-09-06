open! Core
module Limit = Core_unix.RLimit.Limit

let set resource value =
  let limit = Limit.Limit (Int64.of_int value) in
  Core_unix.RLimit.set resource { cur = limit; max = limit }
;;

let rec parse limits = function
  | "--" :: executable :: arguments -> List.rev limits, executable, arguments
  | "--cpu" :: value :: rest -> parse ((`Cpu, Int.of_string value) :: limits) rest
  | "--memory" :: value :: rest -> parse ((`Memory, Int.of_string value) :: limits) rest
  | "--file-size" :: value :: rest ->
    parse ((`File_size, Int.of_string value) :: limits) rest
  | "--open-files" :: value :: rest ->
    parse ((`Open_files, Int.of_string value) :: limits) rest
  | [] -> failwith "missing -- executable [arguments...]"
  | option :: _ -> failwith ("invalid resource runner option: " ^ option)
;;

let apply = function
  | `Cpu, value -> set Core_unix.RLimit.cpu_seconds value
  | `File_size, value -> set Core_unix.RLimit.file_size value
  | `Open_files, value -> set Core_unix.RLimit.num_file_descriptors value
  | `Memory, value ->
    (match Core_unix.RLimit.virtual_memory with
     | Ok resource -> set resource value
     | Error error -> failwith (Error.to_string_hum error))
;;

let () =
  let arguments = Sys.get_argv () |> Array.to_list |> List.tl_exn in
  let limits, executable, arguments = parse [] arguments in
  List.iter limits ~f:apply;
  never_returns
    (Core_unix.exec ~prog:executable ~argv:(executable :: arguments) ~use_path:false ())
;;
