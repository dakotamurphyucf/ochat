open Core

(**
 * filter_lines is a function that filters lines from an input file and writes the filtered lines to an output file.
 *
 * @param input_file The file to read lines from.
 * @param output_file The file to write filtered lines to.
 * @param condition The condition to filter lines by.
 *)
let filter_lines ~input_file ~output_file ~condition =
  let lines = In_channel.read_lines input_file in
  let filtered_lines = List.filter lines ~f:condition in
  Out_channel.write_lines output_file filtered_lines