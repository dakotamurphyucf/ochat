open! Core

let effect_of_string = function
  | "network" -> Shell_access.Effect.Network
  | "child_processes" -> Child_processes
  | "arbitrary_code" -> Arbitrary_code
  | "privilege_change" -> Privilege_change
  | value when String.is_prefix value ~prefix:"read:" -> Read_path (String.drop_prefix value 5)
  | value when String.is_prefix value ~prefix:"write:" -> Write_path (String.drop_prefix value 6)
  | value -> Unknown value
;;

let analyze worker context =
  Hook_worker.invoke worker (Hook_payload.context ~event:"effect_analysis" context)
  |> Result.bind ~f:(function
    | Hook_protocol.Add_effects values ->
      Ok (Shell_access.Analyzer.Add (List.map values ~f:effect_of_string))
    | Replace_effects values -> Ok (Replace (List.map values ~f:effect_of_string))
    | _ ->
      Error Hook_worker.{ code = "shell.hook_action"; message = "invalid analyzer action"; stderr = None })
;;
