open Core

type t =
  { header : string option
  ; body : string
  ; footnotes : string list
  ; metadata : (string * string) list
  }

let make ?header ?(footnotes = []) ?(metadata = []) ~body () =
  { header; body; footnotes; metadata }
;;

let to_string (p : t) : string =
  let segments = ref [] in
  (match p.header with
   | Some h when not (String.is_empty (String.strip h)) -> segments := h :: !segments
   | _ -> ());
  segments := p.body :: !segments;
  (match p.footnotes with
   | [] -> ()
   | lst -> segments := String.concat lst ~sep:"\n---\n" :: !segments);
  if not (List.is_empty p.metadata)
  then (
    let kv_lines =
      p.metadata
      |> List.rev (* keep original insertion order *)
      |> List.map ~f:(fun (k, v) -> Printf.sprintf "<!-- %s: %s -->" k v)
      |> String.concat ~sep:"\n"
    in
    segments := kv_lines :: !segments);
  !segments |> List.rev |> String.concat ~sep:"\n"
;;

let add_metadata (p : t) ~key ~value : t =
  { p with metadata = (key, value) :: p.metadata }
;;
