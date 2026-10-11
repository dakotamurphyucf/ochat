open Core
module Id = Chatml_git_host.Object_id

module Mode = struct
  type t =
    | Tree
    | Regular
    | Executable
    | Symlink
    | Gitlink
    | Unsupported
  [@@deriving equal, sexp_of]

  let of_int = function
    | 0o40000 -> Tree
    | 0o100644 -> Regular
    | 0o100755 -> Executable
    | 0o120000 -> Symlink
    | 0o160000 -> Gitlink
    | _ -> Unsupported
  ;;
end

module Entry = struct
  type t =
    { name : string
    ; id : Id.t
    ; mode : Mode.t
    ; directory_marker : bool
    }

  let name t = t.name
  let id t = t.id
  let mode t = t.mode
end

module Error = struct
  type t =
    | Wrong_kind
    | Malformed
    | Entry_limit
    | Name_limit
  [@@deriving equal, sexp_of]
end

type t = Entry.t list

let entries t = t

let ordered first second =
  let a = Entry.name first
  and b = Entry.name second in
  let al = String.length a
  and bl = String.length b in
  let rec compare i =
    let ac =
      if i < al then Char.to_int a.[i] else if first.Entry.directory_marker then 47 else 0
    in
    let bc =
      if i < bl
      then Char.to_int b.[i]
      else if second.Entry.directory_marker
      then 47
      else 0
    in
    if ac <> bc then ac < bc else if i >= al || i >= bl then false else compare (i + 1)
  in
  compare 0
;;

let decode object_ ~max_entries ~checkpoint =
  if not (Git_object.Kind.equal (Git_object.kind object_) Tree)
  then Error Error.Wrong_kind
  else (
    let text = Git_object.body object_ in
    let bytes = String.length text in
    let format = Id.object_format (Git_object.id object_) in
    let width =
      match format with
      | Sha1 -> 20
      | Sha256 -> 32
    in
    let rec mode offset value digits =
      if offset >= bytes || digits > 11 || value > 0xffffffff
      then Error Error.Malformed
      else if Char.equal text.[offset] ' '
      then if digits = 0 then Error Error.Malformed else Ok (value, offset + 1)
      else (
        let digit = Char.to_int text.[offset] - 48 in
        if digit < 0 || digit > 7 || value > (Int.max_value - digit) / 8
        then Error Error.Malformed
        else mode (offset + 1) ((value * 8) + digit) (digits + 1))
    in
    let rec name start offset =
      if offset >= bytes
      then Error Error.Malformed
      else if Char.equal text.[offset] '\000'
      then if offset = start then Error Error.Malformed else Ok offset
      else if offset - start >= 1024
      then Error Error.Name_limit
      else if Char.equal text.[offset] '/'
      then Error Error.Malformed
      else name start (offset + 1)
    in
    let rec loop offset count seen previous acc =
      checkpoint ();
      if offset = bytes
      then
        Ok
          (List.fold acc ~init:[] ~f:(fun result entry ->
             checkpoint ();
             entry :: result))
      else if count >= max_entries
      then Error Error.Entry_limit
      else
        let open Result.Let_syntax in
        let%bind raw_mode, start = mode offset 0 0 in
        let%bind stop = name start start in
        let oid_start = stop + 1 in
        if width > bytes - oid_start
        then Error Error.Malformed
        else (
          let name = String.sub text ~pos:start ~len:(stop - start) in
          if String.equal name "." || String.equal name ".." || Set.mem seen name
          then Error Error.Malformed
          else (
            let hex = Bytes.create (width * 2) in
            for i = 0 to width - 1 do
              let byte = Char.to_int text.[oid_start + i] in
              Bytes.set hex (i * 2) "0123456789abcdef".[byte lsr 4];
              Bytes.set hex ((i * 2) + 1) "0123456789abcdef".[byte land 15]
            done;
            let id =
              Id.create ~object_format:format (Bytes.to_string hex) |> Or_error.ok_exn
            in
            let entry =
              { Entry.name
              ; id
              ; mode = Mode.of_int raw_mode
              ; directory_marker = raw_mode land 0o170000 = 0o40000
              }
            in
            if Option.exists previous ~f:(fun first -> not (ordered first entry))
            then Error Error.Malformed
            else
              loop
                (oid_start + width)
                (count + 1)
                (Set.add seen name)
                (Some entry)
                (entry :: acc)))
    in
    loop 0 0 String.Set.empty None [])
;;

let find t ~name ~checkpoint =
  List.find t ~f:(fun entry ->
    checkpoint ();
    String.equal (Entry.name entry) name)
;;
