open! Core

module Command = struct
  type t =
    { program : string
    ; arguments : string list
    }

  let create program arguments =
    if String.equal program ""
    then invalid_arg "Shell_access.Command.create: empty program";
    if List.exists (program :: arguments) ~f:(fun value -> String.contains value '\000')
    then invalid_arg "Shell_access.Command.create: NUL byte in argv";
    { program; arguments }
  ;;

  let equal a b =
    String.equal a.program b.program && List.equal String.equal a.arguments b.arguments
  ;;

  let basename t = Filename.basename t.program
  let to_argv t = t.program :: t.arguments

  let quote word =
    if String.equal word ""
    then "''"
    else if
      String.for_all word ~f:(function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | '/' | ':' -> true
        | _ -> false)
    then word
    else "'" ^ String.concat ~sep:"'\\''" (String.split word ~on:'\'') ^ "'"
  ;;

  let to_string t = String.concat ~sep:" " (List.map (to_argv t) ~f:quote)
end

module Chain = struct
  type condition =
    | Always
    | On_success
    | On_failure

  type pipeline = Command.t list

  type t =
    { first : pipeline
    ; rest : (condition * pipeline) list
    }

  type parse_error =
    { offset : int
    ; message : string
    }

  type token =
    | Word of string * int
    | Pipe of int
    | And of int
    | Or of int
    | Semi of int

  exception Parse_error of parse_error

  let fail offset message = raise (Parse_error { offset; message })
  let single command = { first = [ command ]; rest = [] }

  let create ~first ~rest =
    if List.exists (first :: List.map rest ~f:snd) ~f:List.is_empty
    then Error "shell chain pipelines must not be empty"
    else Ok { first; rest }
  ;;

  let commands t = t.first @ List.concat_map t.rest ~f:snd

  let tokenize input =
    let length = String.length input in
    let word = Buffer.create 32 in
    let word_start = ref 0 in
    let started = ref false in
    let tokens = ref [] in
    let start i =
      if not !started
      then (
        started := true;
        word_start := i)
    in
    let add i c =
      start i;
      Buffer.add_char word c
    in
    let flush () =
      if !started
      then (
        tokens := Word (Buffer.contents word, !word_start) :: !tokens;
        Buffer.clear word;
        started := false)
    in
    let rec single_quote origin i =
      if i >= length
      then fail origin "unterminated single quote"
      else if Char.equal input.[i] '\''
      then i + 1
      else (
        add i input.[i];
        single_quote origin (i + 1))
    and double_quote origin i =
      if i >= length
      then fail origin "unterminated double quote"
      else (
        match input.[i] with
        | '"' -> i + 1
        | '\\' ->
          if i + 1 >= length then fail i "trailing escape in double quote";
          add i input.[i + 1];
          double_quote origin (i + 2)
        | '`' -> fail i "command substitution is not supported"
        | '$' when i + 1 < length && Char.equal input.[i + 1] '(' ->
          fail i "command substitution is not supported"
        | c ->
          add i c;
          double_quote origin (i + 1))
    and loop i =
      if i >= length
      then (
        flush ();
        List.rev !tokens)
      else (
        match input.[i] with
        | ' ' | '\t' | '\r' | '\n' ->
          flush ();
          loop (i + 1)
        | '\'' ->
          start i;
          loop (single_quote i (i + 1))
        | '"' ->
          start i;
          loop (double_quote i (i + 1))
        | '\\' ->
          if i + 1 >= length then fail i "trailing escape";
          add i input.[i + 1];
          loop (i + 2)
        | ';' ->
          flush ();
          tokens := Semi i :: !tokens;
          loop (i + 1)
        | '|' ->
          flush ();
          if i + 1 < length && Char.equal input.[i + 1] '|'
          then (
            tokens := Or i :: !tokens;
            loop (i + 2))
          else (
            tokens := Pipe i :: !tokens;
            loop (i + 1))
        | '&' ->
          flush ();
          if i + 1 < length && Char.equal input.[i + 1] '&'
          then (
            tokens := And i :: !tokens;
            loop (i + 2))
          else fail i "background execution is not supported"
        | '<' | '>' -> fail i "redirection is not supported by structured execution"
        | '`' -> fail i "command substitution is not supported"
        | '$' when i + 1 < length && Char.equal input.[i + 1] '(' ->
          fail i "command substitution is not supported"
        | '(' | ')' | '{' | '}' -> fail i "shell grouping is not supported"
        | c ->
          add i c;
          loop (i + 1))
    in
    loop 0
  ;;

  let token_offset = function
    | Word (_, offset) | Pipe offset | And offset | Or offset | Semi offset -> offset
  ;;

  let parse_tokens tokens =
    let rec command words = function
      | Word (word, _offset) :: rest -> command (word :: words) rest
      | rest ->
        (match List.rev words with
         | [] ->
           let offset =
             match rest with
             | [] -> 0
             | token :: _ -> token_offset token
           in
           fail offset "expected a command"
         | program :: arguments -> Command.create program arguments, rest)
    in
    let rec pipeline commands tokens =
      let command, tokens = command [] tokens in
      match tokens with
      | Pipe _ :: rest -> pipeline (command :: commands) rest
      | _ -> List.rev (command :: commands), tokens
    in
    let first, tokens = pipeline [] tokens in
    let rec rest acc = function
      | [] -> { first; rest = List.rev acc }
      | ((Semi _ | And _ | Or _) as separator) :: tokens ->
        let condition =
          match separator with
          | Semi _ -> Always
          | And _ -> On_success
          | Or _ -> On_failure
          | _ -> assert false
        in
        let next, tokens = pipeline [] tokens in
        rest ((condition, next) :: acc) tokens
      | Pipe offset :: _ -> fail offset "unexpected pipeline separator"
      | Word (_, offset) :: _ -> fail offset "missing command separator"
    in
    rest [] tokens
  ;;

  let parse input =
    try
      match tokenize input with
      | [] -> Error { offset = 0; message = "empty command" }
      | xs -> Ok (parse_tokens xs)
    with
    | Parse_error error -> Error error
  ;;

  let parse_error_to_string { offset; message } =
    Printf.sprintf "shell parse error at byte %d: %s" offset message
  ;;
end

module Input = struct
  type t =
    | Empty
    | Text of string
    | Bytes of Bigstring.t

  let byte_length = function
    | Empty -> 0
    | Text value -> String.length value
    | Bytes value -> Bigstring.length value
  ;;

  let to_string = function
    | Empty -> ""
    | Text value -> value
    | Bytes value -> Bigstring.to_string value
  ;;

  let sha256 = function
    | Empty -> None
    | (Text _ | Bytes _) as input ->
      Some Digestif.SHA256.(to_hex (digest_string (to_string input)))
  ;;
end

module Limits = struct
  type t =
    { wall_time_seconds : float
    ; idle_time_seconds : float option
    ; max_stdin_bytes : int
    ; max_stdout_bytes : int
    ; max_stderr_bytes : int
    ; max_total_bytes : int
    ; cpu_seconds : int option
    ; memory_bytes : int option
    ; file_size_bytes : int option
    ; open_files : int option
    }

  let default =
    { wall_time_seconds = 180.
    ; idle_time_seconds = Some 60.
    ; max_stdin_bytes = 1_048_576
    ; max_stdout_bytes = 1_000_000
    ; max_stderr_bytes = 1_000_000
    ; max_total_bytes = 1_500_000
    ; cpu_seconds = None
    ; memory_bytes = None
    ; file_size_bytes = None
    ; open_files = None
    }
  ;;
end

module Capabilities = struct
  type sandbox =
    | Required
    | Preferred
    | Direct_unsafe

  type t =
    { read_roots : string list
    ; write_roots : string list
    ; network : bool
    ; allow_child_processes : bool
    ; allow_arbitrary_code : bool
    ; allow_privilege_change : bool
    ; sandbox : sandbox
    }

  let read_only ~roots =
    { read_roots = roots
    ; write_roots = []
    ; network = false
    ; allow_child_processes = false
    ; allow_arbitrary_code = false
    ; allow_privilege_change = false
    ; sandbox = Required
    }
  ;;

  let development ~workspace =
    { read_roots = [ workspace ]
    ; write_roots = [ workspace ]
    ; network = false
    ; allow_child_processes = true
    ; allow_arbitrary_code = true
    ; allow_privilege_change = false
    ; sandbox = Preferred
    }
  ;;
end

module Executable = struct
  type fingerprint =
    { device : int
    ; inode : int
    ; mode : int
    ; uid : int
    ; gid : int
    ; size : int64
    ; mtime : float
    ; sha256 : string
    }

  type t =
    { requested : string
    ; path : string
    ; canonical_path : string
    ; trusted : bool
    ; fingerprint : fingerprint
    }

  let same_fingerprint a b =
    Int.equal a.device b.device
    && Int.equal a.inode b.inode
    && Int.equal a.mode b.mode
    && Int.equal a.uid b.uid
    && Int.equal a.gid b.gid
    && Int64.equal a.size b.size
    && Float.equal a.mtime b.mtime
    && String.equal a.sha256 b.sha256
  ;;
end

module Path_util = struct
  let eio_path fs path = Eio.Path.(fs / path)

  let file_exists ~fs path =
    try
      match Eio.Path.kind ~follow:true (eio_path fs path) with
      | `Not_found -> false
      | _ -> true
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | _ -> false
  ;;

  let absolute ~cwd path =
    if Filename.is_relative path then Filename.concat cwd path else path
  ;;

  let normalize path =
    let absolute = not (Filename.is_relative path) in
    let parts = String.split path ~on:'/' in
    let parts =
      List.fold parts ~init:[] ~f:(fun acc -> function
        | "" | "." -> acc
        | ".." ->
          (match acc with
           | [] -> []
           | _ :: rest -> rest)
        | part -> part :: acc)
      |> List.rev
    in
    (if absolute then "/" else "") ^ String.concat ~sep:"/" parts
  ;;

  let components path =
    String.split path ~on:'/'
    |> List.filter ~f:(fun component -> not (String.is_empty component))
  ;;

  let canonical ~fs path =
    let rec loop ~links resolved pending =
      if links > 40 then failwith ("too many symbolic links while resolving " ^ path);
      match pending with
      | [] -> "/" ^ String.concat ~sep:"/" (List.rev resolved)
      | "" :: rest | "." :: rest -> loop ~links resolved rest
      | ".." :: rest -> loop ~links (Option.value (List.tl resolved) ~default:[]) rest
      | component :: rest ->
        let candidate = "/" ^ String.concat ~sep:"/" (List.rev (component :: resolved)) in
        (match Eio.Path.kind ~follow:false (eio_path fs candidate) with
         | `Symbolic_link ->
           let target = Eio.Path.read_link (eio_path fs candidate) in
           let resolved, pending =
             if Filename.is_relative target
             then resolved, components target @ rest
             else [], components target @ rest
           in
           loop ~links:(links + 1) resolved pending
         | `Not_found -> failwith ("path does not exist: " ^ candidate)
         | _ -> loop ~links (component :: resolved) rest)
    in
    let absolute = if Filename.is_relative path then "/" ^ path else path in
    loop ~links:0 [] (components absolute)
  ;;

  let canonical_or_parent ~fs path =
    let rec loop suffix candidate =
      if file_exists ~fs candidate
      then (
        let base = canonical ~fs candidate in
        List.fold suffix ~init:base ~f:Filename.concat)
      else (
        let parent = Filename.dirname candidate in
        if String.equal parent candidate
        then normalize path
        else loop (Filename.basename candidate :: suffix) parent)
    in
    loop [] path
  ;;

  let contains ~fs ~root path =
    let root = canonical_or_parent ~fs root |> normalize in
    let path = canonical_or_parent ~fs path |> normalize in
    String.equal root path
    || (String.length path > String.length root
        && String.is_prefix
             path
             ~prefix:(if String.equal root "/" then "/" else root ^ "/"))
  ;;
end

module Resolver = struct
  type pinned =
    { path : string
    ; sha256 : string option
    ; trusted : bool
    }

  type t =
    { trusted_roots : string list
    ; search_path : string list option
    ; executables : pinned String.Map.t
    }

  let create
        ?(trusted_roots = [ "/bin"; "/usr/bin"; "/usr/sbin"; "/sbin" ])
        ?search_path
        ?(executables = [])
        ()
    =
    { trusted_roots; search_path; executables = String.Map.of_alist_exn executables }
  ;;

  let env_value key environment =
    let prefix = key ^ "=" in
    Array.find_map environment ~f:(fun entry ->
      if String.is_prefix entry ~prefix
      then Some (String.drop_prefix entry (String.length prefix))
      else None)
  ;;

  let hash_source source =
    let buffer = Cstruct.create 65536 in
    let rec loop context =
      match Eio.Flow.single_read source buffer with
      | 0 -> Digestif.SHA256.(to_hex (get context))
      | count ->
        let bytes = Cstruct.to_bytes (Cstruct.sub buffer 0 count) in
        loop Digestif.SHA256.(feed_bytes context bytes)
      | exception End_of_file -> Digestif.SHA256.(to_hex (get context))
    in
    loop Digestif.SHA256.empty
  ;;

  let fingerprint ~fs path =
    try
      Eio.Path.with_open_in (Path_util.eio_path fs path) (fun source ->
        let stat = Eio.File.stat source in
        match stat.kind with
        | `Regular_file ->
          if stat.perm land 0o111 = 0
          then Error (path ^ " is not executable")
          else (
            let sha256 = hash_source source in
            let current = Eio.File.stat source in
            if
              not
                (Int64.equal stat.dev current.dev
                 && Int64.equal stat.ino current.ino
                 && Int64.equal
                      (Optint.Int63.to_int64 stat.size)
                      (Optint.Int63.to_int64 current.size)
                 && Float.equal stat.mtime current.mtime)
            then Error (path ^ " changed while it was being fingerprinted")
            else
              Ok
                Executable.
                  { device = Int64.to_int_exn stat.dev
                  ; inode = Int64.to_int_exn stat.ino
                  ; mode = stat.perm
                  ; uid = Int64.to_int_exn stat.uid
                  ; gid = Int64.to_int_exn stat.gid
                  ; size = Optint.Int63.to_int64 stat.size
                  ; mtime = stat.mtime
                  ; sha256
                  })
        | _ -> Error (path ^ " is not a regular file"))
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      Error (Printf.sprintf "failed to fingerprint %s: %s" path (Exn.to_string exn))
  ;;

  let fingerprinted_executable t ~fs ~requested ~path ~trusted ~sha256 =
    try
      let canonical_path = Path_util.canonical ~fs path in
      match fingerprint ~fs canonical_path with
      | Error _ as error -> error
      | Ok fingerprint ->
        (match sha256 with
         | Some expected when not (String.Caseless.equal expected fingerprint.sha256) ->
           Error ("executable digest mismatch: " ^ requested)
         | Some _ | None ->
           let trusted =
             (trusted
              || List.exists t.trusted_roots ~f:(fun root ->
                Path_util.contains ~fs ~root canonical_path))
             && fingerprint.mode land 0o022 = 0
           in
           Ok Executable.{ requested; path; canonical_path; trusted; fingerprint })
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      Error (Printf.sprintf "failed to resolve %S: %s" requested (Exn.to_string exn))
  ;;

  let resolve_search t ~fs ~cwd ~environment command =
    let program = command.Command.program in
    let search_path =
      match t.search_path with
      | Some path -> path
      | None ->
        env_value "PATH" environment
        |> Option.value ~default:"/usr/bin:/bin:/usr/sbin:/sbin"
        |> String.split ~on:':'
    in
    let candidates =
      if String.contains program '/'
      then [ Path_util.absolute ~cwd program ]
      else List.map search_path ~f:(fun directory -> Filename.concat directory program)
    in
    match List.find candidates ~f:(Path_util.file_exists ~fs) with
    | None -> Error (Printf.sprintf "executable %S was not found" program)
    | Some path ->
      fingerprinted_executable t ~fs ~requested:program ~path ~trusted:false ~sha256:None
  ;;

  let resolve t ~fs ~cwd ~environment command =
    let program = command.Command.program in
    match Map.find t.executables program with
    | None -> resolve_search t ~fs ~cwd ~environment command
    | Some pinned ->
      fingerprinted_executable
        t
        ~fs
        ~requested:program
        ~path:pinned.path
        ~trusted:pinned.trusted
        ~sha256:pinned.sha256
  ;;

  let verify ~fs executable =
    match fingerprint ~fs executable.Executable.canonical_path with
    | Error _ as error -> error
    | Ok current ->
      if Executable.same_fingerprint executable.fingerprint current
      then Ok ()
      else Error (executable.canonical_path ^ " changed after authorization")
  ;;
end

module Effect = struct
  type kind =
    | Read_path of string
    | Write_path of string
    | Network
    | Child_processes
    | Arbitrary_code
    | Privilege_change
    | Unknown of string

  type t = kind list

  let non_options arguments =
    let rec loop after_dash acc = function
      | [] -> List.rev acc
      | "--" :: rest -> loop true acc rest
      | arg :: rest when after_dash || not (String.is_prefix arg ~prefix:"-") ->
        loop after_dash (arg :: acc) rest
      | _ :: rest -> loop after_dash acc rest
    in
    loop false [] arguments
  ;;

  let paths constructor cwd paths =
    List.map paths ~f:(fun path -> constructor (Path_util.absolute ~cwd path))
  ;;

  let analyze ~raw_shell ~cwd command =
    let name = String.lowercase (Command.basename command) in
    let args = command.arguments in
    let positional = non_options args in
    if raw_shell
    then [ Arbitrary_code; Child_processes; Unknown "opaque shell program" ]
    else if
      List.mem
        [ "sh"
        ; "bash"
        ; "zsh"
        ; "fish"
        ; "python"
        ; "python3"
        ; "ruby"
        ; "perl"
        ; "node"
        ; "env"
        ; "xargs"
        ]
        name
        ~equal:String.equal
    then [ Arbitrary_code; Child_processes ]
    else if
      List.mem
        [ "sudo"; "su"; "doas"; "chown"; "chmod"; "mount"; "umount" ]
        name
        ~equal:String.equal
    then [ Privilege_change; Write_path cwd; Child_processes ]
    else if
      List.mem
        [ "curl"; "wget"; "ssh"; "scp"; "nc"; "ncat"; "telnet" ]
        name
        ~equal:String.equal
    then
      [ Network; Write_path cwd; Unknown "network command effects depend on arguments" ]
    else (
      match name with
      | "pwd" | "echo" | "printf" | "true" | "false" -> []
      | "cat" | "head" | "tail" | "wc" ->
        paths (fun path -> Read_path path) cwd positional
      | "ls" ->
        paths
          (fun path -> Read_path path)
          cwd
          (if List.is_empty positional then [ "." ] else positional)
      | "grep" ->
        (match positional with
         | _pattern :: files -> paths (fun path -> Read_path path) cwd files
         | [] -> [])
      | "rg" ->
        if
          List.exists args ~f:(fun arg ->
            String.equal arg "--pre" || String.is_prefix arg ~prefix:"--pre=")
        then [ Arbitrary_code; Child_processes ]
        else (
          match positional with
          | _pattern :: files -> paths (fun path -> Read_path path) cwd files
          | [] -> [ Read_path cwd ])
      | "rm" | "rmdir" | "touch" | "mkdir" ->
        paths (fun path -> Write_path path) cwd positional
      | "cp" | "mv" ->
        (match List.rev positional with
         | destination :: reversed_sources ->
           paths (fun path -> Write_path path) cwd [ destination ]
           @ paths (fun path -> Read_path path) cwd (List.rev reversed_sources)
         | [] -> [ Unknown "missing source and destination" ])
      | "find" ->
        if
          List.exists args ~f:(fun arg ->
            List.mem [ "-exec"; "-execdir"; "-ok"; "-okdir" ] arg ~equal:String.equal)
        then [ Read_path cwd; Arbitrary_code; Child_processes ]
        else if List.mem args "-delete" ~equal:String.equal
        then [ Read_path cwd; Write_path cwd ]
        else [ Read_path cwd ]
      | "git" | "cargo" | "make" | "dune" | "npm" | "pnpm" | "opam" ->
        [ Read_path cwd
        ; Write_path cwd
        ; Arbitrary_code
        ; Child_processes
        ; Unknown "build or VCS command may invoke helpers"
        ]
      | _ ->
        [ Arbitrary_code
        ; Child_processes
        ; Unknown ("no effect specification for " ^ name)
        ])
  ;;

  let requires_arbitrary_code effects = List.mem effects Arbitrary_code ~equal:Poly.equal

  let check_capabilities capabilities ~fs ~cwd:_ effects =
    let check = function
      | Read_path path ->
        if
          List.exists capabilities.Capabilities.read_roots ~f:(fun root ->
            Path_util.contains ~fs ~root path)
          || List.exists capabilities.write_roots ~f:(fun root ->
            Path_util.contains ~fs ~root path)
        then Ok ()
        else Error ("read outside allowed roots: " ^ path)
      | Write_path path ->
        if
          List.exists capabilities.write_roots ~f:(fun root ->
            Path_util.contains ~fs ~root path)
        then Ok ()
        else Error ("write outside allowed roots: " ^ path)
      | Network ->
        if capabilities.network then Ok () else Error "network access is disabled"
      | Child_processes ->
        if capabilities.allow_child_processes
        then Ok ()
        else Error "child process creation is disabled"
      | Arbitrary_code ->
        if capabilities.allow_arbitrary_code
        then Ok ()
        else Error "arbitrary code execution is disabled"
      | Privilege_change ->
        if capabilities.allow_privilege_change
        then Ok ()
        else Error "privilege changes are disabled"
      | Unknown reason ->
        if capabilities.allow_arbitrary_code
        then Ok ()
        else Error ("unknown effect requires arbitrary-code capability: " ^ reason)
    in
    let rec loop = function
      | [] -> Ok ()
      | item :: rest ->
        (match check item with
         | Ok () -> loop rest
         | Error _ as error -> error)
    in
    loop effects
  ;;

  let to_strings effects =
    List.map effects ~f:(function
      | Read_path path -> "read:" ^ path
      | Write_path path -> "write:" ^ path
      | Network -> "network"
      | Child_processes -> "child_processes"
      | Arbitrary_code -> "arbitrary_code"
      | Privilege_change -> "privilege_change"
      | Unknown reason -> "unknown:" ^ reason)
  ;;
end

module Context = struct
  type origin =
    | Tool
    | Moderator
    | Host of string

  type request_kind =
    | Structured
    | Script_file
    | Raw_shell

  type stdin_kind =
    | Empty
    | Pipeline
    | Supplied

  type t =
    { request_id : string
    ; runtime_id : string
    ; manifest_sha256 : string
    ; command : Command.t
    ; executable : Executable.t
    ; cwd : string
    ; environment : string array
    ; request_kind : request_kind
    ; stdin_kind : stdin_kind
    ; stdin_sha256 : string option
    ; stdin_bytes : int
    ; script_sha256 : string option
    ; script_preview : string option
    ; origin : origin
    ; effects : Effect.t
    ; capabilities : Capabilities.t
    ; policy_action : string option
    ; policy_matches : string list
    ; session_id : string option
    }
end

module Analyzer = struct
  type result =
    | Add of Effect.t
    | Replace of Effect.t

  type t = Context.t -> (result, string) Result.t
end

module Matcher = struct
  type t = Context.t -> bool

  let any _ = true
  let program expected context = String.equal expected context.Context.command.program

  let basename expected context =
    String.equal expected (Command.basename context.Context.command)
  ;;

  let resolved_path expected context =
    String.equal expected context.Context.executable.canonical_path
  ;;

  let trusted_executable context = context.Context.executable.trusted

  let program_regex pattern =
    try
      let regex = Re.Perl.compile_pat pattern in
      Ok (fun context -> Re.execp regex context.Context.command.program)
    with
    | exn -> Error (Exn.to_string exn)
  ;;

  let rec prefix expected actual =
    match expected, actual with
    | [], _ -> true
    | _, [] -> false
    | x :: xs, y :: ys -> String.equal x y && prefix xs ys
  ;;

  let argv_prefix expected context =
    prefix expected (Command.to_argv context.Context.command)
  ;;

  let argument expected context =
    List.exists context.Context.command.arguments ~f:(String.equal expected)
  ;;

  let argument_contains substring context =
    List.exists context.Context.command.arguments ~f:(fun value ->
      String.length substring = 0 || Re.execp (Re.compile (Re.str substring)) value)
  ;;

  let has_effect predicate context = List.exists context.Context.effects ~f:predicate

  let no_unknown_effects context =
    not
      (List.exists context.Context.effects ~f:(function
         | Effect.Unknown _ -> true
         | _ -> false))
  ;;

  let request_kind expected context = Poly.equal expected context.Context.request_kind
  let all matchers context = List.for_all matchers ~f:(fun matcher -> matcher context)
  let any_of matchers context = List.exists matchers ~f:(fun matcher -> matcher context)
  let negate matcher context = not (matcher context)
  let custom matcher = matcher
  let matches matcher context = matcher context
end

module Policy = struct
  type action =
    | Allow
    | Ask
    | Deny

  type rule =
    { id : string
    ; action : action
    ; reason : string option
    ; matcher : Matcher.t
    }

  type t =
    { default : action
    ; rules : rule list
    }

  type match_ =
    { rule_id : string
    ; action : action
    ; reason : string option
    }

  type decision =
    { action : action
    ; matches : match_ list
    ; reason : string
    }

  let rule ~id ~action ?reason matcher = { id; action; reason; matcher }
  let create ?(default = Ask) rules = { default; rules }

  let rank = function
    | Allow -> 0
    | Ask -> 1
    | Deny -> 2
  ;;

  let string_of_action = function
    | Allow -> "allow"
    | Ask -> "ask"
    | Deny -> "deny"
  ;;

  let evaluate t context =
    let matches =
      List.filter_map t.rules ~f:(fun rule ->
        if Matcher.matches rule.matcher context
        then Some { rule_id = rule.id; action = rule.action; reason = rule.reason }
        else None)
    in
    let action =
      match matches with
      | [] -> t.default
      | _ ->
        List.fold matches ~init:Allow ~f:(fun strongest (matched : match_) ->
          if rank matched.action > rank strongest then matched.action else strongest)
    in
    let reasons =
      List.filter_map matches ~f:(fun (matched : match_) ->
        if Poly.equal matched.action action
        then
          Some
            (match matched.reason with
             | None -> matched.rule_id
             | Some reason -> matched.rule_id ^ ": " ^ reason)
        else None)
    in
    let reason =
      match reasons with
      | [] -> "default policy: " ^ string_of_action action
      | _ -> String.concat ~sep:"; " reasons
    in
    { action; matches; reason }
  ;;

  let conservative_default () =
    let open Matcher in
    let destructive =
      any_of
        (List.map
           [ "rm"
           ; "rmdir"
           ; "sudo"
           ; "su"
           ; "doas"
           ; "mkfs"
           ; "shutdown"
           ; "reboot"
           ; "halt"
           ; "poweroff"
           ]
           ~f:basename)
    in
    let inert =
      all
        [ trusted_executable
        ; no_unknown_effects
        ; any_of (List.map [ "pwd"; "echo"; "printf"; "true"; "false" ] ~f:basename)
        ]
    in
    create
      ~default:Ask
      [ rule
          ~id:"deny-privileged-or-destructive"
          ~action:Deny
          ~reason:"destructive or privilege-changing command"
          destructive
      ; rule ~id:"trusted-inert-command" ~action:Allow inert
      ]
  ;;
end

module Secret_filter = struct
  type t =
    { secrets : string list
    ; replacement : string
    }

  let create ?(replacement = "[REDACTED]") secrets =
    { secrets = List.filter secrets ~f:(Fn.non String.is_empty); replacement }
  ;;

  let empty = create []

  let redact t text =
    List.fold t.secrets ~init:text ~f:(fun text secret ->
      String.substr_replace_all text ~pattern:secret ~with_:t.replacement)
  ;;

  let redact_command t command =
    Command.create
      (redact t command.Command.program)
      (List.map command.arguments ~f:(redact t))
    |> Command.to_string
  ;;
end

module Stable_hash = struct
  let add_part buffer part =
    Buffer.add_string buffer (Int.to_string (String.length part));
    Buffer.add_char buffer ':';
    Buffer.add_string buffer part
  ;;

  let strings parts =
    let buffer = Buffer.create 128 in
    List.iter parts ~f:(add_part buffer);
    Digestif.SHA256.(to_hex (digest_string (Buffer.contents buffer)))
  ;;
end

module Approval = struct
  type identity =
    { manifest_sha256 : string
    ; runtime_id : string
    ; request_kind : Context.request_kind
    ; command_hash : string
    ; executable_sha256 : string
    ; argv : string list
    ; cwd_sha256 : string
    ; environment_sha256 : string
    ; stdin_sha256 : string option
    ; stdin_bytes : int
    ; script_sha256 : string option
    }

  type scope =
    | Once
    | Exact_session of { expires_at : float option }
    | Prefix_session of
        { prefix : string list
        ; expires_at : float option
        }
    | Durable_exact of { expires_at : float option }

  type request =
    { context : Context.t
    ; policy : Policy.decision
    ; identity : identity
    ; display_command : string
    ; rationale : string option
    }

  type response =
    | Approve
    | Approve_for of scope
    | Deny of string
    | Rewrite of Command.t

  type reviewer_metadata =
    { reviewer_id : string
    ; reviewer_kind : string
    ; model : string option
    ; input_tokens : int option
    ; output_tokens : int option
    ; latency_ms : int option
    }

  type review =
    { response : response
    ; metadata : reviewer_metadata option
    }

  type reviewer = request -> response
  type reviewer_with_metadata = request -> review

  type grant =
    { identity : identity
    ; scope : scope
    ; session_id : string option
    }

  type store =
    { lookup : now:float -> session_id:string option -> identity -> (bool, string) result
    ; remember :
        session_id:string option
        -> identity
        -> scope
        -> reviewer_metadata option
        -> (unit, string) result
    }

  let request_kind_name = function
    | Context.Structured -> "structured"
    | Script_file -> "script_file"
    | Raw_shell -> "raw_shell"
  ;;

  let identity context =
    let argv = Command.to_argv context.Context.command in
    let cwd_sha256 = Stable_hash.strings [ context.cwd ] in
    let environment =
      Array.to_list context.environment |> List.sort ~compare:String.compare
    in
    let environment_sha256 = Stable_hash.strings environment in
    let parts =
      [ context.manifest_sha256
      ; context.runtime_id
      ; request_kind_name context.request_kind
      ; context.executable.canonical_path
      ; context.executable.fingerprint.sha256
      ; cwd_sha256
      ; environment_sha256
      ; Option.value context.stdin_sha256 ~default:""
      ; Int.to_string context.stdin_bytes
      ; Option.value context.script_sha256 ~default:""
      ]
      @ argv
    in
    { manifest_sha256 = context.manifest_sha256
    ; runtime_id = context.runtime_id
    ; request_kind = context.request_kind
    ; command_hash = Stable_hash.strings parts
    ; executable_sha256 = context.executable.fingerprint.sha256
    ; argv
    ; cwd_sha256
    ; environment_sha256
    ; stdin_sha256 = context.stdin_sha256
    ; stdin_bytes = context.stdin_bytes
    ; script_sha256 = context.script_sha256
    }
  ;;

  let same_execution_scope left right =
    String.equal left.manifest_sha256 right.manifest_sha256
    && String.equal left.runtime_id right.runtime_id
    && Poly.equal left.request_kind right.request_kind
    && String.equal left.executable_sha256 right.executable_sha256
    && String.equal left.cwd_sha256 right.cwd_sha256
    && String.equal left.environment_sha256 right.environment_sha256
    && Option.equal String.equal left.stdin_sha256 right.stdin_sha256
    && Int.equal left.stdin_bytes right.stdin_bytes
    && Option.equal String.equal left.script_sha256 right.script_sha256
  ;;

  let not_expired now = function
    | None -> true
    | Some expires_at -> Float.(now <= expires_at)
  ;;

  let is_approved_in grants ~now ~session_id identity =
    let exact grant =
      String.equal grant.identity.command_hash identity.command_hash
      && same_execution_scope grant.identity identity
    in
    List.exists !grants ~f:(fun grant ->
      match grant.scope with
      | Once -> false
      | Exact_session { expires_at } ->
        Option.equal String.equal grant.session_id session_id
        && exact grant
        && not_expired now expires_at
      | Prefix_session { prefix; expires_at } ->
        Option.equal String.equal grant.session_id session_id
        && List.is_prefix identity.argv ~prefix ~equal:String.equal
        && same_execution_scope grant.identity identity
        && not_expired now expires_at
      | Durable_exact { expires_at } -> exact grant && not_expired now expires_at)
  ;;

  let remember_in grants ~session_id identity scope _reviewer =
    (match scope with
     | Once -> ()
     | scope -> grants := { identity; scope; session_id } :: !grants);
    Ok ()
  ;;

  let create_store ?lookup ?remember () =
    let grants = ref [] in
    let lookup =
      Option.value lookup ~default:(fun ~now ~session_id identity ->
        Ok (is_approved_in grants ~now ~session_id identity))
    in
    let remember =
      Option.value remember ~default:(remember_in grants)
    in
    { lookup; remember }
  ;;

  let is_approved store ~now ~session_id identity =
    store.lookup ~now ~session_id identity
  ;;

  let remember store ~session_id identity scope reviewer =
    store.remember ~session_id identity scope reviewer
  ;;

  let prompt request =
    let effects = String.concat ~sep:", " (Effect.to_strings request.context.effects) in
    let rationale = Option.value request.rationale ~default:"<none supplied>" in
    Printf.sprintf
      "Review this process execution plan. Reply with exactly one JSON object and no \
       prose. Allowed decisions are deny, allow_once, allow_session, \
       allow_prefix_session, durable_allow, or rewrite. A denial requires reason. A \
       rewrite requires program and string-array arguments. Optional expires_at is a \
       Unix timestamp.\n\
       Command: %s\n\
       Resolved executable: %s\n\
       Executable SHA-256: %s\n\
       Working directory: %s\n\
       Effects: %s\n\
       Policy: %s (%s)\n\
       Agent rationale: %s"
      request.display_command
      request.context.executable.canonical_path
      request.context.executable.fingerprint.sha256
      request.context.cwd
      effects
      (Policy.string_of_action request.policy.action)
      request.policy.reason
      rationale
  ;;

  let response_of_json text =
    let allowed_fields =
      String.Set.of_list
        [ "decision"; "reason"; "program"; "arguments"; "prefix"; "expires_at" ]
    in
    let field name fields = List.Assoc.find fields name ~equal:String.equal in
    let require_only fields names =
      let allowed = String.Set.of_list names in
      match List.find (List.map fields ~f:fst) ~f:(Fn.non (Set.mem allowed)) with
      | None -> Ok ()
      | Some name -> Error (Printf.sprintf "field %S is not valid for this decision" name)
    in
    let string_field name fields =
      match field name fields with
      | Some (`String value) -> Ok value
      | Some _ -> Error (Printf.sprintf "field %S must be a string" name)
      | None -> Error (Printf.sprintf "field %S is required" name)
    in
    let string_array name fields =
      match field name fields with
      | Some (`Array values) ->
        Result.all
          (List.map values ~f:(function
             | `String value -> Ok value
             | _ -> Error (Printf.sprintf "field %S must contain only strings" name)))
      | Some _ -> Error (Printf.sprintf "field %S must be an array" name)
      | None -> Error (Printf.sprintf "field %S is required" name)
    in
    let expires_at fields =
      match field "expires_at" fields with
      | None | Some `Null -> Ok None
      | Some (`Number value) ->
        (match Float.of_string_opt value with
         | Some value when Float.is_finite value -> Ok (Some value)
         | Some _ -> Error "expires_at must be finite"
         | None -> Error "expires_at must be a JSON number")
      | Some _ -> Error "expires_at must be a JSON number"
    in
    try
      match Jsonaf.of_string text with
      | `Object fields ->
        let keys = List.map fields ~f:fst in
        let duplicate = List.find_a_dup keys ~compare:String.compare in
        let unknown = List.find keys ~f:(Fn.non (Set.mem allowed_fields)) in
        (match duplicate, unknown with
         | Some key, _ -> Error (Printf.sprintf "duplicate field %S" key)
         | _, Some key -> Error (Printf.sprintf "unknown field %S" key)
         | None, None ->
           Result.bind (string_field "decision" fields) ~f:(function
             | "allow_once" | "allow" ->
               Result.map (require_only fields [ "decision" ]) ~f:(fun () -> Approve)
             | "allow_session" ->
               Result.bind
                 (require_only fields [ "decision"; "expires_at" ])
                 ~f:(fun () ->
                   Result.map (expires_at fields) ~f:(fun expires_at ->
                     Approve_for (Exact_session { expires_at })))
             | "allow_prefix_session" ->
               Result.bind
                 (require_only fields [ "decision"; "prefix"; "expires_at" ])
                 ~f:(fun () ->
                   Result.bind (string_array "prefix" fields) ~f:(fun prefix ->
                     if List.is_empty prefix
                     then Error "approval prefix must not be empty"
                     else
                       Result.map (expires_at fields) ~f:(fun expires_at ->
                         Approve_for (Prefix_session { prefix; expires_at }))))
             | "durable_allow" ->
               Result.bind
                 (require_only fields [ "decision"; "expires_at" ])
                 ~f:(fun () ->
                   Result.map (expires_at fields) ~f:(fun expires_at ->
                     Approve_for (Durable_exact { expires_at })))
             | "deny" ->
               Result.bind
                 (require_only fields [ "decision"; "reason" ])
                 ~f:(fun () ->
                   Result.bind (string_field "reason" fields) ~f:(fun reason ->
                     if String.is_empty reason
                     then Error "denial reason must not be empty"
                     else Ok (Deny reason)))
             | "rewrite" ->
               Result.bind
                 (require_only fields [ "decision"; "program"; "arguments" ])
                 ~f:(fun () ->
                   Result.bind (string_field "program" fields) ~f:(fun program ->
                     Result.map (string_array "arguments" fields) ~f:(fun arguments ->
                       Rewrite (Command.create program arguments))))
             | decision -> Error (Printf.sprintf "unknown decision %S" decision)))
      | _ -> Error "moderator response must be a JSON object"
    with
    | exn -> Error ("invalid moderator JSON: " ^ Exn.to_string exn)
  ;;

  let reviewer_of_llm ~complete request =
    try
      match complete (prompt request) with
      | Error error -> Deny ("LLM moderator failed: " ^ error)
      | Ok response ->
        (match response_of_json response with
         | Ok response -> response
         | Error error -> Deny ("LLM moderator returned invalid JSON: " ^ error))
    with
    | exn -> Deny ("LLM moderator raised: " ^ Exn.to_string exn)
  ;;
end

module Interceptor = struct
  type command_result =
    { command : Command.t
    ; executable : Executable.t option
    ; status : Eio.Process.exit_status
    ; stdout : string
    ; stderr : string
    ; stdout_truncated : bool
    ; stderr_truncated : bool
    ; intercepted_by : string option
    ; untrusted_output : bool
    }

  type before =
    | Continue
    | Rewrite of Command.t
    | Respond of command_result
    | Reject of string

  type kind =
    | Trusted_substitute
    | Output_filter

  type t =
    { name : string
    ; kind : kind
    ; before : (Context.t -> before) option
    ; after : (command_result -> command_result) option
    }

  let trusted_substitute ~name ~before =
    { name; kind = Trusted_substitute; before = Some before; after = None }
  ;;

  let output_filter ~name ~after =
    { name; kind = Output_filter; before = None; after = Some after }
  ;;

  let name t = t.name
  let kind t = t.kind
end

module Audit = struct
  type termination =
    | Timed_out of float
    | Idle_timed_out of float
    | Output_limit_exceeded of int
    | Cancelled

  type event =
    | Resolved of Context.t
    | Policy_decided of Context.t * Policy.decision
    | Approval_requested of Approval.request
    | Approval_answered of Approval.request * string
    | Reviewer_completed of Approval.request * Approval.reviewer_metadata * string
    | Intercepted of string * Context.t
    | Plan_created of string * string * Context.t
    | Started of string * Context.t * int option
    | Output of string * Context.t * [ `Stdout | `Stderr ] * int
    | Finished of string * Context.t * Eio.Process.exit_status
    | Terminated of string * Context.t * termination
    | Rejected of Context.t * string

  type envelope =
    { sequence : int64
    ; timestamp : float
    ; session_id : string option
    ; runtime_id : string
    ; manifest_sha256 : string
    ; request_id : string
    ; plan_id : string option
    ; event : event
    ; dropped_fields : String.Set.t
    ; replacement_fields : string String.Map.t
    }

  type failure_policy =
    | Ignore_failure
    | Deny_start
    | Terminate_runtime

  type t =
    { failure_policy : failure_policy
    ; write : envelope -> (unit, string) result
    }

  let create ~failure_policy write = { failure_policy; write }
  let write t envelope = t.write envelope
  let failure_policy t = t.failure_policy
  let ignore = create ~failure_policy:Ignore_failure (fun _envelope -> Ok ())

  let filter t filter =
    { t with
      write =
        (fun envelope ->
          Result.bind (filter envelope) ~f:(function
            | None -> Ok ()
            | Some envelope -> t.write envelope))
    }
  ;;

  let context = function
    | Resolved context
    | Policy_decided (context, _)
    | Intercepted (_, context)
    | Plan_created (_, _, context)
    | Started (_, context, _)
    | Output (_, context, _, _)
    | Finished (_, context, _)
    | Terminated (_, context, _)
    | Rejected (context, _) -> context
    | Approval_requested request
    | Approval_answered (request, _)
    | Reviewer_completed (request, _, _) ->
      request.Approval.context
  ;;

  let plan_id = function
    | Plan_created (_, plan_id, _)
    | Started (plan_id, _, _)
    | Output (plan_id, _, _, _)
    | Finished (plan_id, _, _)
    | Terminated (plan_id, _, _) -> Some plan_id
    | Resolved _
    | Policy_decided _
    | Approval_requested _
    | Approval_answered _
    | Reviewer_completed _
    | Intercepted _
    | Rejected _ -> None
  ;;

  let is_before_start = function
    | Resolved _
    | Policy_decided _
    | Approval_requested _
    | Approval_answered _
    | Reviewer_completed _
    | Intercepted _
    | Plan_created _
    | Rejected _ -> true
    | Started _ | Output _ | Finished _ | Terminated _ -> false
  ;;
end

module Execution_plan = struct
  type t =
    { id : string
    ; context : Context.t
    ; limits : Limits.t
    ; environment : string array
    ; cwd : string
    ; resource_runner : Executable.t option
    }
end

module Backend = struct
  type confinement =
    | Verified
    | Declared
    | Unconfined

  type spawn =
    { executable : string
    ; argv : string list
    ; environment : string array
    }

  type simulated =
    { status : Eio.Process.exit_status
    ; stdout : string
    ; stderr : string
    }

  type repeated_flag = { flag : string }

  type atom =
    | Literal of string
    | Cwd
    | Target_executable
    | Command_argv
    | Read_roots of repeated_flag
    | Write_roots of repeated_flag
    | Network_flag of string
    | Resource_limit_args

  type t =
    { name : string
    ; available_fn : Eio.Fs.dir_ty Eio.Path.t -> bool
    ; prepare_fn :
        Eio.Fs.dir_ty Eio.Path.t -> Execution_plan.t -> (spawn, string) Result.t
    ; simulate_fn :
        (Execution_plan.t -> stdin:string -> (simulated, string) Result.t) option
    ; confinement : confinement
    ; eligible_for_required : bool
    }

  let name t = t.name
  let confinement t = t.confinement

  let availability t ~fs =
    try
      if t.available_fn fs
      then Ok ()
      else Error (Printf.sprintf "backend %s is unavailable" t.name)
    with
    | exn ->
      Error (Printf.sprintf "backend %s check failed: %s" t.name (Exn.to_string exn))
  ;;

  let available t ~fs = Result.is_ok (availability t ~fs)

  let command_available ~fs executable =
    if String.contains executable '/'
    then Path_util.file_exists ~fs executable
    else
      Sys.getenv "PATH"
      |> Option.value ~default:"/usr/bin:/bin:/usr/sbin:/sbin"
      |> String.split ~on:':'
      |> List.exists ~f:(fun directory ->
        Path_util.file_exists ~fs (Filename.concat directory executable))
  ;;

  let has_os_limits limits =
    Option.is_some limits.Limits.cpu_seconds
    || Option.is_some limits.memory_bytes
    || Option.is_some limits.file_size_bytes
    || Option.is_some limits.open_files
  ;;

  let resource_args limits =
    List.concat
      [ Option.value_map limits.Limits.cpu_seconds ~default:[] ~f:(fun value ->
          [ "--cpu"; Int.to_string value ])
      ; Option.value_map limits.memory_bytes ~default:[] ~f:(fun value ->
          [ "--memory"; Int.to_string value ])
      ; Option.value_map limits.file_size_bytes ~default:[] ~f:(fun value ->
          [ "--file-size"; Int.to_string value ])
      ; Option.value_map limits.open_files ~default:[] ~f:(fun value ->
          [ "--open-files"; Int.to_string value ])
      ]
  ;;

  let apply_resource_runner plan ~executable ~argv =
    if not (has_os_limits plan.Execution_plan.limits)
    then Ok (executable, argv)
    else (
      match plan.resource_runner with
      | None -> Error "OS resource limits require a configured resource runner"
      | Some runner ->
        Ok
          ( runner.Executable.canonical_path
          , (runner.canonical_path :: resource_args plan.limits)
            @ [ "--"; executable ]
            @ List.tl_exn argv ))
  ;;

  let direct =
    { name = "direct-unsafe"
    ; available_fn = (fun _fs -> true)
    ; confinement = Unconfined
    ; eligible_for_required = false
    ; simulate_fn = None
    ; prepare_fn =
        (fun _fs plan ->
          let executable = plan.Execution_plan.context.executable.canonical_path in
          Result.map
            (apply_resource_runner
               plan
               ~executable
               ~argv:(executable :: plan.context.command.arguments))
            ~f:(fun (executable, argv) ->
              { executable; argv; environment = plan.environment }))
    }
  ;;

  let seatbelt_escape value =
    value
    |> String.substr_replace_all ~pattern:"\\" ~with_:"\\\\"
    |> String.substr_replace_all ~pattern:"\"" ~with_:"\\\""
  ;;

  let seatbelt_rule operation path =
    Printf.sprintf "(allow %s (subpath \"%s\"))" operation (seatbelt_escape path)
  ;;

  let macos_seatbelt =
    { name = "macos-seatbelt"
    ; available_fn =
        (fun fs ->
          String.equal Sys.os_type "Unix"
          && Path_util.file_exists ~fs "/usr/bin/sandbox-exec"
          && String.equal (Core_unix.Utsname.sysname (Core_unix.uname ())) "Darwin")
    ; confinement = Verified
    ; eligible_for_required = true
    ; simulate_fn = None
    ; prepare_fn =
        (fun _fs plan ->
          let context = plan.Execution_plan.context in
          let executable = context.executable.canonical_path in
          Result.map
            (apply_resource_runner
               plan
               ~executable
               ~argv:(executable :: context.command.arguments))
            ~f:(fun (limited_executable, limited_argv) ->
              let read_roots =
                [ "/System"; "/usr/lib"; "/usr/share"; "/private/etc"; "/dev" ]
                @ context.capabilities.read_roots
                @ context.capabilities.write_roots
              in
              let exec_paths =
                List.dedup_and_sort
                  [ executable; limited_executable ]
                  ~compare:String.compare
                |> List.map ~f:(fun path ->
                  Printf.sprintf
                    "(allow process-exec (literal \"%s\"))"
                    (seatbelt_escape path))
              in
              let profile =
                String.concat
                  ~sep:"\n"
                  ([ "(version 1)"
                   ; "(deny default)"
                   ; "(allow file-read-metadata)"
                   ; "(allow sysctl-read)"
                   ; "(allow mach-lookup)"
                   ]
                   @ exec_paths
                   @ (if context.capabilities.allow_child_processes
                      then [ "(allow process-fork)" ]
                      else [])
                   @ List.map read_roots ~f:(seatbelt_rule "file-read*")
                   @ List.map
                       context.capabilities.write_roots
                       ~f:(seatbelt_rule "file-write*")
                   @ if context.capabilities.network then [ "(allow network*)" ] else [])
              in
              { executable = "/usr/bin/sandbox-exec"
              ; argv = "/usr/bin/sandbox-exec" :: "-p" :: profile :: "--" :: limited_argv
              ; environment = plan.environment
              }))
    }
  ;;

  let linux_bubblewrap ?(executable = "bwrap") () =
    { name = "linux-bubblewrap"
    ; available_fn =
        (fun fs ->
          String.equal (Core_unix.Utsname.sysname (Core_unix.uname ())) "Linux"
          && command_available ~fs executable)
    ; confinement = Verified
    ; eligible_for_required = true
    ; simulate_fn = None
    ; prepare_fn =
        (fun fs plan ->
          let context = plan.Execution_plan.context in
          let target = context.executable.canonical_path in
          Result.map
            (apply_resource_runner
               plan
               ~executable:target
               ~argv:(target :: context.command.arguments))
            ~f:(fun (limited_executable, limited_argv) ->
              let bind flag path = [ flag; path; path ] in
              let system_roots =
                List.filter
                  [ "/usr"; "/bin"; "/sbin"; "/lib"; "/lib64"; "/etc" ]
                  ~f:(Path_util.file_exists ~fs)
              in
              let args =
                [ executable; "--die-with-parent"; "--new-session"; "--unshare-all" ]
                @ (if context.capabilities.network then [ "--share-net" ] else [])
                @ [ "--proc"; "/proc"; "--dev"; "/dev"; "--tmpfs"; "/tmp" ]
                @ List.concat_map system_roots ~f:(bind "--ro-bind")
                @ List.concat_map context.capabilities.read_roots ~f:(bind "--ro-bind")
                @ List.concat_map context.capabilities.write_roots ~f:(bind "--bind")
                @ [ "--chdir"; plan.cwd; "--" ]
                @ limited_argv
              in
              { executable; argv = args; environment = plan.environment }))
    }
  ;;

  let fake ~name simulate =
    { name
    ; available_fn = (fun _fs -> true)
    ; prepare_fn = (fun _fs _plan -> Error "fake backend has no spawn plan")
    ; simulate_fn = Some simulate
    ; confinement = Verified
    ; eligible_for_required = true
    }
  ;;

  let repeated { flag } values = List.concat_map values ~f:(fun value -> [ flag; value ])

  let expand_atom plan target_argv = function
    | Literal value -> [ value ]
    | Cwd -> [ plan.Execution_plan.cwd ]
    | Target_executable -> [ plan.context.executable.canonical_path ]
    | Command_argv -> target_argv
    | Read_roots flag -> repeated flag plan.context.capabilities.read_roots
    | Write_roots flag -> repeated flag plan.context.capabilities.write_roots
    | Network_flag value -> if plan.context.capabilities.network then [ value ] else []
    | Resource_limit_args -> resource_args plan.limits
  ;;

  let external_ ~name ~wrapper ~confinement ~accept_declared_confinement atoms =
    let command_count = List.count atoms ~f:(function Command_argv -> true | _ -> false) in
    if not (Int.equal command_count 1)
    then Error "external backend requires exactly one command_argv atom"
    else
      Ok
        { name
        ; available_fn = (fun fs -> Result.is_ok (Resolver.verify ~fs wrapper))
        ; confinement
        ; eligible_for_required =
            (match confinement with
             | Verified -> true
             | Declared -> accept_declared_confinement
             | Unconfined -> false)
        ; simulate_fn = None
        ; prepare_fn =
            (fun fs plan ->
              Result.bind (Resolver.verify ~fs wrapper) ~f:(fun () ->
                let target = plan.Execution_plan.context.executable.canonical_path in
                let argv = target :: plan.context.command.arguments in
                let argv = wrapper.canonical_path :: List.concat_map atoms ~f:(expand_atom plan argv) in
                Ok { executable = wrapper.canonical_path; argv; environment = plan.environment }))
        }
  ;;

  let prepare t ~fs plan = t.prepare_fn fs plan
  let simulate t = t.simulate_fn

  let sandboxed t = t.eligible_for_required

  module For_testing = struct
    let prepare = prepare
  end
end

module Request = struct
  type raw_shell =
    { executable : string
    ; arguments_before_script : string list
    ; script : string
    }

  type script_file =
    { command : Command.t
    ; path : string
    ; source_sha256 : string
    ; executable_sha256 : string
    ; max_source_bytes : int
    }

  type t =
    | Structured of Chain.t
    | Script_file of script_file
    | Raw_shell of raw_shell

  let command command = Structured (Chain.single command)

  let custom_tool ~command_line ~arguments =
    match Chain.parse command_line with
    | Error error -> Error (Chain.parse_error_to_string error)
    | Ok { Chain.first = [ command ]; rest = [] } ->
      Ok
        (Structured
           (Chain.single (Command.create command.program (command.arguments @ arguments))))
    | Ok _ -> Error "custom tool declarations must contain exactly one command"
  ;;

  let script_file
        ~executable
        ~arguments
        ~path
        ~source_sha256
        ~executable_sha256
        ~max_source_bytes
    =
    Script_file
      { command = Command.create executable arguments
      ; path
      ; source_sha256
      ; executable_sha256
      ; max_source_bytes
      }
  ;;

  let raw_shell ?(arguments_before_script = [ "-c" ]) ~executable script =
    Raw_shell { executable; arguments_before_script; script }
  ;;
end

module Executor = struct
  type config =
    { env : Eio_unix.Stdenv.base
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; runtime_id : string
    ; manifest_sha256 : string
    ; policy : Policy.t
    ; capabilities : Capabilities.t
    ; resolver : Resolver.t
    ; reviewer : Approval.reviewer option
    ; reviewer_with_metadata : Approval.reviewer_with_metadata option
    ; approval_store : Approval.store
    ; administrative_check : Context.t -> (unit, string) result
    ; analyzers : Analyzer.t list
    ; interceptors : Interceptor.t list
    ; backends : Backend.t list
    ; cwd_path : Eio.Fs.dir_ty Eio.Path.t option
    ; cwd : string
    ; process_env : string array
    ; limits : Limits.t
    ; resource_runner_path : string option
    ; secret_filter : Secret_filter.t
    ; audit : Audit.t
    ; audit_sequence : int Atomic.t
    ; session_id : string option
    ; pipefail : bool
    }

  type invocation =
    { request : Request.t
    ; input : Input.t
    ; rationale : string option
    ; origin : Context.origin
    }

  type error =
    | Permission_required of Approval.request
    | Denied of string
    | Resolution_error of string
    | Capability_violation of string
    | Sandbox_unavailable of string
    | Interceptor_rejected of string
    | Spawn_error of string
    | Executable_changed of string
    | Script_changed of string
    | Audit_unavailable of string
    | Timed_out of float
    | Idle_timed_out of float
    | Stdin_limit_exceeded of int
    | Output_limit_exceeded of int

  type result =
    { request_id : string
    ; commands : Interceptor.command_result list
    ; status : Eio.Process.exit_status
    ; stdout : string
    ; stderr : string
    ; backend : string
    }

  type request_metadata =
    { stdin_kind : Context.stdin_kind
    ; stdin_sha256 : string option
    ; stdin_bytes : int
    ; script_sha256 : string option
    ; script_preview : string option
    ; script_file : Request.script_file option
    ; expected_executable_sha256 : string option
    ; origin : Context.origin
    }

  type spawn_stage =
    { plan : Execution_plan.t
    ; backend : Backend.t
    ; spawn : Backend.spawn
    }

  type stage =
    | Spawn_stage of spawn_stage
    | Simulated_stage of
        { plan : Execution_plan.t
        ; backend : Backend.t
        ; simulate :
            Execution_plan.t -> stdin:string -> (Backend.simulated, string) Result.t
        }
    | Synthetic_stage of Interceptor.command_result

  exception Execution_error of error
  exception Rewrite_requested of Command.t
  exception Idle_timeout of float
  exception Total_output_limit of int

  let next_id = Atomic.make 0
  let fresh_id () = Printf.sprintf "shell-%08d" (Atomic.fetch_and_add next_id 1)

  let audit_failure config event reason =
    match config.audit.Audit.failure_policy with
    | Audit.Ignore_failure -> ()
    | Deny_start when not (Audit.is_before_start event) -> ()
    | Deny_start | Terminate_runtime -> raise (Execution_error (Audit_unavailable reason))
  ;;

  let emit config event =
    let event_context = Audit.context event in
    let sequence = Atomic.fetch_and_add config.audit_sequence 1 |> Int64.of_int in
    let envelope =
      Audit.
        { sequence
        ; timestamp = Eio.Time.now (Eio.Stdenv.clock config.env)
        ; session_id = event_context.session_id
        ; runtime_id = event_context.runtime_id
        ; manifest_sha256 = event_context.manifest_sha256
        ; request_id = event_context.request_id
        ; plan_id = Audit.plan_id event
        ; event
        ; dropped_fields = String.Set.empty
        ; replacement_fields = String.Map.empty
        }
    in
    match config.audit.write envelope with
    | Ok () -> ()
    | Error reason -> audit_failure config event reason
    | exception exn -> audit_failure config event (Exn.to_string exn)
  ;;

  let dangerous_environment_key key =
    List.exists
      [ "BASH_ENV"
      ; "ENV"
      ; "IFS"
      ; "PYTHONHOME"
      ; "PYTHONPATH"
      ; "RUBYLIB"
      ; "PERL5LIB"
      ; "NODE_OPTIONS"
      ; "GIT_EXEC_PATH"
      ; "GIT_CONFIG"
      ; "GIT_CONFIG_COUNT"
      ]
      ~f:(String.equal key)
    || String.is_prefix key ~prefix:"DYLD_"
    || String.is_prefix key ~prefix:"LD_"
  ;;

  let split_environment entry =
    match String.lsplit2 entry ~on:'=' with
    | None -> entry, ""
    | Some pair -> pair
  ;;

  let sanitize_path path =
    path
    |> String.split ~on:':'
    |> List.filter ~f:(fun entry ->
      (not (String.is_empty entry)) && not (Filename.is_relative entry))
    |> List.dedup_and_sort ~compare:String.compare
    |> String.concat ~sep:":"
    |> fun path -> if String.is_empty path then "/usr/bin:/bin:/usr/sbin:/sbin" else path
  ;;

  let sanitize_environment environment =
    let table = String.Table.create () in
    Array.iter environment ~f:(fun entry ->
      let key, value = split_environment entry in
      if not (dangerous_environment_key key) then Hashtbl.set table ~key ~data:value);
    Hashtbl.update table "PATH" ~f:(fun value ->
      sanitize_path (Option.value value ~default:"/usr/bin:/bin:/usr/sbin:/sbin"));
    List.iter
      [ "PAGER", "cat"; "GIT_PAGER", "cat"; "TERM", "dumb"; "NO_COLOR", "1" ]
      ~f:(fun (key, data) -> Hashtbl.set table ~key ~data);
    Hashtbl.to_alist table
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.map ~f:(fun (key, value) -> key ^ "=" ^ value)
    |> Array.of_list
  ;;

  let validate_limits limits =
    if Float.(limits.Limits.wall_time_seconds <= 0.)
    then invalid_arg "wall_time_seconds must be positive";
    Option.iter limits.idle_time_seconds ~f:(fun seconds ->
      if Float.(seconds <= 0.) then invalid_arg "idle_time_seconds must be positive");
    List.iter
      [ limits.max_stdin_bytes
      ; limits.max_stdout_bytes
      ; limits.max_stderr_bytes
      ; limits.max_total_bytes
      ]
      ~f:(fun value -> if value <= 0 then invalid_arg "output limits must be positive")
  ;;

  let config
        ~env
        ~runtime_id
        ~manifest_sha256
        ~policy
        ~capabilities
        ?(resolver = Resolver.create ())
        ?reviewer
        ?reviewer_with_metadata
        ?(approval_store = Approval.create_store ())
        ?(administrative_check = fun _ -> Ok ())
        ?(analyzers = [])
        ?(interceptors = [])
        ?(backends =
          [ Backend.macos_seatbelt; Backend.linux_bubblewrap (); Backend.direct ])
        ?cwd
        ?process_env
        ?(limits = Limits.default)
        ?resource_runner
        ?(secret_filter = Secret_filter.empty)
        ?(audit = Audit.ignore)
        ?(audit_sequence = Atomic.make 0)
        ?session_id
        ?(pipefail = false)
        ()
    =
    validate_limits limits;
    let fs = Eio.Stdenv.fs env in
    let cwd_path = cwd in
    let cwd =
      Option.value_map
        cwd_path
        ~default:(Eio.Path.native_exn (Eio.Stdenv.cwd env))
        ~f:Eio.Path.native_exn
    in
    let process_env =
      Option.value process_env ~default:(Core_unix.environment ()) |> sanitize_environment
    in
    { env
    ; fs
    ; runtime_id
    ; manifest_sha256
    ; policy
    ; capabilities
    ; resolver
    ; reviewer
    ; reviewer_with_metadata
    ; approval_store
    ; administrative_check
    ; analyzers
    ; interceptors
    ; backends
    ; cwd_path
    ; cwd
    ; process_env
    ; limits
    ; resource_runner_path = resource_runner
    ; secret_filter
    ; audit
    ; audit_sequence
    ; session_id
    ; pipefail
    }
  ;;

  let select_backend config =
    let available = List.filter config.backends ~f:(Backend.available ~fs:config.fs) in
    let sandboxed = List.filter available ~f:Backend.sandboxed in
    let direct = List.find available ~f:(Fn.non Backend.sandboxed) in
    match config.capabilities.sandbox with
    | Capabilities.Required ->
      (match sandboxed with
       | backend :: _ -> Ok backend
       | [] -> Error (Sandbox_unavailable "no configured sandbox backend is available"))
    | Preferred ->
      (match sandboxed, direct with
       | backend :: _, _ -> Ok backend
       | [], Some backend -> Ok backend
       | [], None -> Error (Sandbox_unavailable "no execution backend is available"))
    | Direct_unsafe ->
      (match direct with
       | Some backend -> Ok backend
       | None -> Error (Sandbox_unavailable "direct execution backend is unavailable"))
  ;;

  let apply_before config context =
    let substitutes =
      List.filter config.interceptors ~f:(fun interceptor ->
        Poly.equal (Interceptor.kind interceptor) Interceptor.Trusted_substitute)
    in
    let rec loop context = function
      | [] -> `Continue context
      | interceptor :: rest ->
        let response =
          match interceptor.Interceptor.before with
          | None -> Interceptor.Continue
          | Some before ->
            (try before context with
             | exn -> Interceptor.Reject ("raised: " ^ Exn.to_string exn))
        in
        (match response with
         | Continue -> loop context rest
         | Rewrite command -> `Rewrite command
         | Respond result ->
           emit config (Audit.Intercepted (Interceptor.name interceptor, context));
           `Respond { result with intercepted_by = Some (Interceptor.name interceptor) }
         | Reject reason ->
           raise
             (Execution_error
                (Interceptor_rejected (Interceptor.name interceptor ^ ": " ^ reason))))
    in
    loop context substitutes
  ;;

  let sanitize_terminal text =
    let output = Buffer.create (String.length text) in
    let rec skip_csi index =
      if index >= String.length text
      then index
      else (
        let code = Char.to_int text.[index] in
        if code >= 0x40 && code <= 0x7e then index + 1 else skip_csi (index + 1))
    in
    let rec loop index =
      if index >= String.length text
      then Buffer.contents output
      else (
        match text.[index] with
        | '\027' when index + 1 < String.length text && Char.equal text.[index + 1] '[' ->
          loop (skip_csi (index + 2))
        | ('\n' | '\r' | '\t') as character ->
          Buffer.add_char output character;
          loop (index + 1)
        | character when Char.to_int character < 0x20 || Char.equal character '\127' ->
          loop (index + 1)
        | character ->
          Buffer.add_char output character;
          loop (index + 1))
    in
    loop 0
  ;;

  let bound text limit =
    if String.length text <= limit then text, false else String.prefix text limit, true
  ;;

  let normalize_result_output config result =
    let stdout =
      result.Interceptor.stdout
      |> sanitize_terminal
      |> Secret_filter.redact config.secret_filter
    in
    let stderr =
      result.stderr |> sanitize_terminal |> Secret_filter.redact config.secret_filter
    in
    let stdout, filter_stdout_truncated = bound stdout config.limits.max_stdout_bytes in
    let stderr, filter_stderr_truncated = bound stderr config.limits.max_stderr_bytes in
    let result =
      { result with
        stdout
      ; stderr
      ; stdout_truncated = result.stdout_truncated || filter_stdout_truncated
      ; stderr_truncated = result.stderr_truncated || filter_stderr_truncated
      ; untrusted_output = true
      }
    in
    let total = String.length result.stdout + String.length result.stderr in
    if total > config.limits.max_total_bytes then raise (Total_output_limit total);
    result
  ;;

  let finalize_result config result =
    let result = normalize_result_output config result in
    List.fold config.interceptors ~init:result ~f:(fun result interceptor ->
      match interceptor.Interceptor.after with
      | None -> result
      | Some after ->
        (try after result |> normalize_result_output config with
         | (Execution_error _ | Total_output_limit _ | Eio.Cancel.Cancelled _) as exn ->
           raise exn
         | exn ->
           raise
             (Execution_error
                (Interceptor_rejected
                   (Interceptor.name interceptor ^ " raised: " ^ Exn.to_string exn)))))
  ;;

  let verify_script_file config script =
    let path = Eio.Path.(config.fs / script.Request.path) in
    if not (Eio.Path.is_file path)
    then raise (Execution_error (Script_changed "script file is unavailable"));
    let source = Eio.Path.load path in
    if String.length source > script.max_source_bytes
    then raise (Execution_error (Script_changed "script file exceeds its source limit"));
    let digest = Digestif.SHA256.(to_hex (digest_string source)) in
    if not (String.Caseless.equal digest script.source_sha256)
    then
      raise (Execution_error (Script_changed "script file changed after authorization"))
  ;;

  let resolve_resource_runner config =
    if not (Backend.has_os_limits config.limits)
    then None
    else (
      match config.resource_runner_path with
      | None -> None
      | Some runner ->
        let command = Command.create runner [] in
        (match
           Resolver.resolve
             config.resolver
             ~fs:config.fs
             ~cwd:config.cwd
             ~environment:config.process_env
             command
         with
         | Ok executable -> Some executable
         | Error error ->
           raise (Execution_error (Resolution_error ("resource runner: " ^ error)))))
  ;;

  let rec prepare_command_inner
            config
            ~request_id
            ~request_kind
            ~(metadata : request_metadata)
            ?rationale
            ~depth
            command
    =
    if depth > 12 then raise (Execution_error (Denied "too many command rewrites"));
    let executable =
      match
        Resolver.resolve
          config.resolver
          ~fs:config.fs
          ~cwd:config.cwd
          ~environment:config.process_env
          command
      with
      | Ok executable -> executable
      | Error error -> raise (Execution_error (Resolution_error error))
    in
    Option.iter metadata.expected_executable_sha256 ~f:(fun expected ->
      if not (String.Caseless.equal expected executable.fingerprint.sha256)
      then
        raise
          (Execution_error
             (Executable_changed "script executable changed after runtime authorization")));
    let effects =
      Effect.analyze
        ~raw_shell:(Poly.equal request_kind Context.Raw_shell)
        ~cwd:config.cwd
        command
    in
    let initial_context =
      Context.
        { request_id
        ; runtime_id = config.runtime_id
        ; manifest_sha256 = config.manifest_sha256
        ; command
        ; executable
        ; cwd = config.cwd
        ; environment = config.process_env
        ; request_kind
        ; stdin_kind = metadata.stdin_kind
        ; stdin_sha256 = metadata.stdin_sha256
        ; stdin_bytes = metadata.stdin_bytes
        ; script_sha256 = metadata.script_sha256
        ; script_preview = metadata.script_preview
        ; origin = metadata.origin
        ; effects
        ; capabilities = config.capabilities
        ; policy_action = None
        ; policy_matches = []
        ; session_id = config.session_id
        }
    in
    let effects =
      List.fold config.analyzers ~init:effects ~f:(fun effects analyzer ->
        match analyzer { initial_context with effects } with
        | Ok (Analyzer.Add additional) -> effects @ additional
        | Ok (Replace replacement) -> replacement
        | Error message -> effects @ [ Effect.Unknown ("analyzer failed: " ^ message) ])
    in
    let context = { initial_context with effects } in
    (match config.administrative_check context with
     | Ok () -> ()
     | Error reason ->
       emit config (Audit.Rejected (context, reason));
       raise (Execution_error (Denied reason)));
    emit config (Audit.Resolved context);
    let decision = Policy.evaluate config.policy context in
    let context =
      { context with
        policy_action = Some (Policy.string_of_action decision.action)
      ; policy_matches = List.map decision.matches ~f:(fun match_ -> match_.rule_id)
      }
    in
    emit config (Audit.Policy_decided (context, decision));
    (match decision.action with
     | Policy.Deny ->
       emit config (Audit.Rejected (context, decision.reason));
       raise (Execution_error (Denied decision.reason))
     | Allow | Ask -> ());
    match apply_before config context with
    | `Respond result -> Synthetic_stage (finalize_result config result)
    | `Rewrite rewritten ->
      prepare_command_inner
        config
        ~request_id
        ~request_kind
        ~metadata:(metadata : request_metadata)
        ?rationale
        ~depth:(depth + 1)
        rewritten
    | `Continue context ->
      (match
         Effect.check_capabilities
           config.capabilities
           ~fs:config.fs
           ~cwd:config.cwd
           effects
       with
       | Error error -> raise (Execution_error (Capability_violation error))
       | Ok () -> ());
      let approval_identity = Approval.identity context in
      let approved =
        match decision.action with
        | Policy.Allow -> true
        | Deny -> false
        | Ask ->
          let previously_approved =
            Approval.is_approved
              config.approval_store
              ~now:(Eio.Time.now (Eio.Stdenv.clock config.env))
              ~session_id:config.session_id
              approval_identity
            |> Result.map_error ~f:(fun error ->
              Execution_error (Denied ("approval lookup failed: " ^ error)))
          in
          if
            (match previously_approved with
             | Ok approved -> approved
             | Error exn -> raise exn)
          then true
          else (
            let request =
              let display_command =
                match context.script_preview, context.script_sha256 with
                | Some preview, Some digest ->
                  sprintf
                    "%s <script sha256=%s preview=%S>"
                    context.executable.canonical_path
                    digest
                    preview
                | _ -> Secret_filter.redact_command config.secret_filter command
              in
              Approval.
                { context
                ; policy = decision
                ; identity = approval_identity
                ; display_command
                ; rationale
                }
            in
            emit config (Audit.Approval_requested request);
            match config.reviewer_with_metadata, config.reviewer with
            | None, None -> raise (Execution_error (Permission_required request))
            | reviewer_with_metadata, reviewer ->
              let review =
                try
                  match reviewer_with_metadata, reviewer with
                  | Some reviewer, _ -> reviewer request
                  | None, Some reviewer -> Approval.{ response = reviewer request; metadata = None }
                  | None, None -> assert false
                with
                | exn ->
                  Approval.
                    { response = Deny ("reviewer raised: " ^ Exn.to_string exn)
                    ; metadata = None
                    }
              in
              let response = review.Approval.response in
              let response_name =
                match response with
                | Approve -> "approve_once"
                | Approve_for _ -> "approve_scoped"
                | Deny _ -> "deny"
                | Rewrite _ -> "rewrite"
              in
              emit config (Audit.Approval_answered (request, response_name));
              Option.iter review.metadata ~f:(fun metadata ->
                emit config (Audit.Reviewer_completed (request, metadata, response_name)));
              (match response with
               | Approve -> true
               | Approve_for scope ->
                 (match
                    Approval.remember
                      config.approval_store
                      ~session_id:config.session_id
                      approval_identity
                      scope
                      review.metadata
                  with
                  | Ok () -> ()
                  | Error error ->
                    raise
                      (Execution_error
                         (Denied ("approval persistence failed: " ^ error))));
                 true
               | Deny reason -> raise (Execution_error (Denied reason))
               | Rewrite rewritten -> raise (Rewrite_requested rewritten)))
      in
      if not approved then raise (Execution_error (Denied "approval was not granted"));
      Option.iter metadata.script_file ~f:(verify_script_file config);
      let backend =
        match select_backend config with
        | Ok backend -> backend
        | Error error -> raise (Execution_error error)
      in
      let resource_runner = resolve_resource_runner config in
      let plan =
        Execution_plan.
          { id = fresh_id ()
          ; context
          ; limits = config.limits
          ; environment = config.process_env
          ; cwd = config.cwd
          ; resource_runner
          }
      in
      emit config (Audit.Plan_created (Backend.name backend, plan.id, context));
      (match Backend.simulate backend with
       | Some simulate -> Simulated_stage { plan; backend; simulate }
       | None ->
         (match Backend.prepare backend ~fs:config.fs plan with
          | Ok spawn -> Spawn_stage { plan; backend; spawn }
          | Error error -> raise (Execution_error (Sandbox_unavailable error))))
  ;;

  let rec prepare_command
            config
            ~request_id
            ~request_kind
            ~metadata
            ?rationale
            ~depth
            command
    =
    try
      prepare_command_inner
        config
        ~request_id
        ~request_kind
        ~metadata
        ?rationale
        ~depth
        command
    with
    | Rewrite_requested rewritten ->
      prepare_command
        config
        ~request_id
        ~request_kind
        ~metadata
        ?rationale
        ~depth:(depth + 1)
        rewritten
  ;;

  type capture =
    { buffer : Buffer.t
    ; mutable seen : int
    ; mutable truncated : bool
    }

  let create_capture limit =
    { buffer = Buffer.create (Int.min limit 4096); seen = 0; truncated = false }
  ;;

  let read_capture
        config
        ~(plan : Execution_plan.t)
        ~channel
        ~limit
        ~total
        ~last_activity
        source
        capture
    =
    let scratch = Cstruct.create 4096 in
    let rec loop () =
      match Eio.Flow.single_read source scratch with
      | count ->
        last_activity := Eio.Time.now (Eio.Stdenv.clock config.env);
        capture.seen <- capture.seen + count;
        total := !total + count;
        if !total > config.limits.max_total_bytes then raise (Total_output_limit !total);
        let remaining = limit - Buffer.length capture.buffer in
        if remaining > 0
        then
          Buffer.add_string
            capture.buffer
            (Cstruct.to_string (Cstruct.sub scratch 0 (Int.min count remaining)));
        if capture.seen > limit then capture.truncated <- true;
        emit config (Audit.Output (plan.id, plan.context, channel, count));
        loop ()
      | exception End_of_file -> ()
    in
    loop ()
  ;;

  let status_success = function
    | `Exited 0 -> true
    | `Exited _ | `Signaled _ -> false
  ;;

  let verify_executable config executable =
    match Resolver.verify ~fs:config.fs executable with
    | Ok () -> ()
    | Error error -> raise (Execution_error (Executable_changed error))
  ;;

  let verify_plan config plan =
    verify_executable config plan.Execution_plan.context.executable;
    Option.iter plan.resource_runner ~f:(verify_executable config)
  ;;

  let rec audit_termination config = function
    | Idle_timeout seconds -> Some (Audit.Idle_timed_out seconds)
    | Total_output_limit bytes -> Some (Audit.Output_limit_exceeded bytes)
    | Eio.Time.Timeout -> Some (Audit.Timed_out config.limits.wall_time_seconds)
    | Eio.Cancel.Cancelled reason ->
      Some (Option.value (audit_termination config reason) ~default:Audit.Cancelled)
    | _ -> None
  ;;

  let emit_termination config plans exn =
    Option.iter (audit_termination config exn) ~f:(fun termination ->
      List.iter plans ~f:(fun (plan : Execution_plan.t) ->
        emit config (Audit.Terminated (plan.id, plan.context, termination))))
  ;;

  let run_spawn_pipeline config ~stdin stages =
    let manager = Eio.Stdenv.process_mgr config.env in
    try
      Eio.Switch.run
      @@ fun sw ->
    let count = List.length stages in
    let edges =
      List.init (Int.max 0 (count - 1)) ~f:(fun _ -> Eio.Process.pipe ~sw manager)
    in
    let stderr_pipes = List.init count ~f:(fun _ -> Eio.Process.pipe ~sw manager) in
    let final_stdout = Eio.Process.pipe ~sw manager in
    let children =
      List.mapi stages ~f:(fun index stage ->
        verify_plan config stage.plan;
        let stdin_pipe =
          if Int.equal index 0 then None else Some (fst (List.nth_exn edges (index - 1)))
        in
        let stdin_flow =
          match stdin_pipe with
          | None -> Eio.Flow.string_source stdin
          | Some source -> (source :> Eio.Flow.source_ty Eio.Resource.t)
        in
        let stdout_flow =
          if Int.equal index (count - 1)
          then snd final_stdout
          else snd (List.nth_exn edges index)
        in
        let stderr_flow = snd (List.nth_exn stderr_pipes index) in
        try
          let child =
            Eio.Process.spawn
              ~sw
              manager
              ?cwd:config.cwd_path
              ~env:stage.spawn.environment
              ~executable:stage.spawn.executable
              ~stdin:stdin_flow
              ~stdout:stdout_flow
              ~stderr:stderr_flow
              stage.spawn.argv
          in
          emit
            config
            (Audit.Started
               (stage.plan.id, stage.plan.context, Some (Eio.Process.pid child)));
          Option.iter stdin_pipe ~f:Eio.Flow.close;
          Eio.Flow.close stdout_flow;
          Eio.Flow.close stderr_flow;
          child
        with
        | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
        | exn -> raise (Execution_error (Spawn_error (Exn.to_string exn))))
    in
    let stdout_capture = create_capture config.limits.max_stdout_bytes in
    let stderr_captures =
      List.init count ~f:(fun _ -> create_capture config.limits.max_stderr_bytes)
    in
    let statuses = Array.create ~len:count (`Exited 127) in
    let total = ref 0 in
    let last_activity = ref (Eio.Time.now (Eio.Stdenv.clock config.env)) in
    Option.iter config.limits.idle_time_seconds ~f:(fun idle ->
      Eio.Fiber.fork_daemon ~sw (fun () ->
        let rec watch () =
          Eio.Time.sleep (Eio.Stdenv.clock config.env) (Float.min 0.25 (idle /. 4.));
          let elapsed = Eio.Time.now (Eio.Stdenv.clock config.env) -. !last_activity in
          if Float.(elapsed > idle) then raise (Idle_timeout idle) else watch ()
        in
        watch ()));
    let readers =
      (fun () ->
        read_capture
          config
          ~plan:(List.last_exn stages).plan
          ~channel:`Stdout
          ~limit:config.limits.max_stdout_bytes
          ~total
          ~last_activity
          (fst final_stdout)
          stdout_capture)
      :: List.mapi stderr_pipes ~f:(fun index (source, _sink) () ->
        read_capture
          config
          ~plan:(List.nth_exn stages index).plan
          ~channel:`Stderr
          ~limit:config.limits.max_stderr_bytes
          ~total
          ~last_activity
          source
          (List.nth_exn stderr_captures index))
    in
    let waiters =
      List.mapi children ~f:(fun index child () ->
        let status = Eio.Process.await child in
        statuses.(index) <- status;
        let plan = (List.nth_exn stages index).plan in
        emit config (Audit.Finished (plan.id, plan.context, status)))
    in
    Eio.Fiber.all (readers @ waiters);
    List.mapi stages ~f:(fun index stage ->
      let stdout =
        if Int.equal index (count - 1) then Buffer.contents stdout_capture.buffer else ""
      in
      let stderr_capture = List.nth_exn stderr_captures index in
      { Interceptor.command = stage.plan.context.command
      ; executable = Some stage.plan.context.executable
      ; status = statuses.(index)
      ; stdout
      ; stderr = Buffer.contents stderr_capture.buffer
      ; stdout_truncated = Int.equal index (count - 1) && stdout_capture.truncated
      ; stderr_truncated = stderr_capture.truncated
      ; intercepted_by = None
      ; untrusted_output = true
      })
      |> List.map ~f:(finalize_result config)
    with
    | exn ->
      emit_termination config (List.map stages ~f:(fun stage -> stage.plan)) exn;
      raise exn
  ;;

  let run_stage config ~stdin = function
    | Synthetic_stage result -> result
    | Simulated_stage { plan; backend; simulate } ->
      emit config (Audit.Started (plan.id, plan.context, None));
      (try
         match simulate plan ~stdin with
         | Error error -> raise (Execution_error (Spawn_error error))
         | Ok simulated ->
           emit config (Audit.Finished (plan.id, plan.context, simulated.status));
           finalize_result
             config
             { command = plan.context.command
             ; executable = Some plan.context.executable
             ; status = simulated.status
             ; stdout = simulated.stdout
             ; stderr = simulated.stderr
             ; stdout_truncated = false
             ; stderr_truncated = false
             ; intercepted_by = Some (Backend.name backend)
             ; untrusted_output = true
             }
       with
       | exn ->
         emit_termination config [ plan ] exn;
         raise exn)
    | Spawn_stage stage -> List.hd_exn (run_spawn_pipeline config ~stdin [ stage ])
  ;;

  let pipeline_status config results =
    if not config.pipefail
    then (List.last_exn results).Interceptor.status
    else
      List.fold results ~init:(`Exited 0) ~f:(fun selected result ->
        if status_success result.Interceptor.status then selected else result.status)
  ;;

  let run_pipeline config ~stdin stages =
    if
      List.for_all stages ~f:(function
        | Spawn_stage _ -> true
        | _ -> false)
    then (
      let spawns =
        List.map stages ~f:(function
          | Spawn_stage stage -> stage
          | _ -> assert false)
      in
      run_spawn_pipeline config ~stdin spawns)
    else (
      let rec loop stdin results = function
        | [] -> List.rev results
        | stage :: rest ->
          let result = run_stage config ~stdin stage in
          loop result.stdout (result :: results) rest
      in
      loop stdin [] stages)
  ;;

  let prepare_pipeline
        config
        ~request_id
        ~request_kind
        ~(metadata : request_metadata)
        ?rationale
        pipeline
    =
    List.mapi pipeline ~f:(fun index command ->
      let metadata =
        if Int.equal index 0
        then metadata
        else
          { metadata with
            stdin_kind = Context.Pipeline
          ; stdin_sha256 = None
          ; stdin_bytes = 0
          }
      in
      prepare_command
        config
        ~request_id
        ~request_kind
        ~metadata
        ?rationale
        ~depth:0
        command)
  ;;

  let backend_names stages =
    stages
    |> List.filter_map ~f:(function
      | Spawn_stage stage -> Some (Backend.name stage.backend)
      | Simulated_stage stage -> Some (Backend.name stage.backend)
      | Synthetic_stage result ->
        Some (Option.value result.intercepted_by ~default:"interceptor"))
    |> List.dedup_and_sort ~compare:String.compare
    |> String.concat ~sep:"+"
  ;;

  let make_result config ~request_id all_results last_results backend =
    let status = pipeline_status config last_results in
    { request_id
    ; commands = all_results
    ; status
    ; stdout = (List.last_exn last_results).stdout
    ; stderr =
        String.concat
          ~sep:""
          (List.map all_results ~f:(fun result -> result.Interceptor.stderr))
    ; backend
    }
  ;;

  let run_chain config ~request_id ?rationale ~request_kind ~metadata ~stdin chain =
    let first_stages =
      prepare_pipeline
        config
        ~request_id
        ~request_kind
        ~metadata
        ?rationale
        chain.Chain.first
    in
    let first_results = run_pipeline config ~stdin first_stages in
    let rec loop all_results last_results names = function
      | [] ->
        make_result
          config
          ~request_id
          all_results
          last_results
          (String.concat ~sep:"+" (List.rev names))
      | (condition, pipeline) :: rest ->
        let status = pipeline_status config last_results in
        let should_run =
          match condition with
          | Chain.Always -> true
          | On_success -> status_success status
          | On_failure -> not (status_success status)
        in
        if should_run
        then (
          let stages =
            let metadata =
              { metadata with
                stdin_kind = Context.Empty
              ; stdin_sha256 = None
              ; stdin_bytes = 0
              }
            in
            prepare_pipeline
              config
              ~request_id
              ~request_kind
              ~metadata
              ?rationale
              pipeline
          in
          let results = run_pipeline config ~stdin:"" stages in
          loop (all_results @ results) results (backend_names stages :: names) rest)
        else loop all_results last_results names rest
    in
    loop first_results first_results [ backend_names first_stages ] chain.rest
  ;;

  let script_preview config script =
    script
    |> sanitize_terminal
    |> Secret_filter.redact config.secret_filter
    |> fun value -> String.prefix value (Int.min 256 (String.length value))
  ;;

  let request_metadata config invocation =
    let stdin_kind =
      match invocation.input with
      | Input.Empty -> Context.Empty
      | Text _ | Bytes _ -> Supplied
    in
    let script_sha256, script_preview, script_file, expected_executable_sha256 =
      match invocation.request with
      | Request.Raw_shell raw ->
        ( Some Digestif.SHA256.(to_hex (digest_string raw.script))
        , Some (script_preview config raw.script)
        , None
        , None )
      | Structured _ -> None, None, None, None
      | Script_file script ->
        Some script.source_sha256, None, Some script, Some script.executable_sha256
    in
    { stdin_kind
    ; stdin_sha256 = Input.sha256 invocation.input
    ; stdin_bytes = Input.byte_length invocation.input
    ; script_sha256
    ; script_preview
    ; script_file
    ; expected_executable_sha256
    ; origin = invocation.origin
    }
  ;;

  let run config invocation =
    let request_id = fresh_id () in
    let input_bytes = Input.byte_length invocation.input in
    try
      if input_bytes > config.limits.max_stdin_bytes
      then raise (Execution_error (Stdin_limit_exceeded input_bytes));
      let stdin = Input.to_string invocation.input in
      let metadata = request_metadata config invocation in
      Ok
        (Eio.Time.with_timeout_exn
           (Eio.Stdenv.clock config.env)
           config.limits.wall_time_seconds
           (fun () ->
              match invocation.request with
              | Request.Structured chain ->
                run_chain
                  config
                  ~request_id
                  ?rationale:invocation.rationale
                  ~request_kind:Context.Structured
                  ~metadata
                  ~stdin
                  chain
              | Script_file script ->
                run_chain
                  config
                  ~request_id
                  ?rationale:invocation.rationale
                  ~request_kind:Context.Script_file
                  ~metadata
                  ~stdin
                  (Chain.single script.command)
              | Raw_shell raw ->
                let command =
                  Command.create
                    raw.executable
                    (raw.arguments_before_script @ [ raw.script ])
                in
                run_chain
                  config
                  ~request_id
                  ?rationale:invocation.rationale
                  ~request_kind:Context.Raw_shell
                  ~metadata
                  ~stdin
                  (Chain.single command)))
    with
    | Execution_error error -> Error error
    | Idle_timeout seconds -> Error (Idle_timed_out seconds)
    | Total_output_limit bytes -> Error (Output_limit_exceeded bytes)
    | Eio.Time.Timeout -> Error (Timed_out config.limits.wall_time_seconds)
    | Eio.Cancel.Cancelled _ as exn -> raise exn
  ;;

  let error_to_string = function
    | Permission_required request ->
      "permission required for " ^ request.Approval.display_command
    | Denied reason -> "command denied: " ^ reason
    | Resolution_error reason -> "executable resolution failed: " ^ reason
    | Capability_violation reason -> "capability violation: " ^ reason
    | Sandbox_unavailable reason -> "sandbox unavailable: " ^ reason
    | Interceptor_rejected reason -> "interceptor rejected command: " ^ reason
    | Spawn_error reason -> "process execution failed: " ^ reason
    | Executable_changed reason -> "executable identity changed: " ^ reason
    | Script_changed reason -> "script identity changed: " ^ reason
    | Audit_unavailable reason -> "audit unavailable: " ^ reason
    | Timed_out seconds -> Printf.sprintf "request timed out after %.3fs" seconds
    | Idle_timed_out seconds -> Printf.sprintf "request was idle for %.3fs" seconds
    | Output_limit_exceeded bytes ->
      Printf.sprintf "request exceeded total output limit at %d bytes" bytes
    | Stdin_limit_exceeded bytes ->
      Printf.sprintf "request exceeded stdin limit at %d bytes" bytes
  ;;
end
