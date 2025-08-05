open Core

module History = struct
  type t = Openai.Responses.Item.t list [@@deriving bin_io, sexp]
end

(* ----------------------------------------------------------------------- *)
(*  Versioning                                                             *)
(* ----------------------------------------------------------------------- *)

(** Schema version supported by the running binary.  Increment whenever
    {!type:t} changes in a way that requires migrations. *)
let current_version = 2

module Task = struct
  type state =
    | Pending
    | In_progress
    | Done
  [@@deriving bin_io, sexp]

  type t =
    { id : string
    ; title : string
    ; state : state
    }
  [@@deriving bin_io, sexp]

  let create ?id ~title ?(state = Pending) () : t =
    let default_id () =
      let open Core in
      let data =
        let time_ns =
          Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) |> Int63.to_string
        in
        time_ns ^ Int.to_string (Random.bits ())
      in
      Md5.digest_string data |> Md5.to_hex
    in
    let id = Option.value id ~default:(default_id ()) in
    { id; title; state }
  ;;

  (* Prevent -W32 until the function is exercised by external modules *)
  let _ : t = create ~title:"dummy" ()
end

(* ----------------------------------------------------------------------- *)
(*  Latest schema                                                           *)
(* ----------------------------------------------------------------------- *)

(* Make the latest schema directly available at the top-level. *)
type t =
  { version : int
  ; id : string
  ; prompt_file : string
  ; local_prompt_copy : string option
  ; history : History.t
  ; tasks : Task.t list
  ; kv_store : (string * string) list
  ; vfs_root : string
  }
[@@deriving bin_io, sexp]

(* Re-export under [Latest] so migration helpers can refer to the most
   recent schema while callers keep using [Session.t]. *)
module Latest = struct
  type nonrec t = t [@@deriving bin_io, sexp]

  let version = current_version
end

(* ----------------------------------------------------------------------- *)
(*  Legacy schemas and upgrade helpers                                      *)
(* ----------------------------------------------------------------------- *)

module Legacy = struct
  module V0 = struct
    type t =
      { id : string
      ; prompt_file : string
      ; history : History.t
      ; tasks : Task.t list
      ; kv_store : (string * string) list
      ; vfs_root : string
      }
    [@@deriving bin_io, sexp]

    let version = 0
  end

  let upgrade_v0 (v : V0.t) : Latest.t =
    { version = current_version
    ; id = v.id
    ; prompt_file = v.prompt_file
    ; local_prompt_copy = None
    ; history = v.history
    ; tasks = v.tasks
    ; kv_store = v.kv_store
    ; vfs_root = v.vfs_root
    }
  ;;

  module V1 = struct
    type t =
      { version : int
      ; id : string
      ; prompt_file : string
      ; history : History.t
      ; tasks : Task.t list
      ; kv_store : (string * string) list
      ; vfs_root : string
      }
    [@@deriving bin_io, sexp]

    let version = 1
  end

  let upgrade_v1 (v : V1.t) : Latest.t =
    { version = current_version
    ; id = v.id
    ; prompt_file = v.prompt_file
    ; local_prompt_copy = None
    ; history = v.history
    ; tasks = v.tasks
    ; kv_store = v.kv_store
    ; vfs_root = v.vfs_root
    }
  ;;
end

let create
      ?id
      ~prompt_file
      ?local_prompt_copy
      ?(history = [])
      ?(tasks = [])
      ?(kv_store = [])
      ?(vfs_root = "vfs")
      ()
  : t
  =
  let default_id () =
    let data =
      let time_ns = Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) |> Int63.to_string in
      time_ns ^ Int.to_string (Random.bits ())
    in
    Md5.digest_string data |> Md5.to_hex
  in
  let id = Option.value id ~default:(default_id ()) in
  { version = current_version
  ; id
  ; prompt_file
  ; local_prompt_copy
  ; history
  ; tasks
  ; kv_store
  ; vfs_root
  }
;;

(* ------------------------------------------------------------------------- *)
(* IO helpers                                                                *)
(* ------------------------------------------------------------------------- *)

module Bin_p = struct
  type nonrec t = t [@@deriving bin_io]
end

module Io = Bin_prot_utils_eio.With_file_methods (Bin_p)

(* Dummy reference to avoid “unused-value” compiler warnings until the
   functions gain real call-sites in subsequent milestones. *)
let _ = ignore (create ~prompt_file:"/dev/null" ())

(* ------------------------------------------------------------------------- *)
(*  Public helpers                                                            *)
(* ------------------------------------------------------------------------- *)

let reset ?prompt_file (t : t) : t =
  let prompt_file = Option.value prompt_file ~default:t.prompt_file in
  { t with prompt_file; history = [] }
;;

(** Same as {!reset} but preserves the existing conversation history. *)
let reset_keep_history ?prompt_file (t : t) : t =
  let prompt_file = Option.value prompt_file ~default:t.prompt_file in
  { t with prompt_file }
;;
