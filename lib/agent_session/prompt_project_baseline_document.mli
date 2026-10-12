open Core
module Record = Prompt_project_baseline_record
module Error = Record.Error
module Limits = Record.Limits

type t

val generation : t -> int
val entries : t -> Record.Entry.t list
val empty : limits:Limits.t -> (t, Error.t) Result.t

val decode
  :  limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> string
  -> (t, Error.t) Result.t

val replace_successes
  :  t
  -> entries:Record.Success.t list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val encode
  :  t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (string, Error.t) Result.t
