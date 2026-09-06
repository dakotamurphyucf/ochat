(** this module contains code for defining and implementing ochat functions that can be made availible to ochat prompt *)

open Core

module Progress = struct
  type channel =
    [ `Assistant
    | `Reasoning
    | `Stdout
    | `Stderr
    | `Activity
    ]

  type update =
    | Append of string
    | Replace of string

  type t =
    { channel : channel
    ; update : update
    }
end

module Trace = struct
  type outcome =
    | Returned
    | Raised
    | Cancelled

  type tool_kind =
    [ `Function
    | `Custom
    ]

  type t =
    | Tool_started of
        { call_id : string
        ; name : string
        ; kind : tool_kind
        ; payload : string
        }
    | Tool_progress of
        { call_id : string
        ; progress : Progress.t
        }
    | Tool_finished of
        { call_id : string
        ; outcome : outcome
        ; output : Openai.Responses.Tool_output.Output.t option
        }
end

module Invocation = struct
  type observers =
    { progress : Progress.t -> unit
    ; trace : Trace.t -> unit
    }

  type t = observers option

  let silent = None
  let create progress = Some { progress; trace = ignore }
  let create_with_trace ~progress ~trace = Some { progress; trace }

  let emit t progress =
    match t with
    | None -> ()
    | Some observers ->
      Exn.handle_uncaught ~exit:false (fun () -> observers.progress progress)
  ;;

  let emit_trace t trace =
    match t with
    | None -> ()
    | Some observers -> Exn.handle_uncaught ~exit:false (fun () -> observers.trace trace)
  ;;

  let is_observed = Option.is_some
end

type runner = invocation:Invocation.t -> string -> Openai.Responses.Tool_output.Output.t

(* Defines a module type for a ochat function definition. 
   This contains the metadata for a ochat function like name, description, and parameters. 
   Also defines a input_of_string function for converting string inputs from ochat model to the input type defined in the module
*)
module type Def = sig
  type input

  val name : string
  val type_ : string
  val description : string option
  val parameters : Jsonaf.t
  val input_of_string : string -> input
end

(* represents a ochat function implementation *)
type t =
  { info : Openai.Completions.tool
  ; run : string -> Openai.Responses.Tool_output.Output.t
  ; run_with_progress : runner
  }

(* takes a module of type Def and a function Def.input -> string and returns type t. Use to create a ochat function implementation for the given the  ochat function definition and implementation function *)
let create (type a) (module M : Def with type input = a) ~strict run_with_input =
  let run_with_progress ~invocation s =
    run_with_input ~invocation (M.input_of_string s)
  in
  let run s = run_with_progress ~invocation:Invocation.silent s in
  let info =
    Openai.Completions.
      { type_ = M.type_
      ; function_ =
          { name = M.name
          ; description = M.description
          ; parameters = M.parameters
          ; strict
          }
      }
  in
  { info; run; run_with_progress }
;;

let create_function def ?(strict = true) f =
  create def ~strict (fun ~invocation:_ input -> f input)
;;

let create_streaming_function def ?(strict = true) f =
  create def ~strict (fun ~invocation input -> f ~invocation input)
;;

(** 
  takes a (t list) and returns a tuple with openai function defenitions and a hashtbl of the function implementations. Use this function to get the function definitions that need to be passed to the openai api, as well as get a hashtbl that maps function name to implementation so that you can locate the function implementation when the api returns a function call request
*)
let functions ochat_funcs =
  (* tbl of ochat function implementations *)
  let tbl = Hashtbl.create (module String) in
  let details =
    List.fold ochat_funcs ~init:[] ~f:(fun funcs t ->
      Hashtbl.add_exn tbl ~key:t.info.function_.name ~data:t.run_with_progress;
      t.info :: funcs)
  in
  details, tbl
;;
