open! Core

type phase =
  | Authority_acknowledgement
  | Payload_deletion
  | Final_cleanup
  | Rejection_completion

type action =
  | Fail
  | Cancel of (unit -> unit)

type t =
  { mutable armed : (phase * action) option
  ; mutable rejection_skips : int
  ; mutable triggered : bool
  }

let create () = { armed = None; rejection_skips = 0; triggered = false }

let arm_action t phase action =
  t.armed <- Some (phase, action);
  t.triggered <- false;
  t.rejection_skips
  <- (match phase with
      | Rejection_completion -> 1
      | Authority_acknowledgement | Payload_deletion | Final_cleanup -> 0)
;;

let arm t phase = arm_action t phase Fail
let arm_cancel t phase ~cancel = arm_action t phase (Cancel cancel)
let was_triggered t = t.triggered

let trigger t action =
  t.armed <- None;
  t.triggered <- true;
  match action with
  | Fail -> raise (Core_unix.Unix_error (EIO, "injected lifecycle phase", "fixture"))
  | Cancel cancel -> cancel ()
;;

let rec directory
  :  'tags.
     t
  -> ([> Eio.Fs.dir_ty ] as 'tags) Eio.Resource.t
  -> prefix:string
  -> 'tags Eio.Resource.t
  =
  fun t (Eio.Resource.T (resource, handler)) ~prefix ->
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let qualify name =
    if Filename.is_absolute name then name else Filename.concat prefix name
  in
  let root_marker name =
    String.is_substring name ~substring:"/deleted-"
    && String.is_suffix name ~suffix:"/ARCHIVED"
    && not (String.is_substring name ~substring:"/payload/")
  in
  let module Directory = struct
    include Original

    let open_dir resource ~sw name =
      Original.open_dir resource ~sw name
      |> fun child -> directory t child ~prefix:(qualify name)
    ;;

    let rename resource source destination target =
      let qualified = qualify target in
      match t.armed with
      | Some (Authority_acknowledgement, action)
        when String.is_suffix qualified ~suffix:"/ARCHIVED"
             && not (String.is_substring qualified ~substring:"/deleted-") ->
        Original.rename resource source destination target;
        trigger t action
      | Some (Rejection_completion, action)
        when String.is_suffix qualified ~suffix:"/idempotency.sexp" ->
        if t.rejection_skips > 0
        then (
          t.rejection_skips <- t.rejection_skips - 1;
          Original.rename resource source destination target)
        else trigger t action
      | Some
          ( ( Authority_acknowledgement
            | Payload_deletion
            | Final_cleanup
            | Rejection_completion )
          , _ )
      | None -> Original.rename resource source destination target
    ;;

    let unlink resource name =
      let qualified = qualify name in
      let action =
        match t.armed with
        | Some (Payload_deletion, action)
          when String.is_substring qualified ~substring:"/payload/" -> Some action
        | Some (Final_cleanup, action) when root_marker qualified -> Some action
        | Some
            ( ( Authority_acknowledgement
              | Payload_deletion
              | Final_cleanup
              | Rejection_completion )
            , _ )
        | None -> None
      in
      match action with
      | None -> Original.unlink resource name
      | Some Fail -> trigger t Fail
      | Some (Cancel cancel) ->
        Original.unlink resource name;
        trigger t (Cancel cancel)
    ;;
  end
  in
  Eio.Resource.T
    ( resource
    , Eio.Resource.handler
        (H (Eio.Fs.Pi.Dir, (module Directory)) :: Eio.Resource.bindings handler) )
;;

let wrap_env t (env : Eio_unix.Stdenv.base) : Eio_unix.Stdenv.base =
  let resource, prefix = Eio.Stdenv.fs env in
  let fs = directory t resource ~prefix, prefix in
  object
    method fs = fs
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = env#domain_mgr
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;
