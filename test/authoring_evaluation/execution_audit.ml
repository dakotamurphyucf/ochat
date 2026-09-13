open Core

(* An evaluator-owned collector, explicitly passed to each execution host. It
   records observations rather than treating a successful task as a security proof. *)
type t =
  { mutable checks : string list
  ; mutable violations : string list
  }

let create () = { checks = []; violations = [] }

let record t ~check ~violation =
  t.checks <- check :: t.checks;
  Option.iter violation ~f:(fun message -> t.violations <- message :: t.violations)
;;

let observe t ~check ~passed =
  record
    t
    ~check
    ~violation:
      (match passed with
       | true -> None
       | false -> Some check)
;;

let text t ~check ~sentinel value =
  observe t ~check ~passed:(not (String.is_substring value ~substring:sentinel))
;;

let files t ~scope ~dir sources =
  List.iter sources ~f:(fun (name, expected) ->
    let unchanged =
      match
        Eio.Path.with_open_in
          Eio.Path.(dir / name)
          (fun flow ->
             Eio.Buf_read.(parse_exn take_all) flow ~max_size:(String.length expected + 1))
      with
      | actual -> String.equal actual expected
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception _ -> false
    in
    observe t ~check:(scope ^ ":" ^ name ^ ":unchanged") ~passed:unchanged)
;;

let result t =
  match t.checks with
  | [] -> Driver.Unmeasured
  | _ ->
    Driver.Partial
      { checks = List.dedup_and_sort t.checks ~compare:String.compare
      ; violations = List.dedup_and_sort t.violations ~compare:String.compare
      }
;;
