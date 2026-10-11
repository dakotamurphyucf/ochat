open Core
module Project = Chatml_project
module Build = Chatml_project_build
module Roots = Chatml_local_roots
module Git = Chatml_git_host
module Captured = Chatml_source
module Policy = Project_prepare_policy
module Limits = Project_prepare_limits
module Host = Project_prepare_host
module Root = Project_prepare_request.Root
module Request = Project_prepare_request
module Publication = Project_prepare_publication
module Accounting = Project_prepare_accounting
module Error = Project_prepare_error
module Deadline = Project_prepare_deadline
module Attempt = Project_prepare_attempt
module Completed = Project_prepare_result

type t = Project_prepare_result.t

let build = Project_prepare_result.build
let accounting = Project_prepare_result.accounting

let deadline ~limits ~clock =
  (* Limits validates a strictly positive nanosecond span. Converting that finite
     integer span to seconds satisfies Deadline's constructor invariant. *)
  Deadline.create ~clock ~wall_time:limits.Limits.wall_time |> Or_error.ok_exn
;;

let prepare_with_deadline host request ~tools ~policy ~limits ~deadline ~clock ~checkpoint
  =
  Attempt.run ~limits ~policy ~deadline ~clock ~checkpoint ~f:(fun attempt ->
    Project_prepare_graph.prepare attempt ~host ~request ~tools)
  |> Result.map ~f:(fun (build, accounting) -> Completed.create build ~accounting)
;;

let prepare host request ~tools ~policy ~limits ~clock ~checkpoint =
  let deadline = deadline ~limits ~clock in
  prepare_with_deadline host request ~tools ~policy ~limits ~deadline ~clock ~checkpoint
;;

let prepare_and_publish
      host
      request
      ~publication
      ~tools
      ~policy
      ~limits
      ~clock
      ~checkpoint
  =
  let deadline = deadline ~limits ~clock in
  Publication.run publication ~deadline ~clock ~checkpoint ~prepare:(fun () ->
    prepare_with_deadline host request ~tools ~policy ~limits ~deadline ~clock ~checkpoint)
;;
