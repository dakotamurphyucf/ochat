open! Core

let () =
  Managed_send.run ();
  Authored_pending.run ()
;;
