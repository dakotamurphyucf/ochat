open! Core
open Chatml_composition_test

let () =
  Authoring_compaction_tests.exercise Authoring_compaction_tests.with_root;
  Authoring_compaction_tests.exercise Authoring_child_compaction_tests.with_child;
  Documentation_lab_review_tests.exercise_review ()
;;
