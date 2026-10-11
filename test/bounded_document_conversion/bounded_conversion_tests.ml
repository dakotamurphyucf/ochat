open Core
open Expect_test_helpers_core
module C = Webpage_markdown.Bounded_conversion

let%test_module "bounded local content conversion" =
  (module struct
    let limits
          ?(input = 100_000)
          ?(output = 100_000)
          ?(work = 10_000_000)
          ?(nodes = 1_000_000)
          ?(depth = 128)
          ()
      =
      C.Limits.create
        ~max_input_bytes:input
        ~max_output_bytes:output
        ~max_work_steps:work
        ~max_nodes:nodes
        ~max_depth:depth
      |> Or_error.ok_exn
    ;;

    let convert ?(checkpoint = ignore) ?(limits = limits ()) mode contents =
      C.convert mode ~contents ~limits ~checkpoint
    ;;

    let value = function
      | Ok v -> v
      | Error e -> raise_s [%sexp (e : C.Error.t)]
    ;;

    let expect_error expected actual =
      match actual with
      | Error e -> require [%here] (C.Error.equal expected e)
      | Ok _ -> failwith "expected bounded rejection"
    ;;

    let%expect_test "literal, stripped and markdown documents" =
      List.iter
        [ C.Mode.Verbatim, "# local guide\nUse exact files."
        ; Strip_html, "<p> A &amp; B </p><p> C </p>"
        ; Html_to_markdown, "<h1>Guide</h1><p>Use <strong>files</strong>.</p>"
        ; ( Html_to_markdown
          , "<blockquote><ul><li><p>a * b</p><p>next</p></li></ul></blockquote>" )
        ]
        ~f:(fun (mode, contents) ->
          let result = value (convert mode contents) in
          print_s [%sexp (mode : C.Mode.t), (C.Output.contents result : string)]);
      [%expect
        {|
        (Verbatim "# local guide\nUse exact files.")
        (Strip_html "A & B\nC")
        (Html_to_markdown "# Guide\n\nUse **files**\\.")
        (Html_to_markdown "> * >   a \\* b\n\n>   next")
        |}]
    ;;

    let%test_unit "independent literal budgets and before-parser input rejection" =
      let contents = "\000\255x" in
      let exact = limits ~input:3 ~output:3 ~work:3 ~nodes:0 ~depth:0 () in
      let v = value (convert ~limits:exact Verbatim contents) in
      require [%here] (String.equal contents (C.Output.contents v));
      let a = C.Output.accounting v in
      [%test_eq: int] (C.Accounting.input_bytes a) 3;
      [%test_eq: int] (C.Accounting.output_bytes a) 3;
      [%test_eq: int] (C.Accounting.work_steps a) 3;
      [%test_eq: int] (C.Accounting.nodes a) 0;
      [%test_eq: int] (C.Accounting.depth a) 0;
      expect_error Input_limit (convert ~limits:(limits ~input:2 ()) Verbatim contents);
      expect_error Output_limit (convert ~limits:(limits ~output:2 ()) Verbatim contents);
      expect_error Work_limit (convert ~limits:(limits ~work:2 ()) Verbatim contents);
      let polls = ref 0 in
      expect_error
        Input_limit
        (convert Html_to_markdown (String.make 100_001 '<') ~checkpoint:(fun () ->
           incr polls));
      [%test_eq: int] !polls 0;
      require
        [%here]
        (Result.is_error
           (C.Limits.create
              ~max_input_bytes:(-1)
              ~max_output_bytes:0
              ~max_work_steps:0
              ~max_nodes:0
              ~max_depth:0))
    ;;

    let%test_unit "exact/below aggregate HTML budgets and output expansion" =
      List.iter
        [ C.Mode.Strip_html, "<p>A &amp; B</p><p>C</p>", "A & B\nC"
        ; Html_to_markdown, "<p>a * b</p>", "a \\* b"
        ; Html_to_markdown, "<body id=first><body class=second><p>a * b</p>", "a \\* b"
        ; ( Html_to_markdown
          , "<ul><li><p>first</p><p>second</p></li></ul>"
          , "* first\n\n  second" )
        ; Html_to_markdown, "<p><a href='/x'>]</a></p>", "[\\]](/x)"
        ]
        ~f:(fun (mode, contents, expected) ->
          let v = value (convert mode contents) in
          require [%here] (String.equal expected (C.Output.contents v));
          let a = C.Output.accounting v in
          let exact =
            limits
              ~input:(String.length contents)
              ~output:(String.length expected)
              ~work:(C.Accounting.work_steps a)
              ~nodes:(C.Accounting.nodes a)
              ~depth:(C.Accounting.depth a)
              ()
          in
          require
            [%here]
            (String.equal
               expected
               (C.Output.contents (value (convert ~limits:exact mode contents))));
          expect_error
            Output_limit
            (convert
               mode
               contents
               ~limits:(limits ~output:(String.length expected - 1) ()));
          expect_error
            Work_limit
            (convert
               mode
               contents
               ~limits:(limits ~work:(C.Accounting.work_steps a - 1) ()));
          expect_error
            Node_limit
            (convert mode contents ~limits:(limits ~nodes:(C.Accounting.nodes a - 1) ()));
          expect_error
            Depth_limit
            (convert mode contents ~limits:(limits ~depth:(C.Accounting.depth a - 1) ())))
    ;;

    let%test_unit "hostile depth, flat width and every checkpoint interruption" =
      let deep =
        String.concat (List.init 300 ~f:(fun _ -> "<div>"))
        ^ "x"
        ^ String.concat (List.init 300 ~f:(fun _ -> "</div>"))
      in
      expect_error
        Depth_limit
        (convert Html_to_markdown deep ~limits:(limits ~depth:16 ()));
      expect_error Node_limit (convert Strip_html "<p>x</p>" ~limits:(limits ~nodes:7 ()));
      let siblings = 500 in
      let pruned =
        String.concat (List.init siblings ~f:(fun _ -> "<script>x</script>"))
        ^ "<p>keep</p>"
      in
      let pruned_output = value (convert Html_to_markdown pruned) in
      require [%here] (String.equal "keep" (C.Output.contents pruned_output));
      (* Deleting n siblings copies at least n*(n-1)/2 retained list cells. *)
      require
        [%here]
        (C.Accounting.nodes (C.Output.accounting pruned_output)
         >= siblings * (siblings - 1) / 2);
      let wide = "<p>" ^ String.concat (List.init 3000 ~f:(fun _ -> "abc * ")) ^ "</p>" in
      let polls = ref 0 in
      let reference =
        value (convert Html_to_markdown wide ~checkpoint:(fun () -> incr polls))
      in
      require [%here] (!polls > 4);
      List.iter
        [ Failure "owned checkpoint"; Soup.Parse_error "borrowed parser exception" ]
        ~f:(fun sentinel ->
          for stop = 1 to !polls do
            let count = ref 0 in
            let interrupted =
              try
                ignore
                  (convert Html_to_markdown wide ~checkpoint:(fun () ->
                     incr count;
                     if !count = stop then raise sentinel));
                false
              with
              | exn ->
                require [%here] (phys_equal sentinel exn);
                true
            in
            require [%here] interrupted
          done);
      let fresh = value (convert Html_to_markdown wide) in
      require
        [%here]
        (String.equal (C.Output.contents reference) (C.Output.contents fresh));
      let nested_calls = ref 0 in
      let nested =
        value
          (convert Html_to_markdown wide ~checkpoint:(fun () ->
             incr nested_calls;
             require
               [%here]
               (String.equal "ok" (C.Output.contents (value (convert Verbatim "ok"))))))
      in
      require [%here] (!nested_calls > 4);
      require
        [%here]
        (String.equal (C.Output.contents reference) (C.Output.contents nested))
    ;;
  end)
;;
