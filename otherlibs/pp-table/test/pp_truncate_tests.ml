module Pp = Table.Pp

let () = Printexc.record_backtrace false
let print pp = Format.printf "%a\n" Pp.to_fmt pp
let sprint pp = Format.asprintf "%a" Pp.to_fmt pp

(* When inspecting failing tests it can be useful to know more information about
   the failed assertion. Making this value [true] will print out the values of
   the two arguments to the assertion. *)
(* let verbose = true *)
let verbose = false

let assert_eq ~what n m =
  if n <> m then (
    if verbose then Printf.printf "FAIL: %s%3d != %3d\n" what n m;
    failwith (Printf.sprintf "%d is not equal to %d" n m)
  ) else if verbose then
    Printf.printf "OK: %s%3d = %3d\n" what n m

let assert_lt_or_eq ~what n m =
  if n > m then
    failwith (Printf.sprintf "%d is not less than or equal to %d" n m)
  else if verbose then
    Printf.printf "OK: %s%3d <= %3d\n" what n m

(* Here are some invariants we want when truncating a Pp:

   - The length of the truncated Pp should be less than or equal to the
   requested length.

   - The length of the printed Pp should be the same as the one reported by
   truncation. *)
let test_truncate ?ellipsis length pp =
  let pp_original = pp in
  let pp = Table.Pp.truncate ?ellipsis length pp in
  let truncated_length = Table.Pp.line_length pp in
  let observable_length = String.length (sprint pp) in
  assert_eq ~what:"\nobservable_length = truncated_length\n              "
    observable_length truncated_length;
  assert_lt_or_eq ~what:"\ntruncated_length <= length\n             "
    truncated_length length;
  if verbose then
    Printf.printf "pp original:\n%s\npp:\n%s\n--------\n"
      (sprint (Table.Pp.For_tests.pp_self pp |> Pp.vbox))
      (sprint (Table.Pp.For_tests.pp_self pp_original |> Pp.vbox));
  print (Table.Pp.For_tests.pp_self pp |> Pp.box);
  print pp

let test_length str =
  let length = Table.Pp.line_length str in
  assert_eq length ~what:"\nlength = observable_length\n   "
    (String.length (sprint str));
  print_int length

let%expect_test "truncation at the boundary" =
  test_truncate 5 (Pp.text "X");
  [%expect {|
    Text "X"
    X |}]

let%expect_test _ =
  test_truncate 5 (Pp.text "XX");
  [%expect {|
    Text "XX"
    XX |}]

let%expect_test _ =
  test_truncate 5 (Pp.text "XXX");
  [%expect {|
    Text "XXX"
    XXX |}]

let%expect_test _ =
  test_truncate 5 (Pp.text "XXXX");
  [%expect {|
    Text "XXXX"
    XXXX |}]

let%expect_test _ =
  test_truncate 5 (Pp.text "XXXXX");
  [%expect {|
    Seq Text "XX", Verbatim "..."
    XX... |}]

let%expect_test _ =
  print (Table.Pp.truncate 5 (Pp.text "XXXXXXX"));
  [%expect {| XX... |}]

let%expect_test _ =
  test_truncate 5 (Pp.text "XXXXXX");
  [%expect {|
    Seq Text "XX", Verbatim "..."
    XX... |}]

let%expect_test "truncate verbatim" =
  let example = Pp.verbatim "This is some text." in
  test_truncate 1 example;
  [%expect {|
    Verbatim "."
    . |}];
  test_truncate 3 example;
  [%expect {|
    Seq Nop, Verbatim "..."
    ... |}];
  test_truncate 5 example;
  [%expect {|
    Seq Verbatim "Th", Verbatim "..."
    Th... |}];
  test_truncate 7 example;
  [%expect {|
    Seq Verbatim "This", Verbatim "..."
    This... |}];
  test_truncate 10 example;
  [%expect {|
    Seq Verbatim "This is", Verbatim "..."
    This is... |}];
  test_truncate 15 example;
  [%expect
    {|
    Seq Verbatim "This is some", Verbatim "..."
    This is some... |}]

let%expect_test "truncation of newlines" =
  let example =
    Pp.hbox
    @@ Pp.concat ~sep:Pp.newline [ Pp.text "This is"; Pp.text "some text." ]
  in
  test_truncate 5 example;
  [%expect
    {|
    Seq Hbox Concat Newline, [ Text "Th" ], Verbatim "..."
    Th... |}];
  test_truncate 10 example;
  [%expect {|
    Hbox Concat Newline, [ Text "This is" ]
    This is |}];
  test_truncate 20 example;
  [%expect {|
    Hbox Concat Newline, [ Text "This is" ]
    This is |}]

(* FIXME *)
let%expect_test "truncation of newlines in first arg of seq" =
  let example = Pp.seq Pp.newline (Pp.verbatim "foo") in
  test_truncate 10 example;
  [%expect {| Newline |}]

(* FIXME *)
let%expect_test "truncation of newlines in second arg of seq" =
  let example = Pp.seq (Pp.verbatim "foo") Pp.newline in
  test_truncate 10 example;
  [%expect {|
    Seq Verbatim "foo", Newline
    foo |}]

(* FIXME *)
let%expect_test "truncation of newline in first arg of seq in first arg of seq"
    =
  let example =
    Pp.seq (Pp.seq (Pp.verbatim "foo") Pp.newline) (Pp.verbatim "bar")
  in
  test_truncate 10 example;
  [%expect
    {|
    Seq Seq Verbatim "foo", Newline, Verbatim "bar"
    foo
    bar |}]

(* FIXME *)
let%expect_test "truncation of new line in second arg of seq in first arg of \
                 seq" =
  let example =
    Pp.seq (Pp.seq (Pp.verbatim "foo") Pp.newline) (Pp.verbatim "bar")
  in
  test_truncate 10 example;
  [%expect
    {|
    Seq Seq Verbatim "foo", Newline, Verbatim "bar"
    foo
    bar |}]

(* FIXME *)
let%expect_test "truncation of new line in first arg of seq in second arg of \
                 seq" =
  let example =
    Pp.seq (Pp.verbatim "foo") (Pp.seq (Pp.verbatim "bar") Pp.newline)
  in
  test_truncate 10 example;
  [%expect {|
    Seq Verbatim "foo", Seq Verbatim "bar", Newline
    foobar |}]

(* FIXME *)
let%expect_test "truncation of new line in second arg of seq in second arg of \
                 seq" =
  let example =
    Pp.seq (Pp.verbatim "foo") (Pp.seq (Pp.verbatim "bar") Pp.newline)
  in
  test_truncate 10 example;
  [%expect {|
    Seq Verbatim "foo", Seq Verbatim "bar", Newline
    foobar |}]

let example =
  Pp.concat ~sep:Pp.space
    [ Pp.text "This is some text."; Pp.text "This is some more text." ]

(* FIXME *)
let%expect_test "truncation of vbox" =
  test_truncate 10 (Pp.vbox example);
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "10 is not equal to 11") |}]

(* FIXME - after break should be missing *)
let%expect_test "truncation of vbox 2" =
  test_truncate 25 (Pp.vbox example);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "18 is not equal to 19") |}]

(* FIXME *)
let%expect_test "truncation of hbox" =
  test_truncate 10 (Pp.hbox example);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "10 is not equal to 11") |}]

(* FIXME - seperator on concat is missing *)
let%expect_test "truncation of hbox 2" =
  test_truncate 25 (Pp.hbox example);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "25 is not equal to 26") |}]

(* FIXME *)
let%expect_test "truncation of hvbox" =
  test_truncate 10 (Pp.hvbox example);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "10 is not equal to 11") |}]

(* FIXME *)
let%expect_test "truncation of hvbox 2" =
  test_truncate 25 (Pp.hvbox example);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "25 is not equal to 26") |}]

(* FIXME - too much is missing *)
let%expect_test "truncation of hovbox" =
  test_truncate 10 (Pp.hovbox example);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "10 is not equal to 11") |}]

(* FIXME *)
let%expect_test "truncation of hovbox 2" =
  test_truncate 25 (Pp.hovbox example);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "25 is not equal to 26") |}]

(* FIXME *)
let%expect_test "truncation of box" =
  test_truncate 10 (Pp.box example);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "10 is not equal to 11") |}]

(* FIXME *)
let%expect_test "truncation of box 2" =
  test_truncate 25 (Pp.box example);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "25 is not equal to 26") |}]

let%expect_test "truncatation less than 3" =
  (* Should not emit more than 2 characters *)
  test_truncate 2 (Pp.text "XXXX");
  [%expect {|
    Verbatim ".."
    .. |}];
  test_truncate 2 (Pp.text "X");
  [%expect {|
    Text "X"
    X |}]

let%expect_test "truncatation with newlines" =
  (* Should not emit newlines *)
  test_truncate 20 (Pp.text "XX\nXX");
  [%expect {|
    Text "XX\n\
          XX"
    XX
    XX |}]

(* Truncation with boxes and breaks *)

(* Truncation should mostly preserve boxes, however boxes will affect the
   interpretation of breaks, i.e. whether or not the produce a line break.
   Truncation should aim to restrict to a single line, so line breaks are a good
   indication that we need to end the truncation. *)

(* Space is a break that does no shifting but adds 1 space. We should see
   truncation take that length into account. *)
let space_example =
  let open Pp.O in
  Pp.verbatim "First" ++ Pp.space ++ Pp.verbatim "Second"

(* All lines eventually break, so we need to see what happens when they are
   truncated. *)
let long_example =
  Pp.concat_map ~sep:Pp.space ~f:Pp.verbatim
    [ "AAAAAAAA"
    ; "BBBBBBBB"
    ; "CCCCCCCC"
    ; "DDDDDDDD"
    ; "EEEEEEEE"
    ; "FFFFFFFF"
    ; "GGGGGGGG"
    ; "HHHHHHHH"
    ; "IIIIIIII"
    ; "JJJJJJJJ"
    ; "KKKKKKKK"
    ]

(* When there is no box, truncation should make no assumption about the line
   breaking and still render the break. Unfortunately this means that long lines
   may wrap unintentionally. *)
let%expect_test "break with no box" =
  test_truncate 10 space_example;
  [%expect
    {|
    Seq
      Seq Seq Verbatim "First", Break ("", 1, ""), ("", 0, ""), Verbatim "S",
      Verbatim "..."
    First
    S... |}]

let%expect_test "long break with no box" =
  test_truncate 90 long_example;
  [%expect
    {|
      Concat Break ("", 1, ""), ("", 0, ""), [ Verbatim "AAAAAAAA" ]
      AAAAAAAA |}]

(* When there is a hbox, the situation is easy since we never break. Unless we
   are at the magin. *)
let%expect_test "break with hbox" =
  test_truncate 10 (space_example |> Pp.hbox);
  [%expect
    {|
    Seq
      Hbox Seq Seq Verbatim "First", Break ("", 1, ""), ("", 0, ""), Verbatim "S",
      Verbatim "..."
    First S... |}]

(* FIXME *)
let%expect_test "long with hbox" =
  test_truncate 90 (long_example |> Pp.hbox);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "90 is not equal to 91") |}]

(* A vertical box will always break, therefore only the pp before the break
   should be rendered. *)
(* FIXME *)
let%expect_test "break with vbox" =
  test_truncate 10 (space_example |> Pp.vbox);
  [%expect
    {|
    Seq
      Hbox Seq Seq Verbatim "First", Break ("", 1, ""), ("", 0, ""), Verbatim "S",
      Verbatim "..."
    First S... |}]

(* FIXME *)
let%expect_test "long with vbox" =
  test_truncate 90 (long_example |> Pp.vbox);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "8 is not equal to 9") |}]

(* A hbox with a break will act like a hbox and become a vbox when at the
   margin. *)
let%expect_test "break with hvbox" =
  test_truncate 10 (space_example |> Pp.hvbox);
  [%expect
    {|
    Seq
      Hbox Seq Seq Verbatim "First", Break ("", 1, ""), ("", 0, ""), Verbatim "S",
      Verbatim "..."
    First S... |}]

(* FIXME *)
let%expect_test "long with hvbox" =
  test_truncate 90 (long_example |> Pp.hvbox);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "90 is not equal to 91") |}]

let%expect_test "break with hovbox" =
  test_truncate 10 (space_example |> Pp.hovbox);
  [%expect
    {|
    Seq
      Hbox Seq Seq Verbatim "First", Break ("", 1, ""), ("", 0, ""), Verbatim "S",
      Verbatim "..."
    First S... |}]

(* Hovbox puts as much as possible on a single line, so truncation will act the
   same as for hbox similarly for the margin. *)
(* FIXME *)
let%expect_test "long with hovbox" =
  test_truncate 90 (long_example |> Pp.hovbox);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "90 is not equal to 91") |}]

let%expect_test "break with box" =
  test_truncate 10 (space_example |> Pp.box);
  [%expect
    {|
    Seq
      Hbox Seq Seq Verbatim "First", Break ("", 1, ""), ("", 0, ""), Verbatim "S",
      Verbatim "..."
    First S... |}]

(* FIXME *)
let%expect_test "long with box" =
  test_truncate 90 (long_example |> Pp.box);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "90 is not equal to 91") |}]

(* Length tests *)

let%expect_test "length tests" =
  test_length (Pp.verbatim "X");
  [%expect {|
    1 |}];
  test_length (Pp.verbatim "XX");
  [%expect {|
    2 |}];
  test_length (Pp.verbatim "XXX");
  [%expect {|
    3 |}];
  test_length (Pp.verbatim "XXXX");
  [%expect {|
    4 |}];
  test_length (Pp.verbatim "XXXXX");
  [%expect {|
    5 |}];
  test_length (Pp.verbatim "XXXXXX");
  [%expect {|
    6 |}];
  test_length (Pp.verbatim "XXXXXXX");
  [%expect {|
    7 |}];
  test_length (Pp.verbatim "XXXXXXXX");
  [%expect {|
    8 |}]

let%expect_test "legnth of verbatim with space" =
  test_length (Pp.verbatim "123 456");
  [%expect {|
    7 |}]

let%expect_test "length of text with space" =
  test_length (Pp.text "123 456");
  [%expect {|
    7 |}]

let%expect_test "length of seq without space" =
  test_length (Pp.seq (Pp.verbatim "123") (Pp.verbatim "456"));
  [%expect {|
    6 |}]

let%expect_test "length of seq with space" =
  test_length (Pp.seq (Pp.seq (Pp.verbatim "123") Pp.space) (Pp.verbatim "456"));
  [%expect {| 7 |}]

let%expect_test "length of concat without space" =
  test_length (Pp.concat [ Pp.text "123"; Pp.text "456" ]);
  [%expect {| 6 |}]

(* FIXME *)
let%expect_test "length of concat with space" =
  test_length (Pp.concat ~sep:Pp.space [ Pp.text "123"; Pp.text "456" ]);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure "3 is not equal to 7") |}]

let%expect_test "truncate concat" =
  let test length strs =
    test_truncate ~ellipsis:"" length
      (Pp.concat_map ~f:Pp.verbatim ~sep:Pp.space strs)
  in
  test 5 [ "123"; "456" ];
  [%expect
    {|
    Concat Break ("", 1, ""), ("", 0, ""), [ Verbatim "123" ]
    123 |}];
  test 3 [ "123"; "456" ];
  [%expect
    {|
    Seq Concat Break ("", 1, ""), ("", 0, ""), [ Verbatim "123" ], Verbatim ""
    123 |}]

(* Ellipisis test *)

let%expect_test "truncate with different ellipses" =
  let test ellipsis = test_truncate ~ellipsis 5 (Pp.text "-----") in
  test "";
  [%expect {|
    Seq Text "-----", Verbatim ""
    ----- |}];
  test "X";
  [%expect {|
    Seq Text "----", Verbatim "X"
    ----X |}];
  test "XX";
  [%expect {|
    Seq Text "---", Verbatim "XX"
    ---XX |}];
  test "XXX";
  [%expect {|
    Seq Text "--", Verbatim "XXX"
    --XXX |}];
  test "XXXX";
  [%expect {|
    Seq Text "-", Verbatim "XXXX"
    -XXXX |}];
  test "XXXXX";
  [%expect {|
    Seq Nop, Verbatim "XXXXX"
    XXXXX |}];
  test "XXXXXX";
  [%expect {|
    Verbatim "XXXXX"
    XXXXX |}]
