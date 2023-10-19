module String = StringLabels
module List = ListLabels
module Pp = Table.Pp

let verbose = false
let print_pp pp = Format.printf "%a@." Pp.to_fmt pp
let print table = print_pp (Table.pp table)
let lorem_ipsum = "Lorem ipsum dolor sit amet consectetur adipiscing elit."

let%expect_test "simple single cell" =
  let cell width = Table.cell ~width (Pp.text "XXXXX") |> print in
  cell 0;
  [%expect {| |}];
  cell 1;
  [%expect {|
    . |}];
  cell 2;
  [%expect {|
    .. |}];
  cell 3;
  [%expect {|
    ... |}];
  cell 4;
  [%expect {|
    X... |}];
  cell 5;
  [%expect {|
    XXXXX |}]

let%expect_test "single cell with hard wrapping" =
  let cell n = Table.cell ~width:n (Pp.text lorem_ipsum) |> print in
  cell 5;
  [%expect {|
Lorem
ipsum
dolor
sit
amet
co...
ad...
elit. |}];
  cell 10;
  [%expect {|
Lorem
ipsum
sit amet
consect...
adipiscing
elit. |}];
  cell 20;
  [%expect {|
Lorem ipsum dolor
sit amet consectetur
adipiscing elit. |}];
  cell 30;
  [%expect {|
Lorem ipsum dolor sit amet
consectetur adipiscing elit. |}];
  cell 40;
  [%expect {|
Lorem ipsum dolor sit amet consectetur
adipiscing elit. |}];
  cell 50;
  [%expect {|
Lorem ipsum dolor sit amet consectetur adipiscing
elit. |}];
  cell 60;
  [%expect {|
Lorem ipsum dolor sit amet consectetur adipiscing elit. |}]

let%expect_test "join_x solid" =
  let xs = Table.cell ~width:10 (Pp.text @@ String.init 90 ~f:(fun _ -> 'x')) in
  print xs;
  [%expect {|
    xxxxxxx... |}];
  let ys = Table.cell ~width:10 (Pp.text @@ String.init 90 ~f:(fun _ -> 'y')) in
  print ys;
  [%expect {|
    yyyyyyy... |}];
  Table.join_x [ xs; ys ] |> print;
  [%expect {| xxxxxxx...yyyyyyy... |}]

let make_paragraph len word =
  Pp.text @@ String.concat ~sep:" " @@ List.init ~len ~f:(fun _ -> word)

let xs ~align = Table.cell ~align ~width:20 @@ make_paragraph 45 "x"
let ys ~align = Table.cell ~align ~width:20 @@ make_paragraph 25 "y"

let%expect_test "join_x solid" =
  Table.join_x [ xs ~align:`Left; ys ~align:`Left ] |> print;
  [%expect
    {|
    x x x x x x x x x x y y y y y y y y y y
    x x x x x x x x x x y y y y y y y y y y
    x x x x x x x x x x y y y y y
    x x x x x x x x x x
    x x x x x |}];
  Table.join_x [ xs ~align:`Left; ys ~align:`Right ] |> print;
  [%expect
    {|
    x x x x x x x x x x  y y y y y y y y y y
    x x x x x x x x x x  y y y y y y y y y y
    x x x x x x x x x x            y y y y y
    x x x x x x x x x x
    x x x x x |}];
  Table.join_x [ ys ~align:`Left; xs ~align:`Left ] |> print;
  [%expect
    {|
    y y y y y y y y y y x x x x x x x x x x
    y y y y y y y y y y x x x x x x x x x x
    y y y y y           x x x x x x x x x x
                        x x x x x x x x x x
                        x x x x x |}];
  Table.join_x [ ys ~align:`Left; xs ~align:`Right ] |> print;
  [%expect
    {|
    y y y y y y y y y y  x x x x x x x x x x
    y y y y y y y y y y  x x x x x x x x x x
    y y y y y            x x x x x x x x x x
                         x x x x x x x x x x
                                   x x x x x |}];
  Table.join_x [ xs ~align:`Center; ys ~align:`Center ] |> print;
  [%expect
    {|
    x x x x x x x x x x y y y y y y y y y y
    x x x x x x x x x x y y y y y y y y y y
    x x x x x x x x x x      y y y y y
    x x x x x x x x x x
         x x x x x |}]

let%expect_test "join_y solid" =
  Table.join_y [ xs ~align:`Left; ys ~align:`Left ] |> print;
  [%expect
    {|
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x
    y y y y y y y y y y
    y y y y y y y y y y
    y y y y y |}];
  Table.join_y [ ys ~align:`Left; xs ~align:`Left ] |> print;
  [%expect
    {|
    y y y y y y y y y y
    y y y y y y y y y y
    y y y y y
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x |}]

(* Padding tests *)

let cell =
  Table.For_tests.Cell.make ~width:20 ~align:`Left
    (Pp.text "Some text that will fill this cell up nicely.")

let print_cell cell =
  let pp = Table.For_tests.Cell.pp cell in
  Table.For_tests.Cell.For_tests.verbose := verbose;
  Table.For_tests.Cell.For_tests.assert_invariants cell;
  if verbose then print_pp (Table.Pp.For_tests.pp_self pp);
  print_pp pp

let test_padding left right top bottom =
  Table.For_tests.Cell.pad { left; right; top; bottom; kind = `Debug } cell
  |> print_cell

let%expect_test "cell" =
  print_cell cell;
  [%expect {|
    Some text that will
    fill this cell up
    nicely. |}]

let%expect_test "normalization" =
  print_cell (Table.For_tests.Cell.normalize cell);
  [%expect {|
    Some text that will
    fill this cell up
    nicely. |}]

let%expect_test "left padding" =
  test_padding 1 0 0 0;
  [%expect {|
    @Some text that will
    @fill this cell up
    @nicely. |}]

let%expect_test "right padding" =
  test_padding 0 1 0 0;
  [%expect
    {|
    Some text that will @
    fill this cell up   @
    nicely.             @ |}]

let%expect_test "top padding" =
  test_padding 0 0 1 0;
  [%expect
    {|
    @@@@@@@@@@@@@@@@@@@@
    Some text that will
    fill this cell up
    nicely. |}]

let%expect_test "bottom padding" =
  test_padding 0 0 0 1;
  [%expect
    {|
    Some text that will
    fill this cell up
    nicely.
    @@@@@@@@@@@@@@@@@@@@ |}]

let%expect_test "left, right, top, and bottom padding" =
  test_padding 1 1 1 1;
  [%expect
    {|
    @@@@@@@@@@@@@@@@@@@@@@
    @Some text that will @
    @fill this cell up   @
    @nicely.             @
    @@@@@@@@@@@@@@@@@@@@@@ |}]

(* Cells and pp *)

(* Verbatim text in cells only gets truncated*)
let%expect_test "cell verbatim" =
  Pp.verbatim
    "This sentence is verbatim and will not wrap. So this part will be cut off."
  |> Table.For_tests.Cell.make ~width:54 ~align:`Left
  |> print_cell;
  [%expect {| This sentence is verbatim and will not wrap. So thi... |}]

let%expect_test "cell with seq with no box" =
  Pp.seq
    (Pp.text "This is a sequence of text.")
    (Pp.text " That should eventually wrap, but not after the first sentence.")
  |> Table.For_tests.Cell.make ~width:54 ~align:`Left
  |> print_cell;
  [%expect
    {|
    This is a sequence of text.  That should eventually
    wrap, but not after the first sentence. |}]

let%expect_test "cell with seq with hbox" =
  Pp.seq (Pp.text "This is a hbox") (Pp.text "with two parts.")
  |> Pp.hbox
  |> Table.For_tests.Cell.make ~width:54 ~align:`Left
  |> print_cell;
  [%expect {| This is a hbox with two parts. |}]

let%expect_test "cell with seq with vbox" =
  Pp.seq (Pp.text "This is a vbox") (Pp.text "with two parts.")
  |> Pp.vbox
  |> Table.For_tests.Cell.make ~width:54 ~align:`Left
  |> print_cell;
  [%expect {| This is a vbox with two parts. |}]

let%expect_test "cell with seq with box" =
  Pp.seq (Pp.text "This is a box") (Pp.text "with two parts.")
  |> Pp.box
  |> Table.For_tests.Cell.make ~width:54 ~align:`Left
  |> print_cell;
  [%expect {| This is a box with two parts. |}]

let%expect_test "cell with seq with hvbox" =
  Pp.seq (Pp.text "This is a hvbox") (Pp.text "with two parts.")
  |> Pp.hvbox
  |> Table.For_tests.Cell.make ~width:54 ~align:`Left
  |> print_cell;
  [%expect {| This is a hvbox with two parts. |}]

let%expect_test "cell with seq with hovbox" =
  Pp.seq (Pp.text "This is a hovbox") (Pp.text "with two parts.")
  |> Pp.hovbox
  |> Table.For_tests.Cell.make ~width:54 ~align:`Left
  |> print_cell;
  [%expect {| This is a hovbox with two parts. |}]

let%expect_test "tabulate" =
  let align ~row:_ ~col:_ = `Left in
  let width ~row:_ ~col:_ = 20 in
  let table =
    Table.tabulate_rows ~align ~width
      [ [ Pp.text "This is a table where the cells."
        ; Pp.text "Have hard wrapping."
        ]
      ; [ Pp.text "And there can be multiple rows."
        ; Pp.text "And multiple columns."
        ]
      ]
  in
  print table;
  [%expect
    {|
    This is a table      Have hard wrapping.
    where the cells.

    And there can be     And multiple
    multiple rows.       columns. |}];
  print (Table.join_y [ Table.join_x [ table; table ]; table ]);
  [%expect
    {|
    This is a table      Have hard wrapping.  This is a table      Have hard wrapping.
    where the cells.                          where the cells.

    And there can be     And multiple         And there can be     And multiple
    multiple rows.       columns.             multiple rows.       columns.

    This is a table      Have hard wrapping.
    where the cells.

    And there can be     And multiple
    multiple rows.       columns. |}]

let%expect_test "tabulate larger text" =
  let align ~row ~col:_ =
    match row mod 3 with
    | 0 -> `Left
    | 1 -> `Center
    | _ -> `Right
  in
  let width ~row:_ ~col:_ = 20 in
  let table =
    Table.tabulate_rows ~align ~width
      [ [ Pp.text lorem_ipsum; Pp.text lorem_ipsum ]
      ; [ Pp.text lorem_ipsum; Pp.text lorem_ipsum ]
      ; [ Pp.text lorem_ipsum; Pp.text lorem_ipsum ]
      ]
  in
  print table;
  [%expect
    {|
    Lorem ipsum dolor    Lorem ipsum dolor
    sit amet consectetur sit amet consectetur
    adipiscing elit.     adipiscing elit.

     Lorem ipsum dolor    Lorem ipsum dolor
    sit amet consectetur sit amet consectetur
      adipiscing elit.     adipiscing elit.

       Lorem ipsum dolor    Lorem ipsum dolor
    sit amet consectetur sit amet consectetur
        adipiscing elit.     adipiscing elit. |}]

let%expect_test "tabulate 2" =
  let align ~row ~col =
    match row with
    | 0 -> `Center
    | _ -> (
      match col with
      | 0 -> `Right
      | _ -> `Center)
  in
  let width ~row:_ ~col:_ = 10 in
  let left ~row:_ ~col:_ = 0 in
  let right ~row:_ ~col:_ = 1 in
  let top ~row:_ ~col:_ = 0 in
  let bottom ~row:_ ~col:_ = 1 in
  let table =
    Table.tabulate_rows ~left ~right ~top ~bottom ~align ~width
      [ [ Pp.text "Name"; Pp.text "Age"; Pp.text "Grade" ]
      ; [ Pp.text "Alice"; Pp.text "28"; Pp.text "A" ]
      ; [ Pp.text "Bob"; Pp.text "35"; Pp.text "B+" ]
      ; [ Pp.text "Charlie"; Pp.text "22"; Pp.text "C-" ]
      ; [ Pp.text "David"; Pp.text "42"; Pp.text "A" ]
      ; [ Pp.text "Eve"; Pp.text "31"; Pp.text "B" ]
      ; [ Pp.text "Score"; Pp.textf "%f" 95.5; Pp.textf "%d" 88 ]
      ; [ Pp.text "Description"; Pp.text "N/A"; Pp.text "extra"; Pp.text "row" ]
      ]
  in
  print table;
  [%expect
    {|
   Name       Age       Grade

     Alice     28         A

       Bob     35         B+

   Charlie     22         C-

     David     42         A

       Eve     31         B

     Score 95.500000      88

              N/A       extra       row
Descrip... |}]

let%expect_test "tabulate 3" =
  let align ~row ~col =
    match row with
    | 0 -> `Center
    | _ -> (
      match col with
      | 0 -> `Right
      | _ -> `Center)
  in
  let width ~row:_ ~col:_ = 50 in
  let right ~row:_ ~col:_ = 1 in
  let bottom ~row:_ ~col:_ = 1 in
  let table =
    Table.tabulate_rows ~right ~bottom ~align ~width
      [ [ Pp.text "Topic"; Pp.text "Answers" ]
      ; [ Pp.text "Reasons why OCamls like Dunes"
        ; Pp.enumerate ~f:Pp.text [ "It's dry"; "It's sandy"; "It's warm" ]
        ]
      ]
  in
  print table;
  [%expect
    {|
     Topic                                             Answers

    Reasons why OCamls like Dunes        - It's dry - It's sandy - It's warm |}]
