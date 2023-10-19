module List = ListLabels
module String = StringLabels
module Pp = Pp0

module For_tests = struct
  module Cell = Cell
end

type +'a t =
  | Cell of 'a Cell.t
  | Join_x of 'a t list
  | Join_y of 'a t list

let rec flatten = function
  | Cell cell_data -> cell_data
  | Join_x ts ->
    List.fold_left ts ~init:Cell.empty ~f:(fun acc x ->
        Cell.join_x acc (flatten x))
  | Join_y ts ->
    List.fold_left ts ~init:Cell.empty ~f:(fun acc x ->
        Cell.join_y acc (flatten x))

let pp t = flatten t |> Cell.pp
let cell ~width ?align pp = Cell (Cell.make ~width ?align pp)
let join_x ts = Join_x ts
let join_y ts = Join_y ts

let tabulate_columns ?(left = fun ~row:_ ~col:_ -> 0)
    ?(right = fun ~row:_ ~col:_ -> 1) ?(top = fun ~row:_ ~col:_ -> 0)
    ?(bottom = fun ~row:_ ~col:_ -> 1) ~width ~align (data : 'a Pp.t list list)
    =
  let columns =
    List.mapi data ~f:(fun col columns ->
        join_y
          (List.mapi
             ~f:(fun row row_ ->
               row_
               |> Cell.make ~width:(width ~row ~col) ~align:(align ~row ~col)
               |> Cell.pad
                    { left = left ~row ~col
                    ; right = right ~row ~col
                    ; top = top ~row ~col
                    ; bottom = bottom ~row ~col
                    ; kind = `Space
                    }
               |> fun row -> Cell row)
             columns))
  in
  join_x columns

let tabulate_rows ?(left = fun ~row:_ ~col:_ -> 0)
    ?(right = fun ~row:_ ~col:_ -> 1) ?(top = fun ~row:_ ~col:_ -> 0)
    ?(bottom = fun ~row:_ ~col:_ -> 1) ~width ~align (data : 'a Pp.t list list)
    =
  let rows =
    List.mapi data ~f:(fun row rows ->
        join_x
          (List.mapi
             ~f:(fun col column ->
               column
               |> Cell.make ~width:(width ~row ~col) ~align:(align ~row ~col)
               |> Cell.pad
                    { left = left ~row ~col
                    ; right = right ~row ~col
                    ; top = top ~row ~col
                    ; bottom = bottom ~row ~col
                    ; kind = `Space
                    }
               |> fun column -> Cell column)
             rows))
  in
  join_y rows
