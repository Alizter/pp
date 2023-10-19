module List = ListLabels
module String = StringLabels
module Pp = Pp0

(* Invariants: [rows] has length [height] and each row has at most length
   [width]. *)
type +'a t =
  { width : int
  ; height : int
  ; rows : 'a Pp.t list
  ; align : alignment
  }

and alignment =
  [ `Left
  | `Center
  | `Right
  | `Normalized
  | `TODO
  ]

(* TODO: add align arg when we are able to find the size of Pps *)
let pp { rows; _ } = Pp.vbox (Pp.concat_map ~sep:Pp.cut rows ~f:Pp.hbox)

let cumulative_char_count words =
  List.fold_left words ~init:[] ~f:(function
    | [] -> fun word -> [ (String.length word, word) ]
    | (length_so_far, _) :: _ as acc ->
      fun word -> (length_so_far + String.length word + 1, word) :: acc)
  |> List.rev

let hard_wrap ~width words =
  let rec loop offset acc = function
    | [] -> [ List.rev acc ]
    | (length, word) :: words ->
      if length <= offset then
        loop offset (word :: acc) words
      else
        List.rev acc :: loop (offset + width) [ word ] words
  in
  cumulative_char_count words
  |> loop width []
  |> List.map ~f:(Pp.concat_map ~sep:Pp.space ~f:Pp.verbatim)

(* [normalize t] ensures that each row has length [width]. *)
let normalize { width; height; rows; align } =
  match align with
  | `Normalized -> { width; height; rows; align }
  | _ ->
    let rows =
      List.map rows ~f:(fun row ->
          (* We calculate the width of the row by converting it to a string. We
             don't use the length calculation that comes with truncation as the
             trailing spaces would be discounted. *)
          let row_width =
            (* Pp.line_length row *)
            String.length (Format.asprintf "%a" Pp.to_fmt row)
          in
          match Int.compare row_width width with
          (* If the row is exactly the right length, do nothing. *)
          | 0 -> row
          (* If the row is too long, truncate it. *)
          | 1 -> Pp.truncate width row
          (* If the row is too short, pad it. *)
          | -1 ->
            (match align with
            | `Left ->
              Pp.seq row (Pp.break ~nspaces:(width - row_width) ~shift:0)
            | `Center ->
              let left_padding = (width - row_width) / 2 in
              let right_padding = width - row_width - left_padding in
              Pp.seq
                (Pp.break ~nspaces:left_padding ~shift:0)
                (Pp.seq row (Pp.break ~nspaces:right_padding ~shift:0))
            | `Right ->
              Pp.seq (Pp.break ~nspaces:(width - row_width) ~shift:0) row
            | `Normalized -> row
            | `TODO -> failwith "TODO")
            |> Pp.hbox
          | _ -> assert false)
    in
    { width; height; rows; align = `Normalized }

let make ~width ?(align : [ `Left | `Center | `Right ] = `Left) pp =
  (* This kills formatting but that's the price for putting it in a cell. *)
  let rec extract_words pp =
    match (pp : _ Pp.Ast.t) with
    | Text text -> String.split_on_char ~sep:' ' text
    | Verbatim text -> [ text ]
    | Hbox pp
    | Hovbox (_, pp)
    | Hvbox (_, pp)
    | Box (_, pp)
    | Vbox (_, pp) ->
      extract_words pp
    | Break _ -> []
    | Seq (pp1, pp2) -> extract_words pp1 @ extract_words pp2
    (* TODO: put seperator back in *)
    | Concat (_, pps) -> List.concat_map pps ~f:extract_words
    | pp ->
      failwith
        (Format.asprintf "extract_words: %a" Pp.to_fmt
           (Pp.For_tests.pp_self (Pp.of_ast pp)))
  in
  let rows = Pp.to_ast pp |> extract_words |> hard_wrap ~width in
  { width; height = List.length rows; rows; align :> alignment } |> normalize

let join_x t1 t2 =
  let height = max t1.height t2.height in
  let rows =
    List.init ~len:height ~f:(fun i ->
        let row1 = List.nth_opt t1.rows i in
        let row2 = List.nth_opt t2.rows i in
        match (row1, row2) with
        | None, None -> assert false
        | Some row, None ->
          Pp.hbox (Pp.seq row (Pp.break ~nspaces:t2.width ~shift:0))
        | None, Some row ->
          Pp.hbox (Pp.seq (Pp.break ~nspaces:t1.width ~shift:0) row)
        | Some row1, Some row2 -> Pp.hbox (Pp.seq row1 row2))
  in
  { width = t1.width + t2.width; height; rows; align = `Normalized }

let join_y t1 t2 =
  let width = max t1.width t2.width in
  let rows = t1.rows @ t2.rows in
  { width; height = List.length rows; rows; align = `TODO }

let empty = { width = 0; height = 0; rows = []; align = `Left }

module Padding = struct
  type t =
    { top : int
    ; bottom : int
    ; left : int
    ; right : int
    ; kind : [ `Space | `Debug ]
    }
end

let pad { Padding.top; bottom; left; right; kind } cell =
  let { width; height; rows; align } = cell in
  let width = width + left + right in
  let height = height + top + bottom in
  let top_rows =
    List.init ~len:top ~f:(fun _ ->
        match kind with
        | `Space -> Pp.break ~nspaces:width ~shift:0
        | `Debug -> Pp.verbatim (String.make width '@'))
  in
  let bottom_rows =
    List.init ~len:bottom ~f:(fun _ ->
        match kind with
        | `Space -> Pp.break ~nspaces:width ~shift:0
        | `Debug -> Pp.verbatim (String.make width '@'))
  in
  let rows =
    top_rows
    @ List.init
        ~len:(height - top - bottom)
        ~f:(fun i ->
          let row =
            match List.nth_opt rows i with
            | None -> Pp.verbatim ""
            | Some row -> row
          in
          let left =
            match kind with
            | `Space -> Pp.break ~nspaces:left ~shift:0
            | `Debug -> Pp.verbatim (String.make left '@')
          in
          let right =
            match kind with
            | `Space -> Pp.break ~nspaces:right ~shift:0
            | `Debug -> Pp.verbatim (String.make right '@')
          in
          Pp.concat [ left; row; right ])
    @ bottom_rows
  in
  { width; height; rows; align }

module For_tests = struct
  let verbose = ref false

  let assert_eq ~what n m =
    if n <> m then (
      if !verbose then Printf.eprintf "FAIL: %s%3d != %3d\n" what n m;
      failwith (Printf.sprintf "%d is not equal to %d" n m)
    ) else if !verbose then
      Printf.eprintf "OK: %s%3d = %3d\n" what n m

  let assert_lt_or_eq ~what n m =
    if n > m then (
      if !verbose then Printf.eprintf "FAIL: %s%3d >  %3d\n" what n m;
      failwith (Printf.sprintf "%d is not less than or equal to %d" n m)
    ) else if !verbose then
      Printf.eprintf "OK: %s%3d <= %3d\n" what n m

  let assert_invariants { width; height; rows; align } =
    assert_lt_or_eq ~what:"width" 0 width;
    assert_lt_or_eq ~what:"height" 0 height;
    List.iter rows ~f:(fun row ->
        let row_length = Format.asprintf "%a" Pp.to_fmt row |> String.length in
        assert_lt_or_eq ~what:"0 <= row length" 0 row_length;
        assert_lt_or_eq
          ~what:
            (Format.asprintf "row:\n%a\n        row length <= %d" Pp.to_fmt row
               width)
          row_length width;
        assert_lt_or_eq ~what:"row length <= width" row_length width;
        assert_eq ~what:"rows length = height" (List.length rows) height);
    match align with
    | `Normalized ->
      List.iter rows ~f:(fun row ->
          let row_length =
            Format.asprintf "%a" Pp.to_fmt row |> String.length
          in
          assert_eq
            ~what:(Format.asprintf "row:\n%a\n    length = width" Pp.to_fmt row)
            row_length width)
    | _ -> ()
end
