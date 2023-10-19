module List = ListLabels

module String = struct
  include StringLabels

  let take s len = sub s ~pos:0 ~len:(min (length s) len)
end

include Pp

let length_and_truncate offset max_length (type tag) (pp : tag Pp.t) =
  let exception Stop of int * tag Pp.Ast.t list in
  (* [loop length_so_far mode pp] returns the length of [pp] and a truncated
     version of [pp] that is at most [max_length] characters long. [mode] tells
     us what to do when we encounter a break. *)
  let rec loop length_so_far mode (pp : tag Pp.Ast.t) : int * tag Pp.Ast.t =
    if length_so_far >= max_length then
      (0, Nop)
    else
      match ((pp, mode) : tag Pp.Ast.t * _) with
      | Nop, _ -> (0, Nop)
      (* Need to think about newline *)
      | Newline, _ -> raise_notrace (Stop (1, [ Newline ]))
      (* Encountering a break in mode `Hbox means we treat it like a space. We
         aren't concerned with shift because we only care about one line. *)
      | ( (Break ((before, nspaces, after), _) as break)
        , (`Hbox | `Hovbox | `Hvbox | `Box) ) ->
        (nspaces + String.length before + String.length after, break)
      | (Break ((before, nspaces, after), _) as break), (`Vbox | `None) ->
        raise_notrace
          (Stop (nspaces + String.length before + String.length after, [ break ]))
      (* Seq has exceptions for early exits due to breaks to a newline *)
      | Seq (pp1, pp2), _ -> (
        match loop length_so_far mode pp1 with
        | exception Stop (length_so_far, [ pp1 ]) -> (length_so_far, pp1)
        | length_of_pp1, pp1 -> (
          match loop (length_so_far + length_of_pp1) mode pp2 with
          | exception Stop (length_so_far, [ pp2 ]) ->
            (length_so_far + length_of_pp1, Seq (pp1, pp2))
          | length_of_pp2, pp2 -> (length_of_pp1 + length_of_pp2, Seq (pp1, pp2))
          ))
      (* Concat does an early exit if the seperator causes a newline *)
      | Concat (sep, pps), _ -> (
        match loop length_so_far mode sep with
        | exception Stop (_, [ sep ]) -> (
          (* The seperator causes breaks, in this case we can only return the
             first pp *)
          match pps with
          | [] -> (length_so_far, Concat (sep, []))
          | pp :: _ -> (
            match loop length_so_far mode pp with
            | (exception Stop (length_so_far, [ pp ]))
            | length_so_far, pp ->
              (length_so_far, Concat (sep, [ pp ]))
              (* TODO: breaks in pps are perhaps not handed correctly *)))
        | length_of_sep, sep ->
          let length_of_pps, pps =
            try
              List.fold_left pps ~init:(length_so_far, [ Nop ])
                ~f:(fun (length_so_far, pps) pp ->
                  if length_so_far >= max_length then
                    raise_notrace (Stop (length_so_far, pps))
                  else
                    let length_so_far = length_so_far + length_of_sep in
                    if length_so_far >= max_length then
                      raise_notrace (Stop (length_so_far, pps @ [ Nop ]))
                    else
                      let length, pp = loop length_so_far mode pp in
                      (length_so_far + length, pps @ [ pp ]))
            with
            | Stop (length_so_far, pps) -> (length_so_far, pps)
          in
          (length_of_pps, Concat (sep, pps)))
      (* All boxes are converted to hboxes. *)
      (* Hboxes are preseved, mode is switched to hbox *)
      | Hbox pp, _ ->
        let length, pp = loop length_so_far `Hbox pp in
        (length, Hbox pp)
      (* Hovboxes are converted to hboxes, mode is switched to hovbox *)
      | Hovbox (_, pp), _ ->
        let length, pp = loop length_so_far `Hovbox pp in
        (length, Hbox pp)
      (* Hvboxes are converted to hboxes, mode is switched to hvbox *)
      | Hvbox (_, pp), _ ->
        let length, pp = loop length_so_far `Hvbox pp in
        (length, Hbox pp)
      (* Boxes are converted to hboxes, mode is switched to box *)
      | Box (_, pp), _ ->
        let length, pp = loop length_so_far `Box pp in
        (length, Hbox pp)
      (* Vboxes are converted to hboxes, mode is switched to vbox *)
      | Vbox (_, pp), _ ->
        let length, pp = loop length_so_far `Vbox pp in
        (length, Hbox pp)
      (* Tags are preserved. *)
      | Tag (tag, pp), _ ->
        let length, pp = loop length_so_far mode pp in
        (length, Tag (tag, pp))
      (* Verbatim strings are truncated as strings. *)
      | Verbatim s, _ ->
        let length_of_s = String.length s in
        if length_so_far + length_of_s > max_length then
          ( max_length - length_so_far
          , Verbatim (String.take s (max_length - length_so_far)) )
        else
          (length_of_s, Verbatim s)
      (* TODO this should depend on the mode. *)
      (* Text strings are truncated as strings. *)
      | Text s, _ ->
        let length_of_s = String.length s in
        if length_so_far + length_of_s > max_length then
          ( max_length - length_so_far
          , Text (String.take s (max_length - length_so_far)) )
        else
          (length_of_s, Text s)
      | Char c, _ ->
        if length_so_far + 1 > max_length then
          (0, Nop)
        else
          (1, Char c)
  in
  match loop offset `None (Pp.to_ast pp) with
  | (exception Stop (truncated_length, [ pp ]))
  | truncated_length, pp ->
    (truncated_length, Pp.of_ast pp)

let truncate ?(ellipsis = "...") max_length pp =
  let truncated_length, pp = length_and_truncate 0 max_length pp in
  (* TODO: is this assert good to keep? *)
  (* assert (truncated_length <= max_length); *)
  if truncated_length < max_length then
    pp
  else if String.length ellipsis > max_length then
    Pp.verbatim (String.take ellipsis max_length)
  else
    let _, pp_new =
      length_and_truncate (String.length ellipsis) max_length pp
    in
    Pp.seq pp_new (Pp.verbatim ellipsis)

let line_length pp = fst (length_and_truncate 0 Int.max_int pp)

module For_tests = struct
  module Dyn = struct
    type t =
      | Opaque
      | Int of int
      | String of string
      | Char of char
      | List of t list
      | Tuple of t list
      | Variant of string * t list

    let unsnoc l =
      match List.rev l with
      | last :: before_last -> Some (List.rev before_last, last)
      | [] -> None

    let string_in_ocaml_syntax str =
      let is_space = function
        | ' ' ->
          (* don't need to handle tabs because those are already escaped *)
          true
        | _ -> false
      in
      let escape_protect_first_space s =
        let first_char =
          if String.length s > 0 && is_space s.[0] then
            "\\"
          else
            " "
        in
        first_char ^ String.escaped s
      in
      (* CR-someday aalekseyev: should use the method from
         [Dune_lang.prepare_formatter] so that the formatter can fit multiple
         lines on one line. *)
      match String.split_on_char ~sep:'\n' str with
      | [] -> assert false
      | first :: rest -> (
        match unsnoc rest with
        | None -> Pp.verbatim (Printf.sprintf "%S" first)
        | Some (middle, last) ->
          Pp.vbox
            (Pp.concat ~sep:Pp.cut
               (List.map ~f:Pp.verbatim
                  (("\"" ^ String.escaped first ^ "\\n\\")
                   :: List.map middle ~f:(fun s ->
                          escape_protect_first_space s ^ "\\n\\")
                  @ [ escape_protect_first_space last ^ "\"" ]))))

    let pp_sequence start stop x ~f =
      let open Pp.O in
      match x with
      | [] -> Pp.verbatim start ++ Pp.verbatim stop
      | _ ->
        let sep = ";" ^ String.make (String.length start) ' ' in
        Pp.hvbox
          (Pp.concat_mapi ~sep:Pp.cut x ~f:(fun i x ->
               Pp.box
                 ((if i = 0 then
                     Pp.verbatim (start ^ " ")
                   else
                     Pp.verbatim sep)
                 ++ f x))
          ++ Pp.space ++ Pp.verbatim stop)

    let rec pp =
      let open Pp.O in
      function
      | Opaque -> Pp.verbatim "<opaque>"
      | Int i -> Pp.verbatim (string_of_int i)
      | String s -> string_in_ocaml_syntax s
      | Char c -> Pp.char c
      | List xs -> pp_sequence "[" "]" xs ~f:pp
      | Tuple x ->
        Pp.box
          (Pp.char '('
          ++ Pp.concat_map ~sep:(Pp.seq (Pp.char ',') Pp.space) x ~f:pp
          ++ Pp.char ')')
      | Variant (v, []) -> Pp.verbatim v
      | Variant (v, xs) ->
        Pp.hvbox ~indent:2
          (Pp.concat
             [ Pp.verbatim v
             ; Pp.space
             ; Pp.concat_map ~sep:(Pp.seq (Pp.char ',') Pp.space) xs ~f:pp
             ])

    let string x = String x
    let int x = Int x
    let triple f g h (x, y, z) = Tuple [ f x; g y; h z ]
    let list f l = List (List.map ~f l)
  end

  let to_dyn t =
    let open Dyn in
    let rec to_dyn t =
      match (t : _ Pp.Ast.t) with
      | Nop -> Variant ("Nop", [])
      | Seq (x, y) -> Variant ("Seq", [ to_dyn x; to_dyn y ])
      | Concat (x, y) -> Variant ("Concat", [ to_dyn x; list to_dyn y ])
      | Box (i, t) -> Variant ("Box", [ Int i; to_dyn t ])
      | Vbox (i, t) -> Variant ("Vbox", [ Int i; to_dyn t ])
      | Hbox t -> Variant ("Hbox", [ to_dyn t ])
      | Hvbox (i, t) -> Variant ("Hvbox", [ Int i; to_dyn t ])
      | Hovbox (i, t) -> Variant ("Hovbox", [ Int i; to_dyn t ])
      | Verbatim s -> Variant ("Verbatim", [ String s ])
      | Char c -> Variant ("Char", [ Char c ])
      | Break (x, y) ->
        Variant
          ("Break", [ triple string int string x; triple string int string y ])
      | Newline -> Variant ("Newline", [])
      | Text s -> Variant ("Text", [ string s ])
      | Tag (_, t) -> Variant ("Tag", [ Opaque; to_dyn t ])
    in
    to_dyn (Pp.to_ast t)

  (* Function for pretty printing the pp ast itself *)
  let pp_self pp = to_dyn pp |> Dyn.pp
end
