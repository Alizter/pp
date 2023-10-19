module Pp = Pp0

type +'a t

(** Create a cell with a given width. *)
val cell : width:int -> ?align:[ `Left | `Center | `Right ] -> 'a Pp.t -> 'a t

(** Join tables together horizontally. *)
val join_x : 'a t list -> 'a t

(** Join tables together vertically. *)
val join_y : 'a t list -> 'a t

(** Pretty print. *)
val pp : 'a t -> 'a Pp.t

val tabulate_rows :
     ?left:(row:int -> col:int -> int)
  -> ?right:(row:int -> col:int -> int)
  -> ?top:(row:int -> col:int -> int)
  -> ?bottom:(row:int -> col:int -> int)
  -> width:(row:int -> col:int -> int)
  -> align:(row:int -> col:int -> [ `Center | `Left | `Right ])
  -> 'a Pp.t list list
  -> 'a t

val tabulate_columns :
     ?left:(row:int -> col:int -> int)
  -> ?right:(row:int -> col:int -> int)
  -> ?top:(row:int -> col:int -> int)
  -> ?bottom:(row:int -> col:int -> int)
  -> width:(row:int -> col:int -> int)
  -> align:(row:int -> col:int -> [ `Center | `Left | `Right ])
  -> 'a Pp.t list list
  -> 'a t

module For_tests : sig
  module Cell : module type of Cell
end
