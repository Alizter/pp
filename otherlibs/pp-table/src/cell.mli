type +'a t

module Pp = Pp0

val pp : 'a t -> 'a Pp.t
val make : width:int -> ?align:[ `Left | `Center | `Right ] -> 'a Pp.t -> 'a t
val join_x : 'a t -> 'a t -> 'a t
val join_y : 'a t -> 'a t -> 'a t
val empty : 'a t
val normalize : 'a t -> 'a t

module Padding : sig
  type t =
    { top : int
    ; bottom : int
    ; left : int
    ; right : int
    ; kind : [ `Space | `Debug ]
    }
end

val pad : Padding.t -> 'a t -> 'a t

module For_tests : sig
  val verbose : bool ref
  val assert_invariants : 'a t -> unit
end
