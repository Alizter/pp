include module type of Pp

(** TODO rename to line length *)
val line_length : 'a t -> int

val truncate : ?ellipsis:string -> int -> 'a t -> 'a t

module For_tests : sig
  val pp_self : 'a t -> 'b t
end
