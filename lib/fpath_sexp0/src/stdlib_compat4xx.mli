module String : sig
  include module type of String

  val hash : t -> int
  val seeded_hash : int -> string -> int
end
