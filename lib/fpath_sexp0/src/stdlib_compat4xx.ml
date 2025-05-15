module String = struct
  include String

  let hash : string -> int = Hashtbl.hash
  let seeded_hash : int -> string -> int = Hashtbl.seeded_hash
end
