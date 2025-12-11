(*_********************************************************************************)
(*_  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*_  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

(** Distinguishing between absolute and relative paths at the type level. *)

(** An alias to {!type:Absolute_path.t} to avoid circular dependencies. *)
type absolute_path = private Fpath.t

(** An alias to {!type:Relative_path.t} to avoid circular dependencies. *)
type relative_path = private Fpath.t

module Absolute_path : sig
  (** The type of absolute paths.

      Note: Some versions of odoc may display this as [type t = Fpath.t], but
      the actual definition is: [type t = private Fpath.t], which enforces the
      following invariants:

      - The path is absolute (starts with ["/"])
      - The path is normalized via {!Fpath.normalize}, so all [".."] segments
        are resolved *)
  type t = absolute_path

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val seeded_hash : int -> t -> int
  val to_fpath : t -> Fpath.t
  val to_string : t -> string

  (** [of_fpath p] returns [p] normalized via {!Fpath.normalize} and classified
      as an absolute path. Returns [None] if [p] is not an absolute path. *)
  val of_fpath : Fpath.t -> t option

  (** This is a convenient wrapper to compose {!Fpath.of_string} and {!of_fpath}. *)
  val of_string : string -> (t, [ `Msg of string ]) Result.t

  (** [v str] returns a [t] or raises [Invalid_argument]. *)
  val v : string -> t

  (** The root path ["/"]. *)
  val root : t

  (** [append abs rel] appends relative path [rel] to absolute path [abs] and
      normalizes the result via {!Fpath.normalize}.

      The result is guaranteed to stay at or below [abs] in the directory tree,
      since [relative_path] values cannot escape upward. *)
  val append : t -> relative_path -> t

  (** [extend abs seg] appends filesystem segment [seg] to absolute path [abs]
      and normalizes the result via {!Fpath.normalize}. *)
  val extend : t -> Fsegment.t -> t

  val parent : t -> t option

  (** [chop_prefix t ~prefix] removes prefix [prefix] from path [t].
      Returns [Some result] where [result] is [t] with [prefix] removed,
      or [None] if [prefix] is not actually a prefix of [t].

      When [t] equals [prefix], returns [Some empty] where [empty] is the
      empty relative path ["./"]. *)
  val chop_prefix : t -> prefix:t -> relative_path option

  (** [chop_suffix t ~suffix] removes suffix [suffix] from path [t].

      Returns [Some result] where [result] is [t] with [suffix] removed, or
      [None] if [suffix] is not actually a suffix of [t]. When [suffix] is the
      empty relative path ["./"], returns [Some t] unchanged.

      When a non-empty suffix is successfully removed, the result is a directory
      path. *)
  val chop_suffix : t -> suffix:relative_path -> t option

  val is_dir_path : t -> bool
  val to_dir_path : t -> t
  val rem_empty_seg : t -> t

  (** Converts a Path.t to an Absolute_path.t:
      - If the path is already absolute, that's the answer.
      - If the path is relative, it is made absolute by appending it to [root]. *)
  val relativize : root:t -> Fpath.t -> t
end

module Relative_path : sig
  (** Relative paths that are guaranteed not to escape upward.

      Values of type [t] are guaranteed to never escape upward. Paths are
      normalized via {!Fpath.normalize}, and if the result contains leading
      [".."] segments, they are rejected at construction time with
      [Invalid_argument].

      This enforces that values of this type only descend from or stay at their
      starting point. Paths like [".."], ["../foo"], or ["a/../.."] cannot be
      constructed.

      For paths that need to reference parent directories with [".."] segments,
      use {!Fpath.t} directly. *)

  (** The type of relative paths that never escape upward.

      Note: Some versions of odoc may display this as [type t = Fpath.t], but
      the actual definition is [type t = private Fpath.t], which enforces the
      following invariants:

      - The path is relative (does not start with ["/"])
      - The path is normalized via {!Fpath.normalize}
      - The normalized path does not contain leading [".."] segments (does not
        escape upward)

      This invariant is enforced at construction time: all functions that create
      values of type [t] (such as {!of_fpath}, {!of_string}, {!v}, {!extend})
      will raise [Invalid_argument] if the resuling path would violate this
      property.

      Examples:
      - [v "a/b/c"] ✓ (descends only)
      - [v "a/../b"] ✓ (normalizes to ["b"], no escaping)
      - [v "./"] ✓ (the empty path, stays at current level)
      - [v ".."] ✗ (escapes upward, raises [Invalid_argument])
      - [v "../a"] ✗ (escapes then descends, raises [Invalid_argument])
      - [v "a/../.."] ✗ (normalizes to [".."], raises [Invalid_argument]) *)
  type t = relative_path

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val seeded_hash : int -> t -> int
  val to_fpath : t -> Fpath.t
  val to_string : t -> string

  (** [of_fpath p] returns [p] normalized via {!Fpath.normalize} and classified
      as a relative path. Returns [None] if [p] is not a relative path.

      Raises [Invalid_argument] if the normalized path contains leading [".."]
      segments (escapes upward). *)
  val of_fpath : Fpath.t -> t option

  (** This is a convenient wrapper to compose {!Fpath.of_string} and {!of_fpath}. *)
  val of_string : string -> (t, [ `Msg of string ]) Result.t

  (** [v str] returns a [t] or raises [Invalid_argument]. *)
  val v : string -> t

  (** The empty relative path ["./"]. *)
  val empty : t

  (** [append t1 t2] appends relative path [t2] to relative path [t1] and
      normalizes the result via {!Fpath.normalize}.

      Raises [Invalid_argument] if the normalized result would escape upward
      (contains leading [".."] segments). *)
  val append : t -> t -> t

  (** [extend t seg] appends filesystem segment [seg] to path [t] and
      normalizes the result via {!Fpath.normalize}.

      Raises [Invalid_argument] if the normalized result would escape upward
      (contains leading [".."] segments).

      For example, [extend empty (Fsegment.v "..")] raises because it would
      create ["../"] which escapes upward. *)
  val extend : t -> Fsegment.t -> t

  (** [parent t] returns the parent directory of path [t], or [None] if [t]
      has no parent.

      Returns [None] when [t] is the empty relative path ["./"].

      Raises [Invalid_argument] if [t] is an escaping path (contains leading
      [".."] segments) (this should not occur for paths constructed through this
      module's API). *)
  val parent : t -> t option

  (** [of_list segments] constructs a path from a list of filesystem segments
      by folding [extend] over the list starting from [empty].

      Raises [Invalid_argument] if any intermediate step during the fold would
      produce a path that escapes upward.

      For example:
      {[
        of_list [ Fsegment.v "a"; Fsegment.v ".."; Fsegment.v ".." ]
      ]}
      raises because the second [".."] would require going from ["./"] to
      ["../"]. See [extend] for details. *)
  val of_list : Fsegment.t list -> t

  (** [chop_prefix t ~prefix] removes prefix [prefix] from path [t].

      Returns [Some result] where [result] is [t] with [prefix] removed, or
      [None] if [prefix] is not actually a prefix of [t]. When [prefix] is
      [empty] (["./"]), returns [Some t] unchanged. *)
  val chop_prefix : t -> prefix:t -> t option

  (** [chop_suffix t ~suffix] removes suffix [suffix] from path [t].

      Returns [Some result] where [result] is [t] with [suffix] removed, or
      [None] if [suffix] is not actually a suffix of [t]. When [suffix] is
      [empty] (["./"]), returns [Some t] unchanged.

      When a non-empty suffix is successfully removed, the result is a directory
      path. *)
  val chop_suffix : t -> suffix:t -> t option

  val is_dir_path : t -> bool
  val to_dir_path : t -> t
  val rem_empty_seg : t -> t
end

(** This module is re-exported as part of the [Fpath] module. For example:
    [Fpath.classify]. *)
module Export : sig
  (** [classify path] normalizes [path] via {!Fpath.normalize} and classifies
      it as either absolute or relative.

      Raises [Invalid_argument] if [path] is relative and the normalized result
      contains leading [".."] segments (escapes upward). Absolute paths always
      have their [".."] segments resolved by {!Fpath.normalize}. *)
  val classify
    :  Fpath.t
    -> [ `Absolute of Absolute_path.t | `Relative of Relative_path.t ]
end
