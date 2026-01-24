type 'a item =
  | Line of { label : string; seq : 'a }
  | Group of { label : string; items : 'a item list; total : 'a option }
      (** A statement item is either a single line item with data, or a group of items with an
          optional total. Groups can be nested to represent hierarchical financial statements. The
          type parameter ['a] allows for any sequence type (e.g., [Accrual.t Seq.t],
          [Balance.t Seq.t], [Transaction.t Seq.t], or custom types). *)

val line : string -> 'a -> 'a item
(** Create a line item with a label and sequence. *)

val group : ?total:'a -> string -> 'a item list -> 'a item
(** Create a group of items with a label and optional total sequence. *)

val fold :
  line_fn:(string -> 'a -> 'b) -> group_fn:(string -> 'b list -> 'a option -> 'b) -> 'a item -> 'b
(** Fold over a statement tree. [line_fn] is called for each line item with its label and sequence.
    [group_fn] is called for each group with its label, the folded results of its children, and its
    optional total sequence. *)

val iter :
  line_fn:(string -> 'a -> unit) ->
  group_fn:(string -> 'a option -> [> `Enter | `Exit ] -> unit) ->
  'a item ->
  unit
(** Iterate over a statement tree. [line_fn] is called for each line item. [group_fn] is called
    twice for each group: once with [`Enter] before processing children, and once with [`Exit]
    after. *)

val lines : 'a item -> (string * 'a) list
(** Extract all line items from a statement as a flat list of (label, sequence) pairs. Groups are
    not included in the output. *)

val to_list : 'a item -> (string * 'a option) list
(** Convert a statement to a flat list of (label, sequence option) pairs. Groups appear with [None]
    for their sequence, followed by their children, then a total entry if present. *)
