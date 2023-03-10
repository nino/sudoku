exception Number_ouf_of_range

type t
(** The sudoku board *)

type plain_square = Filled of Int.t | Annotations of Int_set.t
type square = int * int * plain_square

val empty : t
val equal : t -> t -> bool
val equal_plain_square : plain_square -> plain_square -> bool
val equal_square : square -> square -> bool
val pp : Format.formatter -> t -> unit
val pp_plain_square : Format.formatter -> plain_square -> unit
val pp_square : Format.formatter -> square -> unit

val of_string : string -> (t, string) result
(** creates a sudoku board when given a multiline string. Each line should
    consist of 9 characters that are either a number from 1 to 9 or a
    period. *)

val to_string : t -> string
(** returns a simple string version of the board. Annotations are ignored, and
    non-filled cells are shown as '.' *)

val is_probably_valid : t -> bool
(** Checks if the board is valid, as far as we can tell without filling in any
    additional squares. I.e., this function will count any board as "probably
    valid" as long as all rows, columns, and houses are subsets of 1..9 and
    don't contain any duplicate numbers. *)

val is_really_valid : t -> bool
(** Checks if the board is solvable or correctly solved. *)

val fill_square : t -> int -> int -> int -> t
(** [fill_square t row col number] puts the number [number] in the cell at
    ([row], [col]) and returns the new board.

    Raises [Number_ouf_of_range] if the value is not in the range 1..9. *)

val print_board : t -> unit
val at : t -> int -> int -> square
val row : t -> int -> square list
val col : t -> int -> square list
val rows : t -> square list list
val columns : t -> square list list
val houses : t -> square list list
val house_around : t -> int -> int -> square list
val annotate_square : t -> int -> int -> Int_set.t -> t
val all_coordinates : (int * int) Seq.t
val is_solved : t -> bool
