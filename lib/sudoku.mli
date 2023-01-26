exception Number_ouf_of_range

type t
(** The sudoku board *)

type square = Filled of Int.t | Annotations of Set.Make(Int).t

val squares : t -> square list
(** Gets the squares of the board *)

val of_string : string -> (t, string) result
(** creates a sudoku board when given a multiline string. Each line should
    consist of 9 characters that are either a number from 1 to 9 or a
    period. *)

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
val solve : t -> (t, string) result
