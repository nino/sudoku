module IntSet' = Set.Make (Int)
include IntSet'

let pp fmt t =
  Format.fprintf fmt "%s"
    (String.concat ~sep:", " (List.map ~f:string_of_int (IntSet'.to_list t)))
