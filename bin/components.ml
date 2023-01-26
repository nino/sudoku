let square_size = 48

let filled_square num ~packing =
  GMisc.label ~text:(string_of_int num) ~justify:`CENTER ~width:square_size
    ~height:square_size ~packing ()

let annotated_square _ann ~packing =
  GMisc.label ~text:"." ~width:square_size ~height:square_size ~packing ()

let square square ~packing () =
  match square with
  | Sudoku.Filled num -> filled_square num ~packing
  | Annotations ann -> annotated_square ann ~packing

let board board ~packing () =
  let grid = GPack.grid ~width:9 ~height:9 ~packing () in
  ListLabels.iteri (Sudoku.squares board) ~f:(fun idx sq ->
      ignore
        (square sq ~packing:(grid#attach ~left:(idx mod 9) ~top:(idx / 9)) ()));
  grid
