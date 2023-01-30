let square_size = 54

let filled_square num ~packing =
  let container =
    GPack.layout ~packing ~width:square_size ~height:square_size ()
  in
  let _ = GButton.button ~label:(string_of_int num) ~packing:container#add () in
  container

let annotated_square _ann ~packing =
  let container =
    GPack.layout ~width:square_size ~height:square_size ~packing ()
  in
  let label =
    GMisc.label ~markup:"A" ~width:square_size ~height:square_size
      ~packing:container#add ()
  in
  Utils.set_css label "* { color: darkgrey; font-size: 6px; }";
  container

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
