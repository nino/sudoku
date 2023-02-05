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
  | Sudoku.Board.Filled num -> filled_square num ~packing
  | Annotations ann -> annotated_square ann ~packing

let board board ~packing () =
  let grid = GPack.grid ~width:9 ~height:9 ~packing () in
  Seq.iter
    (fun (row, col) ->
      let _, _, sq = Sudoku.Board.at board row col in
      ignore (square sq ~packing:(grid#attach ~left:col ~top:row) ()))
    Sudoku.Board.all_coordinates;
  grid
