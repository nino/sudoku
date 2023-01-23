let () =
  let board =
    In_channel.(with_open_text "./assets/board3.txt" input_all)
    |> Sudoku.of_string |> Result.get_exn
  in
  Sudoku.print_board board;
  match Sudoku.solve board with
  | Ok solved ->
      Printf.printf "\n\nThe solved version is:\n%!";
      Sudoku.print_board solved
  | Error err -> Printf.printf "Error: %s\n%!" err

let gui_main () =
  let _ = GMain.init () in
  let window = GWindow.window ~title:"Sudoku" ~border_width:10 () in
  let _ = window#connect#destroy ~callback:GMain.quit in
  let hbox = GPack.hbox ~packing:window#add () in
  let button = GButton.button ~label:"Butto!" ~packing:hbox#pack () in
  ignore button;
  window#show ();
  GMain.main ()

let _ = gui_main ()
