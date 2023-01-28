let solve fname =
  let board =
    In_channel.(with_open_text fname input_all)
    |> Sudoku.Board.of_string |> Result.get_exn
  in
  Sudoku.Board.print_board board;
  match Sudoku.Solver.solve board with
  | Ok solved ->
      Printf.printf "\n\nThe solved version is:\n%!";
      Sudoku.Board.print_board solved;
      Printf.printf "\n\n"
  | Error err -> Printf.printf "Error: %s\n\n%!" err

let () =
  solve "./assets/board1.txt";
  solve "./assets/board2.txt";
  solve "./assets/board3.txt"

(* let gui_main () = *)
(*   let _ = GMain.init () in *)
(*   let window = GWindow.window ~title:"Sudoku" ~border_width:10 () in *)
(*   let _ = window#connect#destroy ~callback:GMain.quit in *)
(*   let hbox = GPack.hbox ~packing:window#add () in *)
(*   (1* let button = GButton.button ~label:"Butto!" ~packing:hbox#pack () in *1) *)
(*   (1* let _ = button#connect#clicked ~callback:GMain.quit in *1) *)
(*   let board_model = *)
(*     In_channel.(with_open_text "./assets/board3.txt" input_all) *)
(*     |> Sudoku.of_string |> Result.get_exn *)
(*   in *)
(*   let board = Components.board board_model ~packing:hbox#pack () in *)
(*   ignore board; *)
(*   window#show (); *)
(*   GMain.main () *)

(* let _ = gui_main () *)
