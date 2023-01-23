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
