let styles = {|
  .annotation {
    font-size: 4px;
    color: green;
  }
|}


let css_provider_from_data data =
  let provider = GObj.css_provider () in
  provider#load_from_data data;
  provider

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
  window#misc#style_context#add_provider (* Does not cascade! *)
    (css_provider_from_data styles)
    GtkData.StyleContext.ProviderPriority.application;

  let _ = window#connect#destroy ~callback:GMain.quit in

  let hbox = GPack.hbox ~packing:window#add () in

  let board_model =
    In_channel.(with_open_text "./assets/board3.txt" input_all)
    |> Sudoku.of_string |> Result.get_exn
  in
  let _ = Components.board board_model ~packing:hbox#pack () in

  window#show ();
  GMain.main ()

let _ = gui_main ()
