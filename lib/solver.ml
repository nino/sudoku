open Board

let all_numbers = Int_set.of_list [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

type located_square = int * int * square [@@deriving eq, show]
(** Coordinates plus square *)

let get_int_set squares =
  let seq = Array.to_seq squares in
  let just_numbers =
    Seq.filter_map (function Filled n -> Some n | _ -> None) seq
  in
  Int_set.of_seq just_numbers

let numbers_visible_from t row_idx col_idx =
  let row = get_int_set (row t row_idx) in
  let col = get_int_set (col t col_idx) in
  let house = get_int_set (house_around t row_idx col_idx) in
  let ( $ ) = Int_set.union in
  row $ col $ house

let candidates_for_number_in_subcollection number subcollection =
  let is_candidate (_row, _col, square) =
    match square with Annotations ann -> Int_set.mem number ann | _ -> false
  in
  Array.filter is_candidate subcollection

let fully_annotate t =
  Seq.fold_left
    (fun t (row, col) ->
      match at t row col with
      | Filled _ -> t
      | Annotations _ -> annotate_square t row col all_numbers)
    t all_coordinates

let remove_directly_seen_numbers_from_annotations t =
  Seq.fold_left
    (fun t (row, col) ->
      match at t row col with
      | Filled _ -> t
      | Annotations annotations ->
          let seen = numbers_visible_from t row col in
          let new_annotations = Int_set.diff annotations seen in
          annotate_square t row col new_annotations)
    t all_coordinates

let fill_obvious_squares t =
  Seq.fold_left
    (fun t (row, col) ->
      match at t row col with
      | Filled _ -> t
      | Annotations ann ->
          if Int_set.cardinal ann = 1 then
            fill_square t row col (Int_set.choose ann)
          else t)
    t all_coordinates

let hidden_singles t =
  let hidden_singles_for_one_subcollection t coll =
    Int_set.fold
      (fun number t ->
        match candidates_for_number_in_subcollection number coll with
        | [| (row, col, _square) |] -> fill_square t row col number
        | _ -> t)
      all_numbers t
  in
  (*
  For each house/row/col: 
    - For each number 1..9:
      - Get coordinates that could hold the number
      - If the set has only 1 item, fill it
   *)
  List.fold_left hidden_singles_for_one_subcollection t
    (houses_with_locations t)

let project_rows_from_houses t = t
let project_cols_from_houses t = t
let x_wing t = t
let y_wing t = t

let try_all_strategies t =
  let strategies =
    [
      remove_directly_seen_numbers_from_annotations;
      fill_obvious_squares;
      hidden_singles;
      project_rows_from_houses;
      project_cols_from_houses;
      x_wing;
      y_wing;
    ]
  in
  (* Return the result of the first strategy that makes progress *)
  List.find_map
    (fun strat ->
      let res = strat t in
      if not (equal res t) then Some res else None)
    strategies

let solve t =
  let annotated = fully_annotate t in
  if not (is_probably_valid t) then Error "The board is invalid"
  else
    let rec solve' t =
      if is_solved t then Ok t
      else
        match try_all_strategies t with
        | Some new_t -> solve' new_t
        | None -> Error "Unable to solve. Not smart enough."
    in
    solve' annotated
