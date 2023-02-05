let all_numbers = Int_set.of_list [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

exception Number_ouf_of_range

type plain_square = Filled of Int.t | Annotations of Int_set.t
[@@deriving eq, show]

type t = plain_square Array.t [@@deriving eq, show]
type square = int * int * plain_square [@@deriving eq, show]

let copy = Array.copy
let empty = Array.init (9 * 9) (fun _ -> Annotations Int_set.empty)
let fully_annotated = Array.init (9 * 9) (fun _ -> Annotations all_numbers)

let square_of_char char =
  match char with
  | '1' .. '9' -> Filled (int_of_char char - int_of_char '0')
  | '.' -> Annotations Int_set.empty
  | _ -> failwith ("Invalid character '" ^ String.make 1 char ^ "'")

(* TODO of_full_string and to_full_string, using the layout from bigboard.txt *)
(* TODO the exported [square] type should be what is currently [located_square] *)

let of_string str =
  let str = String.trim str in
  let rows = String.split_on_char '\n' str |> List.map String.trim in
  if List.length rows <> 9 then Error "Need exactly 9 rows"
  else if not (List.for_all (fun row -> String.length row = 9) rows) then
    Error "Every row needs to be 9 long"
  else
    let squares =
      List.concat_map
        (fun row -> String.to_seq row |> Seq.map square_of_char |> List.of_seq)
        rows
    in
    Ok (Array.of_list squares)

let idx_of_coords row col = (row * 9) + col
let at t row col = (row, col, t.(idx_of_coords row col))

let all_coordinates =
  Seq.init 9 (fun row -> Seq.init 9 (fun col -> (row, col))) |> Seq.concat

let get_int_set (squares : square list) =
  let just_numbers =
    List.filter_map
      (function _, _, Filled n -> Some n | _, _, _ -> None)
      squares
  in
  Int_set.of_list just_numbers

let is_valid_subset squares =
  let numbers =
    List.filter_map
      (function _, _, Filled n -> Some n | _, _, Annotations _ -> None)
      squares
  in
  let numbers_set = Int_set.of_list numbers in
  Int_set.cardinal numbers_set = List.length numbers

let row t row_idx = List.init 9 (fun col_idx -> at t row_idx col_idx)
let rows t = List.init 9 (row t)
let col t col_idx = List.init 9 (fun row_idx -> at t row_idx col_idx)
let columns t = List.init 9 (col t)

let house t row col =
  Seq.init 3 (fun row_offset ->
      Seq.init 3 (fun col_offset ->
          at t ((row * 3) + row_offset) ((col * 3) + col_offset)))
  |> Seq.concat |> List.of_seq

let house_around t row col =
  let house_row = row / 3 in
  let house_col = col / 3 in
  house t house_row house_col

let houses t =
  Seq.init 3 (fun house_row ->
      Seq.init 3 (fun house_col -> house t house_row house_col))
  |> Seq.concat |> List.of_seq

let is_probably_valid (t : t) : bool =
  List.for_all is_valid_subset (rows t)
  && List.for_all is_valid_subset (columns t)
  && List.for_all is_valid_subset (houses t)

let is_solved t =
  List.for_all (fun row -> Int_set.equal (get_int_set row) all_numbers) (rows t)
  && List.for_all
       (fun col -> Int_set.equal (get_int_set col) all_numbers)
       (columns t)
  && List.for_all
       (fun house -> Int_set.equal (get_int_set house) all_numbers)
       (houses t)

let is_really_valid t =
  ignore t;
  failwith "Not implemented yet"

let fill_square t row col number =
  if number < 1 || number > 9 then raise Number_ouf_of_range;
  let new_t = copy t in
  new_t.(idx_of_coords row col) <- Filled number;
  new_t

let annotate_square t row col set =
  let new_t = copy t in
  new_t.(idx_of_coords row col) <- Annotations set;
  new_t

let print_square_simple = function
  | _, _, Filled int -> print_int int
  | _, _, _ -> print_char '.'

let print_board t =
  rows t
  |> List.iter (fun row ->
         List.iter print_square_simple row;
         print_newline ())

let string_of_intset s =
  Int_set.to_seq s |> Seq.map string_of_int |> List.of_seq |> String.concat ","

let to_string t =
  let row_to_string row =
    row
    |> List.map (function _, _, Filled n -> string_of_int n | _, _, _ -> ".")
    |> String.concat ""
  in
  String.concat "\n" (List.map row_to_string (rows t))
