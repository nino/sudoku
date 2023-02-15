let all_numbers = Int_set.of_list [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

exception Number_ouf_of_range

type plain_square = Filled of Int.t | Annotations of Int_set.t
[@@deriving eq, show]

type t = plain_square Array.t [@@deriving eq, show]
type square = int * int * plain_square [@@deriving eq, show]

let copy = Array.copy
let empty = Array.init (9 * 9) ~f:(fun _ -> Annotations Int_set.empty)
let fully_annotated = Array.init (9 * 9) ~f:(fun _ -> Annotations all_numbers)

let square_of_char char =
  match char with
  | '1' .. '9' -> Filled (int_of_char char - int_of_char '0')
  | '.' -> Annotations Int_set.empty
  | _ -> failwith ("Invalid character '" ^ String.make 1 char ^ "'")

(* TODO of_full_string and to_full_string, using the layout from bigboard.txt *)

module Parsing = struct
  let char_to_num = function
    | '1' .. '9' as char -> Some (int_of_char char - int_of_char '0')
    | _ -> None

  let plain_square_of_full_string (str : string) : (plain_square, string) result
      =
    let combined = String.replace ~sub:"\n" ~by:"" str in
    if String.length combined <> 9 then
      Error
        "square description must be 9 characters long (plus newlines, which \
         are ignored"
    else if Char.equal combined.[0] '.' then
      match combined.[4 (* the middle *)] with
      | '1' .. '9' as char -> Ok (Filled (int_of_char char - int_of_char '0'))
      | _ -> Error "invalid number in square"
    else if
      String.exists
        ~f:(function '1' .. '9' -> false | ' ' -> false | _ -> true)
        combined
    then Error "invalid annotations in square"
    else
      Ok
        (Annotations
           (String.to_list combined
           |> List.filter_map ~f:char_to_num
           |> Int_set.of_list))

  let%test "parse empty square" =
    equal_plain_square
      (plain_square_of_full_string
         (String.concat ~sep:"\n" [ "   "; "   "; "   " ])
      |> Result.get_exn)
      (Annotations Int_set.empty)

  let%test "parse fully annotated square" =
    equal_plain_square
      (plain_square_of_full_string
         (String.concat ~sep:"\n" [ "123"; "456"; "789" ])
      |> Result.get_exn)
      (Annotations all_numbers)

  let%test "parse some annotations" =
    equal_plain_square
      (plain_square_of_full_string
         (String.concat ~sep:"\n" [ "  3"; "45 "; "  9" ])
      |> Result.get_exn)
      (Annotations (Int_set.of_list [ 3; 4; 5; 9 ]))

  let%test "parse filled" =
    equal_plain_square
      (plain_square_of_full_string
         (String.concat ~sep:"\n" [ "..."; ".5."; "..." ])
      |> Result.get_exn)
      (Filled 5)

  let%test "fail to parse zero" =
    Result.is_error
      (plain_square_of_full_string
         (String.concat ~sep:"\n" [ "..."; ".0."; "..." ]))

  let%test "fail to parse letter" =
    Result.is_error
      (plain_square_of_full_string
         (String.concat ~sep:"\n" [ "..."; ".f."; "..." ]))

  let of_full_string (str : string) : t = empty
end

let of_string str =
  let str = String.trim str in
  let rows = String.split_on_char ~by:'\n' str |> List.map ~f:String.trim in
  if List.length rows <> 9 then Error "Need exactly 9 rows"
  else if not (List.for_all ~f:(fun row -> String.length row = 9) rows) then
    Error "Every row needs to be 9 long"
  else
    let squares =
      List.concat_map
        ~f:(fun row ->
          String.to_seq row |> Seq.map square_of_char |> List.of_seq)
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
      ~f:(function _, _, Filled n -> Some n | _, _, _ -> None)
      squares
  in
  Int_set.of_list just_numbers

let is_valid_subset squares =
  let numbers =
    List.filter_map
      ~f:(function _, _, Filled n -> Some n | _, _, Annotations _ -> None)
      squares
  in
  let numbers_set = Int_set.of_list numbers in
  Int_set.cardinal numbers_set = List.length numbers

let row t row_idx = List.init 9 ~f:(fun col_idx -> at t row_idx col_idx)
let rows t = List.init 9 ~f:(row t)
let col t col_idx = List.init 9 ~f:(fun row_idx -> at t row_idx col_idx)
let columns t = List.init 9 ~f:(col t)

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
  List.for_all ~f:is_valid_subset (rows t)
  && List.for_all ~f:is_valid_subset (columns t)
  && List.for_all ~f:is_valid_subset (houses t)

let is_solved t =
  List.for_all
    ~f:(fun row -> Int_set.equal (get_int_set row) all_numbers)
    (rows t)
  && List.for_all
       ~f:(fun col -> Int_set.equal (get_int_set col) all_numbers)
       (columns t)
  && List.for_all
       ~f:(fun house -> Int_set.equal (get_int_set house) all_numbers)
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
  List.iter (rows t) ~f:(fun row ->
      List.iter ~f:print_square_simple row;
      print_newline ())

let string_of_intset s =
  Int_set.to_seq s |> Seq.map string_of_int |> List.of_seq
  |> String.concat ~sep:","

let to_string t =
  let row_to_string row =
    List.map row ~f:(function
      | _, _, Filled n -> string_of_int n
      | _, _, _ -> ".")
    |> String.concat ~sep:""
  in
  String.concat ~sep:"\n" (List.map ~f:row_to_string (rows t))
