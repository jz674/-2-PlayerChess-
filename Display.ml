exception Precondition_violated

(* [char_to_symbol s] returns the unicode character of a chess piece
   corresponding to a character [s]
    - [s] is a character for a valid piece*)
let char_to_symbol s =
  match s with
  |'P' -> "♙" |'Q' -> "♕" |'K' -> "♔" |'R' -> "♖" |'B' -> "♗" |'N' -> "♘"
  |'p' -> "♟" |'q' -> "♛" |'k' -> "♚" |'r' -> "♜" |'b' -> "♝" |'n' -> "♞"
  |'_' -> " " |_ -> raise (Precondition_violated)

(* [convert p ind str_acc is_first] returns a string that represents a row
   or file on the chess board. The positions of the pieces are converted into the
   appropriate locations in the string.
     - [p] is a string for a piece's position
     - [ind] is an int for the index of the row
     - [str_acc] accumulates the string
     - [is_first] is a bool checking to see if in the first index
*)
let rec convert p ind str_acc is_first =
  if (ind mod 8) = 0 && not is_first then str_acc else
    let symbol_at_ind =
      if is_first then
        let str_rank = " "^(string_of_int (ind / 8 + 1)) in
        str_rank ^" │ "^( char_to_symbol p.[ind])^" │"
      else " "^( char_to_symbol p.[ind])^" │" in
    convert p (ind + 1) (str_acc^symbol_at_ind) false

(* [string_of_rank p rank] returns the string that represents a row in the
   board. It has the chess characters and the lines to divide columns.
*)
let string_of_rank p rank =
  let first_of_rank = (rank - 1) * 8 in
  convert p first_of_rank "" true

(* [print_ranks p ranks_left_to_print is_first] returns a value of unit.
    This function just prints the chess board to the console by compiling all
    the chess piece characters and border characters.
*)
let rec print_ranks p ranks_left_to_print is_first=
  if (ranks_left_to_print = 0) then ()
  else (
    if is_first then
      (print_endline ("   ┌───┬───┬───┬───┬───┬───┬───┬───┐");
       print_endline (string_of_rank p ranks_left_to_print);
       print_endline ("   ├───┼───┼───┼───┼───┼───┼───┼───┤");
       print_ranks p (ranks_left_to_print - 1) false)
    else if (ranks_left_to_print != 1) then
      (
        print_endline (string_of_rank p ranks_left_to_print);
        print_endline ("   ├───┼───┼───┼───┼───┼───┼───┼───┤");
        print_ranks p (ranks_left_to_print - 1) false )
    else
      (
        print_endline (string_of_rank p ranks_left_to_print);
        print_endline ("   └───┴───┴───┴───┴───┴───┴───┴───┘");
        print_endline ("     a   b   c   d   e   f   g   h");
        print_ranks p (ranks_left_to_print - 1) false )
  )

let print_game g =
  let p = State.string_of_game g in
  print_ranks p 8 true
