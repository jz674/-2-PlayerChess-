exception Precondition_violated of string

(* [figure] represents each chess piece *)
type figure =
  |Pawn |Queen |King |Rook |Bishop |Knight

(* [color] represents the player who owns a piece*)
type color =
  |White |Black

(* [space] represents a space on the board, matched with a chess piece
 * and its color *)
type square =
  |Piece of color * figure
  |Empty

(* [type file] represents a file or vertical column on the chess board *)
type file =
  |A |B |C |D |E |F |G |H

(* [type rank] represents a rank or row on the chess board *)
type rank =
  |One |Two |Three |Four |Five |Six |Seven |Eight

(* [coordinate] represents a location on the board, like (a,5). *)
type coordinate = file * rank

(* [game] represents the entire chess game and keeps track of external
 * variables to statisfy the rules of chess. *)
type game = {
  position : string;
  turn: color;
  wking_coord : coordinate;
  white_pieces: (figure * coordinate) list;
  can_w_lcastle:bool;
  can_w_scastle: bool;
  bking_coord : coordinate;
  black_pieces: (figure* coordinate) list;
  can_b_lcastle:bool;
  can_b_scastle: bool;
  material_balance: int;
  moves50: int;
  past_positions: string list;
  w_knights_developed: (bool*bool); (*queenside * kingside*)
  w_bishops_developed: (bool*bool);
  b_knights_developed: (bool*bool);
  b_bishops_developed: (bool*bool);
  w_queen_moves: int;
  b_queen_moves: int;
  w_castled: bool;
  b_castled: bool;
}

(* [type move] represents a chess move by giving the start and end locations
 * of the move. *)
type move = (coordinate * coordinate)

(* [int_of_file file] is the int corresponding to a file with A to 1,
 * B to 2 and so on*)
let int_of_file file =
  match file with
  |A -> 1 |B -> 2 |C -> 3 |D -> 4 |E -> 5 |F -> 6 |G -> 7 |H -> 8

(* [string_of_file file] is the string letter corresponding to a file with
    A to "A", B to "B" and so on.*)
let string_of_file file =
  match file with
  |A -> "a" |B -> "b" |C -> "c" |D -> "d" |E -> "e" |F -> "f" |G -> "g" |H ->"h"

(* [file_of_int file] is the file corresponding to an int with 1 going to A,
 * 2 going to B and so on. *)
let file_of_int intgr =
  match intgr with
  |1 -> A |2 -> B |3 -> C |4 -> D |5 -> E |6 -> F |7 -> G |8 -> H
  |_ -> raise (Precondition_violated "file int not in 1-8 ")

(* [file_of_int file] is the file corresponding to an int with 1 going to A,
 * 2 going to B and so on. *)
let file_of_char c =
  match c with
  |'a' -> A |'b' -> B |'c' -> C |'d' -> D |'e' -> E |'f' -> F |'g' -> G |'h' -> H
  |_ -> raise (Precondition_violated "char not in a-h")

(* [int_of_rank rank] is the int corresponding to a rank with One going to 1,
 * Two going to 2 and so on. *)
let int_of_rank rank =
  match rank with
  |One -> 1 |Two -> 2 |Three -> 3 |Four -> 4 |Five -> 5 |Six -> 6
  |Seven -> 7 |Eight -> 8

(* [string_of_rank file] is the string corresponding to a rank with One going to "1",
 * Two going to "2" and so on. *)
let string_of_rank rank =
  match rank with
  |One -> "1" |Two -> "2" |Three -> "3" |Four -> "4" |Five -> "5" |Six -> "6"
  |Seven -> "7" |Eight -> "8"

(* [rank_of_int intgr] is the rank corresponding to an int with 1 going to One,
 * 2 going to Two and so on. *)
let rank_of_int intgr =
  match intgr with
  |1 -> One |2 -> Two |3 -> Three |4 -> Four |5 -> Five |6 -> Six
  |7 -> Seven |8 -> Eight|_ -> raise (Precondition_violated "rank int not in 1-8")

let int_of_coord coord =
  let file = fst coord in
  let rank  = snd coord in
  let s_f = int_of_file file in
  let s_r = int_of_rank rank in
  (s_f,s_r)

let string_of_move m =
  let s_coord = fst m in
  let e_coord = snd m in
  let s_f = string_of_file (fst s_coord) in
  let s_r = string_of_rank (snd s_coord) in
  let e_f = string_of_file (fst e_coord) in
  let e_r = string_of_rank (snd e_coord) in
  s_f^s_r^" "^e_f^e_r

let create_move ci_ci =
  let first_tup = fst ci_ci in
  let snd_tup = snd ci_ci in
  let c1 = fst first_tup in
  let i1 = snd first_tup in
  let c2 = fst snd_tup in
  let i2 = snd snd_tup in
  ((file_of_char c1, rank_of_int i1),(file_of_char c2, rank_of_int i2))

let create_coordinate char_int =
  let chr = fst char_int in
  let intgr = snd char_int in
  let file_of_chr = file_of_char chr in
  let rank_of_intgr = rank_of_int intgr in
  (file_of_chr,rank_of_intgr)

let number_moves g = (List.length g.past_positions) / 2

let number_queen_moves g =
  match g.turn with
  |White -> g.w_queen_moves
  |Black -> g.b_queen_moves

let string_of_game g = g.position

(*[opposite_color clr] returns the other constuctor for type color.
 * example: [opposite_color Black] returns White
*)
let opposite_color clr =
  match clr with
  |White ->Black
  |Black -> White

let opposite_turn g = {g with turn = opposite_color g.turn}

(* [int_of_bool b] returns 1 if [b] is true and 0 if [b] is false *)
let int_of_bool b =
  match b with
  |true  -> 1
  |false -> 0

let pieces_developed g =
  match g.turn with
  |White ->
    let w_knights = g.w_knights_developed in
    let w_bishops = g.w_bishops_developed in
    int_of_bool (fst w_knights) +
    int_of_bool (snd w_knights) +
    int_of_bool (fst w_bishops) +
    int_of_bool (snd w_bishops)
  |Black ->
    let b_knights = g.b_knights_developed in
    let b_bishops = g.b_bishops_developed in
    int_of_bool (fst b_knights) +
    int_of_bool (snd b_knights) +
    int_of_bool (fst b_bishops) +
    int_of_bool (snd b_bishops)

let has_castled g =
  match g.turn with
  |White -> g.w_castled
  |Black -> g.b_castled

let can_castle g =
  match g.turn with
  |White -> g.can_w_scastle || g.can_w_lcastle
  |Black -> g.can_b_scastle || g.can_b_lcastle

let turn g =
  match g.turn with
  |White -> "White"
  |Black -> "Black"

(*[index_of_coord coord] converts [coord] to an index *)
let index_of_coord coord =
  8 * ((int_of_rank (snd coord)) - 1) + ((int_of_file (fst coord)) - 1)

(*[coord_of_index index] converts [index] to a coordinate *)
let coord_of_index index =
  let int_rep_rank = (index/8) + 1 in
  let rank_of_ind  = rank_of_int int_rep_rank in
  let int_rep_file = (index mod 8) + 1 in
  let file_of_ind  = file_of_int int_rep_file in
  (file_of_ind,rank_of_ind)

let coordinate_pair_to_move c1 c2 = (c1,c2)

(* [compare_coord coord1 coord2] returns 0 if [coord1] is equal to [coord2], a
 * negative number if [coord1] is less than [coord2], and a positive number if
 * [coord1] is more than [coord2]
*)
let compare_coord coord1 coord2 =
  let int_c1 = index_of_coord coord1 in
  let int_c2 = index_of_coord coord2 in
  compare int_c1 int_c2

(*[get_first] returns the first element of the tuple*)
let get_first = fun (a,_,_) -> a

(*[get_second] returns the second element of the tuple*)
let get_second = fun (_,a,_) -> a

(*[get_third] returns the third element of the tuple*)
let get_third = fun (_,_,a) -> a

(* [string_of_piece clr fig] returns unique single character string based on
 * the color and piece. If [clr] is White, the string is uppercase. Otherwise,
 * lowercase.
 * example: string_of_piece White Pawn returns "P"
 * example: string_of_piece Black Pawn returns "p"
*)
let string_of_piece clr fig =
  match clr with
  |White ->
    (match fig with
     |Pawn->"P" |Queen->"Q" |King->"K" |Rook->"R" |Bishop->"B" |Knight->"N")
  |Black ->
    (match fig with
     |Pawn->"p" |Queen->"q" |King->"k" |Rook->"r" |Bishop->"b" |Knight->"n")

let rec concat lst str_acc index_acc =
  match lst with
  |h::t ->
    if (index_of_coord (get_third h)) > index_acc
    then concat lst (str_acc^"_") (index_acc + 1)
    else
      let str_rep = string_of_piece (get_first h) (get_second h) in
      concat t (str_acc^str_rep) (index_acc+1)
  |[]-> str_acc ^ (String.make (64-index_acc) '_')

(* [create_position w_lst b_lst] is the string representing the position given
 * by the (figure* coordinate) lists [w_lst] and [b_lst]. *)
let create_position w_lst b_lst =
  let w_tuples = List.map (fun (a,b)-> (White,a,b)) w_lst in
  let b_tuples = List.map (fun (a,b)-> (Black,a,b)) b_lst in
  let combined = w_tuples @ b_tuples in
  let compare_three_tup t1 t2 =
    let c1 = get_third t1 in
    let c2 = get_third t2 in
    compare_coord c1 c2 in
  let combined_sorted = List.sort compare_three_tup combined in
  concat combined_sorted "" 0

(* [start_white_pieces] represents a list of all white pieces' inital positions
 * on the chess board
*)
let start_white_pieces =
  [(Rook ,(A,One)); (Knight,(B,One)); (Bishop,(C,One)); (Queen ,(D,One));
   (King  ,(E,One)); (Bishop,(F,One)); (Knight,(G,One)); (Rook  ,(H,One));
   (Pawn  ,(A,Two)); (Pawn  ,(B,Two)); (Pawn  ,(C,Two)); (Pawn  ,(D,Two));
   (Pawn  ,(E,Two)); (Pawn  ,(F,Two)); (Pawn  ,(G,Two)); (Pawn  ,(H,Two))]

(* [start_black_pieces] represent a list of all black pieces' inital positions
 * on the chess board
*)
let start_black_pieces =
  [(Rook ,(A,Eight)); (Knight,(B,Eight)); (Bishop,(C,Eight));
   (Queen ,(D,Eight)); (King  ,(E,Eight)); (Bishop,(F,Eight));
   (Knight,(G,Eight)); (Rook  ,(H,Eight)); (Pawn  ,(A,Seven));
   (Pawn  ,(B,Seven)); (Pawn  ,(C,Seven)); (Pawn  ,(D,Seven));
   (Pawn  ,(E,Seven)); (Pawn  ,(F,Seven)); (Pawn  ,(G,Seven));
   (Pawn  ,(H,Seven))]

(* [start_position] represent the initial position of black and white's pieces
*)
let start_position = create_position start_white_pieces start_black_pieces

let start_game = {
  position = start_position;
  turn =  White;
  wking_coord = (E,One);
  white_pieces = start_white_pieces;
  can_w_lcastle = true;
  can_w_scastle =  true;
  bking_coord  = (E,Eight);
  black_pieces = start_black_pieces;
  can_b_lcastle = true;
  can_b_scastle = true;
  material_balance = 0;
  moves50 = 0;
  past_positions = [];
  w_knights_developed = (false,false);
  w_bishops_developed = (false,false);
  b_knights_developed = (false,false);
  b_bishops_developed = (false,false);
  w_castled = false;
  b_castled = false;
  w_queen_moves = 0;
  b_queen_moves = 0;
}

let get_material_balance g = g.material_balance

(* [get_previous_position g] returns the position before the last move was made*)
let get_previous_position g =
  let prev_pos_lst = g.past_positions in
  match prev_pos_lst with
  |h::t -> h
  |[]   -> g.position

(* [insert_in_position p index square] is the string [p] with the char at index,
 * replaced by the char representing square.
*)
let insert_in_position p index square =
  let sub1 = String.sub p 0 index in
  let sub2 = String.sub p (index + 1) (63 - index) in
  match square with
  |Piece (clr,fig) ->
    let piece_string = string_of_piece clr fig in
    sub1 ^ piece_string ^ sub2
  |Empty -> sub1 ^ "_" ^ sub2

(* [snd_lst] returns the second element of the list of tuples *)
let snd_lst lst = (List.map (fun (_,a) -> a) lst)

(* [move_position p m] is p with the char at the start of m moved to the end
 * of m leaving an underscore from where it came from.
*)
let move_position p m =
  let start_index = index_of_coord (fst m) in
  let letter_moving = Char.escaped (p.[start_index]) in
  let end_index = index_of_coord (snd m) in
  if start_index < end_index then
    let sub1 = String.sub p 0 start_index in
    let sub2 = String.sub p (start_index + 1) (end_index - start_index - 1) in
    let sub3 = String.sub p (end_index + 1) (64 - end_index - 1) in
    sub1^"_"^sub2^letter_moving^sub3
  else if start_index>end_index then
    let sub1 = String.sub p 0 end_index in
    let sub2 = String.sub p (end_index + 1) (start_index - end_index - 1) in
    let sub3 = String.sub p (start_index + 1) (64 - start_index - 1) in
    sub1^letter_moving^sub2^"_"^sub3
  else p

(* [pos_char_to_square s] returns the position of a piece when given char [s] *)
let pos_char_to_square s =
  match s with
  |'P' -> Piece(White, Pawn) |'Q' -> Piece(White, Queen) |'K' -> Piece(White,King)
  |'R' -> Piece(White, Rook) |'B' -> Piece(White, Bishop)|'N' -> Piece(White,Knight)
  |'p' -> Piece(Black, Pawn) |'q' -> Piece(Black, Queen) |'k' -> Piece(Black,King)
  |'r' -> Piece(Black, Rook) |'b' -> Piece(Black, Bishop)|'n' -> Piece(Black,Knight)
  |'_' -> Empty |_ -> raise (Precondition_violated "not a square")

(* [square_at_index p ind] returns the position of a piece when given a position
 * [p] and an index [ind]
*)
let square_at_index p ind =
  let char_at_ind = p.[ind] in
  pos_char_to_square char_at_ind

let square_at_coord p coord =
  let ind_of_coord = index_of_coord coord in
  square_at_index p ind_of_coord

(* [is_piece p ind] returns true if there is piece given a position [p] an the
 * index [ind]. False otherwise.
*)
let is_piece p ind =
  if ind <0 || ind >63 then false else
    let char_at_ind = p.[ind] in
    let square_at_ind = pos_char_to_square char_at_ind in
    match square_at_ind with
    |Empty -> false
    |Piece _ -> true

(* [is_piece_of_color] returns true if piece is a colored piece given
 * a position [p] an the index [ind]
*)
let is_piece_of_color p clr ind =
  let char_at_ind = p.[ind] in
  let square_at_ind = pos_char_to_square char_at_ind in
  match square_at_ind with
  |Empty -> false
  |Piece (clr_p,_) ->
    (match clr_p with
     |x -> x = clr )

(* [is_piece_of_color_fig p clr fig ind] checks if a piece is a figure and
 * color, given a position [p], color [clr], figure [fig] and index [ind]
*)
let is_piece_of_color_fig p clr fig ind =
  let char_at_ind = p.[ind] in
  let square_at_ind = pos_char_to_square char_at_ind in
  match square_at_ind with
  |Empty -> false
  |Piece (clr_p,fig_p) ->
    let is_same_clr =
      (match clr_p with
       |x -> x = clr) in
    let is_same_fig =
      (match fig_p with
       |x -> x = fig) in
    is_same_clr && is_same_fig

(*[remove_fig_coord] removes the figure coordinates [fig_coord_to_remove] from
 * the [fig_coord_lst]
*)
let remove_fig_coord fig_coord_lst fig_coord_to_remove =
  let not_to_r a = not (a = fig_coord_to_remove) in
  List.filter not_to_r fig_coord_lst

(* [remove_piece_at_i_from_g g index] returns a new game state with piece at
 * i in g removed
*)
let remove_piece_at_i_from_g g index =
  let coord_of_ind = coord_of_index index in
  let square = square_at_index g.position index in
  match square with
  |Empty -> g
  |Piece (clr,fig) ->
    let new_p = insert_in_position g.position index Empty in
    (match clr with
     |White ->
       let new_w_pieces = remove_fig_coord g.white_pieces (fig,coord_of_ind) in
       {g with position = new_p; white_pieces = new_w_pieces}
     |Black ->
       let new_b_pieces = remove_fig_coord g.black_pieces (fig,coord_of_ind) in
       {g with position = new_p; black_pieces = new_b_pieces})

(* [is_enpassant_possible p prev_p ind_pawn clr_pwn] returns true if enpassant
 * is possible for the pawn at index ind_pawn in position p. Otherwise false*)
let is_enpassant_possible p prev_p ind_pawn clr_pwn =
  let rank_indicator = (ind_pawn/8) + 1 in
  match clr_pwn with
  |White ->
    if rank_indicator = 5 then
      let ind_two_up_left = ind_pawn + 15 in
      let ind_two_up_right = ind_pawn + 17 in
      let file_indicator = (ind_pawn mod 8) + 1 in
      if file_indicator < 8 && file_indicator > 1 then
        let left_possible =
          is_piece_of_color_fig prev_p Black Pawn ind_two_up_left &&
          not (is_piece prev_p (ind_pawn - 1)) &&
          is_piece_of_color_fig p Black Pawn (ind_pawn - 1) &&
          not (is_piece p ind_two_up_left) in
        let right_possible =
          is_piece_of_color_fig prev_p Black Pawn ind_two_up_right &&
          not (is_piece prev_p (ind_pawn + 1)) &&
          is_piece_of_color_fig p Black Pawn (ind_pawn + 1) &&
          not (is_piece p ind_two_up_right) in
        (left_possible,right_possible)
      else if file_indicator = 1 then
        let right_possible =
          is_piece_of_color_fig prev_p Black Pawn ind_two_up_right &&
          not (is_piece prev_p (ind_pawn + 1)) &&
          is_piece_of_color_fig p Black Pawn (ind_pawn + 1) &&
          not (is_piece p ind_two_up_right) in
        (false, right_possible)
      else
        let left_possible =
          is_piece_of_color_fig prev_p Black Pawn ind_two_up_left &&
          not (is_piece prev_p (ind_pawn - 1)) &&
          is_piece_of_color_fig p Black Pawn (ind_pawn - 1) &&
          not (is_piece p ind_two_up_left) in
        (left_possible,false)
    else (false,false)
  |Black ->
    if rank_indicator = 4 then
      let ind_two_down_left = ind_pawn - 17 in
      let ind_two_down_right = ind_pawn - 15 in
      let file_indicator = (ind_pawn mod 8) + 1 in
      if file_indicator < 8 && file_indicator > 1 then
        let left_possible =
          is_piece_of_color_fig prev_p White Pawn ind_two_down_left &&
          not (is_piece prev_p (ind_pawn - 1)) &&
          is_piece_of_color_fig p White Pawn (ind_pawn - 1) &&
          not (is_piece p ind_two_down_left) in
        let right_possible =
          is_piece_of_color_fig prev_p White Pawn ind_two_down_right &&
          not (is_piece prev_p (ind_pawn + 1)) &&
          is_piece_of_color_fig p White Pawn (ind_pawn + 1) &&
          not (is_piece p ind_two_down_right) in
        (left_possible,right_possible)
      else if file_indicator = 1 then
        let right_possible =
          is_piece_of_color_fig prev_p White Pawn ind_two_down_right &&
          not (is_piece prev_p (ind_pawn + 1)) &&
          is_piece_of_color_fig p White Pawn (ind_pawn + 1) &&
          not (is_piece p ind_two_down_right) in
        (false, right_possible)
      else
        let left_possible =
          is_piece_of_color_fig prev_p White Pawn ind_two_down_left &&
          not (is_piece prev_p (ind_pawn - 1)) &&
          is_piece_of_color_fig p White Pawn (ind_pawn - 1) &&
          not (is_piece p ind_two_down_left) in
        (left_possible,false)
    else (false,false)

(* [pawn_rule] returns all the psuedolegal squares the pawn can move to,
 * ie not considering if the player would be in check by playing a move*)
let pawn_rules p ind_pawn prev_p clr_pwn =
  let start_coord = coord_of_index ind_pawn in
  let file_int  = ind_pawn mod 8 in
  let rank_int = ind_pawn/8 + 1 in
  match clr_pwn with
  |White ->
    let ind_front_right = ind_pawn + 9   in
    let ind_front       = ind_pawn + 8   in
    let ind_front_left  = ind_pawn + 7   in
    let ind_front_two   = ind_pawn + 16  in
    let enpassant = is_enpassant_possible p prev_p ind_pawn White in
    let can_enpassant_left = fst enpassant in
    let can_enpassant_right = snd enpassant in
    let can_cap_front_right = if file_int = 7 then false else
        is_piece_of_color p Black ind_front_right in
    let can_cap_front_left = if file_int = 0 then false else
        is_piece_of_color p Black ind_front_left in
    let can_move_right = can_cap_front_right || can_enpassant_right in
    let can_move_left = can_cap_front_left || can_enpassant_left in
    let diag_caps =
      if file_int > 0 && file_int < 7 then (*pawn on one of b-g files*)
        if (not can_move_right) && (not can_move_left)  then
          []
        else if can_move_right && (not can_move_left)  then
          [(start_coord,coord_of_index ind_front_right)]
        else if (not can_move_right) && can_move_left  then
          [(start_coord, coord_of_index ind_front_left)]
        else
          [(start_coord, coord_of_index ind_front_left);
           (start_coord, coord_of_index ind_front_right)]
      else if file_int = 0 then (*pawn on 'a' file*)
        if can_move_right then
          [(start_coord, coord_of_index ind_front_right)]
        else []
      else (*pawn on h file*)
      if can_move_left then
        [(start_coord, coord_of_index ind_front_left)]
      else [] in
    let can_move_one =
      not (is_piece p ind_front) in
    let is_on_second_rank = (rank_int = 2) in
    let no_piece_two_up =
      if rank_int >= 7 then false else
        not (is_piece p ind_front_two) in
    let can_move_two = can_move_one && is_on_second_rank && no_piece_two_up in
    let forward_moves_and_diag_caps =
      if can_move_one && (not can_move_two) then
        (start_coord, coord_of_index ind_front)::diag_caps
      else if can_move_two && can_move_one then
        (start_coord, coord_of_index ind_front) ::(
          (start_coord, coord_of_index ind_front_two)::diag_caps)
      else
        diag_caps in
    forward_moves_and_diag_caps
  |Black ->
    let ind_front_right = ind_pawn - 7   in
    let ind_front       = ind_pawn - 8   in
    let ind_front_left  = ind_pawn - 9   in
    let ind_front_two   = ind_pawn - 16  in
    let enpassant = is_enpassant_possible p prev_p ind_pawn Black in
    let can_enpassant_left = fst enpassant in
    let can_enpassant_right = snd enpassant in
    let can_cap_front_right = if file_int = 7 then false else
        is_piece_of_color p White ind_front_right in
    let can_cap_front_left = if file_int = 0 then false else
        is_piece_of_color p White ind_front_left in
    let can_move_right = can_cap_front_right || can_enpassant_right in
    let can_move_left = can_cap_front_left || can_enpassant_left in
    let diag_caps =
      if file_int > 0 && file_int < 7 then (*pawn on one of b-g files*)
        if (not can_move_right) && (not can_move_left)  then
          []
        else if can_move_right && (not can_move_left)  then
          [(start_coord, coord_of_index ind_front_right)]
        else if (not can_move_right) && can_move_left  then
          [(start_coord, coord_of_index ind_front_left)]
        else
          [(start_coord, coord_of_index ind_front_left);
           (start_coord, coord_of_index ind_front_right)]
      else if file_int = 0 then (*pawn on 'a' file*)
        if can_move_right then
          [(start_coord, coord_of_index ind_front_right)]
        else []
      else (*pawn on h file*)
      if can_move_left then
        [(start_coord, coord_of_index ind_front_left)]
      else [] in
    let can_move_one = not (is_piece p ind_front) in
    let is_on_seventh_rank = (rank_int = 7) in
    let no_piece_two_up =
      if rank_int <= 2 then false else
        not (is_piece p ind_front_two) in
    let can_move_two = can_move_one && is_on_seventh_rank && no_piece_two_up in
    let forward_moves_and_diag_caps =
      if can_move_one && (not can_move_two) then
        (start_coord, coord_of_index ind_front)::diag_caps
      else if can_move_two && can_move_one then
        (start_coord, coord_of_index ind_front):: (
          (start_coord, coord_of_index ind_front_two)::diag_caps)
      else diag_caps in
    forward_moves_and_diag_caps

(* [knight_rules p ind_k clr_k] returns all the pseudolegal squares the knight can move
 * knight can move to in position p
*)
let knight_rules p ind_k clr_k =
  let i = ind_k in
  let file_start = (i mod 8) + 1 in
  let rank_start = i/8 + 1 in
  let knight_jumps = [(i+15,(-1,2)); (i+17,(1,2)); (i+6,(-2,1)); (i+10,(2,1));
                      (i-15,(1,-2)); (i-17,(-1,-2)); (i-6,(2,-1));
                      (i-10,(-2,-1))] in
  let is_jump_valid ind_tup =
    let end_index = fst ind_tup in
    let in_range = (0 <= end_index) && (63 >= end_index) in
    let not_same_clr_piece = if not in_range then false else
        not (is_piece_of_color p clr_k end_index) in
    let file_end = (end_index mod 8) + 1 in
    let rank_end = end_index/8 + 1 in
    let tup = snd ind_tup in
    let file_diff_correct = (file_end - file_start) = (fst tup) in
    let rank_diff_correct = (rank_end - rank_start) = (snd tup) in
    in_range && not_same_clr_piece && file_diff_correct && rank_diff_correct in
  let valid_jumps = List.filter is_jump_valid knight_jumps in
  let start_coord = coord_of_index ind_k in
  let ind_tup_to_move = fun (index,_) -> (start_coord, coord_of_index index) in
  List.map ind_tup_to_move valid_jumps

(* [walk_incr p clr_to_move index start_coord diag_acc increment] steps
 * through the positon p by incrementing the index by the given increment
 * and stops when it encounters a piece not of the color to move or a piece
 * that can be captured. It returns the steps of its walk.
*)
let rec walk_incr p clr_to_move index start_coord diag_acc increment =
  let next_index = index + increment in
  let next_in_range = (0 <= next_index) && (63 >= next_index) in
  let file = (index mod 8) + 1 in
  if not next_in_range then diag_acc
  else if (increment = 9 || increment = -7 || increment = 1) && file = 8
  then diag_acc
  else if (increment = 7 || increment = -9 || increment = -1) && file = 1
  then diag_acc
  else if is_piece_of_color p (opposite_color clr_to_move) index then diag_acc
  else if is_piece_of_color p clr_to_move next_index then diag_acc
  else
    let next_coord = coord_of_index next_index in
    let move = (start_coord, next_coord) in
    walk_incr p clr_to_move next_index start_coord (move::diag_acc) increment

(* [walk_all_diags p index clr_to_move] steps
 * along all diagnals from index until an obstruction and returns all the
 * squares it stepped along. *)
let walk_all_diags p index clr_to_move =
  let start_coord = coord_of_index index in
  let up_rt_diag = walk_incr p clr_to_move index start_coord [] 9 in
  let up_diags = walk_incr p clr_to_move index start_coord up_rt_diag 7 in
  let up_diags_and_dwn_lft =
    walk_incr p clr_to_move index start_coord up_diags (-9) in
  let all_diags =
    walk_incr p clr_to_move index start_coord up_diags_and_dwn_lft (-7) in
  all_diags

(* [walk_all_lines p index start_acc clr_to_move] steps
 * along all lines from index until an obstruction and returns all the
 * squares it stepped along. *)
let walk_all_lines p index start_acc clr_to_move =
  let start_coord = coord_of_index index in
  let up_line = walk_incr p clr_to_move index start_coord start_acc 8 in
  let up_down_lines = walk_incr p clr_to_move index start_coord up_line (-8) in
  let up_down_lines_and_lft =
    walk_incr p clr_to_move index start_coord up_down_lines (-1) in
  let all_lines =
    walk_incr p clr_to_move index start_coord up_down_lines_and_lft 1 in
  all_lines

(* [bishop_rules p index clr_to_move] returns all the pseudolegal squares
 * the knight at index can move to in position p
*)
let bishop_rules p index clr_to_move = walk_all_diags p index clr_to_move

(* [rook_rules p index clr_to_move] returns all the pseudolegal squares
 * the rook at index can move to in position p
*)
let rook_rules p index clr_to_move = walk_all_lines p index [] clr_to_move

(* [queen_rules p index clr_to_move] returns all the pseudolegal squares
 * the queen at index can move to in position p
*)
let queen_rules p index clr_to_move =
  let diag_moves = walk_all_diags p index clr_to_move in
  let diag_and_line_moves = walk_all_lines p index diag_moves clr_to_move in
  diag_and_line_moves

(* [moves_wo_check_considered g coord] is a list of all psudo legal moves that
 * the piece at coordinate [coord] can make, where a move can be taken a without
 * the consideration if the player is in check. Empty, if there are no pieces
 * in the square at [coord] or if the color of the piece at [coord] is not the
 * same color of the piece moved
*)
let rec moves_wo_check_considered g coord =
  let clr_to_move = g.turn in
  let p = g.position in
  let prev_p = get_previous_position g in
  let coord_index = index_of_coord coord in
  let char_at_coord = p.[coord_index] in
  let square_at_coord = pos_char_to_square char_at_coord in
  let rec is_coord_attacked_help g piece_coord_lst coord =
    match piece_coord_lst with
    |h::t ->
      (match fst h with
       |King ->
         let to_move = g.turn in
         let i = index_of_coord coord in
         let squares_around_coord =
           [i + 1; i - 1; i + 8; i - 8; i + 9; i - 9; i + 7; i - 7] in
         let is_king_clr_to_move ind =
           is_piece_of_color_fig g.position to_move King ind in
         let fun_k ind =
           if (ind < 0) || (ind > 63) then false else is_king_clr_to_move ind in
         let inds_with_k_clr_to_move = List.filter fun_k squares_around_coord in
         if (List.length inds_with_k_clr_to_move) = 1 then true else
           is_coord_attacked_help g t coord
       |_->
         let valid_moves = snd_lst (moves_wo_check_considered g (snd h)) in
         let is_coord_attacked = List.mem coord valid_moves in
         if is_coord_attacked then true else
           is_coord_attacked_help g t coord)
    |[]-> false in
  let is_coord_attacked g coord =
    let ind = index_of_coord coord in
    let g_piece_at_coord_gone = remove_piece_at_i_from_g g ind in
    let to_move = g.turn in
    match to_move with
    |White ->
      let g_black_to_move =
        {g_piece_at_coord_gone with turn = Black} in
      is_coord_attacked_help g_black_to_move g.black_pieces coord
    |Black ->
      let g_white_to_move =
        {g_piece_at_coord_gone with turn = White} in
      is_coord_attacked_help g_white_to_move g.white_pieces coord in
  let is_valid_one_square_k_move start_ind ind =
    let ind_in_range = (ind <= 63) && (ind >= 0) in
    let is_same_color_piece =
      if (not ind_in_range) then false else
        is_piece_of_color g.position g.turn ind in
    let move_indic = (ind mod 8) - (start_ind mod 8) in
    let not_a_h = move_indic <= 1 && move_indic >= -1 in
    ind_in_range && (not is_same_color_piece) && not_a_h in
  (match square_at_coord with
   |Empty -> []
   |Piece (clr,fig) ->
     (match clr with
      |White ->
        if White != clr_to_move then [] else
          (match fig with
           |Pawn -> pawn_rules p coord_index prev_p White
           |Queen-> queen_rules p coord_index White
           |King ->
             let i = coord_index in
             let squares_around_k =
               [i + 1; i - 1; i + 8; i - 8; i + 9; i - 9; i + 7; i - 7] in
             let legal_one_square_inds =
               List.filter (is_valid_one_square_k_move i) squares_around_k in
             let one_square_moves =
               List.map (fun (i) ->
                   (coord, coord_of_index i)) legal_one_square_inds in
             let pieces_block_scastle =
               (is_piece g.position (i+1)) || (is_piece g.position (i+2)) in
             let pieces_block_lcastle = (is_piece g.position (i - 1)) ||
                                        (is_piece g.position (i - 2)) ||
                                        (is_piece g.position (i - 3)) in
             let is_king_in_check = is_coord_attacked g (E,One) in
             let f_one_attacked = is_coord_attacked g (F,One) in
             let d_one_attacked = is_coord_attacked g (D,One) in
             let is_white_rook_h1 =
               is_piece_of_color_fig g.position White Rook
                 (index_of_coord (H,One)) in
             let is_white_rook_a1 = is_piece_of_color_fig g.position White Rook
                 (index_of_coord (A,One)) in
             let w_can_scastle = (g.can_w_scastle && is_white_rook_h1 &&
                                  (not pieces_block_scastle) &&
                                  (not f_one_attacked)) &&
                                  (not is_king_in_check) in
             let w_can_lcastle = g.can_w_lcastle && is_white_rook_a1 &&
                                 (not pieces_block_lcastle) &&
                                 (not d_one_attacked) &&
                                 (not is_king_in_check) in
             let with_scastle =
               if w_can_scastle then ((coord,(G,One))::one_square_moves)
               else one_square_moves in
             let moves_with_castling =
               if w_can_lcastle then ((coord, (C,One))::with_scastle)
               else with_scastle in
             moves_with_castling
           |Rook   -> rook_rules p coord_index White
           |Bishop -> bishop_rules p coord_index White
           |Knight -> knight_rules p coord_index White)
      |Black ->
        if Black != clr_to_move then [] else
          (match fig with
           |Pawn -> pawn_rules p coord_index prev_p Black
           |Queen-> queen_rules p coord_index Black
           |King ->
             let i = coord_index in
             let squares_around_k =
               [i + 1; i - 1; i + 8; i - 8; i + 9; i - 9; i + 7; i - 7] in
             let valid_squares_around_k =
               List.filter (fun ind -> ind <= 63 && ind >=0) squares_around_k in
             let legal_one_square_inds =
               List.filter
                 (is_valid_one_square_k_move i) valid_squares_around_k in
             let one_square_moves =
               List.map
                 (fun (i) -> (coord, coord_of_index i)) legal_one_square_inds in
             let pieces_block_scastle =
               (is_piece g.position (i+1)) || (is_piece g.position (i+2)) in
             let pieces_block_lcastle = (is_piece g.position (i - 1)) ||
                                        (is_piece g.position (i - 2)) ||
                                        (is_piece g.position (i - 3)) in
             let is_king_in_check = is_coord_attacked g (E,Eight) in
             let f_eight_attacked = is_coord_attacked g (F,Eight) in
             let d_eight_attacked = is_coord_attacked g (D,Eight) in
             let is_black_rook_h8 =
               is_piece_of_color_fig g.position Black Rook
                 (index_of_coord (H,Eight)) in
             let is_black_rook_a8 = is_piece_of_color_fig g.position Black Rook
                 (index_of_coord (A,Eight)) in
             let b_can_scastle = g.can_b_scastle && is_black_rook_h8 &&
                                 (not pieces_block_scastle) &&
                                 (not f_eight_attacked) &&
                                 (not is_king_in_check) in
             let b_can_lcastle = g.can_b_lcastle && is_black_rook_a8 &&
                                 (not pieces_block_lcastle) &&
                                 (not d_eight_attacked) &&
                                 (not is_king_in_check) in
             let with_scastle =
               if b_can_scastle then ((coord,(G,Eight))::one_square_moves)
               else one_square_moves in
             let moves_with_castling =
               if b_can_lcastle then ((coord,(C,Eight))::with_scastle)
               else with_scastle in
             moves_with_castling
           |Rook   -> rook_rules p coord_index Black
           |Bishop -> bishop_rules p coord_index Black
           |Knight -> knight_rules p coord_index Black
          )
     )
  )

(* [value_of_wfig wfig] is the material value of a figure*)
let value_of_wfig wfig =
  match wfig with
  |Pawn -> 1 |King -> 3 |Knight -> 3 |Bishop -> 3 |Rook -> 5 |Queen -> 9

(* [value_of_piece piece] is the material value of a figure if the piece is white
 * and negative the mat value of a fig if it is black.*)
let value_of_piece piece =
  let clr_piece = fst piece in
  let fig_piece = snd piece in
  match clr_piece with
  |White -> value_of_wfig fig_piece
  |Black -> -1 * (value_of_wfig fig_piece)

(*[move_help g m] is the new game after m has been applied to g*)
let move_help g m =
  let p = g.position in
  let new_p = move_position p m in
  let clr_to_move = g.turn in
  let start_coord = fst m in
  let start_index = index_of_coord start_coord in
  let char_at_start = p.[start_index] in
  let start_square = pos_char_to_square char_at_start in
  let end_coord = snd m in
  let end_index = index_of_coord end_coord in
  let char_at_end = p.[end_index] in
  let end_square = pos_char_to_square char_at_end in
  let is_capture = is_piece p end_index in
  let new_mat_bal =
    if is_capture then
      (match end_square with
       |Empty -> raise (Precondition_violated "is_piece faulty")
       |Piece (a,b) ->  g.material_balance - (value_of_piece (a,b) ) )
    else g.material_balance in
  let new_pieces =
    match start_square with
    |Empty -> raise (Precondition_violated "start_square empty")
    |Piece (clr_s,fig_s) ->
      (match clr_s with
       |White ->
         let w_figcoord_gone =
           remove_fig_coord g.white_pieces (fig_s,start_coord) in
         let w_new_figcoord_added = (fig_s,end_coord)::w_figcoord_gone in
         (match end_square with
          |Empty-> (w_new_figcoord_added, g.black_pieces)
          |Piece (_,fig_e)->
            let b_end_removed =
              remove_fig_coord g.black_pieces (fig_e,end_coord) in
            (w_new_figcoord_added,b_end_removed) )
       |Black ->
         let b_figcoord_gone =
           remove_fig_coord g.black_pieces (fig_s,start_coord) in
         let b_new_figcoord_added = (fig_s,end_coord)::b_figcoord_gone in
         (match end_square with
          |Empty-> (g.white_pieces, b_new_figcoord_added)
          |Piece (_,fig_e)->
            let w_end_removed =
              remove_fig_coord g.white_pieces (fig_e,end_coord) in
            (w_end_removed,b_new_figcoord_added)
         )
      ) in
  let standard_new_g =
    {g with position = new_p;
            turn =  (match clr_to_move with |White -> Black |Black -> White);
            white_pieces = fst new_pieces;
            black_pieces = snd new_pieces;
            material_balance = new_mat_bal;
            moves50 = if is_capture then 0 else g.moves50 + 1;
            past_positions = g.position::g.past_positions } in
  match start_square with
  |Empty -> g
  |Piece (clr,fig) ->
    match fig with
    |King ->
      let file_indic_strt = (start_index mod 8) + 1 in
      let file_indic_end  = (end_index   mod 8) + 1 in
      let dist =  file_indic_strt - file_indic_end in
      let is_scastling = dist < -1 in
      let is_lcastling = dist >  1 in
      if dist <= 1 && dist >= -1 && (White = clr_to_move) then
        {standard_new_g with can_w_scastle = false;
                             can_w_lcastle = false; wking_coord = end_coord}
      else if dist <= 1 && dist >= -1 && (Black = clr_to_move) then
        {standard_new_g with can_b_scastle = false;
                             can_b_lcastle = false; bking_coord = end_coord
      }
      else if is_scastling && (White = clr_to_move) then
        let p_rook_moved = move_position new_p ((H,One),(F,One)) in
        let white_pieces_rook_removed =
          remove_fig_coord standard_new_g.white_pieces (Rook,(H,One)) in
        let rook_correct = (Rook,(F,One)):: white_pieces_rook_removed in
        {standard_new_g with position = p_rook_moved; can_w_scastle = false;
                             can_w_lcastle = false; wking_coord = (G,One);
                             w_castled = true; white_pieces = rook_correct}
      else if is_scastling && (Black = clr_to_move) then
        let p_rook_moved = move_position new_p ((H,Eight),(F,Eight)) in
        let black_pieces_rook_removed =
          remove_fig_coord standard_new_g.black_pieces (Rook,(H,Eight)) in
        let rook_correct = (Rook,(F,Eight)):: black_pieces_rook_removed in
        {standard_new_g with position = p_rook_moved; can_b_scastle = false;
                             can_b_lcastle = false; bking_coord = (G,Eight);
                             b_castled = true; black_pieces = rook_correct}
      else if is_lcastling && (White = clr_to_move) then
        let p_rook_moved = move_position new_p ((A,One),(D,One)) in
        let white_pieces_rook_removed =
          remove_fig_coord standard_new_g.white_pieces (Rook,(A,One)) in
        let rook_correct = (Rook,(D,One)):: white_pieces_rook_removed in
        {standard_new_g with position = p_rook_moved; can_w_scastle = false;
                             can_w_lcastle = false; wking_coord = (C,One);
                             w_castled = true; white_pieces = rook_correct}
      else if is_lcastling && (Black = clr_to_move) then
        let p_rook_moved = move_position new_p ((A,Eight),(D,Eight)) in
        let black_pieces_rook_removed =
          remove_fig_coord standard_new_g.black_pieces (Rook,(A,Eight)) in
        let rook_correct = (Rook,(D,Eight)):: black_pieces_rook_removed in
        {standard_new_g with position = p_rook_moved; can_b_scastle = false;
                             can_b_lcastle = false; bking_coord = (C,Eight);
                             b_castled = true; black_pieces = rook_correct}
      else raise (Precondition_violated "not possible case")
    |Rook ->
      (match start_coord with
       |(H,One)   -> {standard_new_g with can_w_scastle = false}
       |(H,Eight) -> {standard_new_g with can_b_scastle = false}
       |(A,One)   -> {standard_new_g with can_w_lcastle = false}
       |(A,Eight) -> {standard_new_g with can_b_lcastle = false}
       |_         ->  standard_new_g)
    |Pawn ->
      (match clr_to_move with
       |White ->
         let ind_front_right = start_index + 9 in
         let ind_front_left  = start_index + 7 in
         let wasnt_piece_front_right = not (is_piece p ind_front_right) in
         let wasnt_piece_front_left = not (is_piece p ind_front_left) in
         let rank_end = end_index / 8 + 1 in
         if rank_end = 8 then (*promotion*)
           let p_queen_inserted =
             insert_in_position new_p end_index (Piece (White,Queen)) in
           let wpieces_pawn_removed =
             remove_fig_coord standard_new_g.white_pieces (Pawn,end_coord) in
           let wpieces_with_queen = (Queen,end_coord)::wpieces_pawn_removed in
           {standard_new_g with position = p_queen_inserted;
                                white_pieces = wpieces_with_queen; moves50 = 0;
                                material_balance =
                                  standard_new_g.material_balance +
                (value_of_piece (White,Queen)) - (value_of_piece (White,Pawn))}
           (*enpassant*)
         else if wasnt_piece_front_right && (end_index = ind_front_right) then
           let coord_rt = coord_of_index (start_index + 1) in
           let b_pieces_enp_cap =
             remove_fig_coord standard_new_g.black_pieces (Pawn,coord_rt) in
           let p_pawn_rt_removed =
             insert_in_position new_p (start_index+1) Empty in
           {standard_new_g with position = p_pawn_rt_removed;
                                black_pieces = b_pieces_enp_cap; moves50= 0;
                                material_balance =
                                  standard_new_g.material_balance -
                                  (value_of_piece (Black,Pawn)) }
         else if wasnt_piece_front_left && (end_index = ind_front_left) then
           let coord_lft = coord_of_index (start_index -1) in
           let b_pieces_enp_cap =
             remove_fig_coord standard_new_g.black_pieces (Pawn,coord_lft) in
           let p_pawn_lft_removed =
             insert_in_position new_p (start_index-1) Empty in
           {standard_new_g with position = p_pawn_lft_removed;
                                black_pieces = b_pieces_enp_cap; moves50= 0;
                                material_balance =
                                  standard_new_g.material_balance -
                                  (value_of_piece (Black,Pawn)) }
         else
           {standard_new_g with moves50 = 0}
       |Black ->
         let ind_front_right = start_index - 7 in
         let ind_front_left  = start_index - 9 in
         let wasnt_piece_front_right = not (is_piece p ind_front_right) in
         let wasnt_piece_front_left = not (is_piece p ind_front_left) in
         let rank_end = end_index / 8 + 1 in
         if rank_end = 1 then (*promotion*)
           let p_queen_inserted =
             insert_in_position new_p end_index (Piece (Black,Queen)) in
           let bpieces_pawn_removed =
             remove_fig_coord standard_new_g.black_pieces (Pawn,end_coord) in
           let bpieces_with_queen = (Queen,end_coord)::bpieces_pawn_removed in
           {standard_new_g with position = p_queen_inserted;
                                black_pieces = bpieces_with_queen; moves50 = 0;
                                material_balance =
                                  standard_new_g.material_balance +
                (value_of_piece (Black,Queen)) - (value_of_piece (Black,Pawn))}
           (*enpassant*)
         else if wasnt_piece_front_right && (end_index = ind_front_right) then
           let coord_rt = coord_of_index (start_index + 1) in
           let w_pieces_enp_cap =
             remove_fig_coord standard_new_g.white_pieces (Pawn,coord_rt) in
           let p_pawn_rt_removed =
             insert_in_position new_p (start_index+1) Empty in
           {standard_new_g with position = p_pawn_rt_removed;
                                white_pieces = w_pieces_enp_cap; moves50= 0;
                                material_balance =
                                  standard_new_g.material_balance -
                                  (value_of_piece (White,Pawn)) }
         else if wasnt_piece_front_left && (end_index = ind_front_left) then
           let coord_lft = coord_of_index (start_index -1) in
           let w_pieces_enp_cap =
             remove_fig_coord standard_new_g.white_pieces (Pawn,coord_lft) in
           let p_pawn_lft_removed =
             insert_in_position new_p (start_index-1) Empty in
           {standard_new_g with position = p_pawn_lft_removed;
                                white_pieces = w_pieces_enp_cap; moves50= 0;
                                material_balance =
                                  standard_new_g.material_balance -
                                  (value_of_piece (White,Pawn)) }
         else
           {standard_new_g with moves50 = 0} )
    |Queen ->
      (match clr with
      |White -> {standard_new_g with w_queen_moves = g.w_queen_moves + 1}
      |Black -> {standard_new_g with b_queen_moves = g.w_queen_moves + 1})
    |Knight->
      (match clr with
      |White ->
        if start_coord = (G,One) then
          let current_k_dvl_queenside = fst (g.w_knights_developed) in
          {standard_new_g with w_knights_developed =
                                 (current_k_dvl_queenside, true)}
        else if start_coord = (B,One) then
          let current_k_dvl_kingside = snd (g.w_knights_developed) in
          {standard_new_g with w_knights_developed =
                                 (true, current_k_dvl_kingside)}
        else standard_new_g
      |Black ->
        if start_coord = (G,Eight) then
          let current_k_dvl_queenside = fst (g.b_knights_developed) in
          {standard_new_g with b_knights_developed =
                                 (current_k_dvl_queenside, true)}
        else if start_coord = (B,Eight) then
          let current_k_dvl_kingside = snd (g.b_knights_developed) in
          {standard_new_g with w_knights_developed =
                                 (true, current_k_dvl_kingside)}
        else standard_new_g)
    |Bishop->
      (match clr with
      |White ->
        if start_coord = (F,One) then
          let current_b_dvl_queenside = fst (g.w_bishops_developed) in
          {standard_new_g with w_bishops_developed =
                                 (current_b_dvl_queenside, true)}
        else if start_coord = (C,One) then
          let current_b_dvl_kingside = snd (g.w_bishops_developed) in
          {standard_new_g with w_bishops_developed =
                                 (true, current_b_dvl_kingside)}
        else standard_new_g
      |Black ->
        if start_coord = (F,Eight) then
          let current_b_dvl_queenside = fst (g.b_bishops_developed) in
          {standard_new_g with b_bishops_developed =
                                 (current_b_dvl_queenside, true)}
        else if start_coord = (C,Eight) then
          let current_b_dvl_kingside = snd (g.b_bishops_developed) in
          {standard_new_g with b_bishops_developed =
                                 (true, current_b_dvl_kingside)}
        else standard_new_g)


let rec find_coords g fig_coord_lst fig coord_acc =
  match fig_coord_lst with
  |h::t  ->
    if (fst h) = fig then
      find_coords g t fig (
        ( (snd h), snd_lst (moves_wo_check_considered g (snd h) ) )
        ::coord_acc )
    else find_coords g t fig coord_acc
  |[] -> coord_acc

let char_to_piece chr =
  match chr with
  |'B'->Bishop|'Q'->Queen|'K'->King|'R'->Rook|'N'->Knight|'P'->Pawn
  |_ -> raise (Precondition_violated "not a piece")

let moves g fig_char =
  let fig = char_to_piece fig_char in
  match g.turn with
  | White -> find_coords g g.white_pieces fig []
  | Black -> find_coords g g.black_pieces fig []

let rec insert k_v lst new_lst_acc =
  match lst with
  |h::t ->
    let key_to_insert = fst k_v in
    let key_at_h = fst h in
    if key_at_h = key_to_insert then
      let val_to_insert = snd k_v in
      let val_at_h = snd h in
      let new_kv = (key_at_h, (val_to_insert) :: val_at_h ) in
      t @ (new_kv :: new_lst_acc)
    else
      insert k_v t (h::new_lst_acc)
  |[] -> (fst k_v , [snd k_v]):: new_lst_acc

let rec move_attacking moves piece_coords value coord_attackers_acc =
  match moves with
  |h::t ->
    let end_coord = snd h in
    if List.mem end_coord piece_coords then
      let new_coord_attackers =
        insert (end_coord, value) coord_attackers_acc [] in
      move_attacking t piece_coords value new_coord_attackers
    else move_attacking t piece_coords value coord_attackers_acc
  |[] -> coord_attackers_acc

let rec attackers g to_move_pieces_coords not_to_move_coords coord_attackers_acc=
  match to_move_pieces_coords with
  |h::t ->
    let coord_piece = snd h in
    let valid_moves = moves_wo_check_considered g coord_piece in
    let value = value_of_wfig (fst h) in
    let new_dict =
      move_attacking valid_moves not_to_move_coords value coord_attackers_acc in
    attackers g t not_to_move_coords new_dict
  |[]-> coord_attackers_acc

let rec defenders g coord_defending to_move_pieces_coords defenders_acc =
  match to_move_pieces_coords with
  |h::t ->
    let h_coord = snd h in
    let moves = moves_wo_check_considered g h_coord in
    let moves_end_square = snd_lst moves in
    if List.mem coord_defending moves_end_square then
      let value_h_piece = value_of_wfig (fst h) in
      defenders g coord_defending t (value_h_piece :: defenders_acc)
    else
      defenders g coord_defending t defenders_acc
  |[] -> defenders_acc

let rec value_defenders_added g coord_attackers_lst coord_attcker_dfnders_acc=
  match coord_attackers_lst with
  |h::t ->
    let coord_attacked = fst h in
    let opposite_pieces = match g.turn with
      |White -> g.black_pieces
      |Black -> g.white_pieces  in
    let g_opp_turn = opposite_turn g in
    let coord_attacked_i = index_of_coord coord_attacked in
    let g_no_piece =
      remove_piece_at_i_from_g g_opp_turn coord_attacked_i in
    let g_opp_clr_piece = match g.turn with
      |White ->
        let new_p =
          insert_in_position
            g_no_piece.position coord_attacked_i (Piece (White,Pawn)) in
        {g_no_piece with position = new_p}
      |Black ->
        let new_p =
          insert_in_position
            g_no_piece.position coord_attacked_i (Piece (Black,Pawn)) in
        {g_no_piece with position = new_p}
    in
    let defender_vals =
      defenders g_opp_clr_piece coord_attacked opposite_pieces [] in
    let new_acc =
      (fst h, (snd h , defender_vals ) ) :: coord_attcker_dfnders_acc in
    value_defenders_added g t new_acc
  |[] -> coord_attcker_dfnders_acc

let rec find_fig coord fig_coord_lst =
  match fig_coord_lst with
  |h::t ->
    if snd h = coord then fst h
    else find_fig coord t
  |[]-> raise (Precondition_violated "not present")

let attackers_defenders g =
  let to_move_pieces_coords =
    (match g.turn with
    |White -> g.white_pieces
    |Black -> g.black_pieces) in
  let not_to_move_pieces_coords =
    (match g.turn with
     |White -> g.black_pieces
     |Black -> g.white_pieces) in
  let not_to_move_coords = snd_lst not_to_move_pieces_coords in
  let coord_attackers_lst =
    attackers g to_move_pieces_coords not_to_move_coords [] in
  let defenders_added = value_defenders_added g coord_attackers_lst [] in
  let val_of_coord a =
    let coord = fst a in
    let fig_at_coord = find_fig coord not_to_move_pieces_coords in
    (value_of_wfig fig_at_coord,snd a) in
  List.map val_of_coord defenders_added

let rec is_check_help g piece_coord_lst =
  match piece_coord_lst with
  |h::t ->
    let start_coord = snd h in
    let valid_moves = (moves_wo_check_considered g start_coord) in
    let to_move = g.turn in
    (match to_move with
     |Black ->
       let is_wking_attacked =
         List.mem (start_coord,g.wking_coord) valid_moves in
       if is_wking_attacked then true else
         is_check_help g t
     |White ->
       let is_bking_attacked =
         List.mem (start_coord,g.bking_coord) valid_moves in
       if is_bking_attacked then true else
         is_check_help g t)
  |[]-> false

let is_check g =
  let to_move = g.turn in
  match to_move with
  |White ->
    let g_black_to_move = {g with turn = Black} in
    is_check_help g_black_to_move g.black_pieces
  |Black ->
    let g_white_to_move = {g with turn = White} in
    is_check_help g_white_to_move g.white_pieces

let is_valid_move g m =
  let start_coord = fst m in
  let valid_moves = moves_wo_check_considered g start_coord in
  let is_m_valid = List.mem m valid_moves in
  is_m_valid

(*would the player whose turn it is to move be left in check by playing m*)
let in_check_after_move g m =
  let to_move = g.turn in
  let g_after_move = move_help g m in
  match to_move with
  |White ->
    let after_m_w_to_move = {g_after_move with turn = White} in
    let is_w_in_check_after_m = is_check after_m_w_to_move in
    is_w_in_check_after_m
  |Black ->
    let after_m_b_to_move = {g_after_move with turn = Black} in
    let is_b_in_check_after_m = is_check after_m_b_to_move in
    is_b_in_check_after_m

let rec lst_valid_moves_help piece_coord_lst g move_acc =
  match piece_coord_lst with
  |h::t ->
    let start_coord = snd h in
    let moves_from_h = moves_wo_check_considered g start_coord in
    lst_valid_moves_help t g (moves_from_h @ move_acc)
  |[] -> move_acc

let lst_valid_moves g =
  match g.turn with
  |White ->
    lst_valid_moves_help g.white_pieces g []
  |Black ->
    lst_valid_moves_help g.black_pieces g []


let rec squares_controlled_help piece_coord_lst g move_acc color =
  (match piece_coord_lst with
  |h::t ->
    let piece = fst h in
    (match piece with
     |Pawn ->
     let start_coord = snd h in
     let start_index = index_of_coord start_coord in
     let start_file = fst start_coord in
      (match color with
       |White->
         let ind_front_right = start_index + 9 in
         let ind_front_left = start_index + 7 in
         let coord_front_right = coord_of_index ind_front_right in
         let coord_front_left = coord_of_index ind_front_left in
         if start_file = A then
           let new_move_acc = start_coord :: (coord_front_right :: move_acc) in
           squares_controlled_help t g new_move_acc color
         else if start_file = H then
           let new_move_acc = [start_coord; coord_front_left]@ move_acc in
           squares_controlled_help t g new_move_acc color
         else
           let new_move_acc =
             [start_coord; coord_front_right; coord_front_left] @ move_acc in
        squares_controlled_help t g new_move_acc color

       |Black->
         let ind_front_right = start_index - 7 in
         let ind_front_left = start_index - 9 in
         let coord_front_right = coord_of_index ind_front_right in
         let coord_front_left = coord_of_index ind_front_left in
         if start_file = A then
           let new_move_acc = start_coord :: (coord_front_right :: move_acc) in
           squares_controlled_help t g new_move_acc color
         else if start_file = H then
          let new_move_acc = [start_coord; coord_front_left]@ move_acc in
           squares_controlled_help t g new_move_acc color
         else
           let new_move_acc =
             [start_coord; coord_front_right; coord_front_left] @ move_acc in
        squares_controlled_help t g new_move_acc color
      )
    |_->
      let start_coord = snd h in
      let moves_from_h = snd_lst (moves_wo_check_considered g start_coord) in
      squares_controlled_help t g (moves_from_h @ move_acc) color)
  |[] -> move_acc )

let squares_controlled g =
  match g.turn with
  |White ->
    squares_controlled_help g.white_pieces g [] White
  |Black ->
    squares_controlled_help g.black_pieces g [] Black

let rec all_cause_check move_lst g =
  match move_lst with
  |h::t ->
    let in_check = (in_check_after_move g h) in
    if not in_check then false
    else all_cause_check t g
  |[] -> true

let is_draw_50_or_threefold g =
  let draw_by_50_move = g.moves50 >= 50 in
  let p = g.position in
  let matches = List.filter (fun s -> s = p) g.past_positions in
  let draw_by_threefold = (List.length matches) >= 2 in
  draw_by_50_move||draw_by_threefold

let is_draw g =
  let draw_by_50_move_or_threefold = is_draw_50_or_threefold g in
  let in_check = (is_check g) in
  let valid_moves = lst_valid_moves g in
  let all_v_moves_check = all_cause_check valid_moves g in
  let stalemate = all_v_moves_check && (not in_check) in
  draw_by_50_move_or_threefold || stalemate

let is_legal g m =
  (is_valid_move g m) &&
  (not (in_check_after_move g m) ) (*&&
  (not (is_draw g))*)

let is_valid_move_legal g m = not (in_check_after_move g m)

let rec all_legal_moves_help g v_moves move_acc =
  match v_moves with
  |h::t ->
    if (is_valid_move_legal g h) then all_legal_moves_help g t (h::move_acc)
    else all_legal_moves_help g t move_acc
  |[] -> move_acc

let all_legal_moves g =
  let valid_moves = lst_valid_moves g in
  all_legal_moves_help g valid_moves []

let is_checkmate g =
  let in_check = is_check g in
  if (not in_check) then false else
  let have_no_legal_moves =
    (List.length (all_legal_moves g)) = 0  in
  in_check && have_no_legal_moves

let move g m =
  (*if (is_legal g m)
  then move_help g m
    else g*)
  move_help g m
