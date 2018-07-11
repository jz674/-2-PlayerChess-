 type command =
   |Move of State.move
   |Quit
   |Ai
   |Unknown
   |Vague_Notation
   |Eval

(*[in_a_h c] is true if c is in 'a'-'h' and false otherwise*)
let in_a_h c =
  match c with
  |'a'->true|'b'->true|'c'->true|'d'->true|'e'->true|'f'->true
  |'g'->true|'h'->true|_->false

(*[is_piece_char c] is true if c is in a character representing a piece*)
let is_piece_char c =
  match c with
  |'N'-> true |'Q'->true |'K'-> true |'R'->true|'B' -> true|_->false

(*[in_1_8 c] is true if c is in '1'-'8' and false otherwise*)
let in_1_8 c =
  match c with
  |'1' -> true| '2'-> true| '3'->true|'4' -> true| '5'-> true| '6'->true
  |'7' -> true| '8'-> true| _ ->false

exception Precondition_violated

let parse str g =
  let str_t = String.trim str in
  let len_str_t = String.length str_t in
  if str_t  = "quit" then Quit
  else if str_t = "eval" then Eval
  else if str_t = "ai" then Ai
  else if len_str_t = 2 then
    (let end_file_char = str_t.[0] in
     let end_rank_char = str_t.[1] in
     if (not (in_1_8 end_rank_char )) || (not (in_a_h end_file_char)) then Unknown
     else(
       let end_rank_int = (int_of_char end_rank_char) - 48 in
       let end_coord = State.create_coordinate (end_file_char, end_rank_int) in
       let moves = State.moves g 'P' in
       let rec find_starts_for_end m end_coord start_acc =
         (match m with
          |h:: t ->
            if (List.mem end_coord (snd h)) then
              find_starts_for_end t end_coord ((fst h)::start_acc)
            else find_starts_for_end t end_coord start_acc
          |[] -> start_acc) in
       let start_lst  = find_starts_for_end moves end_coord [] in
       let len_start_lst = List.length start_lst in
       if len_start_lst = 0 then Unknown
       else if len_start_lst > 1 then Vague_Notation
       else
         Move (State.coordinate_pair_to_move (List.hd start_lst) end_coord)
     )
    )
  else if len_str_t = 3 then
    if str_t = "O-O" || str_t = "0-0" then
      let turn = State.turn g in
      (match turn with
      |"White" -> Move (State.create_move (('e',1),('g',1)) )
      |"Black" -> Move (State.create_move (('e',8),('g',8)))
      |_ -> raise Precondition_violated )
    else
    (let piece_char = str_t.[0] in
    let end_file_char = str_t.[1] in
     let end_rank_char = str_t.[2] in
     if (not (in_1_8 end_rank_char )) || (not (in_a_h end_file_char))
        || (not (is_piece_char piece_char)) then Unknown
     else(
       let end_rank_int = (int_of_char end_rank_char) - 48 in
       let end_coord = State.create_coordinate (end_file_char, end_rank_int) in
       let moves = State.moves g piece_char in
      let rec find_starts_for_end m end_coord start_acc =
        (match m with
         |h:: t ->
           if (List.mem end_coord (snd h)) then
             find_starts_for_end t end_coord ((fst h)::start_acc)
           else find_starts_for_end t end_coord start_acc
         |[] -> start_acc) in
       let start_lst  = find_starts_for_end moves end_coord [] in
       let len_start_lst = List.length start_lst in
       if len_start_lst = 0 then Unknown
       else if len_start_lst > 1 then Vague_Notation
       else
        Move (State.coordinate_pair_to_move (List.hd start_lst) end_coord)
      )
    )
  else if len_str_t != 5 then Unknown
  else
  if str_t = "O-O-O" || str_t = "0-0-0" then
    let turn = State.turn g in
    (match turn with
     |"White" -> Move (State.create_move (('e',1),('c',1)) )
     |"Black" -> Move (State.create_move (('e',8),('c',8)))
     |_ -> raise Precondition_violated )
  else
    let file_s = str_t.[0] in
    let is_file_s_valid = in_a_h file_s in
    let rank_s = str_t.[1] in
    let is_rank_s_valid = in_1_8 rank_s in
    let is_space = str_t.[2] = ' ' in
    let file_end = str_t.[3] in
    let is_file_end_valid = in_a_h file_end in
    let rank_end = str_t.[4]  in
    let is_rank_end_valid = in_1_8 rank_end in
    if is_file_s_valid && is_rank_s_valid && is_file_end_valid
       && is_rank_end_valid && is_space then
      Move (State.create_move
              ( (file_s,(int_of_char rank_s) - 48),
                (file_end, (int_of_char rank_end) - 48)))
    else Unknown
