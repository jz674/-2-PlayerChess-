exception Error

(*represents the color of the player to move*)
type color =
  |White |Black

(* [color_of_string s] is White if s is "White" and Black if s is "Black".
 * Precondition: s must be either "White" or "Black"*)
let color_of_string s =
  match s with
  |"White" -> White |"Black"-> Black |_-> raise Error

(* [opp_color clr] is Black if clr is White, and White if clr is Black. *)
let opp_color clr = match clr with |White -> Black |Black -> White

(* [max_lst lst max_acc] is the maximum element of lst.
 * Precondition: max_acc must be initialized to a value smaller than any element
 * of lst. *)
let rec max_lst lst max_acc =
  match lst with
  |h::t -> max_lst t (max h max_acc)
  | [] -> max_acc

(* [min_lst lst min_acc] is the maximum element of lst.
 * Precondition: min_acc must be initialized to a value greater than any element
 * of lst. *)
let rec min_lst lst min_acc =
  match lst with
  |h::t -> min_lst t (min h min_acc)
  | [] -> min_acc

(* [val_int_file i] is the points assigned to each file where 1 represents the
 * a file, 2 the b file, and so on. *)
let val_int_file i =
  if (i = 4 || i = 5) then 0.05
  else if (i = 3) || (i = 6) then 0.02
  else if (i = 2) || (i = 7) then 0.01
  else 0.009

(* [val_int_rank i clr_to_move ] is the points assigned to each rank where 1
 * represents the first rank, 2 the second rank, and so on. *)
let val_int_rank i clr_to_move =
  match clr_to_move with
  |White ->
    if i = 1 then 0.01
    else if i = 2 then 0.02
    else if i = 3 then 0.03
    else if i = 4 then 0.04
    else if i = 5 then 0.05
    else if i = 6 then 0.06
    else if i = 7 then 0.07
    else 0.08
  |Black ->
    if i = 8 then 0.01
    else if i = 7 then 0.02
    else if i = 6 then 0.03
    else if i = 5 then 0.04
    else if i = 4 then 0.05
    else if i = 3 then 0.06
    else if i = 2 then 0.07
    else 0.08

(* [val_square int_of_coord clr_to_move] is the points awarded for controlling a
 * coordinate represented by a tuple of two ints, int_of_coord, in which fst of
 * the tuple is the int representing the file and snd of the tuple is the int
 * representing the rank.*)
let val_square int_of_coord clr_to_move =
  ((val_int_file (fst int_of_coord)) +.
   (val_int_rank (snd int_of_coord) clr_to_move))

(* [which_are_check g_lst check_lst not_check_lst] is the tuple of games in
 * g_lst which are in check * the games in g_lst that are not in check. *)
let rec which_are_check g_lst check_lst not_check_lst=
  match g_lst with
  |h::t ->
    if (State.is_check h) then
      which_are_check t (h::check_lst) not_check_lst
    else
      which_are_check t check_lst (h::not_check_lst)
  |[] -> (check_lst,not_check_lst)

(* [which_are_capture g_lst start_mat_bal cap_lst not_cap_lst] is the tuple of
 * games in g_lst in which there was just a capture * the games in g_lst
 * in which a capture did not just occur.*)
let rec which_are_capture g_lst start_mat_bal cap_lst not_cap_lst =
  match g_lst with
  |h::t ->
    if (State.get_material_balance h) != start_mat_bal then
      which_are_capture t start_mat_bal (h::cap_lst) not_cap_lst
    else
      which_are_capture t start_mat_bal cap_lst (h::not_cap_lst)
  |[]-> (cap_lst,not_cap_lst)

(* [sum_float_lst lst sum_acc] is the sum of the list of floats lst.
 * Precondition: sum_acc must be initialized to 0. *)
let rec sum_float_lst lst sum_acc =
  (match lst with
   |h::t-> sum_float_lst t (sum_acc +. h)
   |[] -> sum_acc)

(* [sum_first_n lst n sum_acc] is the sum of the list of ints lst up to the nth
 * element.
 * Precondition: sum_acc must be initialized to 0 *)
let rec sum_first_n lst n sum_acc =
  match lst with
  |h::t ->
    if n = 0 then sum_acc
    else
    let new_sum_acc = sum_acc + h in
    sum_first_n t (n-1) new_sum_acc
  |[] -> sum_acc

(* [sum lst sum_acc] is the sum of the list of ints lst.
 * Precondition: sum_acc must be initialized to 0 *)
let rec sum lst sum_acc =
  match lst with
  |h::t -> sum t (sum_acc + h)
  |[] -> sum_acc

(*[potential_caps attackers_defenders max_acc] is the largest gain to be
 * had in a capturing any of the pieces described in attackers_defenders.
 * attackers_defenders is the value of the piece attacked * (a list of values
 * of the attackers) * (a list of values of the defenders) *)
let rec potential_caps attackers_defenders max_acc =
  match attackers_defenders with
  |h::t ->
    let val_piece_attacked = fst h in
    let attackers_lst = fst (snd h) in
    let defenders_lst = snd (snd h) in
    let num_attackers = List.length attackers_lst in
    let num_defenders = List.length defenders_lst in
    let attackers_sorted = List.sort compare attackers_lst in
    let lowest_val_attacker = List.hd attackers_sorted in
    if num_defenders = 0 then potential_caps t (max val_piece_attacked max_acc)
    else if num_attackers > num_defenders then
      let val_lost_on_attack = sum_first_n attackers_sorted num_defenders 0 in
      let val_lost_on_defense = sum defenders_lst 0 in
      let result_exchange = val_piece_attacked +
                            val_lost_on_defense - val_lost_on_attack in
      if result_exchange < 0 then
        potential_caps t max_acc
      else
        let new_acc = max (min val_piece_attacked result_exchange) max_acc in
        potential_caps t new_acc
    else if lowest_val_attacker < val_piece_attacked then
      let new_acc = max (val_piece_attacked - lowest_val_attacker) max_acc in
      potential_caps t new_acc
    else
      potential_caps t max_acc
  |[] -> max_acc

let eval g =
  let str_to_move = State.turn g in
  let clr_to_move = color_of_string str_to_move in
  let mat_bal = State.get_material_balance g in
  let mat_bal_float = float_of_int mat_bal in
  let g_opp = State.opposite_turn g in
  let squares_controlled_to_move = State.squares_controlled g in
  let squares_controlled_not_to_move = State.squares_controlled g_opp in
  let value_square_to_move =
    (fun (c) ->  (val_square (State.int_of_coord c) clr_to_move)) in
  let val_to_move =
    List.map value_square_to_move squares_controlled_to_move in
  let value_square_not_to_move =
    (fun (c) ->  (val_square (State.int_of_coord c) (opp_color clr_to_move))) in
  let val_not_to_move =
    List.map value_square_not_to_move squares_controlled_not_to_move in
  let eval_movement_to_move = sum_float_lst val_to_move 0. in
  let eval_movement_not_to_move = sum_float_lst val_not_to_move 0. in
  let to_move_pts_cstl =
    match State.has_castled g with |true -> 1.2 |false-> 0.0 in
  let not_to_move_pts_cstl =
    match State.has_castled g_opp with |true -> 1.2 |false-> 0.0 in
  let to_move_pts_dvlp = (float_of_int (State.pieces_developed g)) *. 0.33 in
  let not_to_move_pts_dvlp =
    (float_of_int (State.pieces_developed g_opp)) *. 0.33 in
  let to_move_lost_castl =
    if not (State.has_castled g) && not (State.can_castle g)
    then (-1.5) else 0. in
  let not_to_move_lost_castl =
    if not (State.has_castled g_opp) && not (State.can_castle g_opp)
    then (-1.5) else 0. in
  let attackers_defenders = State.attackers_defenders g in
  let val_caps = float_of_int (potential_caps attackers_defenders 0) in
  let num_queen_moves_to_move = State.number_queen_moves g in
  let num_queen_moves_not_to_move = State.number_queen_moves g_opp in
  let move_number = State.number_moves g in
  let queen_moves_pts =
    if move_number < 5
    then -0.90 *. (float_of_int (num_queen_moves_to_move -
                                 num_queen_moves_not_to_move))
    else 0.0 in
    (match clr_to_move with
      |White ->
        mat_bal_float
        +. eval_movement_to_move -. eval_movement_not_to_move
        +. to_move_pts_cstl -. not_to_move_pts_cstl
        +. to_move_pts_dvlp -. not_to_move_pts_dvlp +. 0.2
        +. to_move_lost_castl -. not_to_move_lost_castl +. val_caps
          +. queen_moves_pts
      |Black ->
      mat_bal_float
        -. eval_movement_to_move +. eval_movement_not_to_move
        -. to_move_pts_cstl +. not_to_move_pts_cstl
        -. to_move_pts_dvlp +. not_to_move_pts_dvlp -. 0.2
        -. to_move_lost_castl +. not_to_move_lost_castl -. val_caps
        -. queen_moves_pts
  )

(*[eval_min_max g depth is_min moves_deep is_check] is the evaluation of the min
 * tree starting at g and branching to the games reachable from legal moves in g
 * if is_min is true and is the equivalent max tree if is_min is false. Stops
 * recursing when depth goes below some threshold defined in the function.
 * Precondition: is_check is true if g is in check and false if g is not in
 * check *)
let rec eval_min_max g depth is_min moves_deep is_check =
  let str_to_move = State.turn g in
  let clr_to_move = color_of_string str_to_move in
  let legal_moves = State.all_legal_moves g in
  let int_num_legal_moves = List.length legal_moves in
  if (is_check) && (int_num_legal_moves = 0) then
    (match clr_to_move with (*checkmate*)
     |Black -> 10000.0
     |White -> -10000.0)
  else if int_num_legal_moves = 0 then 0. (*stalemate*)
  else if State.is_draw_50_or_threefold g then 0. (*draw*)
  else(
    let get_game_func = fun (m) -> (State.move g m) in
    let next_games = List.map get_game_func legal_moves in
    let current_mat_bal = State.get_material_balance g in
    let checks_fun_result = which_are_check next_games [] [] in
    let checks = fst checks_fun_result in
    let len_checks = List.length checks in
    let not_checks = snd checks_fun_result in
    let captures_fun_result =
      which_are_capture next_games current_mat_bal [] not_checks in
    let caps = fst captures_fun_result in
    let len_caps = List.length caps in
    let not_checks_or_caps = snd captures_fun_result in
    let len_checks_caps = len_checks + len_caps in
    let float_count_legal_moves = float_of_int (int_num_legal_moves) in
    let threshold = 20.0 in
    let new_depth =
      if depth < threshold then depth else
        floor ((depth -. float_count_legal_moves) /. float_count_legal_moves )
    in
    let depth_checks = if len_checks > 0 then 6.0 *. new_depth else new_depth in
    let depth_caps =
      if len_caps > 0 then 5.0 *. new_depth else new_depth in
    let depth_not_checks_caps =
      if len_checks_caps > 0 then 0.5 *. new_depth else new_depth in
    (if new_depth >= threshold (*|| moves_deep < 2*) then
      let f_depth_updated_checks =
        (fun (g) -> (eval_min_max g depth_checks is_min (moves_deep+1) true)) in
      let f_depth_updated_caps =
        (fun (g) -> (eval_min_max g depth_caps is_min (moves_deep+1) false)) in
      let f_depth_updated_not_checks_caps =
        (fun (g) ->
           (eval_min_max g depth_not_checks_caps is_min (moves_deep+1) false)) in
       (match is_min with
        |true ->
          let result_checks = min_lst
              (List.map f_depth_updated_checks checks) max_float in
          let result_caps = min_lst
              (List.map f_depth_updated_caps caps) max_float in
          let result_not_checks_caps = min_lst
              (List.map f_depth_updated_not_checks_caps not_checks_or_caps)
              max_float in
          min (min result_checks result_not_checks_caps) result_caps
        |false ->
          let result_checks = max_lst
              (List.map f_depth_updated_checks checks) (-1. *. max_float) in
          let result_caps = max_lst
              (List.map f_depth_updated_caps caps) (-1. *. max_float) in
          let result_not_checks_caps = max_lst
              (List.map f_depth_updated_not_checks_caps not_checks_or_caps)
              (-1. *. max_float) in
          max (max result_checks result_not_checks_caps) result_caps
       )
     else
       (match is_min with
        |true ->
          let evaluations = List.map eval next_games in
                   min_lst evaluations max_float
        |false -> let evaluations = List.map eval next_games in
                    max_lst evaluations (-1. *. max_float)
       )
    )
  )

(* [print_evals m_eval_lst g] prints all the moves and evaluations in the
 *  move_eval_lst of game g. *)
let rec print_evals m_eval_lst g =
  match m_eval_lst with
  |h::t ->
    let move = fst h in
    let move_str = State.string_of_move (move) in
    let eval = string_of_float (snd h) in
    print_string (move_str ^": "^eval^"   ");
    print_evals t g
  |[]->()

let best_move g depth =
  let str_to_move = State.turn g in
  let clr_to_move = color_of_string str_to_move in
  let legal_moves = State.all_legal_moves g in
  let float_count_legal_moves = float_of_int (List.length legal_moves) in
  let start_depth = (floor (depth /. float_count_legal_moves)) in
  let comp_snd me1 me2 = compare (snd me1) (snd me2) in
  let comp_snd_rev me1 me2 = -1 * (comp_snd me1 me2) in
  (match clr_to_move with
  |White ->
    (let f = (fun (m) ->
        let next_game  = State.move g m in
        (m,eval_min_max next_game start_depth true 0
           (State.is_check next_game)) ) in
    let move_evals  = List.map f legal_moves in
    let sorted_move_evals = List.sort comp_snd_rev move_evals in
    print_evals sorted_move_evals g;
    fst (List.hd sorted_move_evals))

  |Black ->
    (let f = (fun (m) ->
        let next_game  = State.move g m in
      (m,eval_min_max next_game start_depth false 0 (State.is_check next_game))
      ) in
    let move_evals  = List.map f legal_moves in
    let sorted_move_evals = List.sort comp_snd move_evals in
    print_evals sorted_move_evals g;
    fst (List.hd sorted_move_evals)))
