
exception Precondition_violated of string

let rec game_loop st is_ai_playing color_ai_plays =
  let str_turn = State.turn st in
  if is_ai_playing && (str_turn = color_ai_plays) then
    (if State.is_check st then
      (print_endline (str_turn^" is in check");
       print_endline (str_turn^" to move: ");
        print_endline "The AI is thinking...";
         let best_move = Ai.best_move st 3000.0 in
         print_endline ("\nThe AI played "^ State.string_of_move best_move);
         let new_state = State.move st best_move in
         Display.print_game new_state;
         if State.is_checkmate new_state then
           (print_string ("\nCheckmate, " ^str_turn^ " wins.\n");())
         else if State.is_draw new_state then
           (print_string ("\nThe game ended in a draw.\n");())
         else game_loop new_state is_ai_playing color_ai_plays)
    else
    (print_endline "The AI is thinking...";
     let best_move = Ai.best_move st 3000.0 in
     print_endline ("\nThe AI played "^ State.string_of_move best_move);
     let new_state = State.move st best_move in
     Display.print_game new_state;
     if State.is_checkmate new_state then
       (print_string ("\nCheckmate, " ^str_turn^ " wins.\n");())
     else if State.is_draw new_state then
       (print_string ("\nThe game ended in a draw.\n");())
     else game_loop new_state is_ai_playing color_ai_plays))
  else
    (print_string ("\n"^str_turn^" to move: ");
  match read_line () with
  | exception End_of_file -> ()
  | user_input ->
    let command =
       Command.parse user_input st in
    match command with
    |Command.Unknown ->
      (print_endline ((String.trim user_input)^ " is not a valid move");
       game_loop st is_ai_playing color_ai_plays)
    |Command.Quit ->
      (print_endline (str_turn^" has quit the game. Thanks for playing.");())
    |Command.Ai ->
      print_endline "The AI is thinking...";
      let best_move = Ai.best_move st 3000.0 in
      print_endline ("\nThe AI played "^ State.string_of_move best_move);
      let new_state = State.move st best_move in
      Display.print_game new_state;
      if State.is_checkmate new_state then
        (print_string ("\nCheckmate, " ^str_turn^ " wins.\n");())
      else if State.is_draw new_state then
        (print_string ("\nThe game ended in a draw.\n");())
      else game_loop new_state is_ai_playing color_ai_plays
    |Command.Move move ->
      if not (State.is_legal st move) then
        (print_endline ((String.trim user_input)^ " is an illegal move.");
         game_loop st is_ai_playing color_ai_plays) else
      let new_state = State.move st move in
      Display.print_game new_state;
      if State.is_checkmate new_state then
        (print_string ("\nCheckmate, " ^str_turn^ " wins.\n");())
      else if State.is_draw new_state then
        (print_string ("\nThe game ended in a draw.\n");())
      else game_loop new_state is_ai_playing color_ai_plays
    |Command.Eval ->
      let evaluation = Ai.eval st in
      print_endline (string_of_float evaluation);
      game_loop st is_ai_playing color_ai_plays
    |Command.Vague_Notation ->
      print_endline
        ("Your notation is vague. Multiple pieces can move to the square given.");
      game_loop st is_ai_playing color_ai_plays)

(* [play_game f] starts the game loop with the appropriate parameters. *)
let play_game str =
  let initial_state = State.start_game in
  print_endline
    "\nTo make a move, type the file and rank of the start square
followed by a space and the file and rank of the end square.
For example, 'h2 h4' is a valid move. You can also use some
algebraic notation like '0-0', 'e4', and 'Nf3'. Type 'quit' to end
the game. Type 'ai' for the AI to make a move. Type 'eval' to get
the evaluation of the position. ";
  Display.print_game (initial_state);
  match str with
  |"ai white" ->
    print_endline "The AI is playing as white.";
    game_loop initial_state true "White"
  |"ai black" ->
    print_endline "The AI is playing as black.";
    game_loop initial_state true "Black"
  |_ ->
  let initial_state = State.start_game in
  game_loop initial_state false "none"

(* [main ()] starts the REPL, which prompts the player to either play against
 * the ai as white or black or start a normal game. *)
let main () =
  ANSITerminal.(print_string [blue]
"\nWelcome to the best chess Repl around. Type 'ai black' to play as
white against the AI. Type 'ai white' to play as black against the AI.
Type anything else to play a normal game.\n");
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> play_game str

let () = main ()
