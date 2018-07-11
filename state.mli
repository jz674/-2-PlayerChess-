type game

type move

type coordinate

(* [start_game] is the game that represents the starting position of chess.*)
val start_game: game

(* [move m g] returns a new game after executing the given move on the
 * current game. *)
val move: game -> move -> game

(* [is_check g c] is true if in game [g] the king with color [c] is in check.
 * False otherwise. *)
val is_check: game -> bool

(* [is_checkmate g c] is true if in game [g] the king with color [c] is in
 * checkmate. False otherwise. *)
val is_checkmate: game -> bool

(* [is_draw g] is true if game [g] is a draw.
 * False otherwise. *)
val is_draw: game -> bool

(*[is_draw_50_or_threefold g] is true if game g is a draw by the fifty move rule
 * or the threefold repetition rule and is false otherwise. *)
val is_draw_50_or_threefold: game->bool

(* [turn g] is "White" if the if it is the white player's turn in game g and
 * "Black" if it is the black players turn. *)
val turn: game -> string

(* [opposite_turn g] is g with black to move if in g it is white's turn, and
 * is g with white to move if it is black's turn in g. *)
val opposite_turn: game -> game

(* [get_material_balance g] is the result of summing the values of each white
 * piece in g and then subtracting from that sum the values of each black piece
 * in g. The values are Queen = 9, Rook = 5, Bishop = 3, Knight = 3, and
 * pawn = 1. *)
val get_material_balance: game -> int

(* [attackers_defenders g] is a list in which each element describes a piece
 * that the player to move is attacking. Each element is given as the value of the piece
 * being attacked * the list of values of pieces attacking the piece * the
 * the list of values of the pieces defending. The lists of values of the
 * attackers and defenders are sorted least to highest. *)
val attackers_defenders: game -> (int *(int list * int list)) list

(* [is_legal g m] returns true if the move [m] is legal in game [g].
 * False otherwise. *)
val is_legal: game -> move -> bool

(* [string_of_move m] is the string of start square of the move space the end
 * square of the move. For example, a move of any piece from e2 to e4 would
 * be "e2 e4". *)
val string_of_move: move -> string

(* [int_of_coord c] is the pair of integers representing the coordinate such
 *  that (a,1) goes to (1,1), (b,1) goes to (2,1), (a,2) goes to (1,2), and so
 *  on for all 64 different coordinates.*)
val int_of_coord: coordinate -> (int*int)

(* [all_legal_moves g] is a list of all legal moves in game [g]. If game [g]
 * is in a checkmate or draw, the list is empty. *)
val all_legal_moves: game -> move list

(* [lst_valid_moves g] is a list of all the pseudo-legal moves in g, ie
 * possible moves that may or may not leave the player moving a piece in
 * check. *)
val lst_valid_moves: game -> move list

(* [squares_controlled g] are all the coordinates that the player to move could
 *  capture on if there was an opposite colored piece on the square. *)
val squares_controlled: game -> coordinate list

(* [pieces_developed g] is the int in 0-4 representing how many knights and
 * the player to move has moved at least once.*)
val pieces_developed: game-> int

(* [has_castled g] is true if the player to move in g has castled and false
 * otherwise. *)
val has_castled: game -> bool

(* [moves g c] is a list in which each element represents a piece of c that can
 * *)
val moves: game -> char -> (coordinate* (coordinate list)) list

(* [number_moves g] is the number of moves played in g where one move is white
 * moving a piece then black moving a piece. *)
val number_moves: game -> int

(* [number_queen_moves g] is the number of times the queen of the color of the
 * player to move has moved. *)
val number_queen_moves: game -> int

(* [can_castle g] is true if the player to move still has castling rights on
 * either the kingside or the queenside. False otherwise. *)
val can_castle: game -> bool

(* [create_move ((f1,r1),(f2,r2))] represents the chess move from starting
 * square (f1,r1) to (f2,r2) where f1 and f2 are lowercase chars in
 * 'a' - 'h' representing the file and r1 and r2 are in 1 - 8 representing the
 * the rank.*)
val create_move: (char*int) * (char*int) -> move

(*[coordinate_pair_to_move c1 c2] creates a move that starts at square c1 and
 * ends at square c2. *)
val coordinate_pair_to_move: coordinate -> coordinate -> move

(* [create_coordinate tup] is the coordinate whose file is fst tup and whose
 * rank is snd tup.
 * Precondition: fst tup must be in 'a'-'h' and snd tup must be in 1-8. *)
val create_coordinate:(char*int) -> coordinate

(* [string_of_game g] is a 64 length string with the character at index 0
 * representing the piece at (a,1), index 1 representing (a,2), ..., index 8
 * representing (b,1). '_' at an index represents a square with no piece.
 * A capitol letter is a white piece and a lowercase letter is a black piece.
 * P means there is a pawn, K a king, B a bishop, Q a queen, N a knight, and R
 * is a rook. *)
val string_of_game: game -> string
