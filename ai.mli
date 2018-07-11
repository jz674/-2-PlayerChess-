
(* [best_move g f] is the best move for the player to move in game g. f is
 * a parameter used in determining how deeply to search. A recommended value is
 * 3000. . Increasing the number will lead to a deeper search but may lead to a
 * stack overlflow if the search is too deep. best_move has a side effect of
 * of printing each legal move in g and its evaluation. *)
val best_move: State.game -> float -> State.move

(* [eval g] is the evaluation of g where the more positive the result, the better
 *  the position for white, and the more negative the result, the better the
 *  position is for black. Eval is quoted in fractions of a pawn. For example,
 *  an eval of 2.5 means White is 2 and a half pawns better when considering all
 *  factors of the position. *)
val eval: State.game -> float
