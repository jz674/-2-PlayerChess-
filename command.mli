(*[type command] represents the input that the user made into the repl.*)
type command =
  |Move of State.move
  |Quit
  |Ai
  |Unknown
  |Vague_Notation
  |Eval

(* [parse str] is the command that represents player input [str].
 * parse on str "quit" is Quit and str "eval" is Eval. str of the form
 * "e2 e4", "Nf3", and "e4" are Move. If the algebraic notation is not
 * soecific then Vague_Notation. parse "ai" is Ai.
 * described in the writeup. *)
val parse : string -> State.game -> command
