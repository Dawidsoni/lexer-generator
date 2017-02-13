open CommonModule

val generateLexer :
  ('a, char automataEdge, 'a option) automata ->
  ('a -> string option) -> string -> unit
