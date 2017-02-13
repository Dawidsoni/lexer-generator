open CommonModule

val createTokenMatchFunc :
  ('a * ('b, 'c, 'd) automata) list ->
  ('b list, 'e, 'f) automata -> 'b list -> 'a option
