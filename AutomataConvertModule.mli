open CommonModule

val nullNfaToNfa :
  ('a, 'b automataEdge, 'a list) automata ->
  ('a, 'b automataEdge, 'a list) automata

val nullNfaToDfa : ('a, 'b automataEdge, 'a list) automata ->
  ('a list, 'b automataEdge, 'a list option) automata
