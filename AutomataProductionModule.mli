open CommonModule

val createTokNfaPairList :
  ('a, 'b, ('a, 'b) rightGrammarRule) grammar ->
  ('b * (int, 'a automataEdge, int list) automata) list

val mergeNfaList :
  (int, 'a automataEdge, int list) automata list ->
  (int, 'a automataEdge, int list) automata
