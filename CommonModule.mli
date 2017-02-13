open List
open Hashtbl

type ('state, 'letter, 'trans) automata = {
  initState : 'state;
  endStateFunc : 'state -> bool;
  transFunc : 'state -> 'letter -> 'trans;
  stateList : 'state list;
  letterList : 'letter list;
}

type ('terminal, 'nonTerminal, 'grammarRule) grammar = {
  ruleFunc : 'nonTerminal -> 'grammarRule list;
  terminalList : 'terminal list;
  nonTerminalList : 'nonTerminal list;
  tokenFunc : 'nonTerminal -> bool
}

type ('terminal, 'nonTerminal) rightGrammarRule = {
  terminal: 'terminal option;
  nonTerminal: 'nonTerminal option;
}

type ('terminal, 'nonTerminal) rightGrammar =
  ('terminal, 'nonTerminal, ('terminal, 'nonTerminal) rightGrammarRule) grammar

type 'a automataEdge =
  | Empty
  | AutEdge of 'a

exception GrammarIncorrect of string
exception AutomataIncorrect of string

val createHashtbl: unit -> ('a, 'b) Hashtbl.t
val hashtblKeys: ('a, 'b) Hashtbl.t -> 'a list
val hashtblVals: ('a, 'b) Hashtbl.t -> 'b list
val revHashtbl: ('a, 'b) Hashtbl.t -> ('b, 'a) Hashtbl.t
val asHashtbl: 'a list -> ('a, bool) Hashtbl.t
val asEndStateFunc: ('a * 'b) list -> 'a -> 'b
val asTransFunc: ('a * 'b * 'c) list -> 'a -> 'b -> 'c
val flattenEndStateFunc: 'a list -> ('a -> 'b) -> 'a -> 'b
val flattenTransFunc: 'a list -> 'b list -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val getDfaNeighList : ('a, 'b, 'c option) automata -> 'a -> 'c list
