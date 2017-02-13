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

let createHashtbl() =
  Hashtbl.create 100

let hashtblKeys hashtbl =
  Hashtbl.fold (fun key _ acc -> key :: acc) hashtbl []

let hashtblVals hashtbl =
  Hashtbl.fold (fun _ hVal acc -> hVal :: acc) hashtbl []

let revHashtbl hashtbl =
  let result = createHashtbl() in
  let iterFunc mapKey mapVal =
    Hashtbl.add result mapVal mapKey
  in
    Hashtbl.iter iterFunc hashtbl;
    result

let asHashtbl elList =
  let elSet = createHashtbl() in
  let iterFunc elem = Hashtbl.add elSet elem true in
    List.iter iterFunc elList;
    elSet

let asEndStateFunc (sList : ('a * 'b) list) : ('a -> 'b) =
  let stateMap = createHashtbl() in
  let iterFunc (elem, isEnd) = Hashtbl.add stateMap elem isEnd in
    List.iter iterFunc sList;
    fun state -> Hashtbl.find stateMap state

let asTransFunc tList =
  let stateMap = createHashtbl() in
  let iterFunc (state, lett, res) = Hashtbl.add stateMap (state, lett) res in
    List.iter iterFunc tList;
    fun state lett -> Hashtbl.find stateMap (state, lett)

let flattenEndStateFunc stateList endStateFunc =
  let stateMap = createHashtbl() in
  let iterFunc state = Hashtbl.add stateMap state (endStateFunc state) in
    List.iter iterFunc stateList;
    fun state -> Hashtbl.find stateMap state

let flattenTransFunc stateList letterList transFunc =
  let stateMap = createHashtbl() in
  let lettFunc state lett = Hashtbl.add stateMap (state, lett) (transFunc state lett) in
  let stateFunc state = List.iter (lettFunc state) letterList in
    List.iter stateFunc stateList;
    fun state lett -> Hashtbl.find stateMap (state, lett)

let getDfaNeighList aut state =
  let foldFunc acc elem =
    match elem with
     | None -> acc
     | Some neigh ->
      if List.mem neigh acc then acc
      else (neigh::acc)
  in
  let mapFunc letter = aut.transFunc state letter in
  let neighList = List.map mapFunc aut.letterList in
    List.fold_left foldFunc [] neighList
