open Hashtbl
open CommonModule

let updateTokStateMap tokStateMap tok nfa minDfa =
  let dfaIterFunc nfaState dfaState =
    if List.mem nfaState dfaState = false then ()
    else if Hashtbl.mem tokStateMap dfaState then
      if Hashtbl.find tokStateMap dfaState = tok then ()
      else raise (GrammarIncorrect "Grammar is ambigious")
    else Hashtbl.add tokStateMap dfaState tok
  in
  let nfaIterFunc state =
    List.iter (dfaIterFunc state) minDfa.stateList
  in
  let endStateList = List.filter nfa.endStateFunc nfa.stateList in
    List.iter nfaIterFunc endStateList

let asTokenMatchFunc tokStateMap minDfa  =
  fun state ->
    if Hashtbl.mem tokStateMap state then
      Some (Hashtbl.find tokStateMap state)
    else
      None

let createTokenMatchFunc tokNfaList minDfa =
  let tokStateMap = createHashtbl() in
  let iterFunc (tok, nfa) = updateTokStateMap tokStateMap tok nfa minDfa in
    List.iter iterFunc tokNfaList;
    asTokenMatchFunc tokStateMap minDfa
