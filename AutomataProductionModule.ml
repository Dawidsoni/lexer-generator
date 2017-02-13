open List
open Hashtbl
open CommonModule

let getStateList ruleList =
  let foldFunc acc rule =
    match rule.nonTerminal with
     | None -> acc
     | Some state -> (state::acc)
  in
    List.fold_left foldFunc [] ruleList

let mapStateList stateMap stateList vCounter =
  let foldFunc acc state =
    if Hashtbl.mem stateMap state then acc
    else
      (Hashtbl.add stateMap state acc;
      acc + 1)
  in
    List.fold_left foldFunc vCounter stateList

let getNfaStateMap grammar root rootNum =
  let rec aux stateMap stateStack vCounter =
    match stateStack with
     | [] -> vCounter
     | (state::tail) ->
      let neighList = getStateList (grammar.ruleFunc state) in
      let filterFunc elem = Hashtbl.mem stateMap elem = false in
      let filterList = List.filter filterFunc neighList in
      let stack' = filterList @ tail in
      let vCounter' = mapStateList stateMap filterList vCounter in
        aux stateMap stack' vCounter'
  in
  let stateMap = createHashtbl() in
  let _ = Hashtbl.add stateMap root rootNum in
  let vCounter = aux stateMap [root] (rootNum + 1) in
    (vCounter, stateMap)

let getNfaLetterList terminalList =
  let mapFunc terminal = AutEdge terminal in
    Empty :: List.map mapFunc terminalList

let getNfaStateList stateMap vCounter =
  vCounter :: hashtblVals stateMap

let getNfaTrans ruleList letter stateMapFunc vCounter =
  let foldFunc acc rule =
    match (letter, rule.terminal, rule.nonTerminal) with
      | (Empty, None, Some state) ->
          stateMapFunc state :: acc
      | (AutEdge edge, Some terminal, Some state) ->
          if edge = terminal then stateMapFunc state :: acc else acc
      | (AutEdge edge, Some terminal, None) ->
          if edge = terminal then vCounter :: acc else acc
      | (_, _, _) -> acc
  in
    List.fold_left foldFunc [] ruleList

let getNfaTransFunc grammar stateMap vCounter =
  let revStateMap = revHashtbl stateMap in
  let transFunc state letter =
    if state = vCounter then []
    else
      let symbol = Hashtbl.find revStateMap state in
      let ruleList = grammar.ruleFunc symbol in
      let stateMapFunc elem = Hashtbl.find stateMap elem in
        getNfaTrans ruleList letter stateMapFunc vCounter
  in
  let stateList = getNfaStateList stateMap vCounter in
  let letterList = getNfaLetterList grammar.terminalList in
    flattenTransFunc stateList letterList transFunc

let getNfaEndStateFunc grammar stateMap vCounter =
  let revStateMap = revHashtbl stateMap in
  let endStateFunc state =
    if state = vCounter then true
    else
      let symbol = Hashtbl.find revStateMap state in
      let ruleList = grammar.ruleFunc symbol in
      let existsFunc rule =
        match (rule.terminal, rule.nonTerminal) with
         | (None, None) -> true
         | (_, _) -> false
      in
        List.exists existsFunc ruleList
  in
  let stateList = getNfaStateList stateMap vCounter in
    flattenEndStateFunc stateList endStateFunc

let convertToNfa grammar root rootNum =
  let (vCounter, stateMap) = getNfaStateMap grammar root rootNum in
  let letterList = getNfaLetterList grammar.terminalList in
  let stateList = getNfaStateList stateMap vCounter in
  let transFunc = getNfaTransFunc grammar stateMap vCounter in
  let endStateFunc = getNfaEndStateFunc grammar stateMap vCounter in
    {
      initState = rootNum;
      endStateFunc = endStateFunc;
      transFunc = transFunc;
      stateList = stateList;
      letterList = letterList;
    }

let getMergedStateList nfaList =
  let mapFunc nfa = nfa.stateList in
  let stateList = List.flatten (List.map mapFunc nfaList) in
    1 :: stateList

let getMergedTransFunc nfaList stateList letterList =
  let transFunc state letter =
    if state = 1 && letter = Empty then
      List.map (fun nfa -> nfa.initState) nfaList
    else if state = 1 && letter <> Empty then
      []
    else
      let findFunc nfa = List.mem state nfa.stateList in
      let nfa = List.find findFunc nfaList in
          nfa.transFunc state letter
  in
    flattenTransFunc stateList letterList transFunc

let getMergedEndStateFunc nfaList stateList =
  let endStateFunc state =
    if state = 1 then false
    else
      let findFunc nfa = List.mem state nfa.stateList in
      let nfa = List.find findFunc nfaList in
        nfa.endStateFunc state
  in
    flattenEndStateFunc stateList endStateFunc

let getMergedLetterList nfaList =
  let foldFunc acc symbol =
    if List.mem symbol acc then acc else symbol :: acc
  in
  let mapFunc nfa = nfa.letterList in
  let letterList = Empty :: List.flatten (List.map mapFunc nfaList) in
    List.fold_left foldFunc [] letterList

let createTokNfaPairList grammar =
  let foldFunc (nfaList, rootNum) symbol =
    if grammar.tokenFunc symbol = false then (nfaList, rootNum)
    else
      let aut = convertToNfa grammar symbol rootNum in
        ((symbol, aut)::nfaList, rootNum + List.length aut.stateList)
  in
    fst (List.fold_left foldFunc ([], 2) grammar.nonTerminalList)

let mergeNfaList nfaList =
  let stateList = getMergedStateList nfaList in
  let letterList = getMergedLetterList nfaList in
  let transFunc = getMergedTransFunc nfaList stateList letterList in
  let endStateFunc = getMergedEndStateFunc nfaList stateList in
    {
      initState = 1;
      endStateFunc = endStateFunc;
      transFunc = transFunc;
      stateList = stateList;
      letterList = letterList;
    }
