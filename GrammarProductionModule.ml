open List
open Hashtbl
open Pervasives
open Str
open String
open CommonModule


let fileAsStringList fileName =
  let chann = open_in fileName in
  let rec aux () =
    try
  		let line = input_line chann in
  			line :: aux ()
  	with exc ->
  		close_in chann;
  		[]
  in
    aux()

let isTokensLine line =
  sub line 0 6 = "tokens"

let getTokenList stringList =
  let tokenLine = List.find isTokensLine stringList in
  let tokenStr = List.nth (String.split_on_char ':' tokenLine) 1 in
  let tokenList = String.split_on_char ',' tokenStr in
    List.map (fun x -> String.trim x) tokenList

let getRuleList str =
  let ruleList = String.split_on_char '|' str in
  let mapFunc str =
    if String.trim str = "." then (None, None) else
    let rule = String.split_on_char '\'' str in
    let ruleLen = List.length rule in
    if ruleLen = 1 then (None, Some (String.trim (List.hd rule))) else
    let terminal = String.get (List.nth rule 1) 0 in
    let nonTerminal = String.trim (List.nth rule 2) in
    if nonTerminal = "" then (Some terminal, None)
    else (Some terminal, Some nonTerminal)
  in
    List.map mapFunc ruleList

let getSymbolRuleList stringList =
  let foldFunc acc str =
    if String.trim str = "" then acc
    else if isTokensLine str then acc
    else
      let splitStr = Str.split (regexp "=>") str in
      let symbol = String.trim (List.hd splitStr) in
      let ruleList = getRuleList (List.nth splitStr 1) in
         (symbol, ruleList) :: acc
  in
    List.fold_left foldFunc [] stringList

let asRightGrammarRule (terminal, nonTerminal) =
  {
    terminal = terminal;
    nonTerminal = nonTerminal;
  }

let getSymbolRuleMap symbolRuleList =
  let symbolRuleMap = createHashtbl() in
  let iterFunc (elem, elemVal) =
    let ruleList = List.map asRightGrammarRule elemVal in
    if Hashtbl.mem symbolRuleMap elem then
      let oldVal = Hashtbl.find symbolRuleMap elem in
        Hashtbl.replace symbolRuleMap elem (ruleList @ oldVal)
    else
      Hashtbl.add symbolRuleMap elem ruleList
  in
    List.iter iterFunc symbolRuleList;
    symbolRuleMap

let getTokenFunc tokenList =
    let tokenSet = asHashtbl tokenList in
      Hashtbl.mem tokenSet

let getTerminalList symbolRuleList =
  let letterSet = createHashtbl() in
  let ruleIterFunc (terminal, _) =
    match terminal with
     | None -> ()
     | Some letter ->
      if Hashtbl.mem letterSet letter then ()
      else Hashtbl.add letterSet letter true
  in
  let symbolIterFunc (_, ruleList) =
    List.iter ruleIterFunc ruleList
  in
    List.iter symbolIterFunc symbolRuleList;
    hashtblKeys letterSet

let createGrammar stringList =
  let tokenList = getTokenList stringList in
  let symbolRuleList = getSymbolRuleList stringList in
  let symbolRuleMap = getSymbolRuleMap symbolRuleList in
  let ruleFunc = Hashtbl.find symbolRuleMap in
  let nonTerminalList = hashtblKeys symbolRuleMap in
  let tokenFunc = getTokenFunc tokenList in
  let terminalList = getTerminalList symbolRuleList in
    {
      ruleFunc = ruleFunc;
      terminalList = terminalList;
      nonTerminalList = nonTerminalList;
      tokenFunc = tokenFunc;
    }

let grammarFromFile fileName =
  let stringList = fileAsStringList fileName in
    createGrammar stringList
