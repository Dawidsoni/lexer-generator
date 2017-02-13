open Pervasives
open List
open Hashtbl
open String
open CommonModule

let getStateMap aut =
  let rec aux stateMap stateStack stateNum =
    match stateStack with
     | [] -> ()
     | (state::tail) ->
      let neighList = getDfaNeighList aut state in
      let filterFunc elem = (Hashtbl.mem stateMap elem = false) in
      let filterList = List.filter filterFunc neighList in
      let stateStack' = filterList @ tail in
      let foldFunc acc elem = Hashtbl.add stateMap elem acc; (acc + 1) in
      let stateNum' = List.fold_left foldFunc stateNum filterList in
        aux stateMap stateStack' stateNum'
    in
      let stateMap = createHashtbl() in
      Hashtbl.add stateMap aut.initState 0;
      aux stateMap [aut.initState] 1;
      stateMap

let copyFileContent fileInput outChann =
  let inChann = open_in fileInput in
    let rec aux () =
      try
    		let line = input_line inChann in
          output_string outChann (line ^ "\n");
          aux()
    	with exc ->
        output_string outChann "\n";
    		close_in inChann
    in
      aux()

let saveConstVals aut outChann =
  output_string outChann "const int STATE_COUNT = ";
  output_string outChann (string_of_int (List.length aut.stateList));
  output_string outChann ";\nconst int LETTER_COUNT = ";
  output_string outChann (string_of_int (List.length aut.letterList));
  output_string outChann ";\n\n"

let getFormattedTok token =
  String.uppercase_ascii token

let saveTokenListDef aut tokenMatchFunc outChann =
  let tokenSet = createHashtbl() in
  let foldFunc symbol acc =
    match tokenMatchFunc symbol with
     | None -> acc
     | Some token ->
        if Hashtbl.mem tokenSet token then acc
        else
          (Hashtbl.add tokenSet token true;
          let formattedTok = getFormattedTok token in
            ("\t" ^ formattedTok) :: acc)
  in
    let tokenList = List.fold_right foldFunc aut.stateList [] in
    let tokenString = String.concat ",\n" tokenList in
      output_string outChann "enum TokenType {\n";
      output_string outChann tokenString;
      output_string outChann "\n};\n\n"

let getEndStateMap stateMap tokenMatchFunc =
  let stateList = hashtblKeys stateMap in
  let endStateMap = createHashtbl() in
  let iterFunc state =
    let statePos = Hashtbl.find stateMap state in
      match tokenMatchFunc state with
       | None ->
          Hashtbl.add endStateMap statePos (string_of_int (-1))
       | Some token ->
        let formattedTok = getFormattedTok token in
          Hashtbl.add endStateMap statePos formattedTok
  in
    List.iter iterFunc stateList;
    endStateMap

let saveEndStateList stateMap tokenMatchFunc outChann =
  let endStateMap = getEndStateMap stateMap tokenMatchFunc in
  let statePosList = List.sort Pervasives.compare (hashtblKeys endStateMap) in
  let foldFunc num acc =
    let elVal = Hashtbl.find endStateMap num in
      (elVal :: acc)
  in
  let endStateList = List.fold_right foldFunc statePosList [] in
  let endStateListString = String.concat ", " endStateList in
    output_string outChann "int endStateList[STATE_COUNT] = {";
    output_string outChann endStateListString;
    output_string outChann "};\n\n"

let getLetterMap aut =
  let letterMap = createHashtbl() in
  let foldFunc acc elem =
    match elem with
     | Empty -> raise (AutomataIncorrect "Incorrect edge")
     | AutEdge letter -> Hashtbl.add letterMap letter acc;
    (acc + 1)
  in
    let _ = List.fold_left foldFunc 0 aut.letterList in
      letterMap

let saveLetterList letterMap outChann =
  let rec aux acc =
    if acc >= 256 then []
    else if Hashtbl.mem letterMap (char_of_int acc) then
      let symbol = string_of_int (Hashtbl.find letterMap (char_of_int acc)) in
        symbol :: (aux (acc + 1))
    else
      let symbol = string_of_int (-1) in
        symbol :: (aux (acc + 1))
  in
  let letterListString = String.concat ", " (aux 0) in
    output_string outChann "int letterList[256] = {";
    output_string outChann letterListString;
    output_string outChann "};\n\n"

let getTransMap aut letterMap stateMap =
  let transMap = createHashtbl() in
  let letterIterFunc state letter =
    match letter with
     | Empty -> raise (AutomataIncorrect "Incorrect edge")
     | AutEdge edge ->
      let statePos = Hashtbl.find stateMap state in
      let letterPos = Hashtbl.find letterMap edge in
        match aut.transFunc state letter with
          | None -> Hashtbl.add transMap (statePos, letterPos) (-1)
          | Some neigh ->
            let neighPos = Hashtbl.find stateMap neigh in
              Hashtbl.add transMap (statePos, letterPos) neighPos
  in
  let stateIterFunc state = List.iter (letterIterFunc state) aut.letterList in
    List.iter stateIterFunc aut.stateList;
    transMap

let getTransString aut letterMap stateMap =
  let transMap = getTransMap aut letterMap stateMap in
  let stateCount = List.length aut.stateList in
  let letterCount = List.length aut.letterList in
  let rec letterFunc state letter =
    if letter >= letterCount then []
    else
      let trans = string_of_int (Hashtbl.find transMap (state, letter)) in
        trans :: (letterFunc state (letter + 1))
  in
  let rec stateFunc state =
    if state >= stateCount then []
    else
      let stateLine = letterFunc state 0 in
      let stateString = ("\t{" ^ (String.concat ", " stateLine) ^ "}") in
        stateString :: (stateFunc (state + 1))
  in
    String.concat ",\n" (stateFunc 0)

let saveTransList aut letterMap stateMap outChann =
  let transString = getTransString aut letterMap stateMap in
    output_string outChann "int transList[STATE_COUNT][LETTER_COUNT] = {\n";
    output_string outChann transString;
    output_string outChann "\n};\n\n"

let saveAutDef aut tokenMatchFunc outChann =
  let stateMap = getStateMap aut in
  let letterMap = getLetterMap aut in
    saveConstVals aut outChann;
    saveTokenListDef aut tokenMatchFunc outChann;
    saveEndStateList stateMap tokenMatchFunc outChann;
    saveLetterList letterMap outChann;
    saveTransList aut letterMap stateMap outChann

let generateLexer aut tokenMatchFunc fileOutput =
  let outChann = open_out fileOutput in
    copyFileContent "data/includeList.dat" outChann;
    saveAutDef aut tokenMatchFunc outChann;
    copyFileContent "data/tokenFunc.dat" outChann;
    close_out outChann
