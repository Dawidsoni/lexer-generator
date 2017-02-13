open List
open Hashtbl
open Pervasives
open CommonModule

let findNullTrans aut fromState =
  let rec aux stateSet stateStack =
    match stateStack with
     | [] -> []
     | (state::tail) ->
      let neighList = aut.transFunc state Empty in
      let filterFunc elem = Hashtbl.mem stateSet elem = false in
      let filterList = List.filter filterFunc neighList in
      let stack' = filterList @ tail in
      let iterFunc elem = Hashtbl.add stateSet elem true in
        List.iter iterFunc filterList;
        state :: (aux stateSet stack')
  in
    let stateSet = createHashtbl() in
      Hashtbl.add stateSet fromState true;
      aux stateSet [fromState]

let hasNullAcceptPath aut state =
  let stateList = findNullTrans aut state in
  let filterList = List.filter aut.endStateFunc stateList in
    filterList <> []

let noneNullEndStateFunc aut =
  let mapFunc state = (state, hasNullAcceptPath aut state) in
  let endStateList = List.map mapFunc aut.stateList in
    asEndStateFunc endStateList

let transFromStateList transFunc stateList letter =
  let mapFunc elem = transFunc elem letter in
  let edgeList = List.flatten (List.map mapFunc stateList) in
  let foldFunc acc elem = if List.mem elem acc then acc else (elem::acc) in
    List.fold_left foldFunc [] edgeList

let addNullStateTrans aut transFunc =
  let transFunc state letter =
    let stateList = findNullTrans aut state in
      transFromStateList transFunc stateList letter
  in
    flattenTransFunc aut.stateList aut.letterList transFunc

let removeNullTrans aut transFunc =
  let transFunc state letter =
    if letter = Empty then []
    else transFunc state letter
  in
    flattenTransFunc aut.stateList aut.letterList transFunc

let noneNullTransFunc aut =
  let transFunc' = addNullStateTrans aut aut.transFunc in
    removeNullTrans aut transFunc'

let nullNfaToNfa aut =
  let endStateFunc = noneNullEndStateFunc aut in
  let transFunc = noneNullTransFunc aut in
  let letterList = List.filter (fun x -> x <> Empty) aut.letterList in
    {
      initState = aut.initState;
      endStateFunc = endStateFunc;
      transFunc = transFunc;
      stateList = aut.stateList;
      letterList = letterList;
    }

let filterDfaStateList stateSet stateList =
  let foldFunc acc elem =
    if Hashtbl.mem stateSet elem then acc
    else if elem = [] then acc
    else if List.mem elem acc then acc
    else (elem::acc)
  in
    List.fold_left foldFunc [] stateList

let nfaToDfaStateList aut =
let rec aux stateSet stateStack =
  match stateStack with
   | [] -> []
   | (state::tail) ->
    let neighFunc lett = transFromStateList aut.transFunc state lett in
    let sortNeighFunc lett = List.sort Pervasives.compare (neighFunc lett) in
    let neighList = List.map sortNeighFunc aut.letterList in
    let filterList = filterDfaStateList stateSet neighList in
    let stack' = filterList @ tail in
    let iterFunc elem = Hashtbl.add stateSet elem true in
      List.iter iterFunc filterList;
      state :: (aux stateSet stack')
in
  let stateSet = createHashtbl() in
    Hashtbl.add stateSet [aut.initState] true;
    aux stateSet [[aut.initState]]

let nfaToDfaTransFunc aut stateList =
  let transFunc state letter =
    let neigh = transFromStateList aut.transFunc state letter in
    let sortNeigh = List.sort Pervasives.compare neigh in
      match neigh with
       | [] -> None
       | _ -> Some sortNeigh
  in
    flattenTransFunc stateList aut.letterList transFunc

let nfaToDfaEndStateFunc aut stateList =
  let stateFunc state = aut.endStateFunc state in
  let mapFunc state = (state, List.exists stateFunc state) in
  let endStateList = List.map mapFunc stateList in
    asEndStateFunc endStateList

let nfaToDfaFuncTuple aut =
  let stateList = nfaToDfaStateList aut in
  let transFunc = nfaToDfaTransFunc aut stateList in
  let endStateFunc = nfaToDfaEndStateFunc aut stateList in
    (stateList, transFunc, endStateFunc)

let nfaToDfa aut =
  let (stateList, transFunc, endStateFunc) = nfaToDfaFuncTuple aut in
    {
      initState = [aut.initState];
      endStateFunc = endStateFunc;
      transFunc = transFunc;
      stateList = stateList;
      letterList = aut.letterList;
    }

let nullNfaToDfa aut =
  nfaToDfa (nullNfaToNfa aut)
