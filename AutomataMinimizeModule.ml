open List
open Hashtbl
open Pervasives
open CommonModule

let getStatePairList aut =
  let mapFunc state = List.map (fun x -> (state, x)) aut.stateList in
    List.flatten (List.map mapFunc aut.stateList)

let getStatePairSet aut =
  let stateSet = createHashtbl() in
  let addHash hash = Hashtbl.add stateSet hash true in
  let iterInFunc st1 st2 = if st1 < st2 then addHash (st1, st2) else () in
  let iterOutFunc state = List.iter (iterInFunc state) aut.stateList in
    List.iter iterOutFunc aut.stateList;
    stateSet

let isNeighInSet aut stateSet (st1, st2) lett =
  let r1 = aut.transFunc st1 lett in
  let r2 = aut.transFunc st2 lett in
    match (r1, r2) with
     | (None, None) -> false
     | (_, None) -> true
     | (None, _) -> true
     | (Some v1, Some v2) ->
      if v1 = v2 then false
      else if Hashtbl.mem stateSet (v1, v2) then false
      else if Hashtbl.mem stateSet (v2, v1) then false
      else true

let rec removeDiffStateList aut stateSet =
  let isChange = ref false in
  let iterFunc stPair =
    if List.exists (isNeighInSet aut stateSet stPair) aut.letterList then
      (isChange := true;
      Hashtbl.remove stateSet stPair)
    else ()
  in
    List.iter iterFunc (hashtblKeys stateSet);
    if !isChange then
      removeDiffStateList aut stateSet
    else ()

let getSameStateList aut =
  let stateSet = getStatePairSet aut in
  let iterFunc (st1, st2) =
    if aut.endStateFunc st1 <> aut.endStateFunc st2 then
      Hashtbl.remove stateSet (st1, st2)
    else if aut.endStateFunc st1 && aut.endStateFunc st2 then
      Hashtbl.remove stateSet (st1, st2)
    else ()
  in
    List.iter iterFunc (hashtblKeys stateSet);
    removeDiffStateList aut stateSet;
    hashtblKeys stateSet

let getMergedStateMap aut mergeFunc stPairList =
    let stateMap = createHashtbl() in
    let foldFunc state acc (st1, st2) =
      if state = st1 then mergeFunc acc st2
      else if state = st2 then mergeFunc acc st1
      else acc
    in
    let iterFunc state =
      let mergedState = List.fold_left (foldFunc state) state stPairList in
        Hashtbl.add stateMap state mergedState
    in
      List.iter iterFunc aut.stateList;
      stateMap

let getReversedStateMap stateMap =
  let revStateMap = createHashtbl() in
  let iterFunc mapKey mapVal =
    if Hashtbl.mem revStateMap mapVal then
      let oldVal = Hashtbl.find revStateMap mapVal in
        Hashtbl.replace revStateMap mapVal (mapKey::oldVal)
    else
      Hashtbl.add revStateMap mapVal [mapKey]
  in
    Hashtbl.iter iterFunc stateMap;
    revStateMap

let mergeEndStateFunc aut revStateMap =
  let endStateFunc state =
    let stList = Hashtbl.find revStateMap state in
      match stList with
       | [] -> raise (AutomataIncorrect "Incorrect vertex")
       | (x::_) -> aut.endStateFunc x
  in
    flattenEndStateFunc (hashtblKeys revStateMap) endStateFunc

let mergeTransFunc aut stateMap revStateMap =
  let transFunc state letter =
    let findFunc elem = aut.transFunc elem letter <> None in
    try
      let elem = List.find findFunc (Hashtbl.find revStateMap state) in
      let neigh = aut.transFunc elem letter in
        match neigh with
         | None -> None
         | Some neighState ->
          Some (Hashtbl.find stateMap neighState)
    with
    | _ -> None
  in
    flattenTransFunc (hashtblKeys revStateMap) aut.letterList transFunc

let mergeStateList aut mergeFunc stPairList =
  let stateMap = getMergedStateMap aut mergeFunc stPairList in
  let revStateMap = getReversedStateMap stateMap in
  let endStateFunc = mergeEndStateFunc aut revStateMap in
  let transFunc = mergeTransFunc aut stateMap revStateMap in
    {
      initState = Hashtbl.find stateMap aut.initState;
      endStateFunc = endStateFunc;
      transFunc = transFunc;
      stateList = hashtblKeys revStateMap;
      letterList = aut.letterList;
    }

let mergeDfaStateList st1 st2 =
  let foldFunc elem acc =
    match acc with
     | [] -> [elem]
     | (stHd::stTail) ->
      if stHd = elem then acc
      else elem::acc
  in
  let mergedState = List.sort Pervasives.compare (st1 @ st2) in
    List.fold_right foldFunc mergedState []

let minimizeDfa aut =
  let mergeFunc = mergeDfaStateList in
  let statePairList = getSameStateList aut in
    mergeStateList aut mergeFunc statePairList
