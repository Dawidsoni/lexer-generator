open CommonModule

let aut1 =
  {
    initState = 'a';
    endStateFunc = (fun state -> List.mem state ['e'; 'g']);
    transFunc = (fun state letter ->
      match (state, letter) with
        | ('a', AutEdge 0) -> ['e'; 'd']
        | ('a', Empty) -> ['c'; 'b']
        | ('b', Empty) -> ['d']
        | ('d', AutEdge 0) -> ['c']
        | ('d', AutEdge 1) -> ['f']
        | ('f', Empty) -> ['g']
        | ('g', AutEdge 1) -> ['c']
        | _ -> []);
    stateList = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'];
    letterList = [AutEdge 0; AutEdge 1; Empty];
  }


let aut2 =
    {
      initState = 'a';
      endStateFunc = (fun state -> List.mem state ['e']);
      transFunc = (fun state letter ->
        match (state, letter) with
          | ('a', AutEdge 0) -> ['a'; 'b'; 'c'; 'd'; 'e']
          | ('a', AutEdge 1) -> ['d'; 'e']
          | ('b', AutEdge 0) -> ['c']
          | ('b', AutEdge 1) -> ['e']
          | ('c', AutEdge 0) -> []
          | ('c', AutEdge 1) -> ['b']
          | ('d', AutEdge 0) -> ['e']
          | ('d', AutEdge 1) -> []
          | ('e', AutEdge 0) -> []
          | ('e', AutEdge 1) -> []
          | _ -> []);
      stateList = ['a'; 'b'; 'c'; 'd'; 'e'];
      letterList = [AutEdge 0; AutEdge 1];
    }


let aut3 =
    {
      initState = 0;
      endStateFunc = (fun state -> List.mem state [1]);
      transFunc = (fun state letter ->
        match (state, letter) with
          | (0, 'a') -> [1; 2; 3]
          | (0, 'b') -> [2; 3]
          | (1, 'a') -> [1; 2]
          | (1, 'b') -> [2; 3]
          | (2, 'a') -> []
          | (2, 'b') -> [2; 3; 4]
          | (3, 'a') -> [4]
          | (3, 'b') -> [2; 3; 4]
          | (4, 'a') -> []
          | (4, 'b') -> []
          | _ -> []);
      stateList = [0; 1; 2; 3; 4];
      letterList = ['a'; 'b'];
    }


let aut4 =
    {
      initState = ['a'];
      endStateFunc = (fun state -> List.mem state [['c']; ['d']; ['e']]);
      transFunc = (fun state letter ->
        match (state, letter) with
          | (['a'], 0) -> Some ['b']
          | (['a'], 1) -> Some ['c']
          | (['b'], 0) -> Some ['a']
          | (['b'], 1) -> Some ['d']
          | (['c'], 0) -> Some ['e']
          | (['c'], 1) -> Some ['f']
          | (['d'], 0) -> Some ['e']
          | (['d'], 1) -> Some ['f']
          | (['e'], 0) -> Some ['e']
          | (['e'], 1) -> Some ['f']
          | (['f'], 0) -> Some ['f']
          | (['f'], 1) -> Some ['f']
          | _ -> raise Not_found);
      stateList = [['a']; ['b']; ['c']; ['d']; ['e']; ['f']];
      letterList = [0; 1];
    }
