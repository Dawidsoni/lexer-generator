open Pervasives
open List
open GrammarProductionModule
open AutomataProductionModule
open AutomataConvertModule
open AutomataMinimizeModule
open AutomataTokenMatchModule
open LexerGeneratorModule

let saveLexerInFile outputFile minDfa tokenMatchFunc =
  try
    generateLexer minDfa tokenMatchFunc outputFile;
    Printf.printf "Lexer generated in file \'%s\'\n" outputFile
  with exc ->
    Printf.printf "Error while generating lexer\n"

let lexerFromAutomata outputFile tokNfaList minDfa =
  try
    let tokenMatchFunc = createTokenMatchFunc tokNfaList minDfa in
      saveLexerInFile outputFile minDfa tokenMatchFunc
  with exc ->
    Printf.printf "Error: grammar is ambigious\n"

let lexerFromGrammar outputFile grammar =
  try
    let tokNullNfaList = createTokNfaPairList grammar in
    let mapNfaFunc (tok, nfa) = (tok, nullNfaToNfa nfa) in
    let tokNfaList = List.map mapNfaFunc tokNullNfaList in
    let nfaList = snd (List.split tokNfaList) in
    let mergedNfa = mergeNfaList nfaList in
    let mergedDfa = nullNfaToDfa mergedNfa in
    let minDfa = minimizeDfa mergedDfa in
      lexerFromAutomata outputFile tokNfaList minDfa
  with exc ->
    Printf.printf "Error while creating automata\n"

let lexerFromFile inputFile outputFile =
  try
    let grammar = grammarFromFile inputFile in
      lexerFromGrammar outputFile grammar
  with exc ->
    Printf.printf "Error while parsing grammar from file \'%s\'\n" inputFile

let () =
  try
    let inputFile = Sys.argv.(1) in
    let outputFile = Sys.argv.(2) in
      lexerFromFile inputFile outputFile
  with exc ->
    Printf.printf "Error: too few arguments\n"
