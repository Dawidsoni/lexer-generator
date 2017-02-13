COMPILE_LIST=(
  CommonModule.mli \
  CommonModule.ml \
  GrammarProductionModule.mli \
  GrammarProductionModule.ml \
  AutomataProductionModule.mli \
  AutomataProductionModule.ml \
  AutomataConvertModule.mli \
  AutomataConvertModule.ml \
  AutomataMinimizeModule.mli \
  AutomataMinimizeModule.ml \
  AutomataTokenMatchModule.mli \
  AutomataTokenMatchModule.ml \
  LexerGeneratorModule.mli \
  LexerGeneratorModule.ml \
  TestModule.ml \
  main.ml \
)

ocamlopt -o lexerGenerator str.cmxa ${COMPILE_LIST[*]}

rm *.cmi
rm *.cmx
rm *.o
