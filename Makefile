# Speed and NoDebug for Omega libraries
# BASEDIR=../omega
# Debug version of Omega libraries
BASEDIR=/usr/local

# With Haskell profiling information
#RAZVAN_DIR_HS=../omega_stub/lib.prof.hs
#HC_OPTS = -prof -auto-all -package lang -fglasgow-exts -fallow-overlapping-instances -i$(RAZVAN_DIR_HS)


# Without Haskell profiling information
RAZVAN_DIR_HS=../omega_stub/lib.hs
LOC_DIR_HS=../omega_stub/src.hs
#HC_OPTS = -fglasgow-exts -XOverlappingInstances -package containers -i$(RAZVAN_DIR_HS)
HC_OPTS = -fglasgow-exts -XOverlappingInstances -package containers -i$(LOC_DIR_HS)
HC_SOPTS = -O2 --make -static -optc-static -optl-static -optl-pthread $(HC_OPTS)

RAZVAN_DIR_C=../omega_stub/lib
RAZVAN_DIR_SRC_HS=../omega_stub/src.hs

HC=ghc
LIBS=-L$(BASEDIR)/lib
OMEGA_LIBS=-lcode_gen -lomega -lm
RAZVAN_LIBS= 	$(RAZVAN_DIR_C)/omega_stub.o $(RAZVAN_DIR_C)/Exit.o

Obj_main=Main.o

SRCS = ImpMain.hs ImpParser.hs ImpTypeChecker.hs ImpFormula.hs ImpAST.hs \
	FixCalcLexer.hs FixCalcMain.hs FixCalcParser.hs \
	ImpLexer.hs Fresh.hs MyPrelude.hs InSolver.hs ImpSugar.hs ImpTypeInfer.hs ImpTypeCommon.hs ImpFixpoint2k.hs \
	ImpConfig.hs ImpOutInfer.hs ImpHullWiden.hs ImpSTypeChecker.hs 
OBJS = ImpMain.o ImpParser.o ImpTypeChecker.o ImpFormula.o ImpAST.o \
	ImpLexer.o Fresh.o MyPrelude.o InSolver.o ImpSugar.o ImpTypeInfer.o ImpTypeCommon.o ImpFixpoint2k.o \
	ImpConfig.o ImpOutInfer.o ImpHullWiden.o ImpSTypeChecker.o 
.SUFFIXES : .o .hs .hi .lhs .hc .s

#Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c  $< $(HC_OPTS)

.hs.o:
	$(HC) -c  $< $(HC_OPTS)

.y.hs:
	happy -agci $<
  
all: imp fixcalc

imp : $(OBJS) ImpParser.y ImpMain.hs
	rm -f $@
	ghc -o $@ $(HC_OPTS) -lstdc++ ImpMain.hs $(LIBS) $(OMEGA_LIBS) $(RAZVAN_LIBS)

#(OBJS)

clean:
	rm -f *.hi *.o *~ imp ImpParser.hs *.info a.omega a.all.c a.c a.out a.impi a.impt a.omega-err a.pre oc.out fixcalc FixCalcParser.hs log

install: fixcalc
	scp fixcalc popeeaco@loris-7.ddns:/home/popeeaco/bin/.
	cp fixcalc ~popeeaco/public_html/cgi-bin/fixcalc/.

doc: $(SRCS)
	haddock -h -o doc --read-interface=http://www.haskell.org/ghc/docs/6.4.2/html/libraries/base/,/home/popeeaco/personal/research/base.haddock \
	ImpMain.hs ImpTypeChecker.hs ImpFormula.hs ImpAST.hs \
	ImpLexer.hs Fresh.hs MyPrelude.hs InSolver.hs ImpSugar.hs ImpTypeCommon.hs ImpFixpoint2k.hs \
	ImpConfig.hs ImpOutInfer.hs ImpHullWiden.hs ImpSTypeChecker.hs ImpTypeInfer.hs Pig.hs

#####FixCalc
FixCalcOBJS = FixCalcLexer.o FixCalcParser.o ImpAST.o MyPrelude.o Fresh.o ImpConfig.o ImpFixpoint2k.o ImpFormula.o InSolver.o ImpHullWiden.o

fixcalc: $(FixCalcOBJS) FixCalcParser.y FixCalcMain.hs
	rm -f fixcalc
	ghc -o fixcalc $(HC_OPTS) -lstdc++ FixCalcMain.hs $(LIBS) $(OMEGA_LIBS)  $(RAZVAN_LIBS)
#####

static: $(FixCalcOBJS) FixCalcParser.y FixCalcMain.hs
	rm -f fixcalc
	ghc -o fixcalc $(HC_SOPTS) -lstdc++ FixCalcMain.hs $(LIBS) $(OMEGA_LIBS)  $(RAZVAN_LIBS)

depend: ImpParser.hs FixCalcParser.hs
	ghc -M $(HC_OPTS) -i$(RAZVAN_DIR_SRC_HS) $(SRCS)

bsearch:
	gcc -o -O bsearch Primitives.c BSearch.c a.c

initarr:
	gcc -o initarr Primitives.c a.c

mvm:
	gcc -o mvm Primitives.c Mvm.c a.c

swaparr:
	gcc -o swaparr Primitives.c a.c


# DO NOT DELETE: Beginning of Haskell dependencies
../omega_stub/src.hs/Omega_util.o : ../omega_stub/src.hs/Omega_util.hs
../omega_stub/src.hs/Omega_tokens.o : ../omega_stub/src.hs/Omega_tokens.hs
../omega_stub/src.hs/Omega_lexer.o : ../omega_stub/src.hs/Omega_lexer.hs
../omega_stub/src.hs/Omega_lexer.o : ../omega_stub/src.hs/Omega_tokens.hi
../omega_stub/src.hs/Omega_stub.o : ../omega_stub/src.hs/Omega_stub.hs
../omega_stub/src.hs/Omega_types.o : ../omega_stub/src.hs/Omega_types.hs
../omega_stub/src.hs/Omega_types.o : ../omega_stub/src.hs/Omega_stub.hi
../omega_stub/src.hs/Omega_types.o : ../omega_stub/src.hs/Omega_util.hi
../omega_stub/src.hs/Omega_parser.o : ../omega_stub/src.hs/Omega_parser.hs
../omega_stub/src.hs/Omega_parser.o : ../omega_stub/src.hs/Omega_lexer.hi
../omega_stub/src.hs/Omega_parser.o : ../omega_stub/src.hs/Omega_tokens.hi
../omega_stub/src.hs/Omega_parser.o : ../omega_stub/src.hs/Omega_types.hi
../omega_stub/src.hs/Omega.o : ../omega_stub/src.hs/Omega.hs
../omega_stub/src.hs/Omega.o : ../omega_stub/src.hs/Omega_parser.hi
../omega_stub/src.hs/Omega.o : ../omega_stub/src.hs/Omega_stub.hi
../omega_stub/src.hs/Omega.o : ../omega_stub/src.hs/Omega_types.hi
../omega_stub/src.hs/Omega.o : ../omega_stub/src.hs/Omega_util.hi
../omega_stub/src.hs/PFOmega.o : ../omega_stub/src.hs/PFOmega.hs
../omega_stub/src.hs/PFOmega.o : ../omega_stub/src.hs/Omega_stub.hi
../omega_stub/src.hs/PFOmega.o : ../omega_stub/src.hs/Omega_parser.hi
../omega_stub/src.hs/PFOmega.o : ../omega_stub/src.hs/Omega_types.hi
../omega_stub/src.hs/PFOmega.o : ../omega_stub/src.hs/Omega.hi
ImpConfig.o : ImpConfig.hs
MyPrelude.o : MyPrelude.hs
Fresh.o : Fresh.hs
Fresh.o : MyPrelude.hi
Fresh.o : ImpConfig.hi
ImpLexer.o : ImpLexer.hs
FixCalcLexer.o : FixCalcLexer.hs
ImpAST.o : ImpAST.hs
ImpAST.o : MyPrelude.hi
ImpAST.o : ImpConfig.hi
ImpAST.o : Fresh.hi
InSolver.o : InSolver.hs
InSolver.o : MyPrelude.hi
InSolver.o : Fresh.hi
InSolver.o : ImpAST.hi
InSolver.o : ../omega_stub/src.hs/Omega_stub.hi
InSolver.o : ../omega_stub/src.hs/Omega_parser.hi
InSolver.o : ../omega_stub/src.hs/Omega_types.hi
InSolver.o : ../omega_stub/src.hs/PFOmega.hi
InSolver.o : ../omega_stub/src.hs/Omega.hi
ImpFormula.o : ImpFormula.hs
ImpFormula.o : MyPrelude.hi
ImpFormula.o : InSolver.hi
ImpFormula.o : ImpConfig.hi
ImpFormula.o : ImpAST.hi
ImpFormula.o : Fresh.hi
ImpTypeCommon.o : ImpTypeCommon.hs
ImpTypeCommon.o : MyPrelude.hi
ImpTypeCommon.o : ImpFormula.hi
ImpTypeCommon.o : Fresh.hi
ImpTypeCommon.o : ImpConfig.hi
ImpTypeCommon.o : ImpAST.hi
ImpSugar.o : ImpSugar.hs
ImpSugar.o : MyPrelude.hi
ImpSugar.o : ImpTypeCommon.hi
ImpSugar.o : Fresh.hi
ImpSugar.o : ImpAST.hi
ImpHullWiden.o : ImpHullWiden.hs
ImpHullWiden.o : MyPrelude.hi
ImpHullWiden.o : ImpFormula.hi
ImpHullWiden.o : ImpConfig.hi
ImpHullWiden.o : ImpAST.hi
ImpHullWiden.o : Fresh.hi
ImpFixpoint2k.o : ImpFixpoint2k.hs
ImpFixpoint2k.o : MyPrelude.hi
ImpFixpoint2k.o : ImpHullWiden.hi
ImpFixpoint2k.o : ImpFormula.hi
ImpFixpoint2k.o : ImpConfig.hi
ImpFixpoint2k.o : ImpAST.hi
ImpFixpoint2k.o : Fresh.hi
FixCalcParser.o : FixCalcParser.hs
FixCalcParser.o : MyPrelude.hi
FixCalcParser.o : FixCalcLexer.hi
FixCalcParser.o : Fresh.hi
FixCalcParser.o : ImpFormula.hi
FixCalcParser.o : ImpFixpoint2k.hi
FixCalcParser.o : ImpConfig.hi
FixCalcParser.o : ImpAST.hi
ImpOutInfer.o : ImpOutInfer.hs
ImpOutInfer.o : MyPrelude.hi
ImpOutInfer.o : ImpTypeCommon.hi
ImpOutInfer.o : ImpFixpoint2k.hi
ImpOutInfer.o : ImpFormula.hi
ImpOutInfer.o : ImpConfig.hi
ImpOutInfer.o : ImpAST.hi
ImpOutInfer.o : Fresh.hi
ImpTypeInfer.o : ImpTypeInfer.hs
ImpTypeInfer.o : MyPrelude.hi
ImpTypeInfer.o : ImpTypeCommon.hi
ImpTypeInfer.o : ImpOutInfer.hi
ImpTypeInfer.o : ImpFixpoint2k.hi
ImpTypeInfer.o : ImpFormula.hi
ImpTypeInfer.o : ImpConfig.hi
ImpTypeInfer.o : ImpAST.hi
ImpTypeInfer.o : Fresh.hi
ImpSTypeChecker.o : ImpSTypeChecker.hs
ImpSTypeChecker.o : MyPrelude.hi
ImpSTypeChecker.o : ImpTypeCommon.hi
ImpSTypeChecker.o : ImpFormula.hi
ImpSTypeChecker.o : Fresh.hi
ImpSTypeChecker.o : ImpConfig.hi
ImpSTypeChecker.o : ImpAST.hi
ImpTypeChecker.o : ImpTypeChecker.hs
ImpTypeChecker.o : MyPrelude.hi
ImpTypeChecker.o : ImpTypeCommon.hi
ImpTypeChecker.o : ImpFormula.hi
ImpTypeChecker.o : ImpConfig.hi
ImpTypeChecker.o : ImpAST.hi
ImpTypeChecker.o : Fresh.hi
FixCalcMain.o : FixCalcMain.hs
FixCalcMain.o : MyPrelude.hi
FixCalcMain.o : ImpConfig.hi
FixCalcMain.o : FixCalcParser.hi
ImpParser.o : ImpParser.hs
ImpParser.o : MyPrelude.hi
ImpParser.o : ImpLexer.hi
ImpParser.o : ImpAST.hi
ImpMain.o : ImpMain.hs
ImpMain.o : MyPrelude.hi
ImpMain.o : ImpTypeInfer.hi
ImpMain.o : ImpTypeChecker.hi
ImpMain.o : ImpSTypeChecker.hi
ImpMain.o : ImpSugar.hi
ImpMain.o : ImpParser.hi
ImpMain.o : ImpConfig.hi
ImpMain.o : ImpAST.hi
ImpMain.o : Fresh.hi
# DO NOT DELETE: End of Haskell dependencies
