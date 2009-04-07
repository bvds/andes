#
#  Make algebra routines and algebra lib
#
#  -fPIC is needed to make DLL to be used by Allegro
#  -pg  for gprof
CPPFLAGS = -Wall -fPIC -g
#            * remove these *****
CPPFLAGS += -DAW_POW_FIX -DAW_EXP -DSOLVER_EXPORTS -DUNITENABLE 
#                                                                -DNO_DLL
#
#  debug version flags
#       
CPPFLAGS += -DWITHDBG -DTRACE_OUTPUT
#
#  For OS X, do things a bit differently:
#
ifeq ($(shell uname),Darwin)
SO	= dylib
SHARED	= -dynamiclib -install_name $(shell pwd)/../../libSolver.dylib
LN_THIS_DIR = -L.
else   # standard case
SO      = so
SHARED  = -shared
LN_THIS_DIR = -Wl,-R,.
endif

src_objects = Solver.o   \
                     equaleqs.o     justsolve.o      plussort.o \
	checkeqs.o   expr.o                          polysolve.o \
	checksol.o   exprp.o                         powonev.o \
	             factorout.o                     purelin.o \
	cleanup.o    fixupforpls.o  lookslikeint.o   qsrtexpr.o \
	             flatten.o                       rationalize.o \
	coldriver.o  getaline.o                      recassign.o \
	             getall.o       moreexpr.o       slvlinonev.o \
	copyexpr.o   getallfile.o   multsort.o       solveknownvar.o \
	desperate.o  getaneqwu.o    newindy.o        solvetool.o \
	despquadb.o  getavar.o      nlsolvov.o       solvetrigb.o \
        despquad.o                  normexpr.o       solvetrig.o \
        dimchkeqf.o  		    numfactorsof.o   subexpin.o \
        dimenchk.o                  numunknowns.o    substin.o \
        dimens.o                    ordinvars.o      treechk.o \
	distfrac.o   indyset.o      ordunknowns.o    trigsimp.o \
	dofactor.o   indysgg2.o                      unitabr.o \
	donlsolv.o   indysgg3.o     parse.o          utils.o \
	dopurelin.o  indysgg.o      parseeqwunits.o  valander.o \
	dotrig.o     parseunit.o\
	eqnokay.o    ispos.o        physconsts.o \
	eqnumsimp.o  justonev.o     physvar.o 

libSolver.so libSolver: $(src_objects) Makefile
	$(CXX) $(SHARED) $(src_objects) -o ../../libSolver.$(SO) \

executable solver-program: libSolver solver-program.o
	$(CXX) $(CPPFLAGS) -o ../../solver-program solver-program.o \
	$(LN_THIS_DIR) -L../../ -lSolver

clean:
	rm *.o
#
# The following was generated from:  gcc -MM -MG *.cpp
#
equaleqs.o: equaleqs.cpp decl.h expr.h dimens.h dbg.h standard.h
justsolve.o: justsolve.cpp decl.h expr.h dimens.h \
  dbg.h standard.h extstruct.h unitabr.h justsolve.h
plussort.o: plussort.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
checkeqs.o: checkeqs.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
expr.o: expr.cpp decl.h expr.h dimens.h unitabr.h dbg.h standard.h
polysolve.o: polysolve.cpp decl.h expr.h dimens.h extoper.h extstruct.h \
  standard.h dbg.h
checksol.o: checksol.cpp decl.h expr.h dimens.h extoper.h extstruct.h \
  standard.h mconst.h dbg.h
exprp.o: exprp.cpp decl.h expr.h dimens.h unitabr.h dbg.h standard.h
powonev.o: powonev.cpp decl.h expr.h dimens.h dbg.h standard.h
factorout.o: factorout.cpp decl.h expr.h dimens.h dbg.h standard.h
purelin.o: purelin.cpp decl.h expr.h dimens.h extoper.h extstruct.h \
  standard.h dbg.h
cleanup.o: cleanup.cpp decl.h expr.h dimens.h extoper.h dbg.h standard.h
fixupforpls.o: fixupforpls.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
lookslikeint.o: lookslikeint.cpp extstruct.h standard.h expr.h dimens.h
qsrtexpr.o: qsrtexpr.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
flatten.o: flatten.cpp decl.h expr.h dimens.h extoper.h dbg.h standard.h
rationalize.o: rationalize.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
coldriver.o: coldriver.cpp extstruct.h standard.h expr.h dimens.h decl.h \
  unitabr.h indyset.h expr.h valander.h indysgg.h dbg.h justsolve.h coldriver.h
getaline.o: getaline.cpp
recassign.o: recassign.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
colget.o: colget.cpp decl.h expr.h dimens.h \
  dbg.h standard.h extstruct.h unitabr.h \
  mconst.h
getall.o: getall.cpp decl.h expr.h dimens.h extoper.h extstruct.h \
  standard.h mconst.h dbg.h unitabr.h
moreexpr.o: moreexpr.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
slvlinonev.o: slvlinonev.cpp decl.h expr.h dimens.h dbg.h standard.h \
  extoper.h extstruct.h
copyexpr.o: copyexpr.cpp expr.h dimens.h
getallfile.o: getallfile.cpp
multsort.o: multsort.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
solveknownvar.o: solveknownvar.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
desperate.o: desperate.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
getaneqwu.o: getaneqwu.cpp decl.h expr.h dimens.h extoper.h extstruct.h \
  standard.h mconst.h unitabr.h dbg.h
newindy.o: newindy.cpp decl.h expr.h dimens.h \
  dbg.h standard.h extstruct.h indyset.h valander.h \
  unitabr.h indysgg.h
solvetool.o: solvetool.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
despquadb.o: despquadb.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
getavar.o: getavar.cpp decl.h expr.h dimens.h extstruct.h standard.h \
  mconst.h dbg.h unitabr.h
nlsolvov.o: nlsolvov.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h mconst.h
solvetrigb.o: solvetrigb.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h mconst.h
despquad.o: despquad.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
normexpr.o: normexpr.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
solvetrig.o: solvetrig.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h mconst.h
dimchkeqf.o: dimchkeqf.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
geteqs.o: geteqs.cpp
numfactorsof.o: numfactorsof.cpp decl.h expr.h dimens.h dbg.h standard.h
subexpin.o: subexpin.cpp decl.h expr.h dimens.h dbg.h standard.h \
  extstruct.h
dimenchk.o: dimenchk.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
numunknowns.o: numunknowns.cpp decl.h expr.h dimens.h
substin.o: substin.cpp decl.h expr.h dimens.h extstruct.h standard.h \
  dbg.h
dimens.o: dimens.cpp dimens.h
ordinvars.o: ordinvars.cpp decl.h expr.h dimens.h dbg.h standard.h
treechk.o: treechk.cpp decl.h expr.h dimens.h extoper.h dbg.h standard.h
distfrac.o: distfrac.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h
indyset.o: indyset.cpp indyset.h expr.h dimens.h valander.h \
  decl.h expr.h extstruct.h standard.h dbg.h
ordunknowns.o: ordunknowns.cpp decl.h expr.h dimens.h dbg.h standard.h
trigsimp.o: trigsimp.cpp decl.h expr.h dimens.h extoper.h mconst.h dbg.h \
  standard.h
dofactor.o: dofactor.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
indysgg2.o: indysgg2.cpp decl.h expr.h dimens.h dbg.h standard.h \
  extstruct.h indyset.h expr.h valander.h unitabr.h indysgg.h
unitabr.o: unitabr.cpp decl.h expr.h dimens.h unitabr.h dbg.h standard.h \
  units.h prefixes.h
donlsolv.o: donlsolv.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
indysgg3.o: indysgg3.cpp decl.h expr.h dimens.h dbg.h standard.h \
  extstruct.h indyset.h expr.h valander.h unitabr.h indysgg.h \
  extoper.h
parse.o: parse.cpp
utils.o: utils.cpp
dopurelin.o: dopurelin.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h extstruct.h
indysgg.o: indysgg.cpp decl.h expr.h dimens.h dbg.h standard.h \
  extstruct.h indyset.h expr.h valander.h unitabr.h indysgg.h \
  backdoor.cpp
parseeqwunits.o: parseeqwunits.cpp decl.h expr.h dimens.h dbg.h \
  standard.h
valander.o: valander.cpp decl.h expr.h dimens.h \
  dbg.h standard.h extstruct.h valander.h \
  mconst.h
dotrig.o: dotrig.cpp decl.h expr.h dimens.h extoper.h dbg.h standard.h \
  extstruct.h
parseunit.o: parseunit.cpp decl.h expr.h dimens.h extoper.h extstruct.h \
  standard.h mconst.h unitabr.h dbg.h
eqnokay.o: eqnokay.cpp decl.h expr.h dimens.h dbg.h standard.h \
  extstruct.h unitabr.h indysgg.h extoper.h valander.h
ispos.o: ispos.cpp decl.h expr.h dimens.h dbg.h standard.h extstruct.h
physconsts.o: physconsts.cpp dimens.h expr.h dbg.h standard.h pconsts.h
eqnumsimp.o: eqnumsimp.cpp decl.h expr.h dimens.h extoper.h dbg.h \
  standard.h mconst.h
justonev.o: justonev.cpp decl.h expr.h dimens.h
physvar.o: physvar.cpp decl.h expr.h dimens.h dbg.h standard.h
Solver.o: Solver.cpp Solver.h \
  lrdcstd.h indysgg.h dbg.h standard.h coldriver.h
solver-program.o: solver-program.cpp decl.h expr.h dimens.h extstruct.h \
  standard.h Solver.h lrdcstd.h indysgg.h indyset.h valander.h dbg.h
