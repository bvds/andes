#
#  Make Solver routines and solver lib
#
CPPFLAGS= -Wall -DNO_DLL
# path to look for files
VPATH=../src
 
solve_objects = colander.o dllsupport.o  Solver.o indysgg3.o

solver: $(solve_objects) main.o
	$(CXX) $(CFLAGS) -o solver $(solve_objects) main.o

#
# The following was generated from:  gcc -MM -MG *.cpp
#

colander.o: colander.cpp lrdcstd.h ../src/extstruct.h ../src/standard.h \
  ../src/expr.h ../src/dimens.h ../src/unitabr.h
dllsupport.o: dllsupport.cpp lrdcstd.h ../src/extstruct.h \
  ../src/standard.h ../src/expr.h ../src/dimens.h ../src/unitabr.h
main.o: main.cpp ../src/decl.h ../src/extstruct.h Solver.h lrdcstd.h ..\
  ../src/indysgg.h ../src/indyset.h ../src/dbg.h
Solver.o: Solver.cpp Solver.h lrdcstd.h ../src/indysgg.h ../src/dbg.h \
  ../src/standard.h
