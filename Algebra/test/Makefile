#
#  Make Solver test program
#
CPPFLAGS = -Wall -I../src -DWITHDBG -g
#
#  For OS X, do things a bit differently:
#
ifeq ($(shell uname),Darwin)
SO	= dylib
else  # standard case
SO      = so
endif

#solve_lib = -L../../ -lSolver
solve_lib = ../../libSolver.$(SO)

solver: main.o $(solve_lib)
	$(CXX) $(CPPFLAGS) -o solver $(solve_lib) main.o

str:   $(solve_lib) str.o
	$(CXX) $(CPPFLAGS) -o str $(solve_lib) str.o

main.o: ../../Solver.log main.cpp
	$(CXX) $(CPPFLAGS) -c main.cpp

clean:
	rm *.o
