#
#  Make Solver test program
#
CPPFLAGS= -Wall -I../src
 
#solve_lib = -L../../ -lSolver
solve_lib = ../../libSolver.so

solver: ../../libSolver.so main.o
	$(CXX) $(CFLAGS) -o solver $(solve_lib) main.o

str:   ../../libSolver.so str.o
	$(CXX) $(CFLAGS) -o str $(solve_lib) str.o
