// main.cpp -- test drive solver.dll
// Copyright (C) 2001 by ???????????????? -- All Rights Reserved.
// Author(s): Linwood H. Taylor <lht@lzri.com>
// Modified:
//   14 March 2001 - lht -- created
//////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <string>
#define IAmMain
#include "extstruct.h"
using namespace std;

// #include "your .h file with the prototypes here"
void indyEmpty();
void indyAddVar(const char* const name, double value);
void indyDoneAddVar();
void indyAddEquation(int eqnID, const char* const equation);
bool isIndependent(int setID, int eqnID);
void indyAddEq2Set(int setID, int eqnID);
void indyKeepN(int setI, int numberToKeep);

#define Logn(s) std::cout << s << std::endl;
#define Logb(s) std::cout << ((s) ? "true" : "false" ) << std::endl;
//////////////////////////////////////////////////////////////////////////////

int main(int argc, char* argv[]) {
   for (int i=0; i<argc; i++) {
  Logn(argv[i]);
  }
  try{
    indyEmpty();
    indyAddVar("x", static_cast<double>(20));
    indyAddVar("y", static_cast<double>(30));
    indyDoneAddVar();
    
    indyAddEquation(0, "(= 190 (+ (* 2 x) (* 5 y)))");
    indyAddEquation(1, "(= 0 (- (* 3 x) (* 2 y)))");
    indyAddEquation(2, "(= 1 (/ (* 3 x) (* 2 y)))");
    
    indyAddEq2Set(0, 0);
    
    Logb(isIndependent(0, 1));
    Logb(isIndependent(0, 2));

    indyAddEq2Set(0, 1);
    Logb(isIndependent(0, 2));
    indyEmpty();
  }
  catch(string message)
    { std::cout << message << std::endl; exit(1); }
  return(0);
}

//////////////////////////////////////////////////////////////////////////////
// end of main.cpp
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
//////////////////////////////////////////////////////////////////////////////



