//////////////////////////////////////////////////////////////////////////////
// main.cpp -- test drive solver.dll
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
// Author(s): Linwood H. Taylor <lht@lzri.com>
// Modified: by joel to fit in jsolver
//			14 March 2001 - lht -- created
//////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include "../src/decl.h"
#include "../src/extstruct.h"
#include "../src/Solver.h"
RETURN_CSTRING solverDoLog(const char* const src);
#include "../src/indysgg.h"
#include "../src/indyset.h"
#include "../src/dbg.h"
using namespace std; 

#define LOGn(s) cout << s << endl
extern vector<indyset> *listofsets;

int indyCanonHowIndy(int setID, int eqnID, vector<int> * & linexpand,
		     vector<int> * & mightdepend );
int indyStudHowIndy(int setID, int eqnID, vector<int> * & linexpand,
		     vector<int> * & mightdepend );
string powersolve(const int howstrong, const string varname, 
		  const int destslot);

extern vector<valander *> *canongrads;
extern vector<valander *> studgrads;
void doinitinit();

//////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[]) {

  if (argc > 2) { cerr << "Usage: " << argv[0] << " [dbgmask]" << endl; 
		  exit(1) ; }
  if (argc == 2) dbglevel = strtoul(argv[1],0,0);
  doinitinit();
  solverDoLog("t");  // make log file
//////////////////////////////////////////////////////////////////////////////
// include the contents of a log file
// made by turning on (solve-do-log "t") in lisp
// or solverDoLog("t"); in c++
//
#include "../../Solver.log"
  closeupshop();
  return 0;
}
