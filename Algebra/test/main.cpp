//////////////////////////////////////////////////////////////////////////////
// main.cpp -- test drive solver.dll
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
// Author(s): Linwood H. Taylor <lht@lzri.com>
// Modified: by joel to fit in jsolver
//			14 March 2001 - lht -- created
//////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include "decl.h"
#include "../src/extstruct.h"
#include "../src/Solver.h"
#include "../src/indysgg.h"
#include "../src/indyset.h"
#include "../src/dbg.h"
// #include "../linwood/lzmem.h"
// #include "../linwood/lzstd.h"
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
  int k;
  for (int i=0; i<argc; i++) {
    LOGn(argv[i]);
  }
  if (argc > 2) { cerr << "Usage: " << argv[0] << " [dbgmask]" << endl; 
		  exit(1) ; }
  if (argc == 2) dbglevel = (unsigned int) strtol(argv[1],0,16);
  doinitinit();
//////////////////////////////////////////////////////////////////////////////
// BvdS: what is this???
//#include "XSolver.log"
// 
  LOGn("<only thing left is to closeupshop>");
  closeupshop();
  LOGn("< closed up shop>");
  return 0;
}
