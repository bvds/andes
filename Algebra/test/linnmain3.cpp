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
#include "indyset.h"
#include "valander.h"
#include "dbg.h"
using namespace std;

// #include "your .h file with the prototypes here"
void indyEmpty();
void indyAddVar(const char* const name, double value,
		const char* const unitstr);
void indyDoneAddVar();
void indyAddCanonEq(int eqnID, const char* const equation);
bool indyIsCanonIndy(int setID, int eqnID);
bool indyIsStudIndy(int setID, int eqnID);
int  indyAddStudEq(int slot, const char* const equation);
void indyAddEq2CanSet(int setID, int eqnID);
void indyKeepN(int setI, int numberToKeep);
string indyExpSetCanEq(int setID, int eqnID);
string indyExpSetStudEq(int setID, int eqnID);
bool getall(const string oneline);

#define Logm(s) cout << "Logm: "; std::cout << s << std::endl;
#define Logb(s) cout << "Logb: "; std::cout << ((s) ? "true" : "false" ) << std::endl;
//////////////////////////////////////////////////////////////////////////////

extern bool gotthevars, setupdone;
extern int numindysets, numvars;
extern vector<indyset> *listofsets;
extern vector<valander *> *canongrads;
int numparams;

int main(int argc, char* argv[]) {
  int k;
  numparams = 0;
  cout << "Entering linnmain3" << endl;
  for (k=0; k<argc; k++) {
    Logm(argv[k]);
  }
  if (argc == 2) dbglevel = strtol(argv[1],0,16); 
  cout<< "dbglevel " << hex << dbglevel << endl;
  try{
    cout << "ready to try stuff" << endl;
    indyEmpty();
    cout << "exit from indyEmpty, now declare vars" << endl;
    
    indyAddVar("dist_TORTOISE_12", 1.0, "m");
    indyAddVar("sp_TORTOISE_12", 0.01, "m/s");
    indyAddVar("dist_HARE_12", 50.0, "m");
    indyAddVar("sp_HARE_12", 0.5, "m/s");
    indyAddVar("t_12", 100., "s");
    getall("(POSITIVE t_12)");
    cout << "done, now call indyDoneAddVar" << endl;
    indyDoneAddVar();
    cout << "done, now define equations" << endl;
    
    indyAddCanonEq(0, "(= dist_TORTOISE_12 (DNUM  1 m))");
    indyAddCanonEq(1, "(= sp_TORTOISE_12 (DNUM  0.01 m/s))");
    indyAddCanonEq(2, "(= dist_HARE_12 (DNUM 50 m))");
    indyAddCanonEq(3, "(= sp_HARE_12 (/ dist_HARE_12 t_12))");
    indyAddCanonEq(4, "(= sp_TORTOISE_12 (/ dist_TORTOISE_12 t_12))");
    
    cout << "set 0 for tortoise info only" << endl;
    indyAddEq2CanSet(0, 0);
    indyAddEq2CanSet(0, 1);
    indyAddEq2CanSet(0, 4);
    cout << "set 1 for hare info only" << endl;
    indyAddEq2CanSet(1, 2);
    indyAddEq2CanSet(1, 3);
    cout << "set 2 complete info" << endl;
    indyAddEq2CanSet(2, 0);
    indyAddEq2CanSet(2, 1);
    indyAddEq2CanSet(2, 4);
    indyAddEq2CanSet(2, 2);
    indyAddEq2CanSet(2, 3);
    cout << "done making indy sets" << endl;
    cout << "get a student equation" << endl;
    cout << "First student tries time without units" << endl;
    cout << indyAddStudEq(0,"(= t_12 100)") << endl;
    cout << "Next student tries time wrong value" << endl;
    cout << indyAddStudEq(0,"(= t_12 (DNUM 50 s))") << endl;
    cout << "Finally student tries time correct value" << endl;
    cout << indyAddStudEq(0,"(= t_12 (DNUM 100 s))") << endl;
    cout << "Now check its independence" << endl;
    for (k=0; k<3; k++) {
      if (indyIsStudIndy(k, 0))
	cout << "indpendent of set " << k << endl;
      else  cout << indyExpSetStudEq(k, 0) << endl;
    }
    cout << endl << "The sizes of canonvars, canongrads, canoneqf," << endl
	 << "   numsols and listofsets are "
	 << canonvars->size() << ", " 
	 << canongrads->size() << ", " 
	 << canoneqf->size() << ", "
	 << numsols->size() << ", "
	 << listofsets->size() << endl;
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



