// colget.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//	solve equations for problem in Andes2,
//	from files generated with units, new format, 3/22/01
//	extracted from colander, other half in justsolve


#include <fstream>
#include "../src/decl.h"
using namespace std;
#define IAmMain
#include "../src/dbg.h"
#include "../src/extstruct.h"
#include "../src/unitabr.h"
#include "mconst.h"  // define DEGTORAD
#define DEG2RAD DEGTORAD

#define DBG(A) DBGF(FILEGET,A)

#ifdef FAKEDEG
#define EQDIR "eqs.wu/"
#else
#define EQDIR "eqs.wur/"
#endif

void constsfill();		// in physconsts.cpp
bool solveeqs(ofstream & outfile);
int checksol(const binopexp * const eqexpr, const vector<double> * const sols,
	     const double reltverr);
void dimchkeqf(ostream & outstr);

unitabrs unittable;

//////////////////////////////////////////////////////////////////////////////
// added to fix tan(90, 270, (2n+1)90 degrees) problems
#include <cmath>

double myTan(double x) {
  int sf = (int)floor((x/DEG2RAD)-0.00000000000000001);
  // get closest integer to multiple of 90 deg minus allowance

  int ef = (int)floor((x/DEG2RAD)+0.00000000000000001);
  // get closest integer to multiple of 90 deg plus allowance

  if ((sf == ef) && (sf % 90 == 0) && (((sf / 90) % 2) == 1)) {
    // if the same integer and it's a multiple of (2n+1)90 then
    throw(std::string("Illegal Argument to trig function")); // throw the eexception
  }
  return tan(x); // otherwise it is okay
}

int main(int argc, char *argv[])
{
  string buf, fullbuf;
  canoneqf = new vector<binopexp *>;
  canonvars = new vector<physvar *>;
  string token;
  int k;
  ifstream eqfile;
  
  if (argc > 2) { cerr << "Usage: " << argv[0] << "[dbgmask]" << endl; 
		  exit(1) ; }
  if (argc == 2) dbglevel = (unsigned int) strtol(argv[1],0,16);

  cerr << "Problem name: ";
  for(k=0;k<100;k++)
    {
      cin >> buf;
      if (buf.size()!=0) 
	{
	  //  __CYGWIN__ is defined at work, not at home on my Sun
#ifdef __CYGWIN__
	  fullbuf = "C:/joel/math/data/"  /* changed 1/5/01 */
#else
#ifdef WIN32
    	  fullbuf = "C:/joel/math/data/"  /* changed 1/5/01 */
#else
	  fullbuf = string(getenv("HOME")) + "/programming/c++/andes/data/"
#endif
#endif
	    + string(EQDIR) + buf + ".eqf";
	  DBG( cerr << "about to open " << fullbuf << endl; );
	  eqfile.open(fullbuf.c_str());
	  DBG( cerr << "tried to open " << fullbuf << endl; );
	  if (eqfile != 0) break;
	  else
	    cerr << "Couldn't open " << fullbuf
		 << ", try another filename:" << endl;
	}
      else cerr << "Please enter problem name: ";
    }
  DBG( cerr << "did open " << fullbuf << endl; );

  try
    {
      unittable.fill();
      constsfill();
      paramasgn = new vector<binopexp *>; // added 4/18 so not in canoneqf
      if (!getallfile(eqfile)) throw(string("couldn't finish reading input"));
      cout << "Number of equations is " << canoneqf->size() << endl;
      eqfile.close();
      cout << "Number of variables found: " << canonvars->size() << endl;
      numsols = new vector<double>(canonvars->size(),HUGE_VAL); // numsols is
      ofstream maple((buf+".jsol").c_str()); 				// now
      if (solveeqs(maple)) {				// needed in solveqs
	bool discrep = false;
	for (k = 0; k < canoneqf->size(); k++) {
	  if (checksol((*canoneqf)[k],numsols,RELERR) > 1) {
	    if (!discrep) {
	      maple << "<DISCREPANCIES>" << endl;
	      discrep = true;
	    }
	    maple << (*canoneqf)[k]->getInfix() << endl;
	  }
	} // loop over equations to check
	dimchkeqf(maple);
      }	// done only if solution seemed complete
    }  
  catch(string message)
    {
      cerr << message << endl;
      cout << message << endl;
      exit(1);
    }
  return(0);
}
