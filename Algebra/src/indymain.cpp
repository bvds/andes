// indymain.cc	driver for test of reading the canonical equations from
//	a *.eqs file, and the solutions from a .jsol file.
//	Then finding the gradients of the
//	former about the latter, then constructing a set S of independent
//	equations from the canonical ones, by sequentially accepting or
//	rejecting canonical equations if they are/are not independent of
//	the previous ones 
//
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include "decl.h"
#define IAmMain
#include "dbg.h"
#include "extstruct.h"
#include "indyset.h"
#include "unitabr.h"
#include <math.h>
using namespace std;

#define DBG(A) DBGF(FILEGET,A)

void constsfill();		// in physconsts.cpp
unitabrs unittable;

int main(int argc, char *argv[])
{
  string buf, fullbuf;
  canoneqf = new vector<binopexp *>;
  canonvars = new vector<physvar *>;

  ifstream eqfile;
  ifstream solfile;
  
  int k, q;
  int numvars;

  if (argc > 2) {
    cout << "Usage: " << string(argv[0]) << " [dbgmask]"<< endl; 
    exit(1);}
#ifdef WITHDBG
  if (argc == 2 ) dbglevel = (int) strtol(argv[1],0,16);
  cerr << "argc = " << argc << ", dbglevel = " << dbglevel << endl;   // diag
#endif
  cerr << "Problem name: ";
  for(k=0;k<100;k++)
    {
      cin >> buf;
      if (buf.size()!=0) 
        {
	  // changed 2/20 to do colander rather than clips problems
#ifdef __CYGWIN__
          fullbuf = "C:/joel/math/data/eqs.wu/"
#else
#ifdef WIN32
          fullbuf = "C:/joel/math/data/eqs.wu/"
#else
          fullbuf = "/space/home/shapiro/programming/c++/data/eqs.wu/"
#endif
#endif
            + buf + ".eqf";
          DBG( cerr << "about to open " << fullbuf << endl;);
          eqfile.open(fullbuf.c_str());
          DBG( cerr << "tried to open " << fullbuf << endl;);
          if (eqfile == 0) {
            cerr << "Couldn't open " << fullbuf
                 << ", try another filename:" << endl;
            continue;
          }
#ifdef __CYGWIN__
          fullbuf = "C:/joel/math/data/cawu/"
#else
#ifdef WIN32
          fullbuf = "C:/joel/math/data/cawu/"
#else
          fullbuf = "/space/home/shapiro/programming/c++/data/cawu/"
#endif
#endif
            + buf + ".jsol";
          DBG( cerr << "about to open " << fullbuf << endl;);
          solfile.open(fullbuf.c_str());
          DBG( cerr << "tried to open " << fullbuf << endl; );
          if (solfile != 0)  break;
          else cerr << "Couldn't open " << fullbuf
                 << ", try another filename:" << endl;
        }
      else cerr << "Please enter problem name: ";
    }
  DBG( cerr << "did open " << fullbuf << endl;);
  try {
    unittable.fill();
    constsfill();

    if (!getallfile(eqfile)) throw(string("couldn't finish reading input"));
    cout << "Number of equations is " << canoneqf->size() << endl;
    eqfile.close();
    numvars = canonvars->size();
    DBG( cout << "Number of variables found in equations: " 
	 << numvars << endl;);
    // do we need to use vars? maybe using canonvars ok?
    vector<varindx> *vars = new vector<varindx>(numvars,-1);
    for (k = 0; k < numvars; k++) 
      (*vars)[k]= k;
    numsols = getsols(solfile);	// this is binopexp or double? DOUBLES!
    DBG( cout << "got " << numsols->size() << " solutions"<< endl;);
    solfile.close();
    if (numvars != numsols->size())
      throw(string("number of vars solved unequal to number which appear"));

    indyset indy(numvars);
    for (k = 0, q=0; k < canoneqf->size(); k++)
      if (indy.isindy((*canoneqf)[k]))
	{
	  indy.placelast();
	  cout << "Added " << (*canoneqf)[k]->getInfix() 
	       << " as independent equation # " << q << endl;
	  q++;
	}
      else
	{
	  vector<double>expansion = indy.expandlast();
	  cout << "Equation " << (*canoneqf)[k]->getInfix() <<
	    " is not independent but expands with coefs" << endl;
	  for (int r=0; r < q; r++) 
	    cout << expansion[r] << ", ";
	  cout << endl << "in terms of the independent equations."<< endl;
	}
    cout << "At end of equations, there were " << q << 
      " independent equations out of " << canoneqf->size()
	 << ", in " << numvars << " variables" << endl;
  } // end of try
  catch(string message) 
    {  
      cerr << message << endl;
      cout << message << endl;
      return(1);
    }
  return(0);
} // end of main program.
