// list2.cc	driver for test of parsing and reading CLIPS equations
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//	As was on Nov 17, 2000, except for some attempts to correct on 1/5/01
//	solves as many equations as possible using iterative results from
//	linear equations in a single unknown. 
//	1.5: I think it does more than that
//   Writes out as many variables solved as possible in file with
//	extension jsol. If that solves all the equations, that is all
//	that appears in that file. If equations remain, writes out 
//	a line 
//      PROBLEM HAD neqs in nvars, REMAINING EQUATIONS: neqs in nvar VARIABLES
//	followed by the equations, one per line, then a line VARIABLES:,
//	followed by the remaining unsolved for variables. Then if arg[1]
//	is "i", enters an interactive mode.
//
//   modified Oct 19, to read from file 
//   was   t:\vanlehn\andes\lisp\pdata\<probname>\eqs.clp     (at work)
//   was s:/lobby/joels/math/code/eqs.prob/<probname>.eqs	(at work)
//   now C:/joel/math/data/eqs.prob/<probname>.eqs	(at work)
//   ~shapiro/programming/c++/data/eqs/<probname>.eqs at home
//   assumes this file has only equations, one per line, without "unknown"s

#include <fstream>
#include "decl.h"
using namespace std;
#define IAmMain
#include "dbg.h"
#include "extstruct.h"

#define DBG(A) DBGF(FILEGET,A)

int main(int argc, char *argv[])
{
  string buf, fullbuf;
  vector<string> *eqs;
  vector<varindx> *vars;
  
  string token;
  int k;
  ifstream eqfile;
  bool INTERACT = false;
  
  if (argc > 3) throw("Usage: " + string(argv[0]) + "[i|n [dbgmask]]"); 
  if ((argc >= 2) && (argv[1][0] == 'i')) INTERACT = true;
#ifdef WITHDBG
  if (argc == 3) dbglevel = (unsigned int) strtol(argv[2],0,16);

  cerr << "argc = " << argc << ", dbglevel = " << hex << dbglevel 	// diag
       << " interactive is " << ((INTERACT) ? "true" : "false") << endl;// diag
#endif
  cerr << "Problem name: ";
  for(k=0;k<100;k++)
    {
#ifdef WIN32
    cin >> buf;
#else
      buf = getaline(cin);
#endif
      cout << buf << endl;
      if (buf.size()!=0) 
	{
	  //  __CYGWIN__ is defined at work, not at home on my Sun
#ifdef __CYGWIN__
	  fullbuf = "C:/joel/math/data/eqs.prob/"  /* changed 1/5/01 */
#else
#ifdef WIN32
	  fullbuf = "C:/joel/math/data/eqs.prob/"
#else
	  fullbuf = "/space/home/shapiro/programming/c++/data/eqs/"
#endif
#endif
	    + buf + ".eqs";
	  DBG( cerr << "about to open " << fullbuf << endl; );
	  eqfile.open(fullbuf.c_str());
	  DBG( cerr << "tried to open " << fullbuf << endl; );
	  if (eqfile != 0) {
	    cout << "Opened problem " << buf << endl;
	    break;  }
	  else
	    cerr << "Couldn't open " << fullbuf
		 << ", try another filename:" << endl;
	}
      else cerr << "Please enter problem name: ";
    }
  DBG( cerr << "did open " << fullbuf << endl; );
  eqs = geteqsfromeqsfile (eqfile);
  cout << "Number of equations is " << eqs->size() << endl;
  eqfile.close();
  
  canoneqf = new vector<binopexp *>;
  DBG( { cout << "Checking initialization in list2: " << endl;
	 cout << "eqs and canoneqf have sizes "<< eqs->size() << ", " 
	      << canoneqf->size() << endl; } );
  
  try
    {
      canonvars = makevarlist(eqs,canoneqf);
      cout << "Number of variables found: " << canonvars->size() << endl;
      ofstream solfile((buf+".jsol").c_str());
      numpasses = 0;
      vars = new vector<varindx>(canonvars->size(),(varindx)0);
      for (k = 0; k < canonvars->size(); k++) (*vars)[k] = k;
      checkeqs(canoneqf, vars, solfile);
      DBG( cout << "After first checkeqs, left with "
	     << canoneqf->size() << " equations and "
	     << vars->size() << " unknowns unsolved"
	     << endl; );

      if (canoneqf->size() > 0)
	{
	  // report status
	  solfile << "REMAINING EQUATIONS: " << canoneqf->size()
	  << " in " << vars->size() << " VARIABLES" << endl;
	  cout << "REMAINING EQUATIONS: " << canoneqf->size()
	  << " in " << vars->size() << " VARIABLES" << endl;
	  for (k=0; k < canoneqf->size(); k++)
	    {
	      solfile << (*canoneqf)[k]->getInfix() << endl;
	      cout << (*canoneqf)[k]->getInfix() << endl;
	      cerr << (*canoneqf)[k]->getInfix() << endl;
	    }
	}
      if (vars->size() > 0)
	{
	  solfile << "unsolved VARIABLES:" << endl;
	  cout << "unsolved VARIABLES:" << endl;
	  for (k=0; k < vars->size(); k++) 
	    solfile << (*canonvars)[(*vars)[k]]->clipsname << endl;
	}
      if (canoneqf->size() > 0)
	if (INTERACT) gointeract(canoneqf, vars);
    return(0);
  }
  catch(string message)
    {
      cerr << message << endl;
      cerr << message << endl;
      return(1);
  }
}
