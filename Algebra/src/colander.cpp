// colander.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//   Note: this is an old version, before we incorporated equation checking
//   into the colander program. The new version replaces this program by
//   colget.cpp


//	solve equations for problem in Andes2,
//	from files generated with units, new format, 3/22/01
//	Just as list2 but for new solver format


#include <fstream>
#include "decl.h"
using namespace std;
#define IAmMain
#include "dbg.h"
#include "extstruct.h"
#include "unitabr.h"

#define DBG(A) DBGF(FILEGET,A)

#ifdef FAKEDEG
#define EQDIR "eqs.wu/"
#else
#define EQDIR "eqs.wur/"
#endif


void constsfill();		// in physconsts.cpp

unitabrs unittable;

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
	  fullbuf = "C:/Andes2/data/"  /* changed 1/5/01 */
#else
#ifdef WIN32
    	  fullbuf = "C:/Andes2/data/"  /* changed 1/5/01 */
#else
	  fullbuf = "/space/home/shapiro/programming/c++/data/"
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
      if (!getallfile(eqfile)) throw(string("couldn't finish reading input"));
      cout << "Number of equations is " << canoneqf->size() << endl;
      eqfile.close();
      cout << "Number of variables found: " << canonvars->size() << endl;
      ofstream maple((buf+".jsol").c_str());
      vector<varindx> *vars = new vector<varindx>;
      expr * eqexpr;
      expr * dimtroub;
      for (k = 0; k < canoneqf->size(); k++) {
	eqexpr = (expr *)(*canoneqf)[k];
	eqnumsimp(eqexpr,true);
	dimtroub = dimenchk(true,eqexpr);
	if (dimtroub != (expr *)NULL) {
	  cout << "Dimenchk trouble on equation " << eqexpr->getInfix()
	       << ", trouble at " << endl;
	  dimtroub->dbgprint(4);
	  throw(string("dimensional inconsistency in input equation"));
	}
	if (eqexpr->etype != binop) 
	  throw(string("dimenchk made nonbinop equation"));
	(*canoneqf)[k] = (binopexp *) eqexpr;
	cout << "Input " << k << ":" << (*canoneqf)[k]->getInfix() << endl;
      }
      

      for (k = 0; k < canonvars->size(); k++) 
	if ((*canonvars)[k]->isused) vars->push_back(k);
      numpasses = 0;
      checkeqs(canoneqf, vars, maple);
      DBG( cout << "After first checkeqs, left with "
	        << canoneqf->size() << " equations and "
	        << vars->size() << " unknowns unsolved"
	        << endl; );

      if (canoneqf->size() > 0)
	{
	  // report status
	  maple << "<UNSLVEQS>" << endl;
	  cout << "REMAINING EQUATIONS: " << canoneqf->size()
	  << " in " << vars->size() << " VARIABLES" << endl;
	  for (k=0; k < canoneqf->size(); k++)
	    {
	      maple << (*canoneqf)[k]->getInfix() << endl;
	      cout << (*canoneqf)[k]->getInfix() << endl;
	      cerr << (*canoneqf)[k]->getInfix() << endl;
	    }
	}
      if (vars->size() > 0)
	{
	  maple << "<UNSLVVARS>" << endl;
	  cout << "unsolved VARIABLES:" << endl;
	  for (k=0; k < vars->size(); k++) 
	    maple << "(" << (*canonvars)[(*vars)[k]]->clipsname 
	          << " NIL)" << endl;
	}
    }
  catch(string message)
    {
      cerr << message << endl;
      cout << message << endl;
      exit(1);
    }
  return(0);
}
