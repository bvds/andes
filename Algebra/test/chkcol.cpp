// chkcol.cpp	checking solution output as from list2
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//	(either here or in checksol need to add check of inequalities)

#include <fstream>
#include "decl.h"
#define IAmMain
#include "extoper.h"
#include "extstruct.h"
using namespace std;
#include "unitabr.h"
#include "dbg.h"

#define DBG(A) DBGF(FILEGET,A)

#ifdef FAKEDEG
#define EQDIR "eqs.wu/"
#define SOLDIR "cawu/"
#else
#define EQDIR "eqs.wur/"
#define SOLDIR "cawur/"
#endif

int checksol(const binopexp * const eqexpr, const vector<double> * const sols,
	     const double reltverr);
void dimchkeqf(ostream & outstr);
void constsfill();
unitabrs unittable;

int main(int argc, char *argv[])
{
  string buf, fullbuf;

  canoneqf = new vector<binopexp *>;
  canonvars = new vector<physvar *>;
  
  string token;
  int k;
  ifstream eqfile, solfile;
  bool INTERACT = false;
  
  if (argc > 2) throw("Usage: " + string(argv[0]) + "[dbgmask]"); 
#ifdef WITHDBG
  if (argc == 2) dbglevel = (int) strtol(argv[1],0,16);
#endif

  DBG( cerr << "argc = " << argc << ", dbglevel = " << dbglevel << endl;);

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
	  fullbuf = "/space/home/shapiro/programming/c++/data/"
#endif
#endif
	    + string(EQDIR) + buf + ".eqf";
	  DBG( cerr << "about to open " << fullbuf << endl; );
	  eqfile.open(fullbuf.c_str());
	  DBG( cerr << "tried to open " << fullbuf << endl; );
	  if (eqfile == 0) {
	    cerr << "Couldn't open " << fullbuf
		 << ", try another filename:" << endl;
	    continue;
	  }
#ifdef __CYGWIN__
	  fullbuf = "C:/joel/math/data/"
#else
#ifdef WIN32
	  fullbuf = "C:/joel/math/data/"
#else
	  fullbuf = "/space/home/shapiro/programming/c++/data/"
#endif
#endif
	    + string(SOLDIR) + buf + ".jsol";
	  DBG( cerr << "about to open " << fullbuf << endl; );
	  solfile.open(fullbuf.c_str());
	  DBG( cerr << "tried to open " << fullbuf << endl; );
	  if (solfile != 0)  break;
	  else cerr << "Couldn't open " << fullbuf
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
      numsols = getsols(solfile);
      
      for (k = 0; k < canoneqf->size(); k++)
	checksol((*canoneqf)[k],numsols,RELERR);
      dimchkeqf(cout);
    }
      
  catch(string message)
    {
      cerr << message << endl;
      cout << message << endl;
      exit(1); 
    }
  return(0);
}
