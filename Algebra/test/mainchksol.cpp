// mainchksol.cc	checking solution output as from list2
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//	(either here or in checksol need to add check of inequalities)

#include <fstream>
#include "decl.h"
#define IAmMain
#include "extoper.h"
#include "extstruct.h"
using namespace std;
#include "dbg.h"

#define DBG(A) DBGF(FILEGET,A)

void checksol(const vector<binopexp *> * eqexpr, 
	      const vector<binopexp *> * sols,
	      const double reltverr);

int main(int argc, char *argv[])
{
  string buf, fullbuf;
  vector<string> *eqs;
  vector<string> *solstr;
  
  string token;
  int k, q;
  ifstream eqfile, solfile;
  bool INTERACT = false;

  if (argc > 3) throw("Usage: " + string(argv[0]) + "[i|n [dbgmask]]"); 
  if ((argc >= 2) && (argv[1][0] == 'i')) INTERACT = true;

#ifdef WITHDBG
  dbglevel = 0x0;
  if (argc == 3) dbglevel = (int) strtol(argv[2],0,16);
  cerr << "argc = " << argc << ", dbglevel = " << dbglevel 	// diag
       << " interactive is " << ((INTERACT) ? "true" : "false") << endl;// diag
#endif  

  cerr << "Problem name: ";
  for(k=0;k<100;k++)
    {
      buf = getaline(cin);
      if (buf.size()!=0) 
	{
	  //  __CYGWIN__ is defined at work, not at home on my Sun
#ifdef __CYGWIN__
	  fullbuf = "C:/joel/math/data/eqs.prob/"  /* changed 1/5/01 */
#else
#ifdef WIN32
	  fullbuf = "C:/joel/math/data/eqs.prob/"  /* changed 1/5/01 */
#else
	  fullbuf = "/space/home/shapiro/programming/c++/data/eqs/"
#endif
#endif
	    + buf + ".eqs";
	  DBG( cerr << "about to open " << fullbuf << endl; );
	  eqfile.open(fullbuf.c_str());
	  DBG( cerr << "tried to open " << fullbuf << endl; );
	  if (eqfile == 0) {
	    cerr << "Couldn't open " << fullbuf
		 << ", try another filename:" << endl;
	    continue;
	  }
#ifdef __CYGWIN__
	  fullbuf = "C:/joel/math/data/jsols/"
#else
#ifdef WIN32
	  fullbuf = "C:/joel/math/data/jsols/"
#else
	  fullbuf = "/space/home/shapiro/programming/c++/data/jsols/"
#endif
#endif
	    + buf + ".jsol";
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
  eqs = geteqsfromeqsfile (eqfile);
  cout << "Number of equations is " << eqs->size() << endl;
  eqfile.close();
  solstr = geteqsfromeqsfile (solfile);
  cout << "Number of solutions is " << solstr->size() << endl;
  solfile.close();
  canoneqf = new vector<binopexp *>;
  solsexpr = new vector<binopexp *>;
  canonvars = (vector<physvar *> *) NULL;

  try
    {
      canonvars = makevarlist(eqs,canoneqf);
      cout << "Number of variables found in equations: " 
	   << canonvars->size() << endl;
      if (canonvars->size() > solstr->size())
	throw(string("number of vars solved less than number which appear"));
      for (k=0; k < solstr->size(); k++)
	{
	  stack<string> *toklist = parseclipseq((*solstr)[k]);
	  bool gotnum=false;
	  bool gotvar=false;
	  bool goteq =false;
	  binopexp * thissol = new binopexp();
	  thissol->op = &equals;

	  while (!toklist->empty())
	    { 
	      string token = toklist->top(); toklist->pop();
	      if (isanum(token)) 
		{
		  if (gotnum) throw(string("two numbers in sol"));
		  gotnum = true;
		  thissol->rhs = new numvalexp(token);
		  continue;
		}
	      if (token.compare("(")==0) continue;
	      if (token.compare(")")==0) continue;
	      if (token.compare("=")==0)
		{
		  if (!gotnum) throw(string("got = without following number"));
		  goteq = true;
		  continue;
		}
	      if (token.compare("-")==0) {
		cerr << "In equation " << k << endl;
		throw(string("I didn't think I'ld get unary - in sols"));
	      }
	      if (token.compare("*")==0) {
		cerr << "In equation " << k << endl;
		throw(string("I didn't think I'ld get * in sols")); 
	      }
	      if (token.compare("^")==0) {
		cerr << "In equation " << k << endl;
		throw(string("I didn't think I'ld get ^ in sols")); }
	      // should get here only on a clipsname
	      if ((!(gotnum && goteq)) || gotvar) {
		cerr << "In equation " << k << ", " << token  << endl;
		throw(string("got clipsname when not expecting it")); }
	      gotvar = true;
	      for (q=0;q < canonvars->size(); q++) 
		if ((*canonvars)[q]->clipsname==token) break;
	      if (q==canonvars->size()) {
		cerr << "In equation " << k << ", variable " << token << endl;
		throw(string("Got variable sol for unknown variable"));  }
	      thissol->lhs = new physvarptr(q);
	    }
	  if (!gotvar) {
	    cerr << "In equation " << k << endl;
	    throw(string("Never got variable in sol")); 
	  }
	  solsexpr->push_back(thissol);
	}
      DBG( for (k=0; k < solsexpr->size();k++) 
	     cout << (*solsexpr)[k]->getInfix() << endl; );
      
      checksol(canoneqf,solsexpr,RELERR);
	}
      
  catch(string message)
    {
      cerr << message << endl;
      cout << message << endl;
      exit(1); 
    }
  return(0);
}
