// getsols.cpp
//   vector<binopexp *> *getsols(ifstream & solfile)


#include "decl.h"
// #include "infix2expr.h"
#include "extoper.h"
#include "extstruct.h"
// #ifdef WITHDBG
// #include "dbg.h"
// #endif

vector<binopexp *> *getsols(ifstream & solfile) // this is the meat of 
{					// mainchksols, which should be 
  int k, q;			 		 // modified to use it.

  //  if (dbg2level & GETSOLS) cout << "Entering getsols" << endl;
  vector<binopexp *> *sols = new vector<binopexp *>;
  vector<string> *solstr = geteqsfromeqsfile (solfile);
  //  if (dbg2level & GETSOLS) cout << "getsols got " << solstr->size()<< endl;
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
	  thissol->lhs->MKS= (*canonvars)[q]->MKS;
	}
      if (!gotvar) {
	cerr << "In equation " << k << endl;
	throw(string("Never got variable in sol")); 
      }
      sols->push_back(thissol);
    }
  return(sols);
}
