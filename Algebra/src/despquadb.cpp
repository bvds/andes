// despquadb.cpp	more needed for quadratic desperation
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
#include <math.h>
using namespace std;

// need to change diag below.
#define DBG(A) DBGF(NEWCKEQSOUT,A)

// NOTE: this code has not been made to handle units.

vector<double> *twoqcfex(expr *ex, const varindx v1, const varindx v2);

/************************************************************************
 * twoquadcoef(eq, v1, v2) returns a vector of length 6, with the 	*
 *	numerical coefficients c_i so that the equation is equivalent to*
 *	c_0 v1^2 + c_1 v1 v2 + c_2 v2^2 + c_3 v1 + c_4 v2 + c_5 = 0	*
 ************************************************************************/
vector<double> *twoquadcoef(const binopexp *eq, const varindx v1,
			    const varindx v2)
{
  DBG( cout << "Twoquadcoef called on equation:" << endl
       << eq->getInfix() << endl << "in variables " << 
       (*canonvars)[v1]->clipsname << " and " <<
       (*canonvars)[v2]->clipsname << endl; );
  expr * meat = copyexpr(eq->lhs);
  if ((eq->rhs->etype != numval) || ((numvalexp *) eq->rhs)->value != 0)
    apluskb(meat,eq->rhs,-1.);
  vector<double> *retval = twoqcfex(meat,v1,v2);
  meat->destroy();
  DBG( { cout << "Twoquadcoef returning ";
         if (retval == (vector<double> *) NULL) cout << "false" << endl;
         else {
	   for (int k = 0; k < 6; k++) cout << (*retval)[k] << ", ";
	   cout << endl; } 
       } );
  return(retval);
}

vector<double> *twoqcfex(expr *ex, const varindx v1, const varindx v2)
{
  int k, q;  
  vector<double> *retval;
  DBG( cout << "Twoqcfex called on expression:" << endl
       << ex->getInfix() << endl; );
  eqnumsimp(ex,true);
  switch (ex->etype)
    {
    case numval:
      retval = new vector<double>(6,0.);
      (*retval)[5]=((numvalexp *)ex)->value;
      return(retval);
    case physvart:
      if (((physvarptr *)ex)->varindex == v1)
	{
	  retval = new vector<double>(6,0.);
	  (*retval)[3]= 1.;
	  return(retval);
	}
      if (((physvarptr *)ex)->varindex == v2)
	{
	  retval = new vector<double>(6,0.);
	  (*retval)[4]= 1.;
	  return(retval);
	}
      return((vector<double> *) NULL);
    case binop:
      {
      if (((binopexp *)ex)->op->opty != topowe) 
	return((vector<double> *) NULL);
      expr * brhs = ((binopexp *)ex)->rhs;
      if ((brhs->etype != numval) || 
	  !lookslikeint(((numvalexp *)brhs)->value, q) ||
	  (q != 2))
	return((vector<double> *) NULL);
      retval = twoqcfex(((binopexp *)ex)->lhs, v1, v2);
      DBG( { cout << "Twoqcfex on lhs of binop returned ";
	     if (retval == (vector<double> *) NULL) cout << "false" << endl;
	     else {
	       for (int k = 0; k < 6; k++) cout << (*retval)[k] << ", ";
	       cout << endl; } 
	   } );
      if (retval == (vector<double> *) NULL) return(retval);
      for (k=0;k<3;k++) if ((*retval)[k] != 0)
	{
	  delete retval;
	  return((vector<double> *) NULL); }
      (*retval)[0] = pow((*retval)[3],2);
      (*retval)[1] = (*retval)[3] * (*retval)[4];
      (*retval)[2] = pow((*retval)[4],2);
      (*retval)[3] = 2. * (*retval)[3] * (*retval)[5];
      (*retval)[4] = 2. * (*retval)[4] * (*retval)[5];
      (*retval)[5] = pow((*retval)[5],2);
      return(retval);
      }
    case n_op:
      {
	n_opexp * nopex = (n_opexp *) ex;
	if (nopex->op->opty == pluse)
	  {
	    retval = new vector<double>(6,0.);
	    vector<double> * thterm;
	    for (k = 0; k < nopex->args->size(); k++)
	      {
		thterm = twoqcfex((*nopex->args)[k],v1,v2);
		DBG( { cout << "Twoqcfex on arg " << k << " of plus returned ";
		       if (thterm == (vector<double> *) NULL) 
			 cout << "false" << endl;
		       else {
			 for (int k = 0; k < 6; k++) 
			   cout << (*thterm)[k] << ", ";
			 cout << endl; } 
		     } );
		if (thterm == (vector<double> *) NULL)
		  { delete retval; return thterm; }
		for (q = 0; q < 6; q++) (*retval)[q] += (*thterm)[q];
	      }
	    if (thterm != (vector<double> *) NULL) delete thterm;
	    return(retval);
	  }
	if (nopex->op->opty != multe)
	  throw(string("twocqcfex called on unknown n_op"));
	int curpow = 0;
	double factor = 1.;
	vector<double> * thterm;
	for (k = 0; k < nopex->args->size(); k++)
	  {
	    if (((*nopex->args)[k])->etype == numval)
	      factor *= ((numvalexp *)(*nopex->args)[k])->value;
	    else
	      {
		if (curpow >= 2) 
		  {delete retval; return((vector<double> *)NULL);}
		thterm = twoqcfex((*nopex->args)[k],v1,v2);
		DBG( { cout << "Twoqcfex on arg " << k << " of mult returned ";
		       if (thterm == (vector<double> *) NULL) 
			 cout << "false" << endl;
		       else {
			 for (int k = 0; k < 6; k++) 
			   cout << (*thterm)[k] << ", ";
			 cout << endl; } 
		     } );
		// BvdS: added Aug 2005
		if (thterm == (vector<double> *) NULL) return(thterm);
		// else this line will cause a memory exception:
		if (((*thterm)[0] == 0) && ((*thterm)[1] == 0) &&
		    ((*thterm)[2] == 0)) 
		  switch(curpow)
		    {
		    case 0:
		      curpow = 1;
		      retval = thterm;
		      break;
		    case 1:
		      curpow = 2;
		      (*retval)[0] = (*retval)[3] * (*thterm)[3];
		      (*retval)[1] = (*retval)[3] * (*thterm)[4]
			+ (*retval)[4] * (*thterm)[3];
		      (*retval)[2] = (*retval)[4] * (*thterm)[4];
		      (*retval)[3] = (*retval)[3] * (*thterm)[5]
			+ (*retval)[5] * (*thterm)[3];
		      (*retval)[4] = (*retval)[4] * (*thterm)[5]
			+ (*retval)[5] * (*thterm)[4];
		      (*retval)[5] = (*retval)[5] * (*thterm)[5];
		      delete thterm;
		      break;
		    case 2:
		      delete retval;
		      delete thterm;
		      return((vector<double> *) NULL);
		    default:
		      throw(string("Impossible happened in toqcfex"));
		    }
		else
		  {
		    if (curpow >= 1)
		      {
			delete retval;
			delete thterm;
			return((vector<double> *) NULL);
		      }
		    retval = thterm;
		    curpow = 2;
		  }
	      } // end of kth term not numval
	  } // end of loop over multipliers
	if (curpow == 0)
	  {
	    delete retval;
	    return((vector<double> *) NULL);
	  }
	for (k = 0; k < 6; k++) (*retval)[k] *= factor;
	return(retval);
      } // end of case n_op
    case function:
      return((vector<double> *) NULL);
    case unknown:
    case fake:
    default:
      throw(string("unknown expr sent to twocfex"));
    } // end of switch on ex->etype
}
