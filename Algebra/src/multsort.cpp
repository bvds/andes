// multsort.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include "decl.h"
#include "extoper.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(PLUSSORT,A)
#define DBGM(A) DBGFM(PLUSSORT,A)
void qsrtexpr(vector<expr *> *Vptr);


/************************************************************************
 * bool multsort(expr * & ex)						*
 *	Sorts the factors in a product, combining terms when possible	*
 *. on entry ex must be a mult n_op, but it might be changed on exit.	*
 *	Returns true if a change was made beyond reordering		*
 ************************************************************************/
bool multsort(expr * & ex)
{
  int q1, q2, k;
  bool answer = false;
#ifdef WITHDBG
  int thisdbg = ++dbgnum;
#endif

  DBG(cout << "multsort " << thisdbg << " with " << ex->getInfix() << endl);
  if (ex->etype != n_op) throw(string("multsort called on non-n_op"));
  if (((n_opexp *)ex)->op->opty != multe)
    throw(string("multsort called on non-mult n_op"));
  vector<expr *> *v = ((n_opexp *)ex)->args;
  DBGM(cout << "Multsort about to call qsrtexpr" << endl;);
  qsrtexpr(v);
  DBGM(cout << "Multsort after sort, " << ex->getInfix() << endl;);
  expr * base1, * base2, * exp1, * exp2;
  bool waspow1, waspow2;
  for (q1 = 0; q1+1 < v->size(); q1++)
    {
      if ( ((*v)[q1]->etype == binop) &&
	   (((binopexp *)(*v)[q1])->op->opty == topowe))
	{
	  base1 = ((binopexp *)(*v)[q1])->lhs;
	  exp1 =  ((binopexp *)(*v)[q1])->rhs;
	  waspow1 = true;
	}
      else { 
	base1 = (*v)[q1]; 
	exp1 = new numvalexp(1); 
	exp1->MKS.put(0,0,0,0,0);
	waspow1 = false; 
      }
      for (q2 = q1+1; q2 < v->size(); q2++)
	{
	  if ( ((*v)[q2]->etype == binop) &&
	       (((binopexp *)(*v)[q2])->op->opty == topowe))
	    {
	      base2 = ((binopexp *)(*v)[q2])->lhs;
	      exp2 =  ((binopexp *)(*v)[q2])->rhs;
	      waspow2 = true;
	    }
	  else { 
	    base2 = (*v)[q2]; 
	    exp2 = new numvalexp(1); 
	    exp2->MKS.put(0,0,0,0,0);
	    waspow2 = false; 
	  }
	  if (equaleqs(base1,base2))
	    {
	      answer = true;
	      apluskb(exp1,exp2,1.);
	      (*v)[q2]->destroy();		// remove second factor. if it
	      if (!waspow2) exp2->destroy();	//  was topow exp2 is already
	      for (k=q2+1;k<v->size();k++)		// done by q2 destroy
		(*v)[k-1] = (*v)[k];
	      v->pop_back();
	      q2--;

	      eqnumsimp(exp1,true);
	      
	      if (exp1->etype == numval)
		{
		  if (((numvalexp *)exp1)->value == 0)
		    {
		      (*v)[q1]->destroy();	// remove first factor too.
		      for (k=q1+1;k<v->size();k++)
			(*v)[k-1] = (*v)[k];
		      v->pop_back();
		      q1--;
		      break;
		    }
		  if (((numvalexp *)exp1)->value == 1)
		    {
		      (*v)[q1] = base1;	// This fixes up (*v)[q1]
		      waspow1 = false;  // delete exponent
		      continue;		// skip to next q2
		    }
		} // end of if combined exponent is numval. if neither 0 or 1
	      if (waspow1) ((binopexp *)(*v)[q1])->rhs = exp1;
	      else {
		(*v)[q1] = new binopexp(&topow,base1,exp1);
		waspow1 = true;
	      }
	    } // end of what to do if bases match
	  else
	    if(!waspow2) exp2->destroy();
	} // end of loop on q2, second factor. Break above is intended to pass

      if(!waspow1) exp1->destroy();
    } // end of loop on q1, first factor. 		to next q1 start.
  if (((n_opexp *)ex)->args->size() < 2) unnop(ex);
  DBG(cout << "Multsort " << thisdbg << " returning " 
      << ((answer) ? "true" : "false")
	   << " with ex=" << ex->getInfix() << endl);
  return(answer);
}
