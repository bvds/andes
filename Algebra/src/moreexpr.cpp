// additions to  Class expr
//   one day will
//   define functions add, subtr, mult on two expr *, returning an plus n_op,
// 	possibly with < 2 args.
//   Not sure how to handle overloading. I am doing everything with pointers,
//   so I don't know how to overload operators
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include <string>
#include <stdio.h>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(MOREEX,A)

// contains
void apluskb(expr * & a1, const expr * const a2, const double coef);
void apluskb(expr * & a1, const expr * const a2, numvalexp * nv);
void minuseq(expr * & a1, const expr * const a2);

/***************************************************************************
 *  apluskb(expr * & a1, const expr * const a2, const double coef)	   *
 *	a1 <- a1 + coef * a2.    coef MUST BE DIMENSIONLESS!		   *
 *      a1 is left as an n_op of type plus containing the answer. 	   *
 *	It may have less than two args, but is still returned as a plus    *
 *	n_op. If it was a plus n_op, its location will not have changed.   *
 *	No attempt is made to simplify, and result should be flattened	   *
 *	and eqnumsimped after return.					   *
 *  apluskb(expr * & a1, const expr * const a2, numvalexp *nv)		   *
 *	same as above, but nv may have dimensions.			   *
 *	Consumes nv.							   *
 ***************************************************************************/
void apluskb(expr * & a1, const expr * const a2, const double coef) 
{
  numvalexp * nv = new numvalexp(coef);
  nv->MKS.put(0,0,0,0,0);
  apluskb(a1, a2, nv);
  return;
}

void apluskb(expr * & a1, const expr * const a2, numvalexp *nv)
{			
  n_opexp * answer;
  numvalexp * fk;
  int thisdbg = ++dbgnum;
  DBG( cout << "Entering apluskb " << thisdbg << " with " << a1->getInfix() 
            << ", " << a2->getInfix() <<", " << nv->getInfix() << endl;);
  if ((a2->etype == numval) && (((numvalexp *)a2)->value == 0))    return;
  if ((a1->etype != n_op) || ((n_opexp *)a1)->op->opty != pluse)
    {				// force a1 into form of pluse
      answer = new n_opexp(&myplus);
      answer->addarg(a1);
      a1 = answer;
      if (a1 == (*((n_opexp *)a1)->args)[0]) 
	throw(string("Whoops, apluskb has a real bug!!!"));
      DBG( { cout << "apluskb " << thisdbg << " made new n_op: " << endl;
             cout << answer->getInfix() << ", and a1 is now "
	          << a1->getInfix() << endl;} );
    }
  else answer = ( n_opexp * ) a1; // Okay, a1 is now plus, even if only 1 term

  if ((a2->etype == n_op) 			// if a2 is sum, do each term. 
      && ((n_opexp *)a2)->op->opty == pluse)
    {		
      for (int k = 0; k < ((n_opexp *)a2)->args->size(); k++)
	{
	  apluskb(a1, (*((n_opexp *)a2)->args)[k],(numvalexp *)copyexpr(nv));
	  DBG( cout << "apluskb " << thisdbg << ": " << k 
	       << "th term moved and a1 is now " << a1->getInfix() << endl; );
	}
      nv->destroy();
      return;
    }
  else
    {				// a2 is not a plus, treat as one term.
      for (int k = 0; k < answer->args->size(); k++) // check to see if lhs
	{			// sum already has a term proportional to it.
	  if (((*answer->args)[k]->etype == numval) &&
	      ((numvalexp *)(*answer->args)[k])->value == 0)
	    {
	      (*answer->args)[k]->destroy();
	      (*answer->args)[k] = copyexpr(a2);
	      kmult((*answer->args)[k],nv);
	      a1 = answer;
	      return;
	    }
	  DBG( cout << "apluskb " << thisdbg << ": About to uptonum arg " 
	       << k << endl; );
	  if (uptonum((*answer->args)[k],a2,fk))    // if term on lhs and term
	    {			// to be added differ only by numval, 
				// just change its coefficient
	      DBG( cout << "Apluskb " << thisdbg << ": uptonum returned true " 
		   << "on " << k << "'th term of a1"<< endl; );
	      if (!(nv->MKS == fk->MKS)) 
		{
		  cout << "Trouble in apluskb, trying to add ";
		  fflush(stdout);
		  cout << fk->getInfix() << " * " ;
		  fflush(stdout);
		  cout << a2->getInfix() << " to ";
		  fflush(stdout);
		  cout << (*answer->args)[k]->getInfix() << endl;
		  fflush(stdout);
		  throw(string(
			     "attempt to add terms of different dimensions"));
		}
	      kmult((*answer->args)[k],1. + (nv->value/fk->value)); // AW: what blocks divide by zero???
	      fk->destroy();
	      nv->destroy();
	      if (((*answer->args)[k]->etype == numval) &&   
		  ((numvalexp *)(*answer->args)[k])->value == 0.)
		{
		  (*answer->args)[k]->destroy();
		  for (int q=k+1; q < answer->args->size(); q++)
		    (*answer->args)[q-1]=(*answer->args)[q];
		  answer->args->pop_back();
		}
	      a1 = answer;	// added 2/7/01 - is it needed? Made no diff
	      DBG( cout << "return1 from apluskb " << thisdbg << " with "
		        << a1->getInfix() << endl; );
	      return;
	    } // end of if uptonum
	} 			//  below here if no term on lhs matched rhs
      DBG( cout << "Apluskb " << thisdbg 
	        << ": uptonum returned false on all terms" << endl; );
      expr * a3 = copyexpr(a2);
      kmult(a3,nv);
      answer->addarg(a3);
      DBG( cout << "return2 from apluskb " << thisdbg << " with " 
	        << a1->getInfix() << endl; );
      a1 = answer;	// added 2/7/01 - is it needed? Made no diff
      return;
    } // else a2 is not a plus
}

void minuseq(expr * & a1, const expr * const a2)    //   a1 -= a2
{					
  DBG( cout << "minuseq called" << endl; );
  apluskb(a1,a2,-1.);
  return;
}
