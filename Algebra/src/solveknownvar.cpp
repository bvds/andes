// solveknownvar   (linear trivial version - solves a*v+b = c*v+d, 
//	where a,b,c,d are all numvals.
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
		// apluskb used in place of many lines below
#include "decl.h"
#include "extoper.h"
#include "extstruct.h"
#include "dbg.h"
#include <math.h>
using namespace std;

#define DBG(A) DBGF(SOLVKV,A)

/************************************************************************
 *  double addnum(double a, double b)					*
 *	returns a+b, but returns 0 if |answer| < RELERR * (|a| + |b|)	*
 ************************************************************************/
double addnum(double a, double b)
{
  double ans = a + b;
  if (fabs(ans) < RELERR *(fabs(a) + fabs(b))) return(0.);
  else return(ans);
}


/************************************************************************
 *  bool solveknownvar(expr * & eq)					*
 *	returns true and reforms eq to form  v = num, if possible	*
 *	only to be called on an equation which contains only one	*
 *	variable, and known to be linear, and eqnumsimped. So is at	*
 *	worst a*v+b = c*v+d, (has it been flattened?)			*
 *	returns false if not done, for example if 0*v = something	*
 ************************************************************************/
bool solveknownvar(expr * & eq)
{		
  int k;	
  bool flattened;
  DBG( cout << "SOLVKV  called to solve equation " << eq->getInfix() << endl;);
  if (eq->etype != binop)
    throw(string("tried to solve a non-binop non-equation"));
  binopexp *bineq = (binopexp *) copyexpr(eq);
  if (bineq->op->opty != equalse)
    throw(string("tried to solve a non-equation"));
  if ((bineq->lhs->etype == physvart) &&
      (bineq->rhs->etype == numval)) {
    eq->destroy();
    eq = bineq;
    return(true);	   // already solved!
  }
  if ((bineq->rhs->etype != numval)   ||		// bring all to lhs
      (((numvalexp *)bineq->rhs)->value != 0)) 		// from here on, 
    {							// rhs = numvalexp(0)
      DBG( cout << "Solveknownvar about to move rhs to lhs" << endl; );
      apluskb(bineq->lhs,bineq->rhs,-1.);
      bineq->rhs->destroy();
      bineq->rhs = new numvalexp(0);
      bineq->rhs->MKS = bineq->lhs->MKS;
      DBG( { cout << "after move to lhs, bineq is" << endl;
	     bineq->dbgprint(4); } ) ;
    }
  flattened = flatten(bineq->lhs); 	// Note flatten may have left 
  DBG({					// a*v + (-1 * c * v) +b -d
    cout << "from lhs of bineq, flatten just returned "
	 << (flattened ?"true":"false" ) << endl;
    bineq->lhs->dbgprint(4); } );
  eqnumsimp(bineq->lhs,true);			// eqnumsimp might have left
  DBG({			// a*v + (c * v ) +d = 0
    cout << "After eqnumsimping that, we have" << endl;
    bineq->lhs->dbgprint(4); } );
  switch (bineq->lhs->etype)
    {
    case unknown:
    case fake:
      throw(string("solveknownvar has fake/unknown lhs"));
    case numval:
      //      throw(string("solveknownvar called on numval = 0"));
      bineq->destroy();
      return(false);
    case function:
      throw(string("solveknownvar called on function = 0"));
    case binop:
      throw(string("solveknownvar called on binop = 0"));
    case physvart:
      DBG( cout << "SOLVKV  called on already solved eq, returning "
		<< bineq->getInfix() << endl; );
      eq->destroy();
	  eq = bineq;
      return(true);
    case n_op:
      n_opexp * eqlhs = (n_opexp *) bineq->lhs;
      if (eqlhs->op->opty == multe) // if eq is num * var = 0 or var * num = 0
	{			    //   drop num, but (as of 8/17/01) fix
	  if (! ((eqlhs->args->size() == 2) && 		// 0's MKS
		 ( ( ((*eqlhs->args)[0]->etype == numval) &&
		     ((*eqlhs->args)[1]->etype == physvart))   ||
		   ( ((*eqlhs->args)[1]->etype == numval) &&
		     ((*eqlhs->args)[0]->etype == physvart))   ) ))
	    throw(string("got impossible lhs n_op in solveknownvar"));
	  if ((*eqlhs->args)[1]->etype == physvart)
	    {
	      bineq->lhs = (*eqlhs->args)[1];
	      bineq->rhs->MKS += (*eqlhs->args)[0]->MKS * -1.; // 8/17/01
	      bineq->MKS += (*eqlhs->args)[0]->MKS * -1.; // 8/17/01
	      (*eqlhs->args)[0]->destroy();
	    }
	  else
	    {
	      bineq->lhs = (*eqlhs->args)[0];
	      bineq->rhs->MKS += (*eqlhs->args)[1]->MKS * -1.; // 8/17/01
	      bineq->MKS += (*eqlhs->args)[1]->MKS * -1.; // 8/17/01
	      (*eqlhs->args)[1]->destroy();
	    }
	  // rmed 2/4/01 need to check	  delete eqlhs->args;
	  delete eqlhs;
	  DBG( cout << "SOLVKV  at end of lhs = mult, returning "
	            << eq->getInfix() << endl; ) ;
	  eq->destroy();
	  eq = bineq;
	  return(true);
	}
      else if (eqlhs->op->opty == pluse)
	{
	  double a = 0;
	  double b = 0;
	  dimens dima, dimb;
//dont need   for (k = 0; k < 5; k++) { dima[k] = UNKNDIM; dimb[k] = UNKNDIM; }
	  int pv = -1;		// will hold index to single physvar if found
	  int indx;
	  
	  for (k=0; k < eqlhs->args->size(); k++)
	    {
	      if ((*eqlhs->args)[k]->etype == numval) {
		b = addnum(b,((numvalexp *)(*eqlhs->args)[k])->value);
		if (dimb.unknp()) dimb = (*eqlhs->args)[k]->MKS;
		if (!(dimb == (*eqlhs->args)[k]->MKS)) 
		  throw(string(
			   "Solveknownvar couldnt add different dimensions"));
	      }
	      else if ((*eqlhs->args)[k]->etype == n_op)
		{
		  n_opexp * eqlharg = (n_opexp *) (*eqlhs->args)[k];
		  if (eqlharg->args->size() != 2) {
		    cout << "in solveknownvar, plus term " << k 
			 << " has " << eqlharg->args->size()
			 << " factors, it is " << eqlharg->getInfix() << endl;
		    throw(string(
			 "arg of + lhs of solveknownvar != two terms mult"));
		    }
		  
		  if ((*eqlharg->args)[0]->etype == numval)
		    {			// put them in order with numval second
		      expr * temp = (*eqlharg->args)[0];
		      (*eqlharg->args)[0] = (*eqlharg->args)[1];
		      (*eqlharg->args)[1] = temp;
		    }
		  if ((*eqlharg->args)[0]->etype != physvart)
		    throw(string(
			 "lh of lhs of solveknownvar no physvar"));
		  indx = ((physvarptr *)(*eqlharg->args)[0])->varindex;
		  if (pv < 0) pv = indx;
		  if (pv != indx) {
		    bineq->destroy();
		    return(false);}
		  if ((*eqlharg->args)[1]->etype != numval)
		    throw(string(
			 "lh of lhs of solveknownvar no physvar"));
		  a = addnum(a,((numvalexp *)(*eqlharg->args)[1])->value);
		  if (dima.unknp()) dima = (*eqlharg->args)[1]->MKS;
		  if (!(dima == (*eqlharg->args)[1]->MKS) )
		  throw(string(
			   "Solveknownvar couldnt add different dimensions"));

		}
	      else if ((*eqlhs->args)[k]->etype == physvart)
		{
		  indx = ((physvarptr *)(*eqlhs->args)[k])->varindex;
		  if (pv < 0) pv = indx;
		  if (pv != indx) {
		    bineq->destroy();
		    return(false);}
		  a = addnum(a,1.);
		  if (dima.unknp()) dima.put(0,0,0,0,0);
		  if (!dima.zerop())
		    throw(string(
			   "Solveknownvar couldnt add 1 to dimensioned coef"));
		}
	      else
		throw(string(
			     "lhs of solveknownvar sum of wrong term"));
	    }
	  if ((pv < 0)|| (a == 0)) {
	    bineq->destroy();
	    return(false);}
	  numvalexp * tempnv = (numvalexp *)bineq->rhs;
	  tempnv->value = -b/a;
	  tempnv->MKS = dima;
	  tempnv->MKS *= -1;
	  tempnv->MKS += dimb;
	  bineq->lhs->destroy();
	  bineq->lhs = new physvarptr(pv);
	  DBG( cout << "SOLVKV  at end of plus, returning "
		    << eq->getInfix() << endl; ) ;
	  eq->destroy();
	  eq = bineq;
	  return(true);
	}
      else throw(string("unknown n_op on lhs in solveknownvar"));
    }
  throw(string("unknown oper in solveknownvar"));
}

