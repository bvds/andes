// int powonev(ex, var)
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//   returns the power to which one variable var occurs in expression ex,

#include "decl.h"
#include <math.h>
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(ORDUNK,A)

/************************************************************************
 *  int powonev(const expr * eq, const varindx var)			*
 *	returns an integer expressive of the order of the equation in 	*
 *	the one variable var, conservatively estimated and truncated 	*
 *	at 3.								*
 ************************************************************************/
int powonev(const expr * eq, const varindx var)	
{
  n_opexp *thnop;
  binopexp *thbin;
  int k, ans;
  
  DBG( cout << "Entering powonev" << endl; );
  switch (eq->etype)
    {
    case unknown:
    case fake:
      throw("powonev called on unknown or fake expr");
    case numval:
      return (0);
    case physvart:
      return ((((physvarptr *)eq)->varindex == var) ? 1 : 0);
      break;
    case binop:
      thbin = (binopexp *) eq;
      switch (thbin->op->opty)
	{
	case equalse:
	case grte:
	case gree:
	  return(max(powonev(thbin->lhs,var),powonev(thbin->rhs,var)));
	case divbye:
	  if (powonev(thbin->rhs,var) == 0) 
	    return (powonev(thbin->lhs,var));
	  else return(3);
	case topowe:
	  if (!(powonev(thbin->rhs,var)== 0)) return(3);
	  k=powonev(thbin->lhs,var);
	  if (k==0) 
	    return(0);		// This was written when I was much more 
	  int q=0;		// ambitious about my equation manip. come back
	  if (thbin->rhs->etype != numval) return(3);
	  if (lookslikeint(((numvalexp *)thbin->rhs)->value, q))
	    {
	      if (q==0) return (0);	// see just above
	      if ((q!=1)&&(q!=2)) return(3);
	      return ((q*k > 3) ? 3 : q*k);
	    }
	  else return(3);
	}
    case n_op:
      thnop = (n_opexp *) eq;
      ans = 0;
      if (thnop->op->opty == pluse)
	for (k=0; k < thnop->args->size(); k++)
	  ans = max(ans,powonev((*thnop->args)[k],var));
      else if (thnop->op->opty == multe)
	for (k=0; k < thnop->args->size(); k++)
	  ans += powonev((*thnop->args)[k],var);
      else throw("n_op neither plus or mult in powonev");
      if (ans>3) return(3);
      else return(ans);
    case function:
      if (powonev(((functexp *) eq)->arg,var) == 0) return(0); // see above
	      //	throw("known function of known arg declared unknown");
      else return(3);
    }
  throw("got to impossible place in powonev");
}


