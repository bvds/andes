// ordunknowns
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//   modified 6/1/01 to have switch to ignore knownedness

#include "decl.h"
#include <math.h>
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(ORDUNK,A)

/************************************************************************
 * ordunknowns(eq, chkknown)  returns an integer expressive of the	*
 *	order of the equations in the unknowns, conservatively 		*
 *	estimated and truncated at 3 [should we up this? cubic 		*
 *  	equations no so bad.]						*
 *   if chkknown = false, all physvarptrs are considered unknown, 	*
 *	regardless of their known fields. Except for numvalexp's, 	*
 *	the value of expr->known is ignored. This emulates how 		*
 *	everything worked before 6/01. 					*
 *   if chkknown = true, assumes expr marked known are, but doesn't	*
 *	trust those not known except for physvarptrs.			*
 ************************************************************************/

int ordunknowns(const expr * eq, const bool chkknown)
{
  n_opexp *thnop;
  binopexp *thbin;
  int k, ans;
  
  DBG( cout << "Entering ordunknowns" << endl; );
  if (chkknown && eq->known) return(0);
  switch (eq->etype)
    {
    case unknown:
    case fake:
      throw("ordunknowns called on unknown or fake expr");
    case numval:
      return(0);
    case physvart:
      return ((chkknown && eq->known) ? 0 : 1);
      break;
    case binop:
      thbin = (binopexp *) eq;
      switch (thbin->op->opty)
	{
	case equalse:
	case grte:
	case gree:
	  return(max(ordunknowns(thbin->lhs, chkknown),
		     ordunknowns(thbin->rhs, chkknown)));
	case divbye:
	  if (ordunknowns(thbin->rhs, chkknown) == 0)
	    return (ordunknowns(thbin->lhs, chkknown));
	  else return(3);
	case topowe:
	  {
	    if (ordunknowns(thbin->rhs, chkknown) != 0) return(3);
	    k=ordunknowns(thbin->lhs, chkknown);
	    if (k==0) 
	      //	    throw("topow with known args declared unknown!");
	      return(0);     // This was written when I was much more 
	    int q=0;	     // ambitious about my equation manip. come back
	    if (thbin->rhs->etype != numval) return(3);
	    if (lookslikeint(((numvalexp *)thbin->rhs)->value, q))
	      {
		if (q==0) return (0);	// see just above
		//	throw("topow with zero exponent declared unknown");
		if ((q!=1)&&(q!=2)) return(3);
		return ((q*k > 3) ? 3 : q*k);
	      }
	    else return(3);
	  }
	default:
	  break;  // goto error below
	}
      break;
    case n_op:
      thnop = (n_opexp *) eq;
      ans = 0;
      if (thnop->op->opty == pluse)
	for (k=0; k < thnop->args->size(); k++)
	  ans = max(ans,ordunknowns((*thnop->args)[k], chkknown));
      else if (thnop->op->opty == multe)
	for (k=0; k < thnop->args->size(); k++)
	  ans += ordunknowns((*thnop->args)[k], chkknown);
      else throw("n_op neither plus or mult in ordunknowns");
      if (ans>3) return(3);
      else return(ans);
    case function:
      if (ordunknowns(((functexp *) eq)->arg, chkknown) == 0) 
	return(0); // see above
	      //	throw("known function of known arg declared unknown");
      else return(3);
    }
  throw("got to impossible place in ordunknowns");
}

