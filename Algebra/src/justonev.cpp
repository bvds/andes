// justonev.cpp		bool hasjustonevar(const expr * e, varindx pv)
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include <string>
#include "decl.h"
using namespace std;
// no diagnostics

/************************************************************************
 *   bool hasjustonevar(const expr * e, varindx pv)			*
 *	returns true if e contains no physvars or only the physvar pv   *
 *	if pv < 0, and e contains one physvar, it sets pv to its index  *
 *      Thus starting with pv <0,  the expression really contains 	*
 *      exactly one physvar if hasjustonevar returns true AND pv is set *
 ************************************************************************/

bool hasjustonevar(const expr * e, varindx & pv)
{
  switch (e->etype){
  case unknown:
  case fake:
    return(false);
  case numval:
    return(true);
  case physvart:
    if (((physvarptr *)e)->varindex == pv) return(true);
    else if (pv >= 0) return(false);
    pv = ((physvarptr *)e)->varindex;
    return(true);
  case function:
    return(hasjustonevar(((functexp *) e)->arg, pv));
  case binop:
    return(hasjustonevar(((binopexp *) e)->lhs, pv)  &&
	   hasjustonevar(((binopexp *) e)->rhs, pv));
  case n_op:
	{
	  int k;
      n_opexp *th = (n_opexp *) e;
      for (k=0; k < th->args->size(); k++)
        if (!hasjustonevar((*th->args)[k], pv)) return(false);
      // return(true);  // AW: wrong if degenerate 0 arg n_op?
		return (th->args->size() > 0); // must have at least 1 arg
	}
  default:
		throw(string("unknown expr type in hasjustonevar"));
  }
}
