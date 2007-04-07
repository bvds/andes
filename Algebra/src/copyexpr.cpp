// copyexpr.cpp          should really be a constructor, but don't know how.
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
				// modified 3/15/01 to include dimens
/************************************************************************
 *   expr * p = copyexpr(expr * old)					*
 *   makes a new copy of the expr old (including all subnodes, but	*
 *      not new physvars)						*
 *   and returns a pointer to it.					*
 ************************************************************************/
#include "expr.h"
using namespace std;

// has no diagnostics included.

expr * copyexpr(const expr * old )
{			
  expr * ret;
  switch (old->etype)	
    {
    case unknown:
      throw(string("Can't copy an unknown type expr"));
    case fake:      
      return(new fakeexpr());
    case numval:
      ret = new numvalexp(((numvalexp *) old)->value);
      break;
    case physvart:
      ret = new physvarptr( ((physvarptr *)old)->varindex );
      break;
    case binop:			// also oper's do not get copied!
      ret = new binopexp( ((binopexp *) old)->op,
			   copyexpr(((binopexp *) old)->lhs),
			   copyexpr(((binopexp *) old)->rhs));
      break;
    case function:
      ret = new functexp( ((functexp *) old)->f,
			   copyexpr(((functexp *) old)->arg));
      break;
    case n_op:
      {
	vector<expr *> *newl = new vector<expr *>
        	(((n_opexp *) old)->args->size());
	for (int k=0; k < ((n_opexp *) old)->args->size(); k++)
	  (*newl)[k]=copyexpr((*((n_opexp *) old)->args)[k]);
	n_opexp * newn = new n_opexp(((n_opexp *) old)->op);
	newn->args = newl;
	ret = newn;
	break;
      }
    default:
      throw(string("Copyexpr called on unknown type of expression"));
    }
  ret->MKS = old->MKS;
  return(ret);
}

void expr::destroy()
{
  switch (this->etype)
    {
    case unknown: delete(this); return;
    case fake: delete((fakeexpr *) this); return;      
    case numval: delete((numvalexp *) this); return;      
    case physvart: delete((physvarptr *) this); return;      
    case binop:
      ((binopexp *) this)->lhs->destroy();
      ((binopexp *) this)->rhs->destroy();
      delete((binopexp *) this);
      return;
    case function:
      ((functexp *) this)->arg->destroy();
      delete((functexp *) this);
      return;
    case n_op:
      for (int k=0; k < ((n_opexp *) this)->args->size(); k++)
	(*(((n_opexp *) this)->args))[k]->destroy();
      delete ((n_opexp *) this)->args;
      delete ((n_opexp *) this);
      return;
    }
}


