// void substin(expr * & target, const expr * assign)
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include "decl.h"
#include "extstruct.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(SUBST,A)

/************************************************************************
 *  bool substin(expr * & target, const binopexp * assign)		*
 *	assign is an equation of the form physvar v = numval.		*
 *	every occurence of v in target is replaced by numval.		*
 *	eqnumsimp should be called on result of top level call if true	*
 *  returns true if change made to target				*
 ************************************************************************/
bool substin(expr * & target, const binopexp * assign)
{				// this could be made more general by not
				// casting value not numvalexp
  if (  (( assign->rhs)->etype != numval) || 
	(( assign->lhs)->etype != physvart))
    throw(string("substin called with assign not var = number"));
  numvalexp *value = (numvalexp *)( assign->rhs);
  varindx replace = ((physvarptr *)( assign->lhs))->varindex;
  DBG( { cout << "replacing " << (*canonvars)[replace]->clipsname << " with "
	      << value->getInfix() << " in" << endl;
	 target->dbgprint(2); } );
  bool answer = false;
  switch (target->etype)
    {
    case numval:
      return(false);
    case physvart:
      if (((physvarptr *)target)->varindex == replace) {
	((physvarptr *)target)->destroy();  //remove previous quantity
	target = copyexpr(value);
	return(true); }
      else return(false);
    case function:
      return(substin(((functexp *)target)->arg, assign));
    case binop:
      answer = substin(((binopexp *)target)->lhs, assign) || answer;
      answer = substin(((binopexp *)target)->rhs, assign) || answer;
      return(answer);
    case n_op:
      {
	for (int k = 0; k < ((n_opexp *)target)->args->size(); k++)
	  answer = substin((*((n_opexp *)target)->args)[k], assign) || answer;
	return(answer);
      }
    case unknown:
    case fake:
    default:
      throw(string("unknown or fake expression in substin first arg"));
    }
}
