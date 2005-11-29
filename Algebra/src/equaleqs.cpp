//  equaleqs.cpp	equality of two expressions
//	bool equaleqs(const expr * exp1, const expr * exp2)
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

/************************************************************************
 *	equaleqs   are two expr exactly the same (but in different 	*
 *		locations)						*
 *	It would probably be better if we said (+ A B) equals (+ B A), 	*
 *		but this doesn't					*
 *	Note: numerical tolerance relative accuracy relerr (=1.e-11)	*
 ************************************************************************/
#include "decl.h"
#include "dbg.h"
#include "extstruct.h"
#include <math.h>
#define DBG(A) DBGF(EQUALEQ,A)
using namespace std;

bool equaleqs(const expr * exp1, const expr * exp2)
{
  bool answer;
  int k;

#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif
  DBG(cout << "entering equaleqs call " << thisdbg << " with " 
      << exp1->getInfix() << " and " << exp2->getInfix() << endl);
  switch(exp1->etype)
    {
    case numval:
      answer = ((exp2->etype == numval) &&
	       (fabs( ((numvalexp *)exp1)->value 
		      - ((numvalexp *)exp2)->value)
		<= RELERR *
		(fabs( ((numvalexp *)exp1)->value) 
		 + fabs( ((numvalexp *)exp2)->value))));
      goto ret;
    case physvart:
      answer = ((exp2->etype == physvart) &&
		(((physvarptr *)exp1)->varindex ==
		 ((physvarptr *)exp2)->varindex));
      goto ret;
    case function:
      answer = ( (exp2->etype == function) &&
		 (((functexp *)exp1)->f->opty ==((functexp *)exp2)->f->opty) &&
		 equaleqs( ((functexp *)exp1)->arg,
			   ((functexp *)exp2)->arg));
      goto ret;
    case binop:
      answer = ( (exp2->etype == binop) &&
		 equaleqs( ((binopexp *)exp1)->lhs,
			   ((binopexp *)exp2)->lhs)  &&
		 equaleqs( ((binopexp *)exp1)->rhs,
			   ((binopexp *)exp2)->rhs));
      goto ret;
    case n_op:
      DBG( cout << "Equaleqs entering n_op with " << endl
	   << exp1->getInfix() << endl << exp2->getInfix() << endl; );
      if (exp2->etype != n_op) goto retno;
      DBG( cout << "Equaleqs exp2 is n_op " << endl;  );
      if ( ((n_opexp *)exp1)->args->size() !=
	   ((n_opexp *)exp2)->args->size() ) goto retno;
      DBG( cout << "Equaleqs 1 & 2 have same size " << endl;  );
      for (k=0; k < ((n_opexp *)exp1)->args->size(); k++) 
	{
	  if (!equaleqs( (*((n_opexp *)exp1)->args)[k],
			 (*((n_opexp *)exp2)->args)[k])) goto retno;
	  DBG( cout << "Args " << k << " agreed." << endl; );
	}
      answer = true;
      goto ret;
    case unknown:
    case fake:
    default:
      throw(string("equaleqs called with invalid exp1"));
    }
 retno:
  answer = false;
 ret:
  DBG( cout << "Returning " << ((answer) ? "true" : "false") 
       << " from equaleqs " << thisdbg << endl);
  return(answer);
}
