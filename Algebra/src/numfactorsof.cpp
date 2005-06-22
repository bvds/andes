// numfactorsof.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include "decl.h"
#include "dbg.h"
#include <cmath>
using namespace std;
#define DBG(A) DBGF(NUMFACT,A)

/************************************************************************
 *	int numfactorsof(const expr * factor,const expr * expression)	*
 *	  returns TWICE the power to which factor occurs as factor of	*
 *	  expression. If the power is not half integral, returns -1	*
 *	  (This is dubious, isn't it? what happens in product x * x^.2  *
 *	NOTE: does not check denominators. Only negative return is a 	*
 *	  signal that power can't be returned				*
 *	NOTE: factors are only found inside mults, powers, sqrt, or	*
 *	  as the whole expression except for numval.			*
 *	NOTE: if factor is 0 or +- 1, returns 0				*
 *	NOTE: if factor is numval and |factor| < 1, returns at most 2	*
 *	NOTE: If factor is numval, returns true only if expression is	*
 *	  numval or mult starting with numval, and that numval, when	*
 *	  divided by sqrt factor, lookslike having an integer quotient	*
 *	NOTE: may return 0 incorrectly on factor = e^p or n_op		*
 ************************************************************************/
int numfactorsof(const expr * factor,const expr * expression)
{
  int k, q;
  double factval;
  double exprval;
  binopexp * binfact;
  binopexp * binexpr;
  n_opexp * nopexpr;
  functexp * ffact;
  functexp * fexp;
  
#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif

  switch(factor->etype) {
  case numval:			// factor is a number
    factval = sqrt(fabs(((numvalexp *)factor)->value));
    if (lookslikeint(factval,q) && (q <= 1)) 
      {
	DBG( cout << "NUMFACT " << thisdbg << " returns 0 on 0 or 1" << endl;);
	return(0);
      }
    switch(expression->etype){
    case numval:
      exprval = ((numvalexp *)expression)->value;
      if (exprval == 0.) return(1000000);
      if (factval <= 1.)	
  	if (lookslikeint( exprval/(factval*factval),q))
	  {
	    DBG( cout << "NUMFACT " << thisdbg 
		 << " returns 2 (max) on factors <= 1" << endl;);
	    return (2);
	  }
	else if (lookslikeint( exprval/(factval),q)) return (1);
  	else 
	  {
	  DBG( cout << "NUMFACT " << thisdbg 
	       << " returning 0 on number factor on number" << endl;);
	  return(0);
	  }
      for (k=0;;k++){
  	exprval = exprval / factval;
  	if (!lookslikeint(exprval,q)) return(k);
      }
    case physvart:		// expression is a physvar
      DBG(cout << "NUMFACT " << thisdbg << 
	  " returning 0 on number factor on physvar" << endl;);
      return(0);
    case function:		// expression is a function
      if (((functexp *)expression)->f->opty == sqrte)
  	{
  	  q = numfactorsof(factor, ((functexp *)expression)->arg);
	  DBG( cout << "NUMFACT " << thisdbg << " returning "
		 << ((q%2 == 0) ? q/2 : -1)
		 << " from number factor on sqrt expression" << endl;);
  	  if (q%2 == 0) return (q/2); else return(-1);
  	}
      if (((functexp *)expression)->f->opty == abse)
	{
	  DBG(cout << "NUMFACT " << thisdbg << 
	      " returning dubious answer on abse" << endl;);
	  return(numfactorsof(factor, ((functexp *)expression)->arg));
				// ??  is x a factor of |x| ?
	}
      else
	{
	  DBG(cout << "NUMFACT " << thisdbg << 
	      " returning 0 on numval factor of function";);
	  return(0);
	}
      
    case binop:			// expr is binop, factor still numval
      binexpr = (binopexp *)expression;
      if (binexpr->op->opty == divbye)
	{
	  DBG(cout << "NUMFACT " << thisdbg << 
	      " number factor of ratio expr" << endl;);
	  return(numfactorsof(factor, ((binopexp *)expression)->lhs));
	}
      if (binexpr->op->opty == topowe)
  	{
	  DBG(cout << "NUMFACT " << thisdbg << 
	      " working on number factor of binop" << endl;);
  	  if (binexpr->rhs->etype != numval) return(0);
  	  if (((numvalexp *)binexpr->rhs)->value <=0) return(0);
  	  if ((q = numfactorsof(factor,binexpr->lhs)) <= 0) return(q);
  	  if (lookslikeint(q * ((numvalexp *)binexpr->rhs)->value,q))
  	    return(q);
  	  else return(-1);
  	}
      if (binexpr->op->opty == equalse)
	{
	  DBG(cout << "NUMFACT " << thisdbg << 
	      " working on number factor of equalse" << endl;);
	  return(min(numfactorsof(factor,binexpr->lhs),
		     numfactorsof(factor,binexpr->rhs)));
	}
      return(0);		// only binops considered are ^ and / and =
    case n_op:			// expr is n_op, factor still numval
      nopexpr = (n_opexp *) expression;
      if (nopexpr->op->opty == pluse)
  	{
	  if (nopexpr->args->size() == 0) return(0);
  	  q = numfactorsof(factor, (*nopexpr->args)[0]);
  	  for (k=1; k < nopexpr->args->size(); k++)
  	    q = min(q,numfactorsof(factor, (*nopexpr->args)[k]));
	  DBG(cout << "NUMFACT " << thisdbg << " returning " << q 
	      << " on numval factor of n_op +";);
  	  return(q);
  	}
      if (nopexpr->op->opty == multe)
  	{				// this doesn't treat returns of -1
  	  q = 0;			// properly
	  for (k=0; k < nopexpr->args->size(); k++)
	    q += numfactorsof(factor, (*nopexpr->args)[k]);
	  DBG( cout << "NUMFACT " << thisdbg << " returning " << q 
	       << " on numval factor of n_op *";);
  	  return(q);
  	}
      throw(string("unknown n_op as expression in numfactorsof"));
    case unknown:
    case fake:
    default:
      throw(string("unknown expr as expression in numfactorsof"));
    } // end of case factor == numval
  case physvart:				// factor is a physvar
    switch(expression->etype){
    case numval:
      if (((numvalexp *)expression)->value == 0.) return(1000000);
      DBG(cout << "NUMFACT " << thisdbg << 
	  " returning 0 on physvar factor of numval" << endl;);
      return(0);
    case physvart:
      {
	bool retb = (((physvarptr *)expression)->varindex ==
		     ((physvarptr *)factor)->varindex);
	DBG(cout << "NUMFACT " << thisdbg << " returning " 
	    << ((retb) ? 2 : 0)
	    << " from physvar in physvar" << endl;);
	return ( (retb) ? 2 : 0);
      }
    case function:		// physvar factor of function?
      if (((functexp *)expression)->f->opty == sqrte)
  	{
	  q = numfactorsof(factor, ((functexp *)expression)->arg);
	  DBG( cout << "NUMFACT " << thisdbg << " returning " 
	       << ((q%2 == 0) ? q/2 : -1)
		 << " from physvar of sqrt" << endl;);
  	  if (q%2 == 0) return (q/2); else return(-1);
  	}
      if (((functexp *)expression)->f->opty == abse) 
	{
	  DBG( cout << "NUMFACT " << thisdbg << 
	       " returning dubious on physvar in abs" << endl;);
	  return(numfactorsof(factor, ((functexp *)expression)->arg));
	}
      else
	{
	  DBG( cout << "NUMFACT " << thisdbg << 
	       " returning 0 on physvar in function" << endl;);
	  return(0);
	}
    case binop:				// physvar factor of binop
      binexpr = (binopexp *)expression;
      if (binexpr->op->opty == divbye)
	{
	  DBG( cout << "NUMFACT " << thisdbg << 
	       " physvar factor of ratio expr" << endl;);
	  return(numfactorsof(factor, ((binopexp *)expression)->lhs));
	}
      if (binexpr->op->opty == topowe)
  	{
	  DBG(cout << "NUMFACT " << thisdbg << 
	      " working on physvar factor of binop" << endl;);
  	  if (binexpr->rhs->etype != numval) return(0);
  	  if (((numvalexp *)binexpr->rhs)->value <=0) return(0);
  	  if ((q = numfactorsof(factor,binexpr->lhs)) <= 0) return(q);
  	  if (lookslikeint(q * ((numvalexp *)binexpr->rhs)->value,q))
  	    return(q);
  	  else return(-1);
  	}
      if (binexpr->op->opty == equalse)
	{
	  DBG(cout << "NUMFACT " << thisdbg << 
	      " working on physvar factor of equalse" << endl;);
	  int q1 = numfactorsof(factor,binexpr->lhs);
	  int q2 = numfactorsof(factor,binexpr->rhs);
	  DBG( cout << "NUMFACT " << thisdbg << " returning " 
	       << min(q1,q2) 
		 << " on physvar in equals " << q1 << " " << q2<< endl;);
	  return(min(q1,q2));
	}
      DBG(cout << "NUMFACT " << thisdbg << 
	  " returning 0 from physvar on binop" << endl;);
      return(0);		// only binops considered are ^ and / and =
    case n_op:				// physvar factor of n_op?
      nopexpr = (n_opexp *) expression;
      if (nopexpr->args->size() == 0) return(0);
      if (nopexpr->op->opty == pluse)
  	{
	  DBG(cout << "NUMFACT " << thisdbg << " physvar on " 
	      << nopexpr->getInfix() << endl;);
  	  q = numfactorsof(factor, (*nopexpr->args)[0]);
  	  for (k=1; k < nopexpr->args->size(); k++)
  	    q = min(q,numfactorsof(factor, (*nopexpr->args)[k]));
	  DBG( cout << "NUMFACT " << thisdbg << " returning " << q 
		 <<" on physvar factor of n_op +" << endl;);
  	  return(q);
  	}
      if (nopexpr->op->opty == multe)
  	{
	  DBG( cout << "NUMFACT " << thisdbg << " physvar on " 
	       << nopexpr->getInfix() << endl;);
  	  q = 0;
	  for (k=0; k < nopexpr->args->size(); k++)
	    q += numfactorsof(factor, (*nopexpr->args)[k]);
	  DBG( cout << "NUMFACT " << thisdbg << " returning " << q 
		 << " on physvar factor of n_op *" << endl;);
  	  return(q);
  	}
      throw(string("unknown n_op as expression in numfactorsof"));
    case unknown:
    case fake:
    default:
      throw(string("unknown expr as expression in numfactorsof"));
    } // end of case physvart
  case function:				// factor is a function
    ffact = (functexp *) factor;
    DBG( cout << "NUMFACT " << thisdbg << 
	 " working on function factor, no further debug" << endl;);
    switch(expression->etype) {
    case numval:
      if (((numvalexp *)expression)->value == 0.) return(1000000);
    case physvart:
      return(0);
    case function:		// this looks wrong!!!
      fexp = (functexp *) expression;
      if (fexp->f->opty != ffact->f->opty) return(0); // ignore sqrt expr?
      if ( (fexp->f->opty == sqrte)  ||
  	   (fexp->f->opty == abse)) 
  	return (numfactorsof(ffact->arg,((functexp *) expression)->arg));
      if (equaleqs(fexp->arg,((functexp *) expression)->arg))
	return(2);
      return(0);
    case binop:					// function factor of binop?
      binexpr = (binopexp *)expression;
      if (binexpr->op->opty == divbye)
  	return(numfactorsof(factor, ((binopexp *)expression)->lhs));
      if (binexpr->op->opty == topowe)
  	{
  	  if (binexpr->rhs->etype != numval) return(0);
  	  if (((numvalexp *)binexpr->rhs)->value <=0) return(0);
  	  if ((q = numfactorsof(factor,binexpr->lhs)) <= 0) return(q);
  	  if (lookslikeint(q * ((numvalexp *)binexpr->rhs)->value,q))
  	    return(q);
  	  else return(-1);
  	}
      if (binexpr->op->opty == equalse)
	return(min(numfactorsof(factor,binexpr->lhs),
		   numfactorsof(factor,binexpr->rhs)));
      return(0);		// only binops considered are ^ and /
    case n_op:					// function factor of n_op?
      nopexpr = (n_opexp *) expression;
      if (nopexpr->op->opty == pluse)
  	{
  	  q = numfactorsof(factor, (*nopexpr->args)[0]);
  	  for (k=1; k < nopexpr->args->size(); k++)
  	    q = min(q,numfactorsof(factor, (*nopexpr->args)[k]));
  	  return(q);
  	}
      if (nopexpr->op->opty == multe)
  	{
  	  q = 0;
	  for (k=0; k < nopexpr->args->size(); k++)
	    q += numfactorsof(factor, (*nopexpr->args)[k]);
  	  return(q);
  	}
      throw(string("unknown n_op as expression in numfactorsof"));
    case unknown:
    case fake:
    default:
      throw(string("unknown expr as expression in numfactorsof"));
    } // end of factor is function
  case binop:					// factor is a binop
    binfact = (binopexp *) factor;
    if (binfact->op->opty == divbye) // count a/b as factor of anything
      return(numfactorsof(binfact->lhs,expression)); //   a is a factor of.
    switch(expression->etype) {
    case numval:
      if (((numvalexp *)expression)->value == 0.) return(1000000);
    case physvart:
    case function:
      return(0);
    case binop:					// binop factor of binop ?
      binexpr = (binopexp *) expression;
      if (binexpr->op->opty != binfact->op->opty) return(0);
      if (!equaleqs(binexpr->lhs,binfact->lhs)) return(0);
      if (equaleqs(binexpr->rhs,binfact->rhs)) return(2);
      if (binfact->op->opty == topowe) // both factor and expr are topowe
	switch (binexpr->rhs->etype) {
	case numval:
	  if (binfact->rhs->etype != numval) return(0);
	  exprval = ((numvalexp *)binexpr->rhs)->value /
	    ((numvalexp *)binfact->rhs)->value;
	  if (lookslikeint(2*exprval,q) && q > 0) return(q);
	  else return(0);
	case physvart:
	case function:
	case binop:
	case n_op:
	  return(0);		// this may miss something
	case unknown:
	case fake:
	default:
	  throw(string("from numfactorsof"));
	}
      if (binexpr->op->opty == equalse)
	return(min(numfactorsof(factor,binexpr->lhs),
		   numfactorsof(factor,binexpr->rhs)));
      return(0);
    case n_op:				// binop factor of n_op ?
      nopexpr = (n_opexp *) expression;
      if (nopexpr->op->opty != multe) return(0);
      q = 0;
      for (k=0; k < nopexpr->args->size(); k++)
	q =+ numfactorsof(factor,(*nopexpr->args)[k]);
      return(q);
    case unknown:
    case fake:
    default:
      throw(string("unknown expr as expression in numfactorsof"));
    }
  case n_op:
	// I guess I couldn't figure out what to do for factor = n_op
	// for + it is hard to do and for * silly. Here is a trivial
				// attempt
    DBG(cout << "NUMFACT " << thisdbg << 
	" working on n_op factor, no further debug" << endl;);
    switch(expression->etype) {
    case numval:
      if (((numvalexp *)expression)->value == 0.) return(1000000);
    case physvart:
      return(0);
    case function:			// n_op factor of function?
      if (((functexp *)expression)->f->opty == sqrte)
	{
	  q = numfactorsof(factor, ((functexp *)expression)->arg);
	  if (q%2 == 0) return (q/2); else return(-1);
	}
      if (((functexp *)expression)->f->opty == abse)
	return(numfactorsof(factor, ((functexp *)expression)->arg));
      else return(0);
    case binop:				// n_op factor of binop?
      binexpr = (binopexp *)expression;
      if (binexpr->op->opty == divbye)
	return(numfactorsof(factor, ((binopexp *)expression)->lhs));
      if (binexpr->op->opty == topowe)
	{
	  if (binexpr->rhs->etype != numval) return(0);
	  if (((numvalexp *)binexpr->rhs)->value <=0) return(0);
	  if ((q = numfactorsof(factor,binexpr->lhs)) <= 0) return(q);
	  if (lookslikeint(q * ((numvalexp *)binexpr->rhs)->value,q))
	    return(q);
	  else return(-1);
	}
      if (binexpr->op->opty == equalse)
	return(min(numfactorsof(factor,binexpr->lhs),
		   numfactorsof(factor,binexpr->rhs)));
      return(0);		// only binops considered are ^ and /
      
    case n_op:				// n_op factor of n_op?
      if (equaleqs(factor,expression)) return (2);
      else return(0);
    case unknown:
    case fake:
    default:
      throw(string("unknown expr as expression in numfactorsof"));
    }
    
  case unknown:
  case fake:
  default:
    throw(string("factor sent to numfactors unknown expr"));
  }
}
