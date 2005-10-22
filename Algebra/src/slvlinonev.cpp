// bool slvlinonev(ex, var)
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//   checks to see if ex is linear in given var, with numerical coef, and if
//	so rewrites it as a (partial) solution and returns true
// bool linvarcoefs(ex, var, numer, coef) works with nonnumerical coef


#include "decl.h"
#include <math.h>
#include <float.h>
#include "dbg.h"
#include "extoper.h"
#include "extstruct.h"
using namespace std;

#define DBG(A) DBGF(LINONEV,A)
#define DBGM(A) DBGFM(LINONEV,A)

/************************************************************************
 *  bool slvlinonev(binopexp * & eq, const physvar * var)		*
 *	checks if eq is a linear equation with constant coef in that 	*
 *	variable (though the constant term may be an arbitrary 		*
 *	expression in other variables). If so, it rewrites the eq	*
 *	as an assignment and returns true. Otherwise eq unchanged.	*
 ************************************************************************/
bool slvlinonev(binopexp * & eq, const varindx var)	
{

  binopexp *thbin;
  expr * theq;
  expr * coef = NULL;
  expr * kex = NULL;

  bool encorep;
  
  DBG( cout << "Entering slvlinonev" << endl; );
  DBGM( cout << "detailed diagnostics enabled" << endl; );
  
  if (powonev((expr *)eq,var) != 1) return(false); // insist linear in one var
  if (eq->op->opty != equalse)
    return(false);		// insist it is an equation
  DBG( cout << "slvlinonev: eq: " << eq->getInfix()
	    << " meets preliminary requirements" << endl; );
  theq = copyexpr(eq);
  thbin = (binopexp *) theq;
  if ((thbin->rhs->etype != numval) || ((numvalexp *)thbin->rhs)->value != 0)
    normexpr(theq);
  thbin = (binopexp *) theq;	// added 1/28/01 - can't understand how it was
  theq = thbin->lhs;
  DBG( cout << "slvlinonev about to linvarcoefs" << endl; );
  if (!linvarcoefs(theq,var,coef,kex))
    {
      thbin->destroy();
      return(false);
    }
  DBG( cout << "linvarcoefs returned true" << endl);
  encorep = true;
  while (encorep) {
    encorep = flatten(coef);
    eqnumsimp(coef,true);
  }
  if ((coef->etype != numval) || 
      (fabs(((numvalexp *)coef)->value) < 2.0*DBL_MIN))
    {
      DBG( cout << "but coef not nonzero number" << endl);
      coef->destroy();
      kex->destroy();
      thbin->destroy();
      return(false);
    }
  kmult(kex,-1./((numvalexp *)coef)->value);
  coef->destroy();
  thbin->lhs->destroy();
  thbin->rhs->destroy();
  thbin->lhs = new numvalexp(var);
  thbin->rhs = kex;
  eq = thbin;
  return(true);
}


/************************************************************************
 * bool linvarcoefs(const expr * ex, const varindx var, 
 *	expr * & numer, expr * & coef)
 *	returns true if ex is linear in var (with coef and const 
 *	possibly expressions in other variables) and it implies 
 *	var = numer/coef
 *	numer and coef 	should be a null pointers on call
 *  All functions, and binops other than a top level =,
 *	or topow to positive int, containing var, will return false
 ************************************************************************/
bool linvarcoefs(const expr * ex, const varindx var, 
	expr * & coef, expr * & numer)
{
  int k, q;

#ifdef WITHDBG
  unsigned long thisdbg = ++dbgnum;
#endif
 DBG(cout << "linvarcoefs " << thisdbg << ", var " 
      << (*canonvars)[var]->clipsname
      << " on expr "<< endl << ex->getInfix() << endl);
  if (numer != NULL) {
    //    throw(string("linvarcoefs called with non-null numer"));
    cerr << "linvarcoefs called with non-null numer" << endl;
    cout << "linvarcoefs called with non-null numer" << endl;
  }
  if (coef != NULL) {
    //    throw(string("linvarcoefs called with non-null coef"));
    cerr << "linvarcoefs called with non-null coef" << endl;
    cout << "linvarcoefs called with non-null coef" << endl;
  }
  switch (ex->etype)
    {
    case unknown:
    case fake:
      throw("linvarcoefs called on unknown or fake expr");
    case numval:
      numer = copyexpr(ex);
      coef = new numvalexp(0);
      DBG(cout << "linvarcoefs " << thisdbg << " returning numval" << endl);
      return (true);
    case physvart:
      if (((physvarptr *)ex)->varindex == var) {
	numer = new numvalexp(0); 
	coef = new numvalexp(1); 
        coef->MKS.put(0,0,0,0,0); }
      else { coef = new numvalexp(0); numer = copyexpr(ex); }
      DBG(cout << "linvarcoefs " << thisdbg << " returning physvar" << endl);
      return(true);
    case binop:
      {
	binopexp * thbin;
	thbin = (binopexp *) ex;
	switch (thbin->op->opty)
	  {
	  case equalse:
	  case grte:
	  case gree:
	    {
	      if (!linvarcoefs(thbin->lhs,var,coef,numer)){
		DBG(cout << "linvarcoefs " << thisdbg << " returning false" 
		    << endl);
		return(false);
	      }
	      expr * rn = NULL;
	      expr * rd = NULL;
	      if (!linvarcoefs(thbin->rhs,var,rd,rn))
		{
		  delete numer; numer = NULL;
		  delete coef; coef = NULL; // fixed 2/3/01
		  DBG(cout << "linvarcoefs " << thisdbg << " returning false" 
		      << endl);
		  return(false);
		}
	      // This makes the error more clear, but still not found...
	      DBG(cout << " coef=" << coef->getInfix() 
		  << " numer=" << numer->getInfix() 
		  << " rd=" << rd->getInfix() 
		  << " rn=" << rn->getInfix() << endl);
	      // Next two exe lines added 8/14/02 JaS because perhaps rn and
	      // rd need reduction (0 * var may cause trouble)
	      eqnumsimp(rn,true);	// added 8/14/02 JaS   made no change
	      eqnumsimp(rd,true);	// added 8/14/02 JaS   made no change
	      apluskb(numer,rn,-1.);
	      apluskb(coef,rd,-1.);
	      delete rn;
	      delete rd;
	      flatten(coef);
	      eqnumsimp(coef,true);
	      numvalexp * dfact = normexpr(coef);
	      if(dfact==NULL){
		delete numer; numer = NULL;
		delete coef; coef = NULL; 
		DBGM(cout << "linvarcoefs " << thisdbg << " returning:  " 
		     << "coef=0, so it is not linear in var" << endl);
		return(false);
	      }
	      dfact->value = 1./dfact->value;
	      dfact->MKS *= -1;
	      kmult(numer,dfact);
	      DBG(cout << "linvarcoefs " << thisdbg 
		  << " returning true from eq, coef " << coef->getInfix() 
		  << ", numer " << numer->getInfix() << endl);
	      return(true);
	    }
	  case divbye:
	    if (doesnthave(thbin,var) )
	      { coef = new numvalexp(0); numer = copyexpr(ex); 
		  DBG(cout << "linvarcoefs " << thisdbg << " returning divby" 
		      << endl);
	      return(true);}
	    if (!doesnthave(thbin->rhs,var)){
		  DBG(cout << "linvarcoefs " << thisdbg << " returning false" 
		      << endl);
		  return(false);
	    }
	    if (!linvarcoefs(thbin->lhs,var,coef,numer)){
	      DBG(cout << "linvarcoefs " << thisdbg << " returning false" 
		  << endl);
	      return(false);
	    }
	    numer = new binopexp(&divby,numer,copyexpr(thbin->rhs));
	    coef = new binopexp(&divby,coef,copyexpr(thbin->rhs));
	    DBG(cout << "linvarcoefs " << thisdbg << " returning divby" 
		<< endl);
	    return(true);
	  case topowe:
	    if (doesnthave(thbin,var) )
	      { coef = new numvalexp(0); numer = copyexpr(ex); return(true);}
	    DBG(cout << "linvarcoefs " << thisdbg << " returning false" 
		<< endl);
	    return(false);
	  default:
	    throw("Undefined binop in "__FILE__);
	  } // end of switch on binop type
      } // end of case binop
      case n_op:
      {
	DBGM(cout << "Entering linvarcoefs n_op" << endl);
	n_opexp *cf;
	n_opexp *ov;
	n_opexp *thnop = (n_opexp *) ex;
	if (thnop->op->opty == multe) {
	  cf = new n_opexp(&mult);
	  ov = new n_opexp(&mult);
	  for (k=0; k < thnop->args->size(); k++){
	    DBGM( cout << "Linvarcoefs n_op mult arg " << k << endl; );
	    if (!linvarcoefs((*thnop->args)[k],var,coef,numer))
	      { cf->destroy(); ov->destroy(); 
	      DBG(cout << "linvarcoefs " << thisdbg << " returning false" 
		  << endl);
	      return(false); }
	    DBGM( cout << "Linvarcoefs mult arg " << k<< " true" <<endl; );
	    if ((coef->etype != numval) || ((numvalexp *)coef)->value != 0)
	      {
		if (cf->args->size() > 0) {
		  cf->destroy(); ov->destroy(); 
		  coef->destroy(); numer->destroy();
		  coef = NULL; numer = NULL; 
		  DBG(cout << "linvarcoefs " << thisdbg << " returning false" 
		      << endl);
		  return(false); 
		}
		cf->addarg(coef);
		coef = NULL;
		for (q = 0; q < ov->args->size(); q++)
		  cf->addarg(copyexpr((*ov->args)[q]));
		ov->addarg(numer); numer = NULL;
	      }
	    else 
	      {
		if (cf->args->size() > 0) cf->addarg(copyexpr(numer));
		ov->addarg(numer); numer = NULL;
		coef->destroy(); coef = NULL;
	      }
	  }
	  DBGM( cout << "Linvarcoefs mult preparing return true" << endl; );
	  if (cf->args->size() == 0) {
	    coef = new numvalexp(0);
	    cf->destroy();
	  }
	  else {
	    coef = cf;
	    flatten(coef);
	    eqnumsimp(coef,true);
	  }
	  numer = ov;
	  flatten(numer);
	  eqnumsimp(numer,true);
	  DBGM( cout << "Linvarcoefs " << thisdbg << "mult returning " 
		<< coef->getInfix()
		<< " coef and const term " << numer->getInfix() << endl);
	  return(true);
	} // end of n-op mult
	if (thnop->op->opty == pluse)
	  cf = new n_opexp(&myplus);
	  ov = new n_opexp(&myplus);
	  for (k=0; k < thnop->args->size(); k++)
	    {
	      DBGM( cout << "Linvarcoefs plus arg " << k << endl; );
	      if (!linvarcoefs((*thnop->args)[k],var,coef,numer))
		{ cf->destroy(); ov->destroy(); 
		DBG(cout << "linvarcoefs " << thisdbg << " returning false" 
		      << endl);
		  return(false); }
	      DBGM(cout << "Linvarcoefs plus arg "<< k << " true" << endl);
	      cf->addarg(coef); coef = NULL;
	      ov->addarg(numer); numer = NULL;
	    }
	  DBGM( cout << "Linvarcoefs plus preparing return true" << endl);
	  coef = cf;
	  flatten(coef);
	  eqnumsimp(coef,true);
	  numer = ov;
	  flatten(numer);
	  eqnumsimp(numer,true);
	  DBGM( cout << "Linvarcoefs " << thisdbg << " plus returning coef " 
		<< coef->getInfix()
		<< " and const term " << numer->getInfix() << endl);
	  return(true);
	} // end of n-op plus
      throw(string("can't get here in linvarcoefs"));
    case function:
      if (powonev(((functexp *) ex)->arg,var) != 0){
	DBG(cout << "linvarcoefs " << thisdbg << " returning false" 
	    << endl);
	return(false);
      } else { 
	coef = new numvalexp(0); numer = copyexpr(ex); 
	DBG(cout << "linvarcoefs " << thisdbg << " returning function" 
	    << endl);
	return(true);
      }
    }
  throw("got to impossible place in linvarcoefs");
}

/************************************************************************
 * bool doesnthave(const expr * ex, const varindx var)			*
 *	true if var does not appear in expression ex			*
 ************************************************************************/
bool doesnthave(const expr * ex, const varindx var)
{
	int k;
	switch(ex->etype)
    {
    case numval:
      return(true);
    case physvart:
      return (((physvarptr *)ex)->varindex != var);
    case function:
      return (doesnthave(((functexp *)ex)->arg,var));
    case binop:
      return (doesnthave(((binopexp *)ex)->lhs,var) &&
	      doesnthave(((binopexp *)ex)->rhs,var));
    case n_op:
      for (k=0; k < ((n_opexp *)ex)->args->size(); k++)
	if (!doesnthave((*((n_opexp *)ex)->args)[k],var)) return(false);
      return(true);
    case unknown:
    case fake:
    default:
      throw(string("doesnthave called with unknown expr"));
    }
}
