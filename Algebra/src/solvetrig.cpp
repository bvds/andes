// solvetrig.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//
//	findtrigvars		called from maketrigvars and recursively
//	maketrigvars  		called from checkeqs only
//	solvetrigvar  		called from checkeqs only
//	trigsearch		called from solvetrigvar and recursively
//	signisknown		called from solvetrigvar

#include <math.h>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "mconst.h"
using namespace std;

#define DBG(A) DBGF(SLVTRIG,A)
#define DTL(A) DBGFM(SLVTRIG,A)

void trigsimp(expr * & ex);

void findtrigvars( expr * & ex, vector<expr *> * &trigvars);
void maketrigvars(vector<binopexp *> * eqexpr,
		  vector<expr *> * &trigvars);
bool solvetrigvar(const expr * const var, vector<binopexp *> * & eqn);
bool trigsearch(const expr * const arg, expr *& coef,
		const expr * const ex, bool & iscos, expr * & oside);
bool signisknown(const expr * const ex);

/************************************************************************
 *	finds equations involving trig functions, makes list of
 *	the arguments of those functions, and for each a list of
 *	equations which use trig functions of those arguments. 
 *	Then for each argument, looks for a pair of equations
 *	of the form 
 * 		c1 fact1 + k1 fact sin(arg) = 0 (c1,c2,k1,k2 are numbers)
 *	and  	c2 fact1 + k2 fact cos(arg) = 0
 *	to get tan(arg)
 ************************************************************************/
//  In fact, in this code, solvetrigvar requires the signs of fact1 and
//  fact to be determined (except it uses isnonneg instead of ispositive)
//   We should change this, but it produces a mod Pi uncertainty in 
//  the angle which is either wrong or requires adding an new variable
//  taking on only integer values (or even just 0 or 1).

//  This is not powerful enough to solve exlmom3a. We should implement
//  a more powerful algorith. if fact sin(arg) = v1 and fact cos(arg)=v2,
//  we can conclude that fact^2 = v1^2 + v2^2. While this does not 
//  return a more tractible equation for arg, it might help determine
//  fact, v1 and v2.

/************************************************************************
 * void findtrigvars( expr * & ex, vector<expr *> * &trigvars)		*
 *	searches expression ex for sin, cos, or tan functions of arg.	*
 *	If found, finds or adds arg to trigvars				*
 ************************************************************************/
void findtrigvars( expr * & ex, vector<expr *> * &trigvars)
{
  int k;
  EQCHK(ex);
#define DEBUG_FINDETRIGVARS 0
#if DEBUG_FINDTRIGVARS
  DBG( cout << "findTRIG: starting looking at " << ex->getInfix() << endl;);
#endif
  switch (ex->etype)
    {
    case numval:
    case physvart:
      return;
    case function:
      { 
	functexp * exfun = (functexp *)ex;
	if ((exfun->f->opty == sine)  ||
	    (exfun->f->opty == cose)  ||
	    (exfun->f->opty == tane))
	  {
#if DEBUG_FINDTRIGVARS
	    DBG( cout << "findTRIG: found a trig: " 
		      << exfun->getInfix() << endl; );
#endif
	    trigsimp(ex);
	    if (ex->etype == function) exfun = (functexp *)ex;
	    else 
	      {
		if ((ex->etype != n_op) ||
		     ((*((n_opexp *)ex)->args)[1]->etype != function))
		  throw(string(
			"lost trig function in findtrivars, impossible"));
		exfun = (functexp *)(*((n_opexp *)ex)->args)[1];
	      }
	    for (k = 0; k < trigvars->size(); k++)
	      {
		if (equaleqs((*trigvars)[k],exfun->arg)) break;
#if DEBUG_FINDTRIGVARS
		DBG( cout << "findTRIG: no dup with var " 
			  << (*trigvars)[k]->getInfix() << endl; );
#endif
	      }
#if DEBUG_FINDTRIGVARS
	    DBG( if (k < trigvars->size())
		 cout << "findTRIG: matches with trigvar " << k << endl; );
#endif
	    if (k == trigvars->size()) {
#if DEBUG_FINDTRIGVARS
	      DBG( cout << "findTRIG: its a new trig: " << endl; );
#endif
	      trigvars->push_back(copyexpr(exfun->arg));
	    }
	  } // end of if trig function
	findtrigvars(exfun->arg,trigvars);
      }
      return;
    case binop:
      findtrigvars(((binopexp *)ex)->lhs,trigvars);
      findtrigvars(((binopexp *)ex)->rhs,trigvars);
      return;
    case n_op:
      for (k = 0; k < ((n_opexp *)ex)->args->size(); k++)
	findtrigvars((*((n_opexp *)ex)->args)[k],trigvars);
      return;
    case unknown:
    case fake:
    default:
      throw(string("findtrigvars called with unknown expr"));
    }
}


/************************************************************************
 * void maketrigvars( vector<binopexp *> * eqexpr,			*
 *		vector<expr *> * &trigvars)		 		*
 *	adds to  trigvars (at least empty list required on input	*
 ************************************************************************/
void maketrigvars( vector<binopexp *> * eqexpr,
		  vector<expr *> * & trigvars)
{
  DBG( cout << "starting maketrigvars with "
	    << eqexpr->size() << " equations." << endl; );
  VEQCHK(eqexpr);
  for (int k=0; k < eqexpr->size(); k++)
  {
    expr * mtexp = (*eqexpr)[k];
    findtrigvars(mtexp,trigvars);
    if (mtexp->etype != binop) 
      throw(string("In maketrigvars findtrigvars returned non-binop"));
    (*eqexpr)[k] = (binopexp *) mtexp;
  }
}

/************************************************************************
 *  bool solvetrigvar(const expr * const arg, 				*
 *		vector<binopexp *> * & eqn)				*
 * 	looks for two equations of the form  				*
 *		cs*fact1 + ks * fact2 sin(arg) = 0			*
 *	and 	cc*fact1 + kc * fact2 cos(arg) = 0			*
 * 	with fact1 and fact2 known positive (actually nonnegative!?!)	*
 *  if found, replaces one with arg = numval and other with 		*
 *	(cs^2+cc^2)*fact1^2 = (ks^2+kc^2)* fact2^2			*
 ************************************************************************/
bool solvetrigvar(const expr * const arg, vector<binopexp *> * & eqn)
{
  int j, k;
  numvalexp *c2, *k2;
  double ktry, c2byk2;
  expr *fact1, *fact2, *oside, *facttry;
  bool iscos, firstiscos;
  DBG( cout << "Entering solvetrigvar, angle arg=" 
       << arg->getInfix() << endl);
  VEQCHK(eqn);
  for (j = 0; j+1 < eqn->size(); j++) // loop to find first equation of pair
    if (trigsearch(arg, fact2,(expr *)(*eqn)[j], iscos, oside))
      {
	DBG( cout << "First equation " << j << ":  " << (*eqn)[j]->getInfix() 
	     << endl << "          Trigsearch returned coef "
	     << fact2->getInfix() << " and constant term "
	     << oside->getInfix() << " and iscos "
	     << ((iscos) ? "true" : "false") << endl);
	// Unless the signs of oside and fact2 are known, there is a
	// 180^o uncertainty in the arg. We should probably have some
	// mechanism for solving up to that uncertainty, but do not
	// currently. Also, we are cheating, because we are assuming
	// coef is nonzero without justification. But if coef is zero,
	// so is oside, and then angle is completely undetermined.
	DBG( cout << "Sign " << (signisknown(oside)?"is":"is not") 
	     << " known for oside=" <<oside->getInfix() << endl);
	if (!signisknown(oside) || !signisknown(fact2)) continue;
	fact1 = oside;
	firstiscos = iscos;
	// first equation reads fact2 * cos arg + fact1 = 0 (if firstiscos)
	for (k = j+1; k < eqn->size(); k++) // loop to find second equation
	  if (trigsearch(arg, facttry, (expr *)(*eqn)[k], iscos, oside))
	    {
	      DBG( cout << "Second Eqn. " << k << ":  " << (*eqn)[k]->getInfix() 
		   << endl << "          Trigsearch returned coef "
		   << facttry->getInfix() << " and constant term "
		   << oside->getInfix() << " and iscos "
		   << ((iscos) ? "true" : "false") << endl);
	      if (iscos == firstiscos || !(uptonum(facttry,fact2,k2))
		  || !(uptonum(oside,fact1,c2))) continue;
	      DBG( cout << "k2 is " << k2->getInfix() << 
		   ", and c2 is " << c2->getInfix() << endl);
	      // second equation reads
	      // k2 * fact2 * sin arg + c2 * fact1 = 0 (if firstiscos)
	      // Thus tan arg = c2/k2 (if firsticos, else cot arg = c2/k2)
	      // and fact2^2=fact1^2(1+c2^2/k2^2)
	      
	      // check if units definitely differ: 
	      if (!(c2->MKS == k2->MKS || c2->MKS.unknp() || k2->MKS.unknp())) 
		throw(string("tan(angle) can't have dimensions"));
	      // BvdS: how do I know k2->value is nonzero?
	      c2byk2 = c2->value/k2->value;
	      ktry = atan(c2byk2);// this is always in [-Pi/2,Pi/2]
	      if (isnonneg(fact2) == isnonneg(fact1)) ktry += M_PI;
	      if (!firstiscos) ktry = M_PI/2 - ktry;
	      if (ktry < 0 ) ktry += 2*M_PI;
	      if (ktry >= 2*M_PI) ktry -= 2*M_PI;
	      // trigsearch will crash if eq doesn't have zero rhs, so
	      n_opexp *eqlhs = new n_opexp(&myplus);
	      DBG(cout << "Diag1: " << arg->getInfix() << endl);
	      eqlhs->addarg(copyexpr(arg));
	      numvalexp *tempnv = new numvalexp(- ktry);
	      tempnv->MKS.put(0,0,0,0,0);
	      eqlhs->addarg(tempnv);
	      (*eqn)[j]->destroy();
	      (*eqn)[j] = new binopexp(&equals,eqlhs,new numvalexp(0));
	      DBG(cout << "Output new Eqn. " <<j << ": " 
		  << (*eqn)[j]->getInfix() << endl);
	      // just made (*eqn)[j]  arg - ktry = 0
	      ktry = sqrt(1 + c2byk2*c2byk2);
	      if (isnonneg(fact2) == isnonneg(fact1)) ktry *= -1.;
	      // about to make (*eqn)[k] be  ktry * fact1 + fact2 = 0
	      n_opexp * tempnop = new n_opexp(&mult);
	      tempnv = new numvalexp(ktry);
	      tempnv->MKS.put(0,0,0,0,0);
	      tempnop->addarg(tempnv);
	      tempnop->addarg(fact1);
	      eqlhs = new n_opexp(&myplus);
	      eqlhs->addarg(tempnop);
	      eqlhs->addarg(fact2);
	      (*eqn)[k]->destroy();
	      (*eqn)[k] = new binopexp(&equals,eqlhs,new numvalexp(0));
	      oside->destroy();
	      facttry->destroy();
	      DBG(  cout << "Output new Eqn. " << k << ":  " 
		    << (*eqn)[k]->getInfix() << endl);
	      return(true);
	    } // end of checking and implementing matching equation
      }	// end of loop searching for first of pair
  return(false);
}

/************************************************************************
 * bool trigsearch(arg, coef, ex, iscos, oside) 			*
 *	when called on an equation, which must have rhs = 0,   		*
 * 	tries to write ex as oside + coef * cos(arg) = 0,		*
 *	[or oside + coef * sin(arg) = 0] and returns coef, oside, 	*
 *	iscos and true if successful, with iscos = true if cos and 	*
 *	false if sin. 							*
 *	iscos is meaningless if coef is numval 0			*
 *	returns false if any other function of arg found		*
 *   but it calls itself with expr inside the equation too:		*
 *	If the expression is of the form  				*
 *		coef * (cos | sin) (arg) + oside 			*
 *	(coef can be zero), it returns coef, oside, and true, and iscos *
 *	true if cosine, false if sine (otherwise unset?)		*
 *   Note: if it returns false, coef and oside are untouched. If it 	*
 *	returns true, they are valid expr *, though I haven't checked	*
 *	if they might be n_ops of <2 args 				*
 ************************************************************************/
bool trigsearch(const expr * const arg, expr *& coef,
		const expr * const ex, bool & iscos, expr * & oside)
{
  n_opexp * cf = (n_opexp *) NULL;
  n_opexp * ov = (n_opexp *) NULL;
  expr * cfe = (expr *) NULL;
  expr * ove = (expr *) NULL;
  int k;

  // if it returns true, are not coef and oside always nonzero? if so,
  // don't need all the checking below
#define DEBUG_TRIGSEARCH 0
#if DEBUG_TRIGSEARCH
  DBG( cout << "Entering trigsearch with arg " << endl
	    << arg->getInfix() << ", in expr: " << ex->getInfix() << endl; );
#endif
  EQCHK(ex);
  switch(ex->etype)
    {
    case numval:
    case physvart:
      oside = copyexpr(ex);
      coef = new numvalexp(0);
#if DEBUG_TRIGSEARCH
      DBG( cout << "trigsearch returns true on physvar or numval " << endl; );
#endif
      return(true);
    case function:
      {
	functexp * fptr = ( functexp * ) ex;
	if (equaleqs(fptr->arg,arg)) 
	  {
#if DEBUG_TRIGSEARCH
	    DBG(cout << "TRIGSEARCH: found function of correct arg " << endl;);
#endif
	    switch(fptr->f->opty)
	      {
	      case sine:
		iscos = false;
		coef = new numvalexp(1);
		coef->MKS.put(0,0,0,0,0);
		oside = new numvalexp(0);
#if DEBUG_TRIGSEARCH
		DBG( cout << "TRIGSEARCH: its a sine , coef =" <<
		     coef->getInfix() << " and oside =" << oside->getInfix()
		     << endl; );
#endif
		return(true);
	      case cose:
		iscos = true;
		coef = new numvalexp(1);
		coef->MKS.put(0,0,0,0,0);
		oside = new numvalexp(0);
#if DEBUG_TRIGSEARCH
		DBG( cout << "TRIGSEARCH: its a cosine , coef =" <<
		     coef->getInfix() << " and oside =" << oside->getInfix()
		     << endl; );
#endif
		return(true);
	      case tane:
	      case abse:
	      default:
#if DEBUG_TRIGSEARCH
		DBG(cout << "Trigsearch returns false" << endl;);
#endif
		return(false);
	      }
	  }
	else
	  {
	    // I don't understand this else 1/24/01 maybe okay, 2/27
	    if (!trigsearch(arg,cfe,fptr->arg,iscos,ove)) return(false);
	    if 		// what for: ((cfe != (expr *) NULL) && 
	      ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
	      {
		cfe->destroy();
		if (ove != (expr *) NULL) ove->destroy();
#if DEBUG_TRIGSEARCH
		DBG(cout << "Trigsearch returns false" << endl;);
#endif
		return(false);
	      }
	    else		// dont I still need to destroy dfe and ove?
	      {
		cfe->destroy(); ove->destroy();	// added 2/27
		coef = new numvalexp(0);
		oside = copyexpr(ex);
#if DEBUG_TRIGSEARCH
		DBG( cout << "TRIGSEARCH: funct arg not right one. coef =" <<
		     coef->getInfix() << " and oside =" << oside->getInfix()
		     << endl; );
#endif
		return(true);
	      }
	  }
      }
    case binop:
      {
#if DEBUG_TRIGSEARCH
	DBG(cout << "Entering trigsearch on binop " <<ex->getInfix() << endl;);
#endif
	binopexp * bptr = (binopexp *) ex;
	if (bptr->op->opty == equalse)
	  {
	    if ((bptr->rhs->etype != numval) || 
		(((numvalexp *)bptr->rhs)->value != 0))
	      throw(string(
		   "trigsearch doesn't expect equations with nonzero rhs"));
	    if (!trigsearch(arg,cfe,bptr->lhs,iscos,ove)) return(false);
	    if ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
	      {	coef = cfe; oside = ove; return(true); }
	    else { cfe->destroy(); ove->destroy(); return(false); }
	  } // below here we have a non-logical kind of binop
	if ((bptr->op->opty == grte) ||(bptr->op->opty == gree)) return(false);
	//	 below here for non-boolean binop
	if (!trigsearch(arg,cfe,bptr->lhs,iscos,ove)) return(false);
	if (cfe != (expr *) NULL) // I don't think it can be NULL
	  { 
	    if((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
	      {
		cfe->destroy();
		if (ove != (expr *) NULL) ove->destroy();
		return(false);
	      }
	    cfe->destroy();
	  }
	if (ove != (expr *) NULL) ove->destroy();
	if (!trigsearch(arg,cfe,bptr->rhs,iscos,ove)) return(false);
	if ((cfe != (expr *) NULL) && 
	    ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0))
	  {
	    cfe->destroy();
	    if (ove != (expr *) NULL) ove->destroy();
#if DEBUG_TRIGSEARCH
	    DBG(cout << "Trigsearch returns false" << endl;);
#endif
	    return(false);
	  }
	cfe->destroy();
	if (ove != (expr *) NULL) ove->destroy();
	oside = copyexpr(ex);	// corrected from ove =, 2/27
	coef = new numvalexp(0);
#if DEBUG_TRIGSEARCH
	DBG( cout << "TRIGSEARCH: binop no arg right one. coef =" <<
	     coef->getInfix() << " and oside =" << oside->getInfix()
	     << endl; );
#endif
	return(true);
      }
    case n_op:
      if (((n_opexp *)ex)->op->opty == multe)
	{
#if DEBUG_TRIGSEARCH
	  DBG( cout << "Entering trigsearch on mult"<< endl; );
#endif
	  cf = new n_opexp(&mult); // keep empty until cos/sin found
//	  cf->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor
	  ov = new n_opexp(&mult);
//	  ov->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor
	  bool iscosthis;
	  for (k = 0; k < ((n_opexp *)ex)->args->size(); k++)
	    {
	      if (!trigsearch(arg,cfe,(*((n_opexp *)ex)->args)[k],
			      iscosthis,ove)) 
		goto abort;
	      if ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
		{
		  if (cf->args->size() > 0) 
		    goto abort;		// found product of two expressions
		  cf->addarg(copyexpr(ov));
		  cf->addarg(cfe);
		  ov->addarg(ove);
		  iscos = iscosthis;
		  continue;
		}
	      if (cf->args->size() > 0) cf->addarg(copyexpr(ove));
	      ov->addarg(ove);
	      cfe->destroy();
	      continue;
	    }
	  // the code inside the if was executed unconditionally before
	  // 1/24/01, and the else not there. I think thats wrong
	  if (cf->args->size() > 0)
	    {
	      coef = cf;
	      flatten(coef);
#if DEBUG_TRIGSEARCH
	      DBG( cout << "Trigsearch: eqnumsimp on " << coef->getInfix()
			<< endl; );
#endif
	      eqnumsimp(coef,true);
#if DEBUG_TRIGSEARCH
	      DBG( cout << "Trigsearch: returned from eqnumsimp on "
			<< coef->getInfix() << endl; );
#endif
	    }
	  else 
	    {
	      coef = new numvalexp(0);
	      cf->destroy();
	    }
	  oside = ov;
#if DEBUG_TRIGSEARCH
	  DBG(cout << "Trigsearch: flatten on " << oside->getInfix() << endl;);
#endif
	  flatten(oside);
#if DEBUG_TRIGSEARCH
	  DBG( cout << "Trigsearch: eqnumsimp on "
		    << oside->getInfix() << endl; );
#endif
	  eqnumsimp(oside,true);
	  return(true);
	}
      else			// its a plus n_op
	{
#if DEBUG_TRIGSEARCH
	  DBG( cout << "Entering trigsearch on plus"<< endl; );
#endif
	  cf = new n_opexp(&myplus);
	  ov = new n_opexp(&myplus);
	  bool iscosthis;
	  for (k = 0; k < ((n_opexp *)ex)->args->size(); k++)
	    {
	      if (!trigsearch(arg,cfe,(*((n_opexp *)ex)->args)[k],
			      iscosthis,ove)) 
		goto abort;
	      if ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
		{
		  if ((cf->args->size() > 0) && (iscos != iscosthis))
		    goto abort;		// found one sine and one cosine
		  cf->addarg(cfe);
		  ov->addarg(ove);
		  iscos = iscosthis; // may not be set !?
		  continue;
		}
	      else ov->addarg(ove);
	      cfe->destroy();
	      continue;
	    }
	  coef = cf;
	  flatten(coef);
	  eqnumsimp(coef,true);
	  oside = ov;
	  flatten(oside);
	  eqnumsimp(oside,true);
	  return(true);
	}
    case unknown:
    case fake:
    default:
      throw(string("unknown expr type passed to trigsearch"));
    }
 abort:
  {
    cf->destroy();
    ov->destroy();
    if (cfe != (expr *) NULL) cfe->destroy();
    if (ove != (expr *) NULL) ove->destroy();
    return(false);
  }
}

/************************************************************************
 * bool signisknown(expr * const ex)      NAME IS MISLEADING		*
 *	returns true if either ex is known to be nonneg or known to be	*
 *	nonpositive. In principle we should require nonzero as well,	*
 *	but this will cripple it, and if it is zero angle is meaningles	*
 ************************************************************************/
bool signisknown(const expr * const ex)
{
  if (isnonneg(ex)) return(true);
  expr * excopy = copyexpr(ex);
  kmult(excopy, -1.);
  eqnumsimp(excopy,true);
  flatten(excopy);
  bool answer = isnonneg(excopy);
  excopy->destroy();
  return(answer);
}
