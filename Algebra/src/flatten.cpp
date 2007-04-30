// flatten    
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//	does rearrangements of nested binops and n_ops
//	this version does it recursively. Revised version Nov 22
//    [this needs checking]  flatten returns true if it changed the expression.
//
//   ?? does flatten guarantee any n_op returned has at least two args?
//
//  replacements:  (prefix notation)
//
//	/ ( / D E) C -> / D ( * C E)		
//	/ ( * D E) C  left alone
//	/ ( + D E) C  left alone   (is this inconsistent with distributing?)
//	/ ( ^ D E) C  left alone  (should check if D = C ?)
//	/ B ( / F G) -> / (* B G ) F
//	/ B ( * F G)  left alone
//	/ B ( + F G)  left alone
//	/ B ( ^ F G)  left alone, but not clear if that is best
//	* ( / D E) C -> / (* D C) E		same for all terms in n_op *
//	* ( * D E) C -> * D E C			same for all terms in n_op *
//	* ( + D E) C -> + ( * D C) ( * E C)	same for all terms in n_op *
//	* ( ^ D E) C  left alone 		same for all terms in n_op *
//	+ ( / D E) C -> / ( + D ( * E C)) E  (note / dominates distr)   "
//	+ ( * D E) C  left alone					"
//	+ ( + D E) C -> + D E C						"
//	+ ( ^ D E) C  left alone					"
//	^ ( / D E) C  left alone (might be reviewed)
//		(if C is a pos int, better to replace with / (^ D C) (^ E C)
//	^ ( * D E) C -> prod (^ d C)
//	^ ( + D E) C  expand if C = 2, otherwise leave alone
//	^ ( ^ D E) C -> ^ D (* E A) if D known positive or E and A ints
//	^ B ( / F G)  leave alone (and give up?)
//	^ B ( * F G)  leave alone
//	^ B ( + F G)  leave alone (barely reviewable)
//	^ B ( ^ F G)  leave alone (and give up?)
//	= (/ D E) G   = D (* E G)	
//      sqrt(x^2) -> abs (x)
//	(sqrt prod -> prod sqrt (nonnegs) * sqrt prod others
//      abs x -> x if x known >=0
//	abs prod -> prod abs				this version
	/* shouldn't we do something about 		assumes no ordering
	 *   ^ sqrt(x) 2 -> x and x > 0			among n_op args
	 *   = num f(x)  ->  x = f^{-1}(num)		except that cleanup
	 *   = (^ D E) F if F num or E 1/2		returns <= 1 numval
	 *   = sqrt(D) F -> D = F^2 and F>0		and only as first 
	 *   log product				arg.
	 *   exp sum
	 *   trig abs
	 *   one way or the other on x^2 and x * x
	 ************************************************************/

#include <math.h>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
using namespace std;

#define DBGM(A) DBGFM(INTFL,A) //additional detail from lowest bit on
#define DBG(A) DBGF(INTFL,A)

bool distfrac(expr * & ex);
bool multsort(expr * & ex);

bool flatten(expr * & e)	// flattens expr e wrt n_ops
{
  int k, q;
  expr * tempexp;
  EQCHK(e);

#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif
  DBG(cout << "flatten " << thisdbg << " called for " 
      << e->getInfix() << endl);

  if ((e->etype == unknown)  ||
      (e->etype == fake))
    throw(string("flatten called on unknown or fake expr"));
  if ( (e->etype == numval) || (e->etype == physvart))  return(false);
  bool answer = false;
  if (e->etype == function)			// top level is function
    {
      functexp * efunct = (functexp *) e;
      answer = flatten(efunct->arg);
      if (efunct->f->opty == sqrte) 
	{						// sqrt of product
	  if ( (efunct->arg->etype == n_op) &&
	       (((n_opexp *)efunct->arg)->op->opty == multe))
	    {
	      n_opexp * fargnop = (n_opexp *) efunct->arg;
	      if (fargnop->args->size() < 2) 
		{
		  // this can't happen if we guarantee all n_ops flatten
		  // returns always have at least two args:
		  unnop(efunct->arg);
		  DBGM(cout << "flatten " << thisdbg  << ":  returns " 
		      << e->getInfix()<< endl);
		  return(true); 
		}
	      DBG(cout << "flatten " << thisdbg << 
		   " called sqrt of product " << endl;
		   // e->dbgprint(0)
		   );
	      n_opexp * rootdone = new n_opexp(&mult); // holds part of prod
	      for (k = 0; k < fargnop->args->size(); k++) // extracted frm sqrt
		if (isnonneg((*fargnop->args)[k]))
		  {
		    tempexp = (expr *) new functexp(&sqrtff,
						    (*fargnop->args)[k]);
		    fargnop->MKS += ((*fargnop->args)[k])->MKS * -1.;//9/29/01
		    e->MKS += ((*fargnop->args)[k])->MKS * -0.5;//9/29/01
		    flatten(tempexp);
		    rootdone->addarg(tempexp);
		    for (q = k; q+1 < fargnop->args->size(); q++)
		      (*fargnop->args)[q] = (*fargnop->args)[q+1];
		    fargnop->args->pop_back();
		    k--;
		  }
	      if (fargnop->args->size() == 1) 
		{
		  unnop(efunct->arg);
		  eqnumsimp(e,true);
		  answer = true;
		}
	      else if (fargnop->args->size() == 0) 
		{
		  e->destroy(); e = rootdone; 
		  DBG(cout << "flatten " << thisdbg  << ":  returns " 
		      << e->getInfix()<< endl);
		  return(true); 
		}
	      if (rootdone->args->size() == 0)
		{
		  delete rootdone;
		  DBG({cout << "flatten " << thisdbg 
			 << " return false from sqrt of product" << endl; });
		  return(answer);
		}
	      DBG(cout << "flatten " << thisdbg 
		   << " return from sqrt of product with done "
		   << endl; 
		     // rootdone->dbgprint(2);
		     //    cout << "and not yet done " << endl;
		   //    e->dbgprint(2)
		   );
	      rootdone->addarg(e);
	      e = rootdone;
	      DBG(cout << "flatten " << thisdbg  << ":  returns " 
		  << e->getInfix()<< endl);
	      return(true);
	    }
	  if (efunct->arg->etype == binop)
	    {
	      binopexp * fargbin = ( binopexp * ) efunct->arg;
	      if ((fargbin->op->opty != topowe) ||	// sqrt (x^even num)
		  (fargbin->rhs->etype != numval) ||    //  or sqrt nonneg^num
		  (!lookslikeint(((numvalexp *)fargbin->rhs)->value,q)) ||
		  ((q%2 == 1) && !isnonneg(fargbin->lhs)))
		{
		  DBGM(cout << "flatten " << thisdbg 
			 << ": nosimp " << e->getInfix() << ", returning "
			 << ((answer) ? "true" : "false") << endl;);
		  return(answer);
		}
	      DBGM(cout << "flatten " << thisdbg 
		     << ": about to reduce sqrt x^2n "
		     << e->getInfix()<< endl);
	      efunct->f = &absff; // replace sqrt by abs and halve	      
	      ((numvalexp *)fargbin->rhs)->value *= 0.5; // rhs of topowe
	      answer = true;
	      DBGM(cout << "flatten " << thisdbg << ": first result x^n "
		     << e->getInfix()<< endl;
		     cout << " efunct->f->opty=" << efunct->f->opty 
		     << " e->f->opty=" << ((functexp *) e)->f->opty << endl);
	      if (q == 2)		// 	sqrt (x^2n) -> x^n, 
		{ 				//  sqrt(|v|^n) -> |v|^(n/2)
		  delete fargbin->rhs;		//  then if n=1, -> x	    
		  tempexp = fargbin->lhs;
		  delete fargbin;
		  efunct->arg = tempexp;
		}
	      DBG( cout << "flatten " << thisdbg << " simplified a sqrt"
		   << endl;);
	      DBGM(cout << "flatten " << thisdbg << ": sqrt x^2 -> |x| : "
		     << e->getInfix()<< endl);
	    } // end of sqrt of binop
	} // end of e is sqrt
      
      // e is now an abs, so just continue rather than return

      if (efunct->f->opty == abse) // top op is abs
	{
	  DBGM(cout << "flatten " << thisdbg << ": looking at abs : "
		 << e->getInfix() << endl);
	  if (isnonneg(efunct->arg)) 		// abs of nonneg 
	    {
	      tempexp = efunct->arg;
	      delete e;
	      e = tempexp;
	      DBG(cout << "flatten " << thisdbg  << ":  returns " 
		  << e->getInfix()<< endl);
	      return(true);
	    }
	  else {
	    DBGM(cout << "flatten " << thisdbg << ": arg not nonnegative " 
		   << endl;);
	  }
	  
	  if ((efunct->arg->etype == n_op) && 		// abs prod
	      ((n_opexp *) efunct->arg)->op->opty == multe)
	    {
	      n_opexp * enop = new n_opexp(&mult);
	      n_opexp * argnop = (n_opexp *) efunct->arg;
	      for (k=0; k < argnop->args->size(); k++)
		{
		  if ((*argnop->args)[k]->etype == numval)
		    {
		      ((numvalexp *)((*argnop->args)[k]))->value =
			fabs( ((numvalexp *)((*argnop->args)[k]))->value );
		      enop->addarg((*argnop->args)[k]);
		      continue;
		    }
		  if (isnonneg((*argnop->args)[k]))
		    enop->addarg((*argnop->args)[k]);
		  else
		    enop->addarg(new functexp
					  (&absff,(*argnop->args)[k]));
		}
	      // rmed 2/4/01 need to check	      delete argnop->args;
	      delete argnop;
	      delete e;
	      e = enop;
	      answer = true;
	    }
	  DBGM(cout << "flatten " << thisdbg << ": returning from abs with " 
		 << ((answer) ? "true" : "false") << endl);
	  return(answer);
	} // end of top op is abs
      if (efunct->f->opty == cose) 		// top op is cos
	if ( (efunct->arg->etype == function) &&
	     ( ((functexp *)(efunct->arg))->f->opty == abse))
	     {
	       functexp * temp = (functexp *) efunct->arg;
	       efunct->arg = temp->arg;
	       delete temp;
	       answer = true;
	     }
      if ( (efunct->f->opty == sine) ||	// top op is sin/tan
	   (efunct->f->opty == tane) )		//   and arg is abs 
	if ( (efunct->arg->etype == function) &&	// make |sin x|
	     ( ((functexp *)(efunct->arg))->f->opty == abse)) // ?is
	  {			// this a good thing to do? It does help on
	    ((functexp *)(efunct->arg))->f->opty = // sin^2
	      efunct->f->opty;
	    efunct->f->opty = abse;
	    answer = true;
	  }
      DBGM(cout << "flatten " << thisdbg << ":  returns " 
	      << e->getInfix() << ", returning "
			 << ((answer) ? "true" : "false") << endl);
      return(answer);
    }
  if (e->etype == binop) 				// top op is binop
    {
      binopexp *ebin = (binopexp *) e;
      if (flatten( ebin->lhs)) answer = true;
      if (flatten( ebin->rhs)) answer = true;

      DBG(cout << "flatten " << thisdbg  << ": starting binop:  " 
	  << e->getInfix() << endl);

      switch(ebin->op->opty){
      case equalse:
	DBG(cout << "flatten " << thisdbg  << ": case equalse:  " 
	    << e->getInfix() << endl);
	if (ebin->lhs->etype != binop) {	//   what about a = sqrt b ? 
	  if ((ebin->rhs->etype != binop) || 
	      (((binopexp *)ebin->rhs)->op->opty != divbye)) 
	    {
	      DBGM(cout << "flatten of equality " << thisdbg << ": returns " 
		     << e->getInfix()<< endl
		     << "locs e[" << (int)e << "], e->lhs[" << 
		     (int)(ebin->lhs) << "], e->rhs[" <<
		     (int)(ebin->rhs) << "]" << endl
		     << "returning " << ((answer) ? "true" : "false") 
		     << endl);
	      return(answer);
	    }
	  else			// A = B / C  ->  (A * C) = B
	    {
	      n_opexp * elhs;
	      binopexp * erhs = (binopexp *) ebin->rhs;
	      e->MKS += erhs->rhs->MKS;	// new eqn is C times before
	      if ( (ebin->lhs->etype == n_op) &&
		   ((n_opexp *)ebin->lhs)->op->opty == multe)
		elhs = (n_opexp *) ebin->lhs;
	      else
		{
		  elhs = new n_opexp(&mult);
		  elhs->addarg(ebin->lhs);		  
		  ebin->lhs = elhs;
		}
	      elhs->addarg(erhs->rhs);
	      ebin->rhs = erhs->lhs;
	      // this isn't right	      ebin->rhs->MKS += erhs->rhs->MKS;
	      delete erhs;
	      DBG(cout << "flatten " << thisdbg  << ":  returns " 
		  << e->getInfix()<< endl);
	      return(true);
          } }  // end of if lhs is binop
	else			// lhs IS a binop
	  {
	    if (((binopexp *)ebin->lhs)->op->opty != divbye) 
	      {
		DBGM(cout << "flatten " << thisdbg << ": not divby returns " 
		       << e->getInfix() << ", returning "
		    << ((answer) ? "true" : "false") << endl);
		return(answer);
	      }
	    n_opexp * erhs;			// (A / B) = C -> A = (B * C)
	    binopexp * elhs = (binopexp *) ebin->lhs;
	    if (ebin->rhs->etype == n_op)
	      erhs = (n_opexp *) ebin->rhs;
	    else
	      {
		erhs = new n_opexp(&mult);
		erhs->addarg(ebin->rhs); 
		ebin->rhs = erhs;
	      }
	    erhs->addarg(elhs->rhs);
	    ebin->MKS += elhs->rhs->MKS; // added 12/10/01 JaS
	    ebin->lhs = elhs->lhs; 	// changed 2/4/01 from ebin->rhs
	    delete elhs;
	    DBG(cout << "flatten " << thisdbg  << ":  returns " 
		<< e->getInfix()<< endl);
	    return(true);
	  }
	DBGM(cout << "flatten " << thisdbg 
	       << ": equalse did nothing, returns " 
	       << e->getInfix() << ", returning "
		    << ((answer) ? "true" : "false") << endl;);
	return(answer);
      case divbye:				// top operator is divide
	DBG(cout << "flatten " << thisdbg  << ":  case divbye:  " 
	  << e->getInfix() << endl);
	switch (ebin->lhs->etype) // see if want to fiddle with numerator,
	  {			// done only if it is also a divide
	  case unknown:
	  case fake:
	    throw(string("unknown or fake expr on lhs of div"));
	  case numval:
	  case physvart:
	  case function:
	    break;
	  case binop:
	    {
	      if (((binopexp *)ebin->lhs)->op->opty != divbye)
		break;
	      answer = true;			// (A / B) / C -> A / (B * C)
	      binopexp *elhs = (binopexp *) ebin->lhs;
	      n_opexp * temp = new n_opexp(&mult);
	      temp->addarg(elhs->rhs);
	      temp->addarg(ebin->rhs);
	      elhs->rhs = temp;
	      delete ebin;
	      e = elhs;
	      break;
	    }
	  case n_op:
	    break;
	  } // end of switch on type of lhs of divby,   now fiddle rhs
	ebin = (binopexp *) e; 		// e may have changed since last done
	if (ebin->rhs->etype != binop) 		// (we still have divby as top)
	  {
	    DBGM( cout << "flatten " << thisdbg << ":  divbye in " 
		    << e->getInfix() << " returning "	    
		    << ((answer) ? "true" : "false") << endl);
	    return(answer);
	  }
	{
	  binopexp *erhs =  (binopexp *) ebin->rhs; // only case is rhs = divby
	  if (erhs->op->opty != divbye) {		// ? assume eqnumsimp
	    DBGM( cout << "flatten " << thisdbg << " divbye in " 
		    << e->getInfix() << " returning "
		    << ((answer) ? "true" : "false") << endl);
	    return(answer); 
	  }
	  answer = true;				// A / (B / C) ->
	  n_opexp * temp = new n_opexp(&mult); 		// (A * C) / B
	  temp->addarg(ebin->lhs);
	  temp->addarg(erhs->rhs);
	  ebin->lhs = temp;
	  ebin->rhs = erhs->lhs;
	  delete erhs;			// still have: 	    e = ebin;
	}
	break;
      case topowe:				// top operator is a topow
	DBG( cout << "topow entry in flatten" << endl;);
	switch (ebin->lhs->etype) 		// fiddle with lhs? 
	  {
	  case unknown:
	  case fake:
	    throw(string("unknown or fake expr on lhs of div"));
	  case numval:
	  case physvart:
	  case function:	// should we change sin^2 to 1 - cos^2 ?
	    DBGM( cout << "flatten " << thisdbg << 
		    " topow lhs makes for no improv " << e->getInfix() 
		    << ", returning "
		    << ((answer) ? "true" : "false") << endl);
	    return(answer);
	  case binop:		// binop raised to power
	    {
	      binopexp * elhs = (binopexp *) ebin->lhs;	
	      if ( (elhs->op->opty == divbye) &&     // num power of fraction
		   (ebin->rhs->etype == numval))
		{
		  double powval =((numvalexp *)ebin->rhs)->value;
		  if (powval < 0)
		    {
		      expr * temp = elhs->lhs; 		// if power negative
		      elhs->lhs = elhs->rhs; 		// number, invert
		      elhs->rhs = temp;			// ratio and change 
		      powval = -powval;			// sign of power
		      ((numvalexp *)ebin->rhs)->value =  powval;
		      answer = true;
		    }
		  if ( lookslikeint(powval,q) ||
		       ( ( elhs->lhs->etype == numval ) &&
			 (((numvalexp *)elhs->lhs)->value >= 0)) ||
		       ( ( elhs->rhs->etype == numval ) &&
			 (((numvalexp *)elhs->rhs)->value > 0)) )
		    {
		      binopexp * numer = 
			new binopexp(&topow, elhs->lhs,copyexpr(ebin->rhs));
		      binopexp * denom = 
			new binopexp(&topow, elhs->rhs,ebin->rhs);
		      delete elhs;
		      ebin->lhs = numer; // if power is integer or if 
		      ebin->rhs = denom; // either numerator or denominator
		      ebin->op = &divby;	 // is positive, replace
		      DBG(cout << "flatten " << thisdbg  << ":  returns " 
			  << e->getInfix() << endl);
		      return(true);
		   }
		}
	      if (elhs->op->opty != topowe) break;
	      // try (positive ^ v1)^v2 -> pos ^ (* v1 v2)
	      if (!ispositive(elhs->lhs)) break;
	      n_opexp * temp = new n_opexp(&mult);
	      temp->addarg(elhs->rhs);
	      temp->addarg(ebin->rhs);
	      ebin->lhs = elhs->lhs;
	      ebin->rhs = temp;
	      delete elhs;			// still have: 	    e = ebin;
	      answer = true;
	    }
	    break;		// is that what we need here?
	  case n_op:			// product or sum raised to power
	    {
	      n_opexp *elhs = (n_opexp *) ebin->lhs;
	      DBG(cout << "n_op to power in flatten "
		   << thisdbg << endl;);
	      if (elhs->op->opty == pluse) 	// only sum^2 gets changed
		{
		  if (ebin->rhs->etype != numval) break;  
		  if (!lookslikeint(((numvalexp *)ebin->rhs)->value,q)
		      || (q != 2))  break;
		  n_opexp * temp = new n_opexp(&myplus);
		  for (k=0; k < elhs->args->size(); k++)
		    {
		      temp->addarg(new binopexp(&topow,
			   copyexpr((*elhs->args)[k]), copyexpr(ebin->rhs) ));
		      for (q=k+1; q < elhs->args->size(); q++)
			{
			  n_opexp * temp2 = new n_opexp(&mult);
			  temp2->addarg(copyexpr(ebin->rhs) ); // just a 2
			  temp2->addarg(copyexpr((*elhs->args)[k]));
			  temp2->addarg(copyexpr((*elhs->args)[q]));
			  temp->addarg(temp2);
			}
		    }
		  e->destroy();
		  e = temp;
		}
	      else				// product to power
		{
		  DBG(cout << "product to power in flatten "
		       << thisdbg << e->getInfix() << endl;);
		  if (ebin->rhs->MKS.unknp()) ebin->rhs->MKS.put(0,0,0,0,0);
		  if (!(ebin->rhs->MKS.zerop()))
		    throw(string("flatten hit non-dimensionless exponent"));
		  if (!((ebin->rhs->etype == numval) ||
		      (ebin->lhs->MKS.zerop())))
		    {
		      DBGM(cout << "flatten " << thisdbg << 
			     ": returns false on " 
			     << e->getInfix() << endl;);
		      return(answer);
		    }
		  // can't handle dimensioned quantity to unknown power
		  for (k=0; k < elhs->args->size(); k++)
		      (*elhs->args)[k] = new binopexp(&topow,(*elhs->args)[k],
					     copyexpr(ebin->rhs));
		  if (!elhs->MKS.zerop())
		    elhs->MKS *= ((numvalexp *)ebin->rhs)->value;
		  ebin->rhs->destroy();
		  delete e;
		  e = elhs;
		} // end of else on lhs = plus (ie end of prod to pow)
	      DBG(cout << "flatten " << thisdbg  << ":  returns " 
		  << e->getInfix()<< endl);
	      return(true);
	    }
	  default:
	    throw(string("impossible expr to power in call to flatten"));
	  }  // end of switch on lfs of topow
      default:
	DBG(cerr<<"Possible error:  case not handled by switch"<<endl);
      }  // end of switch on which binop e is
      DBG(cout << "flatten " << thisdbg  << ":  returns " 
	  << e->getInfix() << endl
          << "          no progress on flatten" << endl);
      return(answer);		// added 2/2/01
    }  // end of if binop
  if (e->etype == n_op)				// top level is n_op
    {
      n_opexp *enop = ( n_opexp *) e;	      // flatten each of the args
      for (k=0; k < enop->args->size(); k++) {
	if (flatten((* enop->args)[k])) answer = true;
      }
      //  if just one arg on call, return non - n_op
      //    shouldn't this be delayed, or do we always repeat flatten if true?
      if (enop->args->size() <= 1)
	{
	  unnop(e);
	  DBG(cout << "flatten " << thisdbg  << ":  returns " 
	      << e->getInfix()<< endl);
	  return(true);
	}

      // check for nested like n_ops and combine numvals. This had be
				// explicit here, but now uses cleanup
      k = enop->args->size();	// note: cleanup does not keep track of whether
      cleanup(enop);		// changes have been made. So flatten may 
      if (k != enop->args->size()) answer = true;  // report false negative.
      DBG(cout << "after check in flatten " << thisdbg << ", enop is " 
	   << enop->getInfix() << endl);
      if (enop->args->size() < 2)
	{ unnop(e); 
	  DBG(cout << "flatten " << thisdbg  << ":  returns " 
	      << e->getInfix()<< endl);
	  return(true); } // why only 0, not 1?

      if(enop->op->opty == pluse)	// case n_op e (top level) is plus
	{
	  if (plussort(e)) 
	    {
	      answer = true;
	      if ((e->etype != n_op) || ((n_opexp *)e)->op->opty != pluse)
		{
		  DBG(cout << "flatten " << thisdbg  << ":  returns " 
		      << e->getInfix()<< endl);
		  return(true);
		}
	    }
	  answer = distfrac(e) || answer;
	  DBGM(cout << "flatten " << thisdbg << ": returns " 
			 << (answer ? "true" : "false")  
			 << e->getInfix() << " after sorting a plus" << endl;
	         //   e->dbgprint(2)
		 );
	  return( answer );
	}
      
      n_opexp * temp;

      if(enop->op->opty == multe)	// case n_op e (top level) is mult
	  {
	    // First remove divides
	    n_opexp * newdenom = new n_opexp(&mult);
	    for (k=0; k < enop->args->size(); k++)
	      {
		if (((*enop->args)[k]->etype == binop)  &&
		    (((binopexp *)(*enop->args)[k])->op->opty == divbye))
		  {
		    binopexp * tempbin = (binopexp *)(*enop->args)[k];
		    newdenom->addarg(tempbin->rhs);
		    (*enop->args)[k]=tempbin->lhs;
		    enop->MKS += tempbin->rhs->MKS; // added 6/7
		    answer = true;
		    delete tempbin;
		  }
	      }

	    for (k=0; k < enop->args->size(); k++)
	      for (q = 0; q < newdenom->args->size(); q++)
		if (equaleqs((*enop->args)[k],(*newdenom->args)[q]))
		  {
		    // cancel common terms.  First, adjust the units
		    enop->MKS += (*enop->args)[k]->MKS * -1.;
		    newdenom->MKS += (*newdenom->args)[q]->MKS * -1.;
		    (*enop->args)[k]->destroy();
		    (*newdenom->args)[q]->destroy();
		    (*enop->args)[k] = (*enop->args)[enop->args->size()-1];
		    (*newdenom->args)[q] 
			  =(*newdenom->args)[newdenom->args->size()-1];
		    enop->args->pop_back();
		    newdenom->args->pop_back();
		    k--;  // since the term has changed, do over
		    break;
		  }

	    if (newdenom->args->size() > 0) // need new if as may have shrunk
	      {
		e = new binopexp(&divby, enop, newdenom);
		flatten(e);
		DBG(cout << "flatten " << thisdbg  << ":  returns " 
		    << e->getInfix()<< endl);
		return(true);	// can't continue as e no longer n_op
	      }
	    else newdenom->destroy();

	    // sort and look for factors to combine into topow.	    
	    e = enop;	     // enop has changed if denominator was cancelled.
	    if (multsort(e)) answer = true;
	    if (e->etype == n_op) enop =(n_opexp *) e; 	// if multplus didn't
	    else {
	      DBG(cout << "flatten " << thisdbg  << ":  returns " 
		  << e->getInfix()<< endl);
	      return(answer);			// or did change type
	    }
	      // if no longer an n_op, cant continue here.
	      // Now try to distribute + args.
	    for (k=0; k < enop->args->size(); k++)
	      {			
		n_opexp *repla;
		if (((*enop->args)[k]->etype == n_op)  &&
		    (((n_opexp *)(*enop->args)[k])->op->opty == pluse))
		  {
		    repla = new n_opexp(&myplus);
		    n_opexp * plusfact = (n_opexp *)(*enop->args)[k];
		    for (q=0; q < plusfact->args->size(); q++)
		      {
			temp = new n_opexp(&mult);
			temp->addarg(copyexpr((*plusfact->args)[q]));
			for (int r=0; r < enop->args->size(); r++)
			  if (r != k)
			    temp->addarg(copyexpr((*enop->args)[r]));
			repla->addarg(temp);
		      }
		    bool flattened; // diag
		    enop->destroy();
		    DBGM(cout << "flatten " << thisdbg
			 << " found and fixed a term in * +:" << endl
			 << "        " << repla->getInfix() << endl);
		    expr *replaexp = (expr *) repla;
		    flattened = flatten(replaexp);
		    e = replaexp;
		    DBG(cout << "flatten " << thisdbg  << ", flattened="
			<< (flattened?"true":"false") << ", return " 
			<< e->getInfix() << endl);
		    return(true);
		  }  // end of what to do on found plus inside mult
	      } // end of loop over args of mult
	      DBGM(cout << "flatten " << thisdbg << ": " 
		     << e->getInfix() << ", returning "
		     << ((answer) ? "true" : "false") << endl;);
	      return(answer);
	  } // end of case n_op e is a mult
      throw(string("unknown n_op expr sent to flatten"));
    }  // end of e = n_op  block
#ifdef WITHDBG
  cout << "flatten " << thisdbg << " bombs with" << endl;
  e->dbgprint(5);		// diag
#endif
  throw(string("unknown expr sent to flatten"));
}

void unnop(expr * & e)
{
  if ( (e->etype != n_op) ||
       (((n_opexp *) e)->args->size() >1 ))
    throw(string("unnop should only be called on n_ops with < 2 args"));
  n_opexp * enop = (n_opexp *) e;
  if (enop->args->size() == 1)
    {
      e = (*enop->args)[0];
      delete enop;
      return;
    }
  if (enop->op->opty == multe) {
    e = new numvalexp(1.);
    e->MKS.put(0,0,0,0,0); }
  else e = new numvalexp(0.);
  
  delete enop;
  return;
}

      
