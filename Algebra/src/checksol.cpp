// checksol.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//    version with choice of relerr. Call with RELERR for problem solution
//    and checking, and with default relerr (=0.01?) for student equations.
#include <math.h>
#include "decl.h"
#include "extoper.h"
#include "extstruct.h"
#include "mconst.h"
#include "dbg.h"

// if 1 degree is a numval with value 1, we have FAKEDEG
#define DTL(A) DBGF(CHKSOL,A)
#define DBG(A) DBGF(OUTSOL,A)

#ifdef FAKEDEG
  #define DEG2RAD DEGTORAD
#else
  #define DEG2RAD 1.
#endif

class answitherr { // evaluation of an expression with error est
  public:
    double value;
    double abserr;
};

answitherr* evalexpr(const expr* const ex, const vector<double>* const sols,
		     const double reltverr);


/************************************************************************
 * int checksol(eqn,sols,reltverr)                                      *
 *      plugs the solution values in sols into the equation eqn.        *
 *      compares descrepancy with expection using relative err reltverr *
 * returns                                                              *
 *    0 if seems within error bars                                      *
 *    1 if within 100 * error bars                                      *
 *    2 if not within 100 * error bars                                  *
 * NOTE: currently the error bar calculation has faults                 *
 ************************************************************************/
int checksol(const binopexp* const eqn, const vector<double>* const sols,
	     const double reltverr) {
  DTL(cout << "entered checksol" << endl;);
  expr* eqexpr = copyexpr(eqn); // g++ refused this without copyexpr
  answitherr* value = evalexpr(eqexpr, sols, reltverr);
  eqexpr->destroy();
  DTL(cout << value->value << "+-" << value->abserr << endl;);
  DBG(cout << "Eqn " << eqn->getInfix() <<
      " balenced with discrepancy " << value->value 
           << " and error bars " << value->abserr << "  :";);
  // AW: after taking out "dubious" hacks below, need to handle case 
  // where lhs and rhs both exactly 0, so that abserr also comes out 0. 
  // Changed to use <= tests on error range to allow for this.
  if ((fabs(value->value) <= value->abserr) 
      /*|| (fabs(value->value) < reltverr)*/) {// dubious - not good for nuclear phys
    DBG(cout << " seems OK" << endl;);
    return(0);
  } else if ((fabs(value->value) <= 100 * value->abserr) 
	     /*|| (fabs(value->value) < 100 * reltverr)*/) {// dubious as above
    DBG(cout << " NOT REALLY OK" << endl;);
    return(1);
  } else {
    DBG(cout << " seems VERY NOT OK on" << endl; cout << eqn->getInfix() 
             << endl;);
    return(2);
  }
}

/************************************************************************
 * evalexpr  evaluates the expression ex at the solution point, given   *
 *      by the vector of SI values sols. It also estimates the maximum  *
 *      error it should have in calculating the value.                  *
 *  It treats an equation lhs = rhs as if it were                       *
 *      the expression lhs-rhs                                          *
 *  Errors contributions from several sources are added in magnitudes,  *
 *      not in quadrature, as we are interested in a maximum rather     *
 *      than an expected error.                                         *
 * Note: The calculation simply guesses the errors in all numvals and   *
 *      solution points, so this is not a real calculation. To do that, *
 *      we would need to propagate errors in eqnumsimp and other places *
 *      numvals are calculated, and give errors on input given values   *
 *      for which we currently have no facility.                        *
 ************************************************************************/
answitherr* evalexpr(const expr* const ex, const vector<double>* const sols,
		     const double reltverr) {
  answitherr* retval = new answitherr;
  int k;
  
  DTL({cout << "entered evalexpr with reltverr = " << reltverr 
	    << " on" << endl << ex->getInfix() << endl;});
  switch (ex->etype) {
    case numval:
      retval->value = ((numvalexp*)ex)->value;
      retval->abserr = reltverr * fabs(retval->value);
      DTL(cout  << "Evalexpr[numval]: " 
	  << retval->value << "+-" << retval->abserr << endl;);
      return(retval);
      break;

    case physvart:
      retval->value = (*sols)[((physvarptr *) ex)->varindex];
      retval->abserr = reltverr * fabs(retval->value);
      DTL(cout  << "Evalexpr[physvar]: " 
	  << retval->value << "+-" << retval->abserr << endl;);
      return(retval);

    case function: {
      answitherr* argval = evalexpr(((functexp *)ex)->arg,sols, reltverr);
      switch(((functexp *)ex)->f->opty) {    // remember, if FAKEDEG, trig 
      case sine:                // functions in degrees! if not, trig functions
	retval->value = sin(DEG2RAD * argval->value);      //  of radians
	retval->abserr = max(fabs(cos(DEG2RAD * argval->value) 
				  * DEG2RAD * (argval->abserr)), reltverr);
	break;
      case cose:
	retval->value = cos(DEG2RAD * argval->value);
	retval->abserr = max(fabs(sin(DEG2RAD * argval->value) 
				  * DEG2RAD * (argval->abserr)), reltverr);
	break;
      case tane:
	retval->value = tan(DEG2RAD * argval->value);
	retval->abserr = fabs((1. + pow(retval->value,2)) 
			      * DEG2RAD * (argval->abserr));
	break;
      case expe:
	retval->value = exp(argval->value);
	retval->abserr = fabs(retval->value * argval->abserr);
	break;
      case lne:
	retval->value = log(argval->value);
	retval->abserr = fabs(argval->abserr / retval->value);
	break;
      case log10e:
	retval->value = log10(argval->value);
	retval->abserr = fabs(argval->abserr / (retval->value * log(10)));
	break;
      case sqrte:
	retval->value = sqrt(argval->value);
	if (retval->value > reltverr) {
	  retval->abserr = (argval->abserr / (2 * retval->value));
	} else {
	  retval->abserr = sqrt(argval->abserr);
	}
	break;
      case abse:
	retval->value = fabs(argval->value);
	retval->abserr = argval->abserr;
	break;
      default:
	throw(string("impossible function in evalexpr"));
      }
      DTL(cout  << "Evalexpr[function]: " 
	  << retval->value << "+-" << retval->abserr << endl;);
      delete argval;
      return(retval);
    }
  case binop: {
    answitherr* lhsval = evalexpr(((binopexp*)ex)->lhs,sols,reltverr);
    answitherr* rhsval = evalexpr(((binopexp*)ex)->rhs,sols,reltverr);
    switch(((binopexp*)ex)->op->opty) {
    case divbye:
      retval->value = lhsval->value/rhsval->value;
      retval->abserr = lhsval->abserr/fabs(rhsval->value) 
	+ rhsval->abserr * lhsval->value/pow(rhsval->value,2);
      delete lhsval;
      delete rhsval;
      break;
    case topowe:
      retval->value = pow(lhsval->value,rhsval->value);
      if (fabs(lhsval->value)>lhsval->abserr) {
	  // AW: joel's suggested corrections for kt5a bug (email 9/8/03)  
	retval->abserr = lhsval->abserr 
	  * fabs(rhsval->value * (retval->value / /*was rhsval*/lhsval->value)) 
	  + /*was lhsval*/rhsval->abserr * fabs(log(fabs(lhsval->value)) * retval->value);
      } else {
	retval->abserr = pow(lhsval->abserr, rhsval->value - rhsval->abserr);
      }
      delete lhsval;
      delete rhsval;
      break;
    case equalse:
      retval->value = lhsval->value - rhsval->value;
      retval->abserr = lhsval->abserr + rhsval->abserr;
      break;
    case grte:
    case gree:
    default:
      throw(string("I can't return eval of >=, >, or unknown expr"));
    }
    DTL( cout  << "Evalexpr[binop]: " 
	 << retval->value << "+-" << retval->abserr << endl;);
    return(retval);
  }
  case n_op: {
    answitherr* argval = new answitherr;
    switch(((n_opexp*)ex)->op->opty) {
    case pluse: {
      retval->value = 0;
      retval->abserr = 0;
      for (k=0; k<((n_opexp*)ex)->args->size(); k++) {
	argval = evalexpr((*((n_opexp *)ex)->args)[k], sols,reltverr);
	retval->value += argval->value;
	retval->abserr += argval->abserr;
      }
      break;
    }
    case multe: {
      retval->value = 1;
      retval->abserr = 0;
      for (k=0; k<((n_opexp*)ex)->args->size(); k++)  {
	argval = evalexpr((*((n_opexp*)ex)->args)[k],sols,reltverr);
	retval->abserr = fabs(argval->value*retval->abserr) 
	  + fabs(argval->abserr * retval->value);
	retval->value *= argval->value;
      }
      break;
    }
    default:
      throw(string("unknown n_op in evalexpr"));
    }
    delete argval;
  }
  DTL(cout << "Evalexpr[n_op]: " 
      << retval->value << "+-" << retval->abserr << endl;);
  return(retval);
  case unknown:
  case fake:
  default:
    throw(string("I can't return eval of fake or unknown expr"));
  }
}
