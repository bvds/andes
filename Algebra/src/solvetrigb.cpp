// solvetrigb.cpp    stronger version of solvetrigvar
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//
//	undotrigvar  		called from checkeqs only

#include <math.h>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "mconst.h"
using namespace std;

#define DBG(A) DBGF(SLVTRIG,A)

bool trigsearch(const expr * const arg, expr *& coef,
		const expr * const ex, bool & iscos, expr * & oside);

//  This is an attempt to make our trig manipulation powerful enough to 
//  solve exlmom3a.  If fact sin(arg) = v1 and fact cos(arg)=v2,
//  we can conclude that fact^2 = v1^2 + v2^2. While this does not 
//  return a more tractible equation for arg, it might help determine
//  fact, v1 and v2.


/************************************************************************
 *  bool undotrigvar(const expr * const arg,   				*
 *		vector<binopexp *> * & eqn)  				*
 * 	looks for two equations of the form  				*
 *		fact1 + coef sin(arg) = 0	(or with sin <-> cos)	*
 *	and 	fact2 + k2 * coef cos(arg) = 0				*
 * 	without worrying about signs or commensurability of 		* 
 *		fact1 & fact2 						* 
 *  if found, adds the equation 					*
 *	fact1^2 + k2^2 fact2^2 = k2^2 coef^2				*
 ************************************************************************/
bool undotrigvar(const expr * const arg, vector<binopexp *> * & eqn)
{
  int j, k;
  numvalexp * k2;
  expr *fact1, *fact2, *coef, *facttry;
  bool iscos, firstiscos;

  VEQCHK(eqn);
  DBG( cout << "Entering undotrigvar with arg " << arg->getInfix() << endl;);
  for (j = 0; j+1 < eqn->size(); j++) // loop over first equation of pair
    if (trigsearch(arg, coef,(expr *)(*eqn)[j], iscos, fact1))
      {
	DBG( cout << j << "th eq. (hits in undotrigvar): " << endl
	          << (*eqn)[j]->getInfix() << endl
	          << "Trigsearch returned true with coef "
	          << coef->getInfix() << " and constant term "
	          << fact1->getInfix() << " and iscos "
		  << ((iscos) ? "true" : "false") << endl; );

	firstiscos = iscos;
	// first equation reads coef * cos arg + fact1 = 0 (if firstiscos)
	for (k = j+1; k < eqn->size(); k++)
	  if (trigsearch(arg, facttry, (expr *)(*eqn)[k], iscos, fact2))
	    {
	      DBG( cout << k << "th eq. was hit in undotrigvar: " << endl
		        << (*eqn)[k]->getInfix() << endl
		        << "Trigsearch returned true with coef "
		        << facttry->getInfix() << " and constant term "
		        << fact2->getInfix() << " and iscos "
			<< ((iscos) ? "true" : "false") << endl; );
	      if ((iscos == firstiscos) ||(!(uptonum(facttry,coef,k2))))
		{
		  facttry->destroy(); 	// shouldn't we have done this in
		  fact2->destroy(); 	// solvetrigvar too?
		  continue;
		}
	      DBG( cout << "undotrigvar: k2 is " << k2->getInfix() << endl; );
	      // second equation reads
	      // k2 * coef * sin arg + fact2 = 0 (if firstiscos),
	      // and therefore we know k2^2 fact1^2 + fact2^2= k2^2 coef^2
	      n_opexp * eqlhs = new n_opexp(&plus);
	      n_opexp * tempnop = new n_opexp(&mult);
//	      tempnop->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor

	      k2->value *= k2->value;
	      k2->MKS *= 2;
	      numvalexp * tempnv = new numvalexp(2);
	      tempnv->MKS.put(0,0,0,0,0);
	      tempnop->addarg(copyexpr (k2));
	      tempnop->addarg(new binopexp(&topow, fact1,copyexpr(tempnv)));
		
	      eqlhs->addarg(tempnop);
	      eqlhs->addarg(new binopexp(&topow, fact2,copyexpr(tempnv)));

	      tempnop = new n_opexp(&mult);
	      k2->value *= -1;
	      tempnop->addarg(k2);
	      tempnop->addarg(new binopexp(&topow, coef,tempnv));
	      eqlhs->addarg(tempnop);
	      eqn->push_back(new binopexp(&equals,eqlhs,new numvalexp(0)));
	      k = eqn->size();
	      DBG( cout << "Undotrigvar just output new equation no. "
		    << k-1<< ": " << (*eqn)[k-1]->getInfix() << endl 
		    << "undotrigvar returning true " << endl; );
	      return(true);
	    } // End of checking and implementing matching equations
      }	// end of loop searching for first of pair
  return(false);
}
