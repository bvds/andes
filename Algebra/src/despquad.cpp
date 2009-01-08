// despquad.cpp    functions to examine equations quadratic in one or two vars.
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
// Modifications by Brett van de Sande, 2005-2008
//
//  This file is part of the Andes Solver.
//
//  The Andes Solver is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Lesser General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  The Andes Solver is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public License
//  along with the Andes Solver.  If not, see <http://www.gnu.org/licenses/>.
//	solvetwoquads	two equations in two variables
//	twoonevareqs	two equations in a single variable.

#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
#include <math.h>
using namespace std;

// need to change diag below.
#define DBG(A) DBGF(NEWCKEQSOUT,A)
#define DBGM(A) DBGFM(NEWCKEQSOUT,A)

vector<double> *twoquadcoef(const binopexp *eq, const varindx v1,
			    const varindx v2);

// Note inconsistent philosophy between desperate and below. Above, if we
// had a linear equation v1 = numer/denom, we did not do a replacement
// into other equations unless that resulted in an equation with a 
// unique solution, whereas here we are doing it if it results in a 
// quartic polynomial, without checking for unique solution.
/************************************************************************
 * solvetwoquads(eq1,eq2,v1,v2)
 *	Given two equations claimed to be overall quadratic in
 *	the two variables v1 and v2, and otherwise numerical, attempts 
 *	to replace the first by a linear equation in v1 in terms of v2,
 *	and the second as a (possibly) quartic equation in v2
 * returns:
 *	0	The equations were inconsistent or other inexplicable
 *		problem arose
 *	1	The two equations were not independent. First is
 *		rewritten as tautology, 0 = 0, and the second is 
 *		unchanged
 *	2	was successful in doing the above
 *	3	the first equation is now only in variable v2, and quadratic
 *		Second equation is now linear or better in v2
 *	4	First equation is now a solution for one variable. Time
 *		to forget desperate and redo checkeqs.
 ************************************************************************/
int solvetwoquads(binopexp * & eq1,  binopexp * & eq2, 
		  const varindx v1, const varindx v2)
{
  int k;
  double fact;
  
  n_opexp * temp1;
  DBG( cout << "Solvetwoquads on " << eq1->getInfix() << endl << " and "
       << eq2->getInfix() << endl << " in variables " <<
       (*canonvars)[v1]->clipsname << " and " << 
       (*canonvars)[v2]->clipsname << endl);
  DBGM(eq1->dbgprint(4); eq2->dbgprint(4););
#if 1 //  Double-check that the units are OK
  {
    expr *inconst1 = dimenchk(true,(expr * &) eq1);
    if (inconst1 != (expr *)NULL) 
      throw(string("solvetwoquads start with units inconsistency for eq1:  ")
	    + inconst1->getInfix());
    expr *inconst2 = dimenchk(true,(expr * &) eq2);
    if (inconst2 != (expr *)NULL) 
      throw(string("solvetwoquads start with units inconsistency for eq2:  ")
	    + inconst2->getInfix());
  }
#endif
  vector<double> * cf1 = twoquadcoef(eq1,v1,v2);
  vector<double> * cf2 = twoquadcoef(eq2,v1,v2);
  if ( (cf1 == (vector<double> *) NULL) ||
       (cf2 == (vector<double> *) NULL))
    throw(string("equation claimed to be fit for solvetwoquads isn't"));
  vector<double> * cfd;
  vector<double> * cfn;
  if (fabs((*cf1)[0]) >= fabs((*cf2)[0])) { cfd=cf1; cfn=cf2; }
  else { cfd=cf2; cfn=cf1; }
  if ((*cfd)[0] == 0.0)		// neither has v1^2 term
    {
      if (fabs((*cf1)[1]) >= fabs((*cf2)[1])) { cfd=cf1; cfn=cf2; }
      if ((*cfd)[1] == 0.0) fact = -1.;	// neither has v1 v2 term either
      else fact = -(*cfn)[1]/(*cfd)[1];	// now num is n v1 + quad(v2), easy.
    } 					// but use -1 just incase they are same
  else  fact = -(*cfn)[0]/(*cfd)[0];

  // switch eq1 and eq2 when eq1 has larger coefficient
  if (cfn == cf2) {	     
    binopexp * tmpbin = eq1; 
    eq1 = eq2;
    eq2 = tmpbin;
  }

  for (k = 0; k < 6; k++) (*cfn)[k] = addnum((*cfn)[k], fact * (*cfd)[k]);
  if (((*cfn)[3]==0.0) && ((*cfn)[1] == 0.0)) // result independent of v1
    {
      if (((*cfn)[2] ==0.0) && ((*cfn)[4] ==0.0)) // independent of v2 too!
	{
	  if ((*cfn)[5] != 0.0) 
	    {
	      delete cf1;
	      delete cf2;
	      DBG( cout << "Solvetwoquads found inconsistency" << endl);
	      return(0); // inconsistency!
	    }
	  eq1->destroy();
	  eq1 = new binopexp(&equals,new numvalexp(0),new numvalexp(0));
	  delete cf1;
	  delete cf2;
	  DBG( cout << "Solvetwoquads: Equations not independent" << endl);
	  return(1);
	} // end of if new equation has no variables
      eq1->lhs->destroy();
      if (eq1->rhs->etype != numval)
	{  eq1->rhs->destroy(); eq1->rhs = new numvalexp(0); }
      ((numvalexp *)(eq1->rhs))->value = 0;
      // need to replace eq1 by equation in just v2, from cfn [2, 4, 5]
      if ((*cfn)[2] == 0)	// wow! it's linear!
	{
	  eq1 = new binopexp(&equals,new physvarptr(v2),
			     new numvalexp(-(*cfn)[5]/(*cfn)[4]));
	  delete cf1; delete cf2; 
	  DBG( cout << "Solvetwoquads: one equation now solve for var:" << endl
	   << eq1->getInfix() << endl;);  
	  return(4);
	}
      fact = -(*cfd)[2]/(*cfn)[2];
      temp1 = new n_opexp(&mult);
      temp1->addarg(new numvalexp((*cfn)[2]));
      temp1->addarg(new binopexp(&topow,new physvarptr(v2),
	  new numvalexp(2)));
      n_opexp * eq1lhsn = new n_opexp(&myplus);
      eq1lhsn->addarg(temp1);
      temp1 = new n_opexp(&mult);
      temp1->addarg(new numvalexp((*cfn)[4]));
      temp1->addarg(new physvarptr(v2));
      eq1lhsn->addarg(temp1);
      eq1lhsn->addarg(new numvalexp((*cfn)[5]));
      eq1->lhs = eq1lhsn;
      delete cf1; delete cf2; 
      numvalexp * nfact = new numvalexp(fact); // added 7/30 
      nfact->MKS = eq2->lhs->MKS;
      nfact->MKS += eq1->lhs->MKS * -1;
      apluskb(eq2->lhs,eq1->lhs,nfact);	// should make eq2 linear in v2
      DBG( cout << "Solvetwoquads: one equation now of one var only:" << endl
	   << eq1->getInfix() << endl);  
      return(3);
    } // end of if new combo independent of v1
  else
    {
      // work on eq1:
      numvalexp * nfact = new numvalexp(fact); // added 7/30 
      nfact->MKS = eq1->lhs->MKS;
      nfact->MKS += eq2->lhs->MKS * -1;
      numvalexp * nfact2 = (numvalexp *) copyexpr(nfact);
      apluskb(eq1->lhs,eq2->lhs,nfact);	// replace eq1 by linear equation
      apluskb(eq1->rhs,eq2->rhs,nfact2); // wouldn't that have worked above 
      
      DBG( cout << "Solvetwoquads: replaced first by:" << endl
	   << "          " << eq1->getInfix() << endl);
      DBGM(eq1->dbgprint(4)); 
#if 1 //  Double-check that the units are OK
      expr *inconst1 = dimenchk(true,(expr * &) eq1);
      if (inconst1 != (expr *)NULL) 
	throw(string("solvetwoquads units inconsistency for eq1:  ")
		+ inconst1->getInfix());
#endif

      // now work on eq2:
      eq2->lhs->destroy();		// as well?
      if (eq2->rhs->etype != numval)
	{  eq2->rhs->destroy(); eq2->rhs = new numvalexp(0); }
      ((numvalexp *)(eq2->rhs))->value = 0;
      temp1 = new n_opexp(&mult);
      temp1->addarg(
	new numvalexp( -(*cfd)[1]*(*cfn)[2]*(*cfn)[1]
		       +(*cfd)[0]*(*cfn)[2]*(*cfn)[2]
		       +(*cfd)[2]*(*cfn)[1]*(*cfn)[1]));
      temp1->addarg(new binopexp(&topow,new physvarptr(v2),
				    new numvalexp(4)));
      n_opexp *ex = new n_opexp(&myplus);
      ex->addarg(temp1);
      temp1 = new n_opexp(&mult);
      temp1->addarg(
	new numvalexp( 2.0*(*cfd)[0]*(*cfn)[2]*(*cfn)[4]
		       -(*cfd)[1]*(*cfn)[2]*(*cfn)[3]
		       -(*cfd)[1]*(*cfn)[4]*(*cfn)[1]
		       +2.0*(*cfd)[2]*(*cfn)[3]*(*cfn)[1]
		       -(*cfd)[3]*(*cfn)[2]*(*cfn)[1]
		       +(*cfd)[4]*(*cfn)[1]*(*cfn)[1]));
      // ??? need to fix dimens
      temp1->addarg(new binopexp(&topow,new physvarptr(v2),
				    new numvalexp(3)));
      ex->addarg(temp1);
      temp1 = new n_opexp(&mult);
      temp1->addarg(
	new numvalexp( 2.0*(*cfd)[0]*(*cfn)[5]*(*cfn)[2]
		       -(*cfd)[1]*(*cfn)[5]*(*cfn)[1]
		       -(*cfd)[1]*(*cfn)[4]*(*cfn)[3]
		       -(*cfd)[3]*(*cfn)[2]*(*cfn)[3]
		       -(*cfd)[3]*(*cfn)[4]*(*cfn)[1]
		       +2.0*(*cfd)[4]*(*cfn)[3]*(*cfn)[1]
		       +(*cfd)[0]*(*cfn)[4]*(*cfn)[4]
		       +(*cfd)[2]*(*cfn)[3]*(*cfn)[3]
		       +(*cfd)[5]*(*cfn)[1]*(*cfn)[1]));
      // ??? need to fix dimens
      temp1->addarg(new binopexp(&topow,new physvarptr(v2),
				    new numvalexp(2)));
      ex->addarg(temp1);
      temp1 = new n_opexp(&mult);
      temp1->addarg(
	new numvalexp( 2.0*(*cfd)[0]*(*cfn)[5]*(*cfn)[4]
		       -(*cfd)[1]*(*cfn)[5]*(*cfn)[3]
		       -(*cfd)[3]*(*cfn)[5]*(*cfn)[1]
		       -(*cfd)[3]*(*cfn)[4]*(*cfn)[3]
		       +2.0*(*cfd)[5]*(*cfn)[3]*(*cfn)[1]
		       +(*cfd)[4]*(*cfn)[3]*(*cfn)[3]));
      // ??? need to fix dimens
      temp1->addarg(new physvarptr(v2));
      ex->addarg(temp1);
      ex->addarg(new numvalexp((*cfd)[0]*(*cfn)[5]*(*cfn)[5]
				  +(*cfd)[5]*(*cfn)[3]*(*cfn)[3]
				  -(*cfd)[3]*(*cfn)[5]*(*cfn)[3]));
      // ??? need to fix dimens
      eq2->lhs = ex;
      delete cf1;
      delete cf2;

      DBG( cout << "Solvetwoquads: replaced second by:" << endl
	   << "          " << eq2->getInfix() << endl);
      DBGM(eq2->dbgprint(4));       
#if 1 //  Double-check that the units are OK
      expr *inconst2 = dimenchk(true,(expr * &) eq2);
      if (inconst2 != (expr *)NULL) 
	  throw(string("solvetwoquads units inconsistency for eq2:  ")
		+ inconst2->getInfix());
#endif

      return(2);
    } // end of else, meaning rewrote both equations
}

int twoonevareqs(binopexp * & eq1,  binopexp * & eq2, const varindx v)
{
  DBG( cout << "twoonevareqs: I got called on " << endl
       << eq1->getInfix() << " and " << eq2->getInfix() << " with variable "
       << (*canonvars)[v]->clipsname << endl
       << "So I think it is time to write me, no?" << endl;);
  return(1);
}

