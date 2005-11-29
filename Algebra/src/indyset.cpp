// indyset.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//	indyset::isindy
//	indyset::placelast
//	indyset::expandlast

#include "indyset.h"
#include "decl.h"
#include "extstruct.h"
#include "valander.h"
#include "dbg.h"
#include <math.h>
using namespace std;

#define DBG(A) DBGF(INDY,A)

indyset::indyset(int numvars) :
  numvars(numvars), numinset(0), lastisvalid(false) { }


/************************************************************************
 * isindy is a member function of indyset - it checks whether its 	*
 *	argument, an expression viewed as the lhs of an equation = 0, 	*
 *	is independent of the previous entries in the set.		*
 *      (If called on an equation, treats it as lhs - rhs = 0)		*
 *   It leaves around info for a subsequent command to (if indy) add 	*
 *	the equation to the set, or if not, to give its expansion in 	*
 * 	terms of the previous equation expressions			*
 ************************************************************************/

//     should candex be a binopexp instead of an expr? Not really, as
//     getvnd expects an expr, preferably not an equation but the lhs of one

bool indyset::isindy(const expr * const candex)
{
  bool answer;
  DBG( cout << "Entering isindy with " << candex->getInfix() << endl;);
  valander * candval = getvnd(candex,canonvars,numsols);
  DBG( cout << "Indyset: getvnd returned " << candval->print() << endl;);
  answer = isindy(candval);
  delete candval;
  return(answer);
}

bool indyset::isindy(const valander * const candval)
{
  candleft.assign(numvars,0.0);
  candleft = candval->gradient;
  candexpand.assign(numinset,0);
  // write the gradient of the new expression as a sum of basis vectors b_k
  // times candexpand[k], + candleft
  for (int k = 0; k < numinset; k++)
    {
      int thisvar = ordervar[k];
      double coef = candleft[thisvar]/basis[k][thisvar];
      for (int q = 0; q < numvars; q++) 
	{
	  double chkerr = fabs(candleft[q])+ fabs(coef*basis[k][q]);
	  candleft[q] += -coef*basis[k][q];
	  if (fabs(candleft[q]) < RELERR * chkerr)
	    {
	      DBG( cout << "setting candleft[" << q << "] = " << candleft[q] 
		   << " to zero with chkerr = " << chkerr << endl);
	      candleft[q] = 0;
	  }
	}
      candexpand[k] = coef;
    }
  lastisvalid = true;
  DBG(cout << "Leaving isindy with coefs "; printdv(candexpand);
      cout << "and with remaining gradient "; printdv(candleft);) ;

  for (int q = 0; q < numvars; q++) if (candleft[q] != 0) return(true);
  return(false);
}

/************************************************************************
 * placelast() adds the previously isindy'd equation (which must have	*
 *	been declared independent) to the set				*
 * Must be first call following the isindy call.			*
 ************************************************************************/
bool indyset::placelast()
{
  int k, q;
  DBG(    cout << "Entering indyset::placelast" << endl; );
  if (!lastisvalid) return(false);
  // insert candleft as new basis vector.
  // The basis vectors can be expanded in terms of the inserted independent
  // vectors as B_n = \sum_{D_{nr} B_r, where D_{rr} = 1, D_{ij}=0 for i<j,
  // and as (at the nth step) E_n = B_n + \sum_{r<n} C_r B_r,
  // B_n = E_n - \sum_{r<n} C_r \sum{q<=r} D_{rq}E_q, so
  // D_{nq} = \delta_{nq} - \sum_{r=q}{n-1} C_r D_{rq}. This is stored in
  // temp[q] and then pushed as the nth element in baseexpand.
  // 
  vector<double> *temp = new vector<double>(numinset+1);
  (*temp)[numinset] = 1.;
  for ( q = 0; q < numinset; q++) 
    {
      (*temp)[q] = 0.;
      for (int r = q; r < numinset; r++) 
	(*temp)[q] += -candexpand[r] * basexpand[r][q];
    }
  basexpand.push_back(*temp);
  basis.push_back(candleft);
  double biggest = -1.; 
  for ( k = 0; k < numvars; k++) 
    if (fabs(candleft[k]) > biggest) { biggest = fabs(candleft[k]); q = k;}
  ordervar.push_back(q);
  lastisvalid = false;
  numinset++;
  DBG( { cout << "Placelast: inserted new basis vector, now have " 
	      << numinset << endl;
	 cout << "Placelast: Its pivot is variable " 
	      << ordervar[ordervar.size()-1] << endl; } ) ;
  return(true);
}

/************************************************************************
 * expandlast returns the vector of coefficients by which the last
 *	isindy'd equation can be expanded in terms of the equations
 *	already entered in the set. Must be first call following the 
 *	isindy call.
 ************************************************************************/
vector<double> indyset::expandlast()
{
  DBG(    cout << "Entering indyset::expandlast" << endl; );
  vector<double> ret = *new vector<double>(numinset,0.0);
  for (int k = 0; k < numinset; k++) 
  {
	// AW: add check for addition result that should be set to zero.
	// followup check on final coeffs in indyHowIndy may not use appropriate 
	// scale if we subtract large numbers here with small differences, but all
	// final coeffs wind up small (pot3b indy bug, email 1/22/04).
	double maxadd = 0.0;       // max abs addend, for zero check below.
    for (int q = k; q < numinset; q++)
	{	  
	   double addend = candexpand[q] * basexpand[q][k];
	   if (fabs(addend) > maxadd)  maxadd = fabs(addend);
	   ret[k] += addend;
	}
	// check sum wrt maxadd to see if it should be zero
    if (fabs(ret[k]) < RELERR * maxadd) {
	    ret[k] = 0;
		DBG ( cout << "ret[k] set to 0" << endl);
	}
  }
  lastisvalid = false;
  return(ret);
}

/************************************************************************
 * keepn(int n)
 *	removes all the equations from the set except the first n.
 *	fails (and returns false) if n > size or n < 0
 ************************************************************************/
bool indyset::keepn(int n) {
  int k;
  if ((n > numinset) || (n < 0)) return(false);
  //    throw(string("miscall to keepn, keep ") + itostr(n) + " out of " 
  //	  + itostr(numinset));
  for (k = numinset - 1; k >= n; k--) {
    ordervar.pop_back();
//    delete &(basexpand[k]);    // had been &basexpand
    basexpand.pop_back();
//    delete &(basis[k]);	    // had been &basis
    basis.pop_back();
  }
  lastisvalid = false;
  numinset = n;
  return(true);
}

