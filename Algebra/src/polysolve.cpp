//    polysolve.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//			[Main function here is slvpolyexpr, but it 
//			 does not check constraints on variables
//			 polysolve in polyslv.cpp does do that. 
//			 Reorganization seems called for.]

#include <cmath>
#include <float.h>
#include "decl.h"
#include "extoper.h"
#include "extstruct.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(POLY,A)
#define POLYDTL(A) DBGFM(POLY,A)

vector<double> *polytopow(const vector<double> * poly, const int pow);
vector<double> *polymult(const vector<double> * poly1,
			 const vector<double> * poly2);
vector<double> *polyadd(const vector<double> * poly1,
			const vector<double> * poly2);
bool polyexpand(const expr * ex,varindx var,vector<double> * & coefs);


/************************************************************************
 * bool polysolve(vector *eqn, vector<varindx> *vars)			*
 *	looks through the list of equations in eqn for ones which	*
 *	involves only one variable and is polynomial in that.		*
 *     It solves each equation and checks to see if exactly one 	*
 *	solution satisfies constraints - if so, it replaces the		*
 *	equation by an assignment statement.				*
 *   returns true if successful on any equation (though it tries all,	*
 *	not stopping after a success.)					*
 ************************************************************************/
//		[Do we have duplication? In desperate we call polyexpand
//		 and look at roots. Here we call slvpolyexpr and check roots

bool polysolve(vector<binopexp *> * eqn, const vector<varindx> *vars)
{
  int k, q;
  bool doagain=false;
  
  for (k = 0; k < eqn->size(); k++)
    {
      varindx onevar = -1;		// negative means not yet set
      if (!hasjustonevar((expr *)(*eqn)[k], onevar)) continue;
      if (onevar < 0) continue;		// didn't find any variable
      vector<varindx> *varth = new vector<varindx>(1,onevar);
      vector<int> *orders = (vector<int> *) NULL;
      if (ordinvars((expr *)(*eqn)[k],varth,orders))
	{
	  vector<double> *solth = slvpolyexpr((expr *)(*eqn)[k],onevar);
	  if ((solth != (vector<double> *) NULL) &&
	      (solth->size() > 0))
	    {
	      for (q = 0; q < solth->size(); q++)
		{
		  if ((((*solth)[q] < 0) && (*canonvars)[onevar]->isnonneg) ||
		      (((*solth)[q] == 0) && (*canonvars)[onevar]->isnonzero))
		    {
		      (*solth)[q] = (*solth)[solth->size()-1];
		      solth->pop_back();
		      q--;
		    }
		}
	      if (solth->size() == 1)
		{
		  (*eqn)[k]->destroy();
		  (*eqn)[k] = new binopexp(&equals,new physvarptr(onevar),
					   new numvalexp((*solth)[0]));
		  DBG( cout << "polysolve found "
		       << (*eqn)[k]->getInfix() << endl;);
		  doagain = true;
		}
	    }
	  if (solth != (vector<double> *) NULL) 
	    delete solth;
	} // end of yes, ordinvars was ok
      delete varth;
      delete orders;
    } // end of loop over k, eqns left to solve
  return(doagain);
}


/************************************************************************
 * vector<double> *slvpolyexpr(const expr * eq, varindx var)		*
 *	expects eq is a polynomial in var, with numerical coefs		*
 * returns vector of real solutions.					*
 * If vector is null something was wrong with eq			*
 ************************************************************************/
vector<double> *slvpolyexpr(const expr * eq, varindx var)
{
  vector<double> * poly = NULL;
  
  DBG( cout << "Entering slvpolyexpr with " << eq->getInfix()
       << " on variable " 
       << (*canonvars)[var]->clipsname << endl;);
  if (eq->etype != binop)
    throw(string("slvpolyexpr called nonbinop (equations are binops"));  
  binopexp *bineq = (binopexp *) eq;
  if (bineq->op->opty != equalse)
    throw(string("slvpolyexpr called on non-equation"));  
  expr * eqcpy = copyexpr(eq);
  normexpr(eqcpy);
  if (!polyexpand(((binopexp *)eqcpy)->lhs,var,poly))
    return(poly);
  vector<double> * answer = findallroots(poly);
  delete poly;
  return(answer);
}

/***************************************************************************
 *  bool polyexpand(const expr * ex,varindx var,vector<double> * & coefs) *
 *	ex should be a polynomial in the physvar var with numerical coefs. *
 *	These are returned in a vector coefs[k] = coefficient of var^k     *
 *  returns false if ex not of this form. ex must not be equation.	   *
 *  coefs should not be pointing at anything on call, and will not if false* 
 ***************************************************************************/
bool polyexpand(const expr * ex,varindx var,vector<double> * & coefs)
{
  int k, q;
  vector<double> * polyt;
  DBG( cout << "Entering polyexpand with " << ex->getInfix() << endl;);
  switch(ex->etype)
    {
    case numval:
      coefs = new vector<double>(1,((numvalexp *)ex)->value);
      return(true);
    case physvart:
      if (((physvarptr *)ex)->varindex != var) return(false);
      coefs = new vector<double>(2,0.);
      (*coefs)[1]=1.;
      POLYDTL( {
	cout << "polyexpand true with coefs ";
	for (k=0; k < coefs->size(); k++) cout << (*coefs)[k] << ", ";
	cout << endl;});
      return(true);
    case function:
      POLYDTL( cout << "Polyexpand false from function" << endl;);
      return(false);
    case binop:
      {
	if (((binopexp *)ex)->op->opty != topowe) return(false);
	binopexp * binex = (binopexp *) ex;
	if (binex->rhs->etype != numval) return(false);
	if (!lookslikeint(((numvalexp *)binex->rhs)->value, q)) return(false);
	if (q < 0) return(false);
	if (!polyexpand(binex->lhs,var,polyt)) return(false);
	coefs = polytopow(polyt,q);
	delete polyt;
	POLYDTL( {
	  cout << "polyexpand true with coefs ";
	  for (k=0; k < coefs->size(); k++) cout << (*coefs)[k] << ", ";
	  cout << endl;});
	return(true);
      }
    case n_op:
      {
	n_opexp * nopex = (n_opexp *) ex;
	polyt = new vector<double>(1);
	vector<double> *polya;
	vector<double> *polyb;
	if (nopex->op->opty == multe)
	  {
	    (*polyt)[0] = 1.;
	    for (k = 0; k < nopex->args->size(); k++)
	      if (!polyexpand((*nopex->args)[k],var,polya))
		{ delete polyt; return(false); }
	      else 
		{
		  polyb = polymult(polyt,polya);
		  delete polyt;
		  delete polya;
		  polyt = polyb;
		}
	    POLYDTL( cout << "Polyexpand end of mult " << endl;);
	  } // end of mult
	else			// n_op is plus
	  {
	    (*polyt)[0]=0.;
	    for (k = 0; k < nopex->args->size(); k++)
	      if (!polyexpand((*nopex->args)[k],var,polya))
		{ delete polyt; return(false); }
	      else 
		{
		  polyb = polyadd(polyt,polya);
		  delete polyt;
		  delete polya;
		  polyt = polyb;
		}
	  } // end of plus
	coefs = polyt;
	POLYDTL( {
	  cout << "polyexpand true from n_op with coefs ";
	  for (k=0; k < coefs->size(); k++) cout << (*coefs)[k] << ", ";
	  cout << endl;});
	return(true);
      } // end of n_op
    case unknown:
    case fake:
    default:
      throw(string("unknown expr sent to polyexpand"));
    }
}

/************************************************************************
 * polytopow  The first arg is a polynomial represented by a vector, 	*
 * 	the second a positive integer.					*
 *   Returns the first arg raised to the power given by the second arg,	*
 *	as a polynomial retpresented by a vector. 			*
 *   Method used is not efficient if the power is big			*
 ************************************************************************/
vector<double> *polytopow(const vector<double> * poly, const int pow)
{				// version only for small powers
  int k;

  DBG( cout << "Entering polytopow"  << endl;);
  if (pow <= 0) throw(string("polytopow called with nonpositive power"));
  vector<double> * polyt= new vector<double>(*poly);
  vector<double> * polya;
  for (k = 1; k < pow; k++)
    {
      polya = polymult(polyt,poly);
      delete polyt;
      polyt = polya;
    }
  POLYDTL( {
    cout << "Return from polyadd with coefs ";
    for  (k=0; k < polyt->size(); k++) cout << (*polyt)[k] << ", ";
    cout << endl;});
  return(polyt);
}

/************************************************************************
 * polymult (poly1,poly2) returns, as a vector, the polynomial given by *
 * 	the product of the polynomials represented by the vectors 	*
 *      poly1 and poly2							*
 ************************************************************************/
vector<double> *polymult(const vector<double> * poly1,
			 const vector<double> * poly2)
{
  int k, q;
  DBG( cout << "Entering polymult"  << endl;);
  vector<double> * polyret = 
    new vector<double> (poly1->size() + poly2->size() - 1, 0.);
  for (k = 0; k < poly1->size(); k++)
    for (q = 0; q < poly2->size(); q++)
      (*polyret)[k+q] += (*poly1)[k] * (*poly2)[q];
  POLYDTL( {
    cout << "Return from polymult with coefs ";
    for  (k=0; k < polyret->size(); k++) cout << (*polyret)[k] << ", ";
    cout << endl;});
  return(polyret);
}


/************************************************************************
 * polyadd (poly1,poly2) returns, as a vector, the polynomial given by	*
 * 	the sum of the polynomials represented by the vectors	 	*
 *      poly1 and poly2							*
 ************************************************************************/
vector<double> *polyadd(const vector<double> * poly1,
			const vector<double> * poly2)
{
  int k;
  vector<double> * polyret;
  DBG( cout << "Entering polyadd"  << endl;);

  if (poly1->size() >= poly2->size())
    {
      polyret = new vector<double>(*poly1); 
      for (k=0; k < poly2->size(); k++)
	(*polyret)[k] += (*poly2)[k];
    }
  else
    { 
      polyret = new vector<double>(*poly2); 
      for (k=0; k < poly1->size(); k++)
	(*polyret)[k] += (*poly1)[k];
    }
  POLYDTL( {
    cout << "Return from polyadd with coefs ";
    for  (k=0; k < poly1->size(); k++) cout << (*polyret)[k] << ", ";
    cout << endl;});
  return(polyret);
}


/************************************************************************
 * double evalpoly(const vector<double> * poly, const double x)		*
 *	returns the value of numerical polynomial poly(x)		*
 ************************************************************************/
double evalpoly(const vector<double> * poly, const double x)
{
  int k;
  double ans = 0.;
  POLYDTL( {
    cout << "Entering evalpoly at x=" << x << " for poly" <<endl;
    for (k = ((int)poly->size())-1; k >= 0; k--)
      cout << (*poly)[k] << "*x^" << k << " +";
    cout << "0"<< endl;
  });
  for (k = ((int)poly->size())-1; k >= 0; k--)
    ans = ans * x + (*poly)[k];
  POLYDTL( cout << "polyeval answering " << ans << endl;);
  return(ans);
}

/************************************************************************
 * double findroot(poly, polyderiv, low, high)				*
 *	we are supposed to already know poly(low) and poly(high) have	*
 *	opposite signs, and polyderiv is the derivative polynomial of	*
 *	poly, and it has no roots in (low, high). 			*
 * returns the root to perhaps accuracy of 10 times machine resolution	*
 ************************************************************************/
double findroot(const vector<double> * poly, 
		const vector<double> * polyderiv, 
		const double low, const double high)
{
  double delx;
  double x = 0.5 * (low + high);
  double xscale = fabs(low) + fabs(high);
  do {
    delx = - evalpoly(poly,x)/evalpoly(polyderiv,x);
    if (x + delx < low) return (findroot(poly, polyderiv,low,x));
    if (x + delx > high) return (findroot(poly, polyderiv,x,high));
    x += delx;
  } while (fabs(delx) > 10. * DBL_EPSILON * xscale);
  return(x);
}

/************************************************************************
 * vector<double> * findallroots(const vector<double> *poly)		*
 *	returns all real roots in ascending order of poly(x)=0		*
 ************************************************************************/
vector<double> * findallroots( vector<double> *poly) // const poly unless poly
{				// has spurious leading 0's
  int k;
  vector<double> * answer;
  DBG( cout << "entering findallroots" << endl;);
  POLYDTL( { 
    cout << "findallroots of poly: ";
    for (k = ((int)poly->size())-1; k >= 0; k--)
      cout << (*poly)[k] << "*x^" << k << " +";
    cout << "0"<< endl;   });
  for (k = ((int)poly->size())-1; k > 0; k--) // leave highest order 0 only in 
    if ((*poly)[k] == 0.) poly->pop_back();  // constant term.
    else break;
  POLYDTL( { 
    cout << "findallroots after prune, poly is ";
    for (k = ((int)poly->size())-1; k >= 0; k--)
      cout << (*poly)[k] << "*x^" << k << " +";
    cout << "0"<< endl;   });
  if (poly->size() <= 1) return(new vector<double>); // no roots of constant
  vector<double> * polyderiv = new vector<double>(poly->size()-1);
  for (k=1;k < poly->size(); k++) 
    (*polyderiv)[k-1] = ((double) k) * (*poly)[k];
  POLYDTL( { 
    cout << "findallroots calculates derivative is: ";
    for (k = ((int)polyderiv->size())-1; k >= 0; k--)
      cout << (*polyderiv)[k] << "*x^" << k << " +";
    cout << "0"<< endl;   });
  vector<double> *derivroots = findallroots(polyderiv);
  if (derivroots->size() == 0) 
    {
      if (poly->size() %2 == 1) return new vector<double>; // zero length, no 
      double bnd = 0.5;
      do { bnd *= 2.; } while
	(evalpoly(poly,bnd) * evalpoly(poly,-bnd) > 0);
      answer = new vector<double>(1,findroot(poly,polyderiv,-bnd,bnd));
      delete polyderiv;
      delete derivroots;
      return(answer);
    }
  answer = new vector<double>;
  // see if poly root below lowest deriv root
  double high = (*derivroots)[0];
  if ((evalpoly(poly,high) * (*poly)[poly->size()-1] 
       * (-1 + 2*(poly->size()%2))) < 0)
    {
      double bnd = 0.5;
	do { bnd *= 2.; } while
	(evalpoly(poly,high) * evalpoly(poly,high - bnd) > 0);
      answer->push_back(findroot(poly,polyderiv,high-bnd,high));
    }
  for (k = 0; k+1 < derivroots->size(); k++) // search root of poly between
    if ((evalpoly(poly,(*derivroots)[k]) * evalpoly(poly,(*derivroots)[k+1]))
	<= 0)
      {
	double rt = 
	  findroot(poly,polyderiv,(*derivroots)[k],(*derivroots)[k+1]);
	if ((answer->size() > 0) && (rt != (*answer)[answer->size()-1]))
	  answer->push_back(rt);
      }
  high = (*derivroots)[k];
  if ((evalpoly(poly,high) * (*poly)[poly->size()-1]) < 0 )
    {
      double bnd = 0.5;
	do { bnd *= 2.; } while
	(evalpoly(poly,high) * evalpoly(poly,high + bnd) > 0);
      answer->push_back(findroot(poly,polyderiv,high,high + bnd));
    }
  delete polyderiv;
  delete derivroots;
  return(answer);
}
