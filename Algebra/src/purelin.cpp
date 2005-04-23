// linear equations 
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include <iostream>
#include <iomanip>
#include "math.h"
#include "decl.h"
#include "extoper.h"
#include "extstruct.h"
#include "dbg.h"
#include "binopfunctions.h"
using namespace std;
#define DBG(A) DBGF(PLSAS,A)

/************************************************************************
 * bool purelinsolv(vector<binopexp *> const * eqs, 			*
 *		vector<varindx> const * vars,				*
 *		vector<binopexp *> * & sols)				*
 *	eqs must contain linear equations in the variables vars with	*
 *	  numerical coefficients only, in the form 			*
 *	  (+ (* numval var) ...) = numval				*
 *	if purelinsolv returns true, it also returns in sols a set	*
 *	  of assignment equations (but with rhs containing undetermined *
 *	  unknowns) which contains all the info in eqs			*
 *	  (but this may be fewer equations than in eqs)			*
 *	sols should be empty vector on call				*
 *   									*
 ************************************************************************/

bool doterm(const expr * term, double * A, const vector<varindx> * vars);
int findwhichvar(expr const * v, const vector<varindx> * vars);



bool purelinsolv(vector<binopexp *> const * eqs,
		 vector<varindx> const * vars,
		 vector<binopexp *> * & sols)
{
  int numeqs = eqs->size();
  int numvars = vars->size();
  int k, q, x;
  int stepr, stepc;
  binopexp * thiseq;
  n_opexp * lhs;
  n_opexp * thisterm;
  double pivot;
  double relerr = 1.E-8;	// should this -> RELERR (= 1.0^-11)?
  double **A;

  int outputprec = 3;
#ifdef WITHDBG
  if (PLSHA & dbglevel) outputprec = 16;
#endif
       /*****************************************************************
	* the equations are converted to				*
	* \sum_{j=0}^{numvar-1} A_{ij} v_j - A_{i numval} = 0		*
	*                NOTE minus sign --^				*
	*****************************************************************/
  A = new double*[numeqs];
  for (k=0; k < numeqs; k++) A[k]= new double[numvars+1];

  for (k=0; k < numeqs; k++) {
    for (q=0; q < numvars; q++) A[k][q] = 0.;
    thiseq = (*eqs)[k];
    if (thiseq->op->opty != equalse)
      throw(string("Non equation in purelinsolv eq "));
    if (thiseq->rhs->etype != numval)
      throw(string("equation with nonnumerical rhs in purelinsolv "));
    else A[k][numvars] = ((numvalexp *)thiseq->rhs)->value;
    switch (thiseq->lhs->etype)
      {
      case physvart:
	if (!doterm(thiseq->lhs,A[k],vars))
	  throw(string("doterm returned false in purelinsolv "));
	break;
      case n_op:
	lhs = (n_opexp *)thiseq->lhs;
	if (lhs->op->opty == multe)
	  if (!doterm(lhs,A[k],vars)) 
	    throw(string("lhs of eq is term not OK, in purelinsolv"));
	  else break;
	for (q=0; q < lhs->args->size(); q++)
	  if (!doterm((*lhs->args)[q],A[k],vars)) 
	    throw(string("lhs of eq has term not OK, in purelinsolv"));
	break;
      case numval:
      case function:
      case binop:
      case unknown:
      case fake:
      default:
	throw(string("lhs of eq in purelinsolv not acceptible"));
      }	// end of switch
  } // end of loop over k (eqs)
  // made A matrix
  //  for each variable (# stepc), find the equation with the largest absolute 
  //  value of the coefficient (pivot). Interchange this equation with lowest
  //  unused (#stepr) , divide rest of this equation by the pivot, then
  //  for each equation other than stepr, subtract the right multiple of 
  //  stepr to make the step column element 0
  //  At step stepr, A[i][j] = 0 for j < stepc (=stepc when done) except for
  //     pivot value
  
  //  I don't really understand how to calculate the largest element in a
  //  column, because the equations have an arbitrary scale. Here I will
  //  try the following: Normalize each equation so that largest coef is 
  //  1. Then for each variable set a comparison number to the largest
  //  |coef| and use that for comparisons to set zero
  double *normvars = new double[numvars];
  for (k=0; k < numeqs; k++) 
    {
      pivot = 0.;
      for (q = 0; q < numvars; q++)
	if (fabs(A[k][q]) > pivot) pivot = fabs(A[k][q]);
      if (pivot != 0)
	for (q = 0; q <= numvars; q++) A[k][q] = A[k][q]/pivot;
    }
  for (q = 0; q < numvars; q++) 
    {
      pivot = 0.;
      for (k=0; k < numeqs; k++)
	if (fabs(A[k][q]) > pivot) pivot = fabs(A[k][q]);
      normvars[q] = pivot;
    }
    DBG( { cout << "In purelinsolv, normvars set to  " ;
           for (k=0; k < numvars; k++) 
	     cout << k << ": " << normvars[k] << "  ";
	   cout << endl;      });
  
  // Now start Gaussian elimination
  for (stepc=0, stepr=0; stepr<numeqs && stepc < numvars; stepr++,stepc++) {

    DBG( {cout << "starting column step " << stepc << " in purelinsolv"
	     << " with stepr = " << stepr;
 	  for (k=0; k < numeqs; k++) {	// excessive, but need temp
	    cout << endl << k << ":\t";
	    for (q = 0; q <= numvars; q++) 
	      cout << setprecision(outputprec) << A[k][q] << "\t"; 
	  }
	  cout << endl;
         });

    pivot = 0;
    while ((pivot == 0) && (stepc < numvars))
      {
	for (k=stepr; k < numeqs; k++)
	  if (fabs(A[k][stepc]) > pivot) {pivot = fabs(A[k][stepc]); q = k;}
/* 	if (pivot < relerr * normvars[stepc++]) pivot = 0.; 
 * the above line was found not to work (use Excir40-Solver.log 7/11/02),
 * primarily because normvars does not accurately reflect the size of a 
 * variable. This test should be rethought, but for now, we drop it. 
 * But we still need to step stepc, so next line was added
 */
	stepc++;
      }
    if (pivot == 0) continue;	// might have had zero pivot on last var
    stepc--;
    double *tempdp = A[q];	// make pivotal equation the step eq
    A[q]=A[stepr];		//  by swapping Eqn q with Eqn stepr
    A[stepr] = tempdp;

    DBG( { cout << "After swap in stepr=" << stepr << ", stepc=" << stepc
		<< " in purelinsolv";
           for (k=0; k < numeqs; k++) {
	     cout << endl << k << ":\t";
	     for (q = 0; q <= numvars; q++) 
	       cout << setprecision(outputprec) << A[k][q] << "\t"; 
	   }
	   cout << endl;
         });
    
    pivot = 1. / A[stepr][stepc];	// normalize pivot row
    for (q = stepc; q <= numvars; q++) A[stepr][q] *=  pivot;
    // remove just solved var from all equations
    for (k = 0; k < numeqs; k++) if (k != stepr)   
      {						   
	pivot = -A[k][stepc];	// add pivot * Eqn stepr to Eqn k
	//	for (q = stepc; q <= numvars; q++) // this enough, but temp
	for (q = 0; q <= numvars; q++) 		// to make sure
	  {
	    A[k][q] += A[stepr][q] * pivot;
	    if (fabs(A[k][q]) < relerr * fabs(A[stepr][q]* pivot))
		A[k][q] = 0.;
	  }
      }
  }  // end of loop over steps. Now left part is numvars dim identity except
     // for variables which couldn't be solved for

  DBG( { cout << "After last step stepr=" << stepr 
	      << ", stepc="<< stepc << " in purelinsolv";
         for (k=0; k < numeqs; k++) {
	   cout << endl << k << ":\t";
	   for (q = 0; q <= numvars; q++) 
	     cout << setprecision(outputprec) << A[k][q] << "\t"; 
	 }
         cout << endl;
       } );

  //  check remaining equations are 0 = 0
  for (k=numvars; k < numeqs; k++)
    if (fabs(A[k][numvars]) > relerr) 
      {
	DBG( { cout << "purelinsolv failed at Eqn " << k
	            << " inconsistency. Here are the variables:" << endl;
	       cout << " number normvar  variable " << endl;
	       for(q = 0; q < numvars; q++)
	         cout << q << ": " << setprecision(3) << normvars[q]<<"\t"
	              << (*canonvars)[(*vars)[q]]->clipsname << endl;
	       cout << "Now the rows in the matrix, one for each equation:"
		    << endl;
	       for(k = 0; k < numeqs; k++) 
		 {
		   cout << k << ": ";
		   for(q = 0; q < numvars; q++) 
		     cout << setprecision(outputprec) << A[k][q] << "\t";    
		   cout << "\t" << setprecision(outputprec) << A[k][numvars]
		        << endl;
		 }
	       cout << "About to abort problem" << endl;
	     } );
	
	for (k=0; k < numeqs; k++) delete A[k];
	delete A;
	return(false);
      }	// end of if error greater than allowed, and of loop k.
    delete [] normvars;
  // output results:
  for (k=0; k < numvars; k++) 
    {
      for (q=0; q < numeqs; q++)
/* Lin modified the next line to 
 * 	if (fabs(A[q][k] > relerr))  // look for equation to write out for this
 * to the one below, but is this right? When making new A's, didn't I do
 * this check already? and will this fail if numbers are too small? 
 * [July 11, 2002]  Removed 7/12/02 see line 147 */
	if (A[q][k] != 0)	// look for equation to write out for this 
	  {			// variable. If it was unsolvable, its nonzero
	    if (fabs(1. -A[q][k]) > relerr)	 // elements should be zapped 
	      {	
		cout << "found non-1 as pivot in result in purelin at row "
		     << q << " column " << k << ", value=" << A[q][k] << endl;
		throw(string("found non-1 as pivot in result in purelin"));
	      }
	    thisterm = new n_opexp(&myplus);
	    // we have lost track of units in solving these equations - 
	    // cheat and set units correctly for variable k
	    dimens dim = (*canonvars)[(*vars)[k]]->MKS;
	    numvalexp * tempnv = new numvalexp(A[q][numvars]);
	    tempnv->MKS = dim;
	    thisterm->addarg(tempnv);
	    thiseq = new binopexp(&equals,new physvarptr((*vars)[k]),thisterm);
	    for (x=k+1; x < numvars; x++)
	      if (A[q][x] != 0) {
		thisterm = new n_opexp(&mult);
//	      thisterm->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor 
		tempnv = new numvalexp(-A[q][x]);
		tempnv->MKS = (*canonvars)[(*vars)[x]]->MKS;
		tempnv->MKS *= -1;
		tempnv->MKS += dim;
		thisterm->addarg(tempnv);
		thisterm->addarg(new physvarptr((*vars)[x]));//2/2/01
		((n_opexp *)thiseq->rhs)->addarg(thisterm);
	      }
	    expr * thiseqex = thiseq;
	    flatten(thiseqex);
	    if (thiseqex->etype != binop)
	      throw(string("Purelin flatten returned non-binop solution!"));
	    sols->push_back((binopexp *)thiseqex);
	    for (x=k+1; x < numvars; x++) A[q][x] = 0; // zapping as above
	    DBG( { cout << "For variable " << k << ", used equation " << q
		        << ", giving solution" << endl;
		   cout << (*sols)[sols->size()-1]->getInfix() << endl;
		   for (x=0; x < numeqs; x++) {
		     cout << endl << x << ":\t";
		     for (q = 0; q <= numvars; q++) 
		       cout << setprecision(outputprec) << A[x][q] << "\t"; 
		   }
		   cout << endl;
	         } );
	    break;
	  } // end of if A[q][k]!=0 and of loop on q
    } // end of loop on k, of numvars.
  // clean up A and depart
  for (k=0; k < numeqs; k++) delete A[k];
  delete A;
  return(true);
}

/************************************************************************
 *  bool doterm(const expr * term, double * A,				*
 *		const vector<varindx> * vars)				*
 *  if term is numval or physvar or numval * physvar, adds to coef in A	*
 *	and returns true, else throws exceptions.			*
 ************************************************************************/
bool doterm(const expr * term, double * A, const vector<varindx> * vars)
{
  int q;
  switch (term->etype)
    {
    case numval:
      A[vars->size()] -= ((numvalexp *)term)->value;
      return(true);
    case physvart:
      if ((q = findwhichvar(term,vars)) < 0)
	throw(string("variable not in vars list in doterm"));
	A[q] += 1.;
	return(true);
    case n_op:
      {
	n_opexp * termnop = (n_opexp *)term;
	if (termnop->op->opty == pluse)
	  throw(string("term in sum is sum, not OK in doterm"));
	switch (termnop->args->size())
	  {
	  case 0:
	    A[vars->size()] -= 1.; // null product is 1
	    return(true);
	  case 1:
	    if ((q = findwhichvar((*termnop->args)[0],vars)) < 0)
	      throw(string("variable not in vars list in doterm"));
	    A[q] += 1.;
	    return(true);
	  case 2:
	    if ( ((*termnop->args)[0]->etype != numval) ||
		 ((*termnop->args)[1]->etype != physvart))
	      throw(string("term a*b but not number * var in doterm"));
	    if ((q = findwhichvar((*termnop->args)[1],vars)) < 0)
	      throw(string("variable not in vars list in doterm"));
	    A[q] += ((numvalexp *)(*termnop->args)[0])->value;
	    return(true);
	  default:
	    throw(string("term a*b*c... not allowed in doterm"));
	  }
      }
    case function:
    case binop:
    case unknown:
    case fake:
    default:
      throw(string("doterm called on wrong type expr"));
    }
}


/************************************************************************
 *  int findwhichvar(expr const * v, const vector<varindx> * vars)	*
 *	returns the index of the physvar v in the vars list, if there.	*
 *  if v is not a physvar, or if it is not found in list, returns -1	*
 ************************************************************************/
int findwhichvar(expr const * v, const vector<varindx> * vars)
{
  if (v->etype != physvart) return (-1);
  for (int k=0; k < vars->size(); k++)
    if (((physvarptr *)v)->varindex == (*vars)[k]) return(k);
  return(-1);
}
