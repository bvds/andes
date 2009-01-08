// qsrtexpr.cpp
// a quicksort routine to sort the terms in an n_op
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

// void qsrtexpr(vector<expr *> *Vptr)	
//	sorts a vector of expressions in order desscribed below
//	(see outoforder)

#include "decl.h"
#include "extoper.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(QSRT,A)

// auxiliary functions:
void qsortpc(vector<expr *> *Vptr, int low, int high);
int pivotpart(vector<expr *> *Vptr , int low, int high);
void swap(vector<expr *> *Vptr, int i, int j);
bool outoforder(const expr * ex1, const expr * ex2);


/************************************************************************
 * void qsrtexpr(vector<expr *> *Vptr)					*
 *	sorts a vector of expressions, as might occur in an n_op	*
 *	in a somewhat arbitrary but nearly definite order		*
 *	(terms which differ by numerical factors are not 		*
 *	deterministically ordered, unless one is an n_op and one isn't)	*
 *	(for details of ordering, see outoforder below)			*
 ************************************************************************/
void qsrtexpr(vector<expr *> *Vptr)	// sorts a vector of expressions as
{					//  in a n_op->args
  DBG( { cout << "qsrtexpr called on " << Vptr->size() << " exprs" << endl;
    for (int k=0; k<Vptr->size();k++) cout << (*Vptr)[k]->getInfix() << endl;
  } ) ;
  qsortpc(Vptr,0,((int)Vptr->size())-1);
    DBG( { cout << "qsrtexpr returning list " << endl;
    for (int k=0; k<Vptr->size();k++) cout << (*Vptr)[k]->getInfix() << endl;
  } ) ;
}				

void qsortpc(vector<expr *> *Vptr, int low, int high)
{
  int pivot;
  DBG(cout << "qsortpc called with low/high "<< low << " / " << high << endl;);
  if (low < high)
    {
      pivot = pivotpart(Vptr,low,high);
      qsortpc(Vptr,low,pivot-1);
      qsortpc(Vptr,pivot+1,high);
    }
}

/************************************************************************
 * returns the index of the entry placed in correct place. Still need	*
 *	to sort, separately, the stuff below and the stuff above	*
 ************************************************************************/
int pivotpart(vector<expr *> *Vptr, int low, int high)
{
  int lastlow, i;
  expr *pivotex;
#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif
  
  swap(Vptr,low, (low+high)/2);
  pivotex=(*Vptr)[low];
  lastlow = low;
  for (i=low+1;i<=high;i++)
    //    if ((*Vptr)[i]<pivotex) 
    if (outoforder(pivotex,(*Vptr)[i]))
      {
	DBG(cout << "Pivotpart: " << thisdbg << " Outoforder returns true" 
	    << endl;);
	lastlow = lastlow + 1;
	swap(Vptr,lastlow,i);
      }
    else
	DBG(cout << "Pivotpart: " << thisdbg 
	    << " Outoforder returns false" << endl;);
  swap(Vptr,low,lastlow);
  return(lastlow);
}

/************************************************************************
 * swap(Vptr,i,j) interchanges the i'th and j'th elements of the vector V  *
 ************************************************************************/
void swap(vector <expr *> * Vptr, int i, int j)
{
  expr * temp;
  temp = (*Vptr)[i];
  (*Vptr)[i] = (*Vptr)[j];
  (*Vptr)[j] = temp;
}

/************************************************************************
 * bool outoforder(const expr * ex1, const expr * ex2)
 *  According to the order below, returns true if 
 *	ex1 must come after ex2
 * 	
 * ordered as follows:
 *    1) by etype
 *    2) for same etype, if binop, n_op by op, or if function, by f
 *    3) if physvart, by varindex
 *    3) by first non-numerical arg (if two factors differ only by
 *		having different or nonexistent numerical args, their
 *		orders are considered equal, and they should be combined.)
 *    4) and by subsequent args in order
 *	   a nonexistent arg precedes existent ones.
 ************************************************************************/
bool outoforder(const expr * ex1, const expr * ex2)
{
#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif
  DBG(cout << "Outoforder " << thisdbg << " called on " <<
	      ex1->getInfix() << ", " << ex2->getInfix() << endl;);
  if (ex1->etype < ex2->etype) return(false);
  if (ex2->etype < ex1->etype) return(true);
  if (ex1->etype == numval) return(false);
  if (ex1->etype == physvart) 
    return( ((physvarptr *)ex2)->varindex < ((physvarptr *)ex1)->varindex);
  if (ex1->etype == function)
    {
      if ( ((functexp *)ex1)->f->opty > ((functexp *)ex2)->f->opty) 
	return(true);
      if ( ((functexp *)ex1)->f->opty < ((functexp *)ex2)->f->opty) 
	return(false);
      return outoforder(((functexp *)ex1)->arg,((functexp *)ex2)->arg) ;
    }
  if (ex1->etype == binop)
    {
      if ( ((binopexp *)ex1)->op->opty > ((binopexp *)ex2)->op->opty) 
	return(true);
      if ( ((binopexp *)ex1)->op->opty < ((binopexp *)ex2)->op->opty) 
	return(false);
      if (outoforder( ((binopexp *)ex1)->lhs,((binopexp *)ex2)->lhs))
	return(true);
      if (outoforder( ((binopexp *)ex2)->lhs,((binopexp *)ex1)->lhs))
	return(false);
      return(outoforder( ((binopexp *)ex1)->rhs,((binopexp *)ex2)->rhs));
    }
  if (ex1->etype == n_op)
    {
      int k1,k2;
      if ( ((n_opexp *)ex1)->op->opty > ((n_opexp *)ex2)->op->opty) 
	return(true);
      if ( ((n_opexp *)ex1)->op->opty < ((n_opexp *)ex2)->op->opty) 
	return(false);
      vector<expr *> *v1 = ((n_opexp *)ex1)->args;
      vector<expr *> *v2 = ((n_opexp *)ex2)->args;
      for (k1=k2=0; k1 < v1->size() && k2 < v2->size(); k1++, k2++)
	{
	  if ((*v1)[k1]->etype == numval) { k2--; continue; }
	  if ((*v2)[k2]->etype == numval) { k1--; continue; }
	  if (outoforder( (*v1)[k1],(*v2)[k2])) return true;
	  if (outoforder( (*v2)[k2],(*v1)[k1])) return false;
	}
      if (k1 < v1->size()) return(true);
      return false;
    }
  throw(string("invalid etype in outoforder"));
}
