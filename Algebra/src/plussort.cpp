// plussort.cpp
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

#include "decl.h"
#include "extoper.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(PLUSSORT,A)
void qsrtexpr(vector<expr *> *Vptr);


/************************************************************************
 * bool plussort(expr * & ex)						*
 *	Sorts the addends in a sum, and combines terms when possible	*
 *. on entry ex must be a plus n_op, but it might be changed on exit.	*
 *	Returns true if a change was made beyond reordering		*
 ************************************************************************/
bool plussort(expr * & ex)
{
  int q, k;
  bool answer = false;

  DBG(cout << "entered plussort with " << ex->getInfix() << endl;);
  if (ex->etype != n_op) throw(string("plussort called on non-n_op"));
  if (((n_opexp *)ex)->op->opty != pluse)
    throw(string("plussort called on non-plus n_op"));
  vector<expr *> *v = ((n_opexp *)ex)->args;
  DBG(cout << "Plussort about to call qsrtexpr" << endl;);
  qsrtexpr(v);
  DBG(cout << "Plussort after sort, " << ex->getInfix() << endl;);
  // check for 0 numvals
  for (k = 0; k < v->size(); k++)
    {
      if ((*v)[k]->etype != numval) break; // only numvals need checking, and
      if (((numvalexp *)(*v)[k])->value == 0) {
	(*v)[k]->destroy();
	for (q = k; q+1 < v->size(); q++) (*v)[q] = (*v)[q+1];
	v->pop_back();
	k--;
	answer = true;
      }
    }
  for (q = 0; q+1 < v->size(); q++)
    {
      numvalexp * addfact=NULL;

      DBG(cout << "At start of q loop in Plussort, q =" << q 
	  << ", vsize is " << v->size() << endl;);
      if (uptonum((*v)[q],(*v)[q+1],addfact) && addfact)
	{
	  DBG(cout << "Plussort found combo, " << (*v)[q]->getInfix()
	      << " with " << (*v)[q+1]->getInfix()
	      << ", #1/#2 = " << addfact->getInfix() << endl);
	  if (!(addfact->MKS.unknp() || addfact->MKS.zerop())){
	    DBG(cout << "Plussort error " << addfact->getInfix()
		<< " has non-trivial dimensions" << endl);
	    throw(string("plussort tries to add terms with diff dimens"));
	  }
	  if (addfact->value == -1.)	// terms cancel
	    {
	      (*v)[q]->destroy();
	      (*v)[q+1]->destroy();
	      for (k = q; k+2 < v->size(); k++)
		(*v)[k] = (*v)[k+2];
	      v->pop_back();
	      v->pop_back();
	      DBG(cout << "Plussort cancelled terms, now has " 
		  << ex->getInfix() << endl;);
	    }
	  else			// combo, not cancellation
	    {
	      if (((*v)[q+1]->etype != n_op) ||
		  (((n_opexp *)(*v)[q+1])->op->opty != multe))
		{
		  DBG(cout << "Plussort converting second term to mult "
		      << endl;);
		  n_opexp *temp = new n_opexp(&mult);
//	      temp->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor 
		  numvalexp * tempnv = new numvalexp(1 + addfact->value);
		  tempnv->MKS.put(0,0,0,0,0);
		  temp->addarg(tempnv);
		  temp->addarg((*v)[q+1]);
		  (*v)[q+1] = temp;
		  DBG(cout << "Plussort conversion/combo gives " 
		      <<(*v)[q+1]->getInfix() << endl;);
		}	// end of not mult
	      else		// is already a mult
		{
		  if ( (*((n_opexp *)(*v)[q+1])->args)[0]->etype != numval)
		    {		// need to add arg at front of list, move rest
		      DBG(cout << "Plussort needs to add numval to args of " 
			  << (*v)[q+1]->getInfix() << endl;);
		      vector<expr *> *targs = ((n_opexp *)(*v)[q+1])->args;
		      targs->push_back((*targs)[targs->size()-1]);
		      for (k = ((int)targs->size()) -2; k > 0; k--)
			(*targs)[k] = (*targs)[k-1];
		      (*targs)[0] = new numvalexp(addfact->value+1.);
		      (*targs)[0]->MKS.put(0,0,0,0,0); // added 12/10/01 JaS
		    } // end of needed to add a numval to mult
		  else ((numvalexp *)(*((n_opexp *)(*v)[q+1])->args)[0])->value
			 *= addfact->value + 1.;
		} // was already a mult
	      (*v)[q]->destroy();
	      for (k = q; k+1 < v->size(); k++)
		(*v)[k] = (*v)[k+1];
	      v->pop_back();
	      // maybe we should eqnumsimp (*v)[q] ?
	      DBG(cout << "Plussort replaced combo with " 
		  << (*v)[q]->getInfix() << endl;);
	    }	// was a combination, not a cancellation
	  answer = true;
	  addfact->destroy();
	  q--;			// need to look at this term again.
	} // end of had a combination or cancellation. 
      DBG(cout << "Plussort at end of q = " << q << ", v->size is "
	  << v->size() << ". Now ex is "
	  << ex->getInfix() << endl;);
      DBG(cout << "Plussort, okay?" << endl;);
    }     // end of loop on q body.
  DBG(cout << "Plussort after loop on q = " << q << endl;);
  DBG(cout << flush ;);
  if (v->size() < 2) unnop(ex);
  DBG(cout << "Plussort returning " << ((answer) ? "true" : "false")
      << " with ex = " << ex->getInfix() << endl;);
  return(answer);
}
