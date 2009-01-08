// solveeqs.cpp
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

#include "justsolve.h"
#include "decl.h"
#include "dbg.h"
#include "extstruct.h"
#include "unitabr.h"

#define DBG(A) DBGF(SOLVEEQS,A)

/************************************************************************
 * solveeqs   solves the equations in canoneqf 				*
 *	in the variables canonvars, writing the answers in outfile	*
 *   With new solprint, it also enter their values in numsols 		*
 *	(new, April 14, 2001), which must be created and sized before	*
 *	entering here. It also now makes a working copy of canoneqf,	*
 *	to present to checkeqs, which will destroy it, so as to		*
 *	preserve the original for checksol				*
 *   After giving all the solutions, it lists, if any,			*
 *	<PARTSLVV> followed by linear equations not solved numerically. *
 *	<UNSLVEQS> followed by equations not solved and not linear	*
 *	<UNSLVVARS> followed by variables used in equations but not	*
 *		solved for						*
 *	<UNUSEDVARS> followed by variables not used in any equation	*
 *		(and therefore not solved for)				*
 *   returns true if there are no UNSLVEQS or UNSLVVARSQ		*
 ************************************************************************/
bool solveeqs(ostream & outfile){
  int k;
  bool isokay = true;
  vector<varindx> *vars = new vector<varindx>;
  vector<binopexp *> * eqn = new vector<binopexp *>(canoneqf->size(),
						    (binopexp *)NULL);
  expr * eqexpr;
  expr * dimtroub;
  for (k = 0; k < canoneqf->size(); k++) {
    eqexpr = copyexpr((*canoneqf)[k]);
    eqnumsimp(eqexpr,true);
    dimtroub = dimenchk(true,eqexpr);
    if (dimtroub != (expr *)NULL) {
      DBG( cout << "Dimenchk trouble on equation " << eqexpr->getInfix()
	   << ", trouble at " << endl;
	   dimtroub->dbgprint(4); ); 
      throw(string("dimensional inconsistency in input equation ")  
		           + eqexpr->getInfix()); // AW: show bad eqn in msg
    }
    if (eqexpr->etype != binop) 
      throw(string("dimenchk made nonbinop equation"));
    (*eqn)[k] = (binopexp *) eqexpr;
    DBG( cout << "Input " << k << " is now:  " 
	 << (*eqn)[k]->getInfix() << endl);
  } // end of k loop over canoneqf
  // now add in parameter assignments
  for (k = paramasgn->size(); k > 0; k--) {
    eqn->push_back((*paramasgn)[k-1]);
    paramasgn->pop_back();
  }
  for (k = 0; k < canonvars->size(); k++) 
    if ((*canonvars)[k]->isused) vars->push_back(k);
  numpasses = 0;
  checkeqs(eqn, vars, outfile);
  DBG( cout << "After first checkeqs, left with "
       << eqn->size() << " equations and "
       << vars->size() << " unknowns unsolved"
       << endl; );
  
  if (eqn->size() > 0)
    {
      isokay = false;
      // report status
      DBG(cout << eqn->size() << " remaining equation in " << vars->size() 
	  << " variables" << endl;
	  for (k=0; k < eqn->size(); k++)
	    cout << (*eqn)[k]->getInfix() << endl;
	  );
      outfile << "<UNSLVEQS>" << endl;
      for (k=0; k < eqn->size(); k++)
	  outfile << (*eqn)[k]->getInfix() << endl;
      for (k = eqn->size(); k > 0;) // destroy remaining equations in eqn
	{
	  k--;
	  (*eqn)[k]->destroy();
	  eqn->pop_back();
	}
    }
  if (vars->size() > 0)
    {
      isokay = false;
      DBG(cout << "unsolved VARIABLES:" << endl;
	  for (k=0; k < vars->size(); k++) 
	  cout << (*canonvars)[(*vars)[k]]->clipsname << "  ";
	  cout << endl;
	  );
      outfile << "<UNSLVVARS>" << endl;
      for (k=0; k < vars->size(); k++) 
	outfile << "(" << (*canonvars)[(*vars)[k]]->clipsname 
	      << " NIL)" << endl;
    }
  bool gotunused = false;
  for (k = 0; k < canonvars->size(); k++)
    if (!(*canonvars)[k]->isused) {
      if (!gotunused) {
	outfile << "<UNUSEDVARS>" << endl;
	gotunused = true;
      }
      outfile << "(" << (*canonvars)[k]->clipsname 
	      << " NIL)" << endl;
    }
  return(isokay);
}

