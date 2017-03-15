// new checkeqs.cpp contains checkeqs	(with pieces pulled out as sep functs
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
//     this version modified 4/29 for recassign and dopurelin outputting
//     on soleqs, a vector<binopexp *>*, rather than to a file.
/************************************************************************
 *  tries to find solution of a set of equations in a set of variables,	*
 *  	and write them out to a solution file				*
 *  void *checkeqs(							*
 *		vector<binopexp *> * & eqn, 				*
 *		vector<varindx> * & vars,   (index into canonvars)	*
 *		ostream & file )					*
 ************************************************************************/
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
#include "binopfunctions.h"
using namespace std;

#define DBG(A) DBGF(NEWCKEQSOUT,A)
//  CHKEQS, NEWCKEQSOUT, CHKEQSDTL   might want to rethink these
#define DBGM(A) DBGFM(CHKEQS,A)
#define DBGEQ(A) DBGF(CHKEQS,A)
#define NEWDBGM(A) DBGFM(NEWCKEQSOUT,A)

/************************************************************************
 * checkeqs tries very hard to solve the problem and write out the 	*
 * 	answer in the form (physvar = numvalexp).			*
 * 	If it can't do that, it writes out what it can as partially 	*
 *	solved (physvar = expr in unsolved variables), followed by	*
 *	REMAINING EQUATIONS: numeqs in numvars VARIABLES		*
 *	(the unused equations)						*
 *	unsolved VARIABLES:						*
 *	list of variables unsolved for 					*
 * Here is what checkeqs tries						*
 *	1) It calls recassign, which					*
 *	 a) recursively writes out all equations which can be expressed	*
 *	   as physvar = numval just by arithmetic algebra on a single	*
 *	   equation, but with each such solution plugged into all the	*
 *	   other equations. This stage ends at the line			*
 *	   // Recursive Assignments done. Remove remaining tautologies	*
 *	 b) Some of the remaining equations may have become free of 	*
 *	   variables as a result of substituting in the above (substin).* 
 *	   These equations should be tautologies and are dropped. 	*
 *	   (Though perhaps they should be checked not to be 		*
 *	   inconsistent. This ends at 					*
 *      //Finished discarding tautologies (and possible inconsistencies)*
 *	 c) The solved-for variables are removed from the variable list	*
 *									*
 *	2) Now, if we have not solved all the equations,		*
 *	   we prepare all the remaining equations which are linear in  	*
 *	   the variables to be solved as simultaneous linear equations	*
 *	   by purelinsolv. The inspection and preparation of suitable 	*
 *	   equations is done by fixupforpls, and the suitable ones are	*
 *	   pointed to in the vector trylineqs, and are removed from the	* 
 *	   list eqn. In general there will be some equations fully	*
 *	   solved (as numbers) and others may only be partially solved	*
 *	   (solved in terms of other, unsolved for, variables)		*
 *	   solutions and partial solutions are returned in sols and	*
 *	   partsols, respectively, and the list				*
 *	   of variables involved in the simultaneous linear equations	*
 *	   is in linvars. The ones to be left unsolved if not all can	*
 *	   be solved should be later in the list. A very weak sorting	*
 *	   is now done (see // particular parameters last), followed	*
 *	   by moving the parameters last, except we have no way yet to	*
 *	   satisfy isparam.						*
 *	   The solved for variables are substituted into the remaining	*
 *	   equations and removed from variable list. A flag doagain	*
 *	   is set to zero. If it is positive at the end of checkeqs	*
 *	   checkeqs will call itself again after the remaining steps 	*
 *	   may have caused the preceding to have more scope.		*
 *		(The block just after doagain = 0 is unclear to me now)	*
 *	   again, tautologies are dropped				*
 *	   This part ends at						*
 *		} // end of if purelinsolv				*
 *	3) An attempt is made to factor out powers of physvars from	*
 *	   the remaining equations. If any become linear thereby, we	*
 *	   certainly want to force a recall of checkeqs. (the current	*
 *	   diag message is inconsistent)				*
 *	4) If an equation is nonlinear but in just a single variable,	*
 *	   nlsolvov attempts to write it as var = number. For example,	*
 *	   exp var = 3 is turned into var = 1.099.. (which is ln 3)	*
 *	5) If an equation is a polynomial in a single variable,		*
 *	   solvepoly finds all its solutions. If precisely one 		*
 *	   satisfies the constraints, the equation is replaced with an	*
 *	   assignment equation.						*
 *	6) Next, normexpr is called in preparation for solvetrig, which	*
 *	   requires all its input equations to have 0 rhs. Then 	*
 *	   solvetrig looks for a pair of equations of the form		*
 *		c1 fact1 = k1 fact sin(arg)				*
 *		c2 fact1 = k2 fact cos(arg)	(c1,c2,k1,k2 numbers)	*
 *	   and if signs of fact1 and fact are known, solves them	*
 *	   (see comments at beginning of solvetrig for more, 		*
 *	7) If we have not yet done something to make us think trying 	*
 *	   again will make more progress,				*
 *	   then a more general but weaker simplification, undotrigvars,	*
 *	   which handles similar pairs but with arbitrary coefs of the	*
 *	   sin and cos, is called.					*
 *	8) If the above has done something to make us think trying 	*
 *	   again will make more progress, skip the next step, but if 	*
 *	   not, in desperation we look for equations linear in one 	*
 *	   variable with its coefficient and the rest polynomials in 	*
 * 	   others. For each variable, linvarcoefs extracts from each 	*
 *	   equation which is a polynomial overall and linear in the 	*
 *	   chosen variable, the coef and the const term, checks their 	*
 *	   order in the other variables, and sets 			*
 *	   thishardness = 3 * order of coef + order of const		*
 *	   (a not very carefully selected heuristic). It checks all the	*
 *	   equations linear in this particular variable #q to find the	*
 * 	   one with minimum hardness, storing the hardness and the 	*
 *	   equation in besthardness[q] and lininthis[q]. Then it looks 	*
 *	   for the q with the best besthardness, rewrites the 		*
 *	   corresponding equation as varq = const/coef. For each other	*
 *	   equation, it tries substituting varq->const/coef to see if 	*
 *	   the result has only one variable and can be rationalized	*
 *	   (made a polynomial).  If so, it tries to find all roots of 	*
 *	   the polynomial and determine whether constraints give a 	*
 *	   single answer. If so, we have solved for that variable,	*
 *	   we set doagain for a recall. If not, we leave unchanged	*
 *	   the equation we tried to substitute into.			*
 *	9) If still no reason to think a repeat will help, we look for 	*
 *	   two equations quadratic in the same variable with numerical	*
 *	   coefs. This can be combined to give one equation linear (or 	*
 *	   free of) that variable, which can replace one of the		*
 *	   quadratic ones. Then if linear our current method might work	*
 *	That's what's here now (3/31). One possible improvement:	*
 *		Currently 8) just tries what looks like the best 	*
 *		   chance, but perhaps one should try the other 	*
 *		   possibilities as well.				*
 ************************************************************************/

void checkeqs( vector<binopexp *> * & eqn, // equations remaining to be slvd
	       vector<varindx> * & vars,	// variables left to be solved
	       ostream & solfile) 		// file for solutions
{
  int k, q;
  int doagain;			// should checkeqs recall itself at end
#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif
  vector<binopexp *> partsols; // partially solved vars in purelin
  vector<binopexp *> * soleqs = new vector<binopexp *>;
  
  numpasses++;
  
  // recursive assignments used to eliminate variables:
  recassign(eqn,vars,soleqs);

  // Simultaneous linear equations solved
  dopurelin(eqn, vars, soleqs, &partsols, doagain);
  for (k = 0; k < soleqs->size(); k++) {
    solfile << (*soleqs)[k]->solprint(false) << endl;
    (*soleqs)[k]->destroy();
  }
  delete soleqs;



  // Now try to extract factors of physvars from the equations.
    // should the following only occur if there are equations left? 
  DBG( { cout << "About to try to factor equations." << endl;
	 for (k = 0; k < eqn->size(); k++) 
	   cout << (*eqn)[k]->getInfix()<<endl; 
	 cout << "Before factoring, we have doagain = "<< doagain << endl; });
  if (dofactor(eqn,vars)) doagain = partsols.size() +1;  // force redo
	      // This setting of doagain below had been conditioned on 
	      //	      if (ordunknowns(eqexpr, false) == 1) 

  DBG( { cout << "Checkeqs " << thisdbg <<
	     ": Just after factor and before calling nlsolvov we have" << endl;
         cout << eqn->size() << " equations left, namely" << endl;
	 for (k = 0; k < eqn->size(); k++) 
	   cout << (*eqn)[k]->getInfix() << endl; 
	 cout << "We now have doagain = "<< doagain << endl;} ) ;

  //
  // Now try to solve any equations which contain only one variable.
  // This ought to be done with nlsolvov (non linear solve in one variable).
  //    but also with polysolve. 
  // First nlsolvov

  if (donlsolv(eqn)) doagain = partsols.size() +1; // force redo
  DBG( cout << "We now have doagain = "<< doagain << endl;);

  // Try polysolve
  DBG( cout<< "Checkeqs " << thisdbg << ": now try polysolve" << endl;);
  if (polysolve(eqn)) doagain = 1;
  
  DBG( { cout << "Checkeqs " << thisdbg << ": after polysolve, doagain is " 
	      << doagain << ", and the equations are:" << endl;
	 for (k = 0; k < eqn->size(); k++) 
	   cout << (*eqn)[k]->getInfix() << endl; } );

  DBG( { cout << "Just after nlsolv and before calling normexpr we have " 
	      << eqn->size() << " equations left, namely" << endl;
  for (k = 0; k < eqn->size(); k++) 
    cout << (*eqn)[k]->getInfix() << endl; } );

  // normexpr and solvetrig
  if (eqn->size() >= 2) {
    int dotriggave = dotrig(eqn); // 8/9/02 JaS
    if (dotriggave == 2) doagain = 1;
    else if (dotriggave == 1) if (polysolve(eqn)) doagain = 1;} // 8/9/02 
  VEQCHK(eqn);

  // check that the equations not previously fixupedforpurelin can now be
  for (k = 0; k < eqn->size(); k++)	// added 11/14 to fix problem which 
    if (fixupforpls((*eqn)[k])) {doagain = 1; break;}  // was really elsewhr
  DBG(cout << "After fixupforpls after undotrigvar, doagain is " << doagain
      << endl;);

  // If we still have no reason to believe a repeat of the preceeding will
  // make further progress, we resort to desperate measures, trying first
  // solving equations linear in one variable but polynomial in others,
  // and then looking for pairs of quadratic equations in the same pair of
  // variables.
  if (doagain <= 0) {		// as last hope, try to solve nonconst linear
    if (desperate(eqn,vars))
      {
	doagain = 1;
	DBG(
	  { cout << "desperate reported success, leaving equations " << endl;
	    for (k = 0; k < eqn->size(); k++) 
	      cout << (*eqn)[k]->getInfix() << endl;
	    cout << "and variables not at all solved:" << endl;
	    for (k = 0; k < vars->size(); k++) 
	      cout << (*canonvars)[(*vars)[k]]->clipsname << endl;
	  } );
      }
    else
      {
	q = k;			// no-op just to have something here if nodebug
	DBG( cout << "desperate reported failure. " << endl;);
      }
  } // close of if doagain < 0 for desperate

  VEQCHK(eqn);
  // now place the partially solved variables back on the eqn list
  for (k = 0; k < partsols.size(); k++)
    {
      eqn->push_back(partsols[k]);
      DBG(cout << "partsols "<< k << " of " << partsols.size() 
	  << " put on equation list:  " << 
	  partsols[k]->getInfix() << endl);
    }
	
  // clean up equations and remove any obvious duplicates
  remove_duplicates(eqn,doagain);

  DBG( { cout << "Just before possible recall of checkeqs, we have" << endl;
         cout << eqn->size() << " equations left, namely" << endl;
	 for (k = 0; k < eqn->size(); k++)
	   cout << (*eqn)[k]->getInfix() << endl;
	 cout << "The remaining " << vars->size() << " variables are" << endl;
	 for (k = 0; k < vars->size(); k++)
	   cout << (*canonvars)[(*vars)[k]]->clipsname << endl;
	 cout << "And doagain is " << doagain 
	      << ", if >0 about to recurse"<< endl; } );
	    
  if (doagain > 0) 
    {
      DBG( cout << "Redoing checkeqs after numpasses" << numpasses<< endl);
      checkeqs(eqn,vars,solfile); 
    }
  
  else				// giving up, might as well write out
    {				// partially solved variables and eliminate 
      DBG(cout << "not repeating checkeqs" << endl);
      if (partsols.size() > 0) solfile << "<PARTSLVV>" << endl;
      for (k=0; k < partsols.size(); k++)
	{
	  NEWDBGM(cout << "partsols "<< k << " of " << partsols.size() 
		  << " output to solfile" << endl);
	  solfile << partsols[k]->getInfix() << endl;
	  for (q=0; q < vars->size(); q++)
	    if (
		(partsols[k]->lhs->etype == physvart) &&
		(((physvarptr *)(partsols[k]->lhs))->varindex
		 == (*vars)[q]))
	      {
		NEWDBGM( cout << "partsols["<< k 
			 << "] solves variable [ " << q << "]" << endl);
		(*vars)[q]=(*vars)[vars->size()-1];
		vars->pop_back();
		break;
	      }
	}
      for (q = 0; q < partsols.size(); q++)
	{
	  // remove any link to this parsol in eqn
	  for (k = 0; k < eqn->size(); k++)
	    if (partsols[q] == (*eqn)[k])
	      {
	      (*eqn)[k] = (*eqn)[eqn->size()-1];
	      eqn->pop_back();
	      break;
	      }
	  // delete equation itself
	  partsols[q]->destroy();
	}
    }
}

// clean up equations and remove any obvious duplicates

void remove_duplicates (vector<binopexp *> *  eqn, int & doagain )
{
  int k, q;
  DBG( { cout << "Just before elimination of redundant eqs, we have" << endl;
         cout << eqn->size() << " equations left, namely" << endl;
	 for (k = 0; k < eqn->size(); k++)
	   cout << (*eqn)[k]->getInfix() << endl; } );
  if (eqn->size() > 1) {
    // remove duplicate or proportional equations:
    DBG( cout << "Before eliminating redundant eqs, fix them up" << endl);
    for (k = 0; k < eqn->size(); k++)
      {
	expr * eqexpr = (*eqn)[k];
	eqnumsimp(eqexpr,true);
	flatten(eqexpr);
	eqnumsimp(eqexpr,true);	// is this overkill?
	numvalexp * answer=normexpr(eqexpr);
	if(answer) answer->destroy(); //stop memory leak
      }
    DBG(cout << "after fixing them up" << endl;
	for (k = 0; k < eqn->size(); k++) 
	  cout << "          " << (*eqn)[k]->getInfix() << endl);

    // remove any null equation.
    // Since RHS is zero, any trivial equation has been reduced to 0=0
    for (q = 0; q < eqn->size(); q++)
      {
	numvalexp * answer;
	expr * eqexpr = (*eqn)[q];
	if((answer=normexpr(eqexpr)))
	  answer->destroy();
	else
	  {
	    (*eqn)[q]->destroy();
	    if(eqn->size()>q+1) (*eqn)[q] = (*eqn)[eqn->size()-1];
	    eqn->pop_back();
	    q--;
	  }
      }
    
    // remove redundant equations.
    for (k = 0; k+1 < eqn->size(); k++)
      for (q = k+1; q < eqn->size(); q++)
	{
	  numvalexp * factd=NULL; // result not used
	  DBGM( cout << "dups? and zeros" << k << " " << q << endl);
	  if (uptonum((*eqn)[k],(*eqn)[q],factd))
	    {
	      if(factd) factd->destroy();
	      DBGM(cout <<"YES dups " << k << " "  << q << endl);
	      (*eqn)[q]->destroy();
	      if(eqn->size()>q+1) (*eqn)[q] = (*eqn)[eqn->size()-1];
	      eqn->pop_back();
	      q--;
	    }
	}
  }
  
}
