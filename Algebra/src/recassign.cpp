// recassign: apply recursive assignment to equations
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//    this had been the first part of checkeqs until pulled out 3/31/01
/************************************************************************
 *  recassign   applies all assignment statements included in eqn list	*
 *  	to the rest of equations, and writes them out in solution file,	*
 *	removing the statement and its variable from the lists		*
 *	Checks remaining equations after simplification to see if they	*
 *	have become assignments, and recurses until no further progress *
 *  void recassign(							*
 *		vector<binopexp *> * & eqn, 				*
 *		vector<varindx> * & vars,   (index into canonvars)	*
 *		vector<binopexp *> * soleqs )				*
 ************************************************************************/
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
#include "binopfunctions.h"
using namespace std;

#define DBG(A) DBGF(NEWCKEQSOUT,A)
//  CHKEQS, NEWCKEQSOUT, CHKEQSDTL   might want to rethink these
#define DTL(A) DBGFM(CHKEQS,A)
#define DBGEQ(A) DBGF(CHKEQS,A)
#define NEWDTL(A) DBGFM(NEWCKEQSOUT,A)
#define OUTNOTE(A) DBGF(OUTSOL,A)

void recassign( vector<binopexp *> * & eqn, // equations remaining to be slvd
	       vector<varindx> * & vars,	// variables left to be solved
	       vector<binopexp *> * soleqs) 		// file for solutions
{
  int j,k, q;
  int numeqs = eqn->size();
  int lastsolved = -1;	// last equation solved before this while iter
  int numsolved = 0;
  
  vector<varindx> varl;	// used only temporarily for numunknowns

  numpasses++;
  
  int whilenum = 1;
  while(lastsolved != numsolved) 
    {
      lastsolved = numsolved;
      
      for (j= lastsolved; j<numeqs; j++) 
	{
	  expr *thiseq = (*eqn)[j];
	  DBGEQ( cout << "in checkeqs on " << thiseq->getInfix() << endl;);
	  DTL( {cout << "dimenchk to be called on "  << endl; 
	  	thiseq->dbgprint(4);});
	  expr *inconst = dimenchk(true,thiseq);
	  if (inconst != (expr *)NULL) 
	    throw(string("Checkeqs: dimenchk returned inconsistency at ")
		  + inconst->getInfix());
	  DTL( {cout << "dimenchk returned, and eqnumsimp to be called on " 
		     << endl; 
	  	thiseq->dbgprint(4);});
	  eqnumsimp(thiseq,true);
	  DTL( {cout << "eqnumsimp returned "  << endl; thiseq->dbgprint(4);});
	  k = ordunknowns(thiseq,false); // order of equations in unknown vars
	  varl.clear();
	  q = numunknowns(thiseq,varl,false); // number of unknown vars in eqn
	  DTL( cout << "ordunknowns and numunknowns returned " << k
	            << ", "  << q << endl;);
	  if ((k==1) && (q==1))  
	    {
	      DTL( { cout << "about to call solveknownvar on" << endl;
	      	     thiseq->dbgprint(2);  } );
	      if (!solveknownvar(thiseq)) 
		cout << "couldn't solve linear eq in one variable!" << endl;
	      else   {		// simple linear equations "solved"
		DTL( { cout << " solveknownvar returned true" << endl;
		       thiseq->dbgprint(2); } );
		if (j != numsolved)
		  (*eqn)[j] = (*eqn)[numsolved];
		if (thiseq->etype != binop) throw(string(
			 "solveknownvar returned a non-binop equation!"));
		expr *troub = dimenchk(true,thiseq);
		if (troub != (expr *) NULL) 
		  DBG(cout << "Dimenchk before write-out had trouble with "
		      << troub->getInfix() << endl);
		(*eqn)[numsolved]=(binopexp *) thiseq;
		DBG(cout << "About to push onto soleqs " 
		    << (*eqn)[numsolved]->getInfix() << endl; );
		// was	solfile << (*eqn)[numsolved]->solprint()  << endl;
		soleqs->push_back((binopexp *)copyexpr((*eqn)[numsolved]));

		numsolved++;
		DTL( { cout << "After solving the " << numsolved << 
		    " equation, before substitutions, remaining equations are "
		       << endl;
		  for (q=numsolved; q < eqn->size(); q++) 
		    cout << q << ": " << (*eqn)[q]->getInfix() << endl;} );
		for (q=numsolved; q < eqn->size(); q++)
		  {
		    expr * eqexpr = (*eqn)[q];
		    DTL( cout << "about to substin " << numsolved-1 << " in "
			 << q << endl;);
		    if (substin(eqexpr,(*eqn)[numsolved-1])) {
		      DTL( cout << "substin worked, about to eqnumsimp eq " 
			   << q << " which is "<< eqexpr->getInfix()<< endl;);
		      eqnumsimp(eqexpr,true);
		      DTL( cout<< "Eqnumsimp returns " << eqexpr->getInfix()
			   << endl;);
		      while(flatten(eqexpr)) // added 2/11. Hope no hangs
			DTL(cout<< "Flatten returns "
			    << eqexpr->getInfix()<< endl;);
		      if (eqexpr->etype != binop) throw(string(
		     "substin/eqnumsimp/flatten gives a non-binop equation!"));
		      (*eqn)[q]=(binopexp *)eqexpr;
		    }
		  }
	      } // end of else (ie did solve linear equation
	    } // end of was linear in one variable
	} // end of loop of equations j
      DTL( { cout << "finished solveknownvar " << whilenum
	          << " with last, num solved = "<< lastsolved
		  << ", " << numsolved << endl; } );
      DBGEQ( { cout << "After writing Asgn||"<< whilenum << ", eqn is" << endl;
	       for (int qk=0; qk < eqn->size(); qk++)
	         cout << qk << ": " << (*eqn)[qk]->getInfix() << endl; } );
      whilenum++;
    } // end of while loop over attempts to rewrite as assignments and plug in.
  whilenum -= 2;
  // END OF RECURSIVE ASSIGNS
  // Recursive Assignments done. Remove remaining tautologies,
  //   write out and discard all used-up equations
  //   remove vars which have had assignment statements written out from
  //   vars list
  // If this has solved the problem, return. Otherwise continues
  //   at TRY PURELINSOLV

  DBG( cout << "after recursive plugins, solved " << numsolved 
       << " of " << vars->size() << " variables" << endl;);
  OUTNOTE(if (numsolved == vars->size()) 
	 cout << "Solved for all variables" << endl;);
  // from equations which are left, eliminate those without variables.
  DBG( cout << "Checking equations " << numsolved << " through "
       << eqn->size()-1 << endl;);
  for (k=numsolved; k < eqn->size(); k++)
    {
      expr * eqexpr = (*eqn)[k];
      eqnumsimp(eqexpr,true); // remove equations without content
      // ?? weren't these all just eqnumsimped and flattened 40-45 lines ago?
      DBG( { cout << "Checking eqn " << k << " after eqnumsimp, is " << endl;
      eqexpr->dbgprint(4); } );
      if (ordunknowns(eqexpr,false) == 0) 	// (ignore inconsistencies!)
	{				// Shouldn't we really check these?
	  DBG( 				// What would we do if not ok? Better
	    cout << "ordunknowns returned 0" << endl;);// to leave for checksol
	  eqexpr->destroy();	// this was missing until 1/28/01
	  (*eqn)[k] = (*eqn)[eqn->size()-1];
	  eqn->pop_back();
	  k--;
	  DBG( cout << "eqn now has " << eqn->size() << " elements" << endl;);
	}
      else 
	{
	  if (eqexpr->etype != binop) throw(string(
			 "eqnumsimp gives a non-binop equation!"));
	  (*eqn)[k] = (binopexp *)eqexpr;
	}
    }
  DBG(cout << "At end of loop to eliminate equations, equations "
      << numsolved << " through " << eqn->size()-1 << " are" << endl;
      for (k=numsolved; k < eqn->size(); k++)
      cout << "          " << (*eqn)[k]->getInfix() << endl);

  //Finished discarding tautologies (and possible inconsistencies).
  // Now discard all used-up equations (which are replaced by 
  // assignments that have been written out to soleqs

  int numvarsorig = vars->size();
  int last = numvarsorig - 1;
  // remove variables from vars if they have been given by assignment
  // statements already output to soleqs
  for (k=0; k < numsolved; k++)
    for (q=0; q < vars->size(); q++) 		// shouldn't this start at
      if (( (*eqn)[k]->lhs->etype == physvart) && 	// q = numsolved ?
	  ( ((physvarptr *)((*eqn)[k]->lhs))->varindex
	    == (*vars)[q]))
	{
	  (*vars)[q]=(*vars)[last--];
	  vars->pop_back();
	  break;
	}
  DBGEQ( cout <<"just removed vars solved by recursive assignment in checkeqs,"
	      << " left with " << vars->size() << " variables unsolved" 
	      << endl; );
  
  // destroy used-up equations
  for (q=0; q < numsolved; q++)
    (*eqn)[q]->destroy();
  for (q=0; q+numsolved < eqn->size(); q++)
    (*eqn)[q]=(*eqn)[q+numsolved];
  for (q=0; q < numsolved; q++)
    eqn->pop_back();

  DBGEQ( cout << 
	 "just removed used-up eqs after recursive assignment in checkeqs,"
	 << " left with "<< eqn->size() << endl;);

  // If we are done, exit
  if (eqn->size() == 0)
    {
      OUTNOTE(cout << "solved all equations by simple recursive assignment" 
	     << endl; );
      return;
    }
}

