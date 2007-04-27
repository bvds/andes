// dofactor	try to factor out each physvar to some power from each eqn
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
using namespace std;

#define DBG(A) DBGF(NEWCKEQSOUT,A)
#define NEWDTL(A) DBGFM(NEWCKEQSOUT,A)

/************************************************************************
 * bool dofactor(vector<binopexp *> * & eqn, vector<varindx> * & vars)	*
 *	An attempt is made to factor out powers of physvars from	*
 *	the equations eqn. If any become linear thereby, we		*
 *	certainly want to force a recall of checkeqs. (the current	*
 *	diag message is inconsistent)					*
 * returns true if a factor has been extracted. One might argue it 	*
 *	should only do so if the resultant equation is linear		*
 ************************************************************************/ 
bool dofactor(vector<binopexp *> * eqn, vector<varindx> * & vars)
{
  int j, k, q;
  bool answer = false;
  
  for (k = 0; k < eqn->size(); k++)
    {
      expr * eqexpr = (*eqn)[k];
      for (q = 0; q < vars->size(); q++)
	{
	  physvarptr *tryfact = new physvarptr((*vars)[q]);
	  NEWDTL( 
	    cout << "trying to factor " << (*canonvars)[(*vars)[q]]->clipsname
	         << " from equations " << (*eqn)[k]->getInfix() << endl; );
	  j = numfactorsof(tryfact,eqexpr);

#ifdef AW_POW_FIX
	  // [AW: added 06/27/03 for Expow7 bug reported in kay email 6/23/03: Solve for W in
	  // W=k*(v2^2 - v1^2) and v1=v2 left equation W = 0 remaining after dopurelin. Zero-
	  // valued W was then factored out here, leaving 1=0, which gets eliminated.]
	  // if var not known non-zero, factoring it out could be invalid, so don't.	  
	  if (j > 0 && ! (*canonvars)[tryfact->varindex]->isnonzero) 
	  {
		  // note missed opportunity in trace - might explain a failure to solve:
          DBG( { cout << "Ignoring chance to factor out " << tryfact->getInfix() 
			          << " because variable not known non-zero." << endl; } );
		  continue; // to next variable q
	  } // [End AW added 06/27/03]
#endif // AW_POW_FIX	  
	  if (j > 0) if (factorout(tryfact,j,eqexpr))
	    {
	      DBG( { cout << "Factored out " << j << "/2 factors of "
			  << tryfact->getInfix() << " to leave"
			  << endl;
		     cout << eqexpr->getInfix() << endl; } );
	      eqnumsimp(eqexpr,true);
	      answer = true;
	      DBG( cout << "As we factored out something, will return true";);
	      // It is not clear if both the return and the diagnostic 
	      // message should not be conditioned on linearity, which will
	      // definitely make progress, while factoring might not lead
	      // to an answer. But it is an irreversible step

	      if (ordunknowns(eqexpr, false) == 0) // maybe should kill with
		{				// known vars too?
		  eqexpr->destroy(); // not here before 1/28/01
		  (*eqn)[k] = (*eqn)[eqn->size()-1];
		  eqn->pop_back();
		  k--;
		  break;
		}
	      else		// can't move this outside q loop because if
		{		// so, break above doesn't skip it.
		  if (eqexpr->etype != binop) {
		    cerr << "On variable " 
			 << (*canonvars)[(*vars)[q]]->clipsname
			 << " factoring from equation "<< (*eqn)[k]->getInfix()
			 << endl;
		    throw(string(
		       "factor/eqnumsimp gives a non-binop equation!"));
		  }
		  (*eqn)[k] = (binopexp *)eqexpr; 
		}
	    } // end of if j>0 (ie if factored out something)
	  delete tryfact;
	} // end of loop over q of variables to try to factor out.
    } // end of loop over k, index of equation to examine
  return(answer);
}

  
