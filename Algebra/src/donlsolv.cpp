// donlsolv simply applies nlsolv to each equation in list of eqs
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

/************************************************************************
 * bool donlsolv  applies nlsolvov to each equation in eqn, 		*
 * returns true if nlsolvov did so on any of them			*
 ************************************************************************/
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
using namespace std;

#define DBG(A) DBGF(NEWCKEQSOUT,A)
#define NEWDTL(A) DBGFM(NEWCKEQSOUT,A)

bool donlsolv(vector<binopexp *> * & eqn)
{
  bool answer = false;
  for (int k = 0; k < eqn->size(); k++)
    {
      if (nlsolvov((*eqn)[k])) {
	answer = true;
	DBG( cout << "nlsolvov succeeded in finding " << (*eqn)[k]->getInfix()
	          << endl << "Forcing repeat" << endl;);
      }
      else NEWDTL( cout << "nlsolvov failed to solve " << (*eqn)[k]->getInfix()
			<< endl; );
    }
  return(answer);
}
