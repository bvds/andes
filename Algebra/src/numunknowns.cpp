// numunknowns
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//   modified 6/1/01 to have switch on whether to look at knownedness

#include "decl.h"
using namespace std;
// no diagnostics

/************************************************************************
 * int numunknowns(expr * eq, vector<int> & varl,const bool chkknown)	*
 *	returns number of  unknown vars in equation, and adds any	*
 *	unknowns found to list varl (which is list on indices)		*
 ************************************************************************/
//  NOTE: we need to rething what it means to be known. 

int numunknowns(expr * eq, vector<varindx> & varl, const bool chkknown)
{		
  int k, indx;	
  switch (eq->etype)
    {
    case unknown:
    case fake:
      throw ("numunknowns called on unknown or fake expr");
    case numval:
      return(0);
    case physvart:
      if (chkknown && ((physvarptr *) eq)->known) return(0);
      indx = ((physvarptr *) eq)->varindex;
      for (k=0; k < varl.size(); k++) if (indx == varl[k]) return(0);
      varl.push_back(indx);
      return(1);
    case binop:
      return(numunknowns(((binopexp *)eq)->lhs, varl, chkknown)
	     + numunknowns(((binopexp *)eq)->rhs, varl, chkknown));
    case function:
      return(numunknowns(((functexp *) eq)->arg, varl, chkknown));
    case n_op:
      {
	n_opexp * thexp = (n_opexp *) eq;
	int val=0;
	for (k=0; k < thexp->args->size(); k++)
	  val += numunknowns((*thexp->args)[k], varl, chkknown);
      return(val);
      }
    default:
      throw("unknown expr type into numunknowns");
    }
  throw("got to impossible place in numunknowns");
}

