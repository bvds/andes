// dimchkeqf.cpp
/************************************************************************
 * dimchkeqf(outStream)							*
 *	reports on any dimensional inconsistency in the equations	*
 *	or in the solution file						*
 ************************************************************************/
#include <stdio.h>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"

#define DBG(A) DBGF(DIMCHK,A)

void dimchkeqf(ostream & outstr)
{
  int k;
  expr * trouble = (expr *) NULL;

  // Note: don't need to check variable units = solution units, as 
  // solprint would refuse to output such a thing.
  DBG( cout << "Entering dimchkeqf." << endl;);

  bool inconsist = false;
  for (k = 0; k < canoneqf->size(); k++) {
    expr *eqexpr = (expr *)(*canoneqf)[k];
    trouble = dimenchk(true,eqexpr);
    if (eqexpr->MKS.unknp()) {
      if (!inconsist){
	outstr << "<INCONSISTENCIES>" << endl;
	inconsist = true; }
      outstr << "Unknown overall dimension undetermined in " 
	     << eqexpr->getInfix() << endl;
    }
    if (trouble != (expr *)NULL) {
      if (!inconsist){
	outstr << "<INCONSISTENCIES>" << endl;
	inconsist = true; }
      outstr << "Dimensional inconsistency at subexpression "
	   << trouble->getInfix() << " in equation " << eqexpr->getInfix() 
	     << endl; 
    }
  }

  DBG( cout << "next check constraints" << endl;);
  for (k = 0; k < canonvars->size(); k++) {
    physvar * thisvar = (*canonvars)[k];
    if (thisvar->isnonneg && ((*numsols)[k] < 0)){
      if (!inconsist){
	outstr << "<INCONSISTENCIES>" << endl;
	inconsist = true; }
      outstr << thisvar->clipsname << " should be nonnegative but is "
	     << (*numsols)[k] << endl;
    }
    if (thisvar->isnonzero && ((*numsols)[k] == 0)) {
      if (!inconsist){
	outstr << "<INCONSISTENCIES>" << endl;
	inconsist = true; }
      outstr << thisvar->clipsname << " should be nonzero but is "
	     << (*numsols)[k] << endl;
    }
  }
}
