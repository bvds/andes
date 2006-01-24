// newindy.cpp
//	
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include "decl.h"
#include "dbg.h"
#include "extstruct.h"
#include "indyset.h"
#include "unitabr.h"
#include <math.h>
#include "indysgg.h"

using namespace std;

#define DBG(A) DBGF(INDYEMP,A)
#define DBGM(A) DBGFM(INDYEMP,A)

// others not in decl.h

extern vector<valander *> *canongrads;
extern vector<valander *> studgrads;
extern int numindysets;
extern vector<indyset> *listofsets;
extern int numvars;
extern vector<vector<int> > * listsetrefs;
extern bool gotthevars;
extern vector<int> *lasttriedeq;

int indyCanonHowIndy(int setID, int eqnID, vector<int> * & linexpand,
		     vector<int> * & mightdepend );
int indyStudHowIndy(int setID, int eqnID, vector<int> * & linexpand,
		     vector<int> * & mightdepend );
int indyHowIndy(int setID, expr * eq, valander * val,vector<int> * & linexpand,
		vector<int> * & mightdepend );

/************************************************************************
 * indyCanonHowIndy							*
 *	Attempts to determine whether canonical equation eqnID is 	*
 *	independent of the equations in set setID, and if not, to give	*
 * 	its expansion in terms of equations in that set.		*
 * Returned value:							*
 *	0: definitely independent					*
 *	1: linear approximation shows dependence on equations linexpand	*
 *	   but there are indications that that dependence might not	*
 *	   extend to independence of the full equation. equations in	*
 *	   mightdepend may also be used, as well as others		*
 *	2: linear approximation shows dependence on equations linexpand	*
 *	   but there are indications that that dependence might not	*
 *	   extend to independence of the full equation, but probably 	*
 *	   only on the equations in mightdepend				*
 *	3: appears to be dependent only on equations in linexpand	*
 *	4: definitely dependent only on equations in linexpand		*
 ************************************************************************/

/************************************************************************
 * indyStudHowIndy							*
 *	does exactly the same thing as indyCanonHowIndy, but with the	*
 *	student equation in slot eqnID rather than with			*
 *	the canonical equation (*canoneqf)[eqnID].			*
 ************************************************************************/

/************************************************************************
 * indyHowIndy								*
 *	does what is described in indyCanonHowIndy, but with equation	*
 *	eq and valander val, rather than taking these from lists	*
 *   This is called from indy(Stud|Canon)HowIndy only after a call to   *
 *      isindy has returned false (ie linear approx shows dependency)   *
 ************************************************************************/

int indyHowIndy(int setID, expr * eq, valander * val,vector<int> * & linexpand,
		vector<int> * & mightdepend )
{
  int k;
  DBG( cout << "entering indyHowIndy on set " << setID << " and equation "
       << eq->getInfix() << endl;
       cout << "Valander " << val->print() <<endl);
  // avoid memory leak if linexpand or mightdepend, as new ones will be made
  if ((linexpand != 0L) && linexpand != (vector<int> *)NULL) // was reversed???
    delete linexpand;
  if ((mightdepend != 0L) && mightdepend != (vector<int> *)NULL) // added 6/6
    delete mightdepend;
  // expcoefs is the set of coefs of the equation in linear approx in setID eqs
  vector<double> expcoefs((*listofsets)[setID].expandlast());
  DBGM(cout << "in indyHowIndy expcoefs = "; printdv(expcoefs));
  linexpand = new vector<int>;
  // BvdS:  why do this????
  double scale = 1.0;  	       // set scale for errors = max(1,{|expcoefs[k]|})
  for (k = 0; k < expcoefs.size(); k++) 
    if (fabs(expcoefs[k]) > scale) scale = fabs(expcoefs[k]);
  for (k = 0; k < expcoefs.size(); k++) 
    if (fabs(expcoefs[k]) > RELERR * scale) // for debugging
       linexpand->push_back((*listsetrefs)[setID][k]);
    else if (fabs(expcoefs[k]) > 0.0)
      {
	DBG(cout << "rejecting equation " << k 
	    << " because coefficient " << fabs(expcoefs[k]) << " < " 
	    << RELERR * scale << endl);
      }
  // linexpand now has canonical equation indices of equations on which 
  // there is a dependence in the linear approximation
  vector<int> wehavevar(numvars,0); // wehavevar[vk] will be the number of 
				    // eqns in dependency set with variable vk
  // get all variables on which any of the equations thought dependent depend
  for (k = 0; k < numvars; k++) {			// for each variable
    if (val->hasvar[k]) wehavevar[k] ++; 		// first new eq
    for (int q = 0; q < linexpand->size(); q++)        // now each in expansion
      if ((*canongrads)[(*linexpand)[q]]->hasvar[k]) wehavevar[k] ++;
  }	// wehavevar[k] is now the number of eqs in dep set depending on v_k
  // count the number of variables this set (+ test eq) depend on
  int ournumvar = 0;
  for (k = 0; k < numvars; k++) {
    if (wehavevar[k] > 0) ournumvar++;
    // eliminate variables for which an equation has nonzero gradient component
    // valender has already rounded down to zero, when appropriate
    if (val->gradient[k] != 0.) wehavevar[k] = 0;
    for (int q = 0; q < linexpand->size(); q++) 
      // valender has already rounded down to zero, when appropriate
      if ((*canongrads)[(*linexpand)[q]]->gradient[k] != 0.) 
	wehavevar[k] = 0;
  }


  bool havebadvar = false;
  bool havemaybeeq = false;
  DBGM(cout <<"before checking numvars with wehavevars, linexpand is ";
      for (int q=0; q < linexpand->size(); q++) 
      cout << (*linexpand)[q] << ", ";
      cout << endl;);
  // now for each variable on which there is dependence but no linear
  for (k = 0; k < numvars; k++) 		// term about sol point,
    if (wehavevar[k] > 0) {			// try to find eqn in full set
      bool haverealeq = false;			// which does have linear term
      DBGM( cout << "indyHowIndy wehavevar " << k << endl;);
		// first, look for other eq in set with nonzero grad comp
      int r;
      int foundeq = -1;
      for (int q = 0; q < (*listofsets)[setID].size(); q++) {
	valander * thisval = (*canongrads)[(*listsetrefs)[setID][q]];
	DBGM( cout << thisval->print() << endl;);
	// valender has already rounded down to zero when appropriate
	if (thisval->gradient[k] != 0.) { // provisional eq to return
	  foundeq = (*listsetrefs)[setID][q];      // try to find one with only
	  DBGM(cout << "foundeq provisionally set to " << foundeq << endl);
	  for (r = 0; r < numvars; r++)		   // this one variable
	    // valender has already rounded down to zero when appropriate
	    if (r != k && thisval->gradient[r] !=0.) break;
	  if (r == numvars) { haverealeq = true; break;}// found perfect eqn
	} // end of found one eq with this comp nonzero
      } // end of checking all eqs in set
      if (foundeq >= 0) {
	DBGM( cout << "found eqn for this var, number " << foundeq << endl);
/* AW: remove case that promoted into linexpand because it is unreliable: 
   stud eqn may contain var, but not depend on var, as F = m*a contains m
   but doesn't depend on m when a=0.
	if (haverealeq && (wehavevar[k] == 1)) { // if only one equation in dep
	  linexpand->push_back(foundeq); 	// set had v_k, and we have 
	  wehavevar[k] = 0; }	// assgn for it, add it to dependency set
	else */ {
	  if (!havemaybeeq) { 
	    mightdepend = new vector<int>; 
	    havemaybeeq = true; }
	  mightdepend->push_back(foundeq);
	} // foundeq but not clear dependency
      }	// end of if foundeq
      else {			// no equation in set has gradient in this
	havebadvar = true;	// needed direction.
	DBGM( cout << "found no eqn in set for this var" << endl;);
      }
    } // end of wehavevar[k]

  DBG(cout << "returning from indyHowIndy with linexpand = ";
      for (int q=0; q < linexpand->size(); q++){
	if(q>0)cout << ", ";
	cout << (*linexpand)[q];
      }
    if (mightdepend != 0L) {
      cout << " and mightdepend = ";
      for (int q=0; q < mightdepend->size(); q++){
	if(q>0)cout << ", ";
        cout << (*mightdepend)[q];
      }
      cout << endl; }
      else cout << " without a mightdepend" << endl);

  if (havebadvar) return(1);
  if (havemaybeeq) return(2);
  // seems dependence is real, but we are only certain if either
  // no variables are dependent on other than ones accounted for, or
  // all equations in suggested dependency are linear. check that.
  for (k = 0; k < numvars; k++) if (wehavevar[k] != 0) break;
  if (k == numvars) return(4);
  if (ournumvar <= linexpand->size()) return(4); // must be dependent!
  // What is below assumes all physvars are marked unknown. Need to do that?
  if (ordunknowns(eq,false) != 1) return(3);
  for (k = 0; k < linexpand->size(); k++)
    if (ordunknowns((*canoneqf)[(*linexpand)[k]],false) != 1) return(3);
  return(4);
}

int indyCanonHowIndy(int setID, int eqnID, vector<int> * & linexpand,
	vector<int> * & mightdepend )
{ 
  DBG(cout << "indyCanonHowIndy asked if ";);
  if (!gotthevars) 
    throw(string("\n indyCanonHowIndy called before indyDoneAddVar"));
  if ((eqnID >= canoneqf->size()) || (eqnID < 0))
    throw(string(
     "\n indyCanonHowIndy called for undefined equation"));
  if ((setID >= listofsets->size()) || (setID < 0)) throw(string(
     "\n indyCanonHowIndy called for undefined set"));
  (*lasttriedeq)[setID] = eqnID;
  DBG(cout << (*canoneqf)[eqnID]->getInfix() << " is independent of the "
      << (*listofsets)[setID].size()<< " equations in set " << setID 
      << " of " << listofsets->size() - 1 << " sets."<< endl;);
  if ((*listofsets)[setID].isindy((*canongrads)[eqnID])) return (0);
				// okay, it is really independent, returned 0
  return (indyHowIndy(setID, (*canoneqf)[eqnID],(*canongrads)[eqnID],
		      linexpand, mightdepend));
}
  
int indyStudHowIndy(int setID, int eqnID, vector<int> * & linexpand,
	vector<int> * & mightdepend )
{ 
  DBG(cout << "entering indyStudHowIndy" << endl);
  if (!gotthevars) 
    throw(string("indyStudHowIndy called before indyDoneAddVar"));
  if ((eqnID >= HELPEQSZ) || (eqnID < 0))
    throw(string(
		 "indyStudHowIndy called for undefined equation"));
  if (studeqf[eqnID] == (binopexp *)NULL)
    throw(string("Student Equation ") + itostr(eqnID) + 
	  " is blank, can't be checked for independence");
  if ((setID >= listofsets->size()) || (setID < 0)) 
    throw(string("in indyStudHowIndy called for undefined set"));
  (*lasttriedeq)[setID] = eqnID;
  DBG(cout << "indyStudHowIndy. Determine if " << studeqf[eqnID]->getInfix() 
      << " is independent of the " << (*listofsets)[setID].size()
      << " equations in set " << setID << endl;);
  if ((*listofsets)[setID].isindy(studgrads[eqnID])) {
    DBG(cout << "indyStudHowIndy return 0, is independant" << endl);
    return(0); // okay, it is really independent, returned 0
  }
  return (indyHowIndy(setID, studeqf[eqnID],studgrads[eqnID],
		      linexpand, mightdepend));
}

  
