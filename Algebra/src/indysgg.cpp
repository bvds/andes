// indysgg.cpp
//    Routine called by the help system for independence and red/green checking
//        
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#define BACKDOOR

#include "decl.h"
#include "dbg.h"
#include "extstruct.h"
#include "indyset.h"
#include "unitabr.h"
#include <math.h>
#include "indysgg.h"

using namespace std;

#define DBG(A) DBGF(INDYEMP,A)

// others not in decl.h
bool getCanonEqn(const string bufst);                    // in getaneqwu.cpp
bool getStudEqn(int slot,const string bufst);            // in getaneqwu.cpp
numvalexp * getfromunits(const string & unitstr);        // in unitabr.cpp

//  Need global variables
bool setupdone = false;        // set the first time indyEmpty is called, 
                               // indicating one-time setup has been done
bool gotthevars;               // set by indyDoneAddVar(), reset by indyEmpty
extern int numparams;		// declared in getallfile
// in extstruct: canonvars, canoneqf

vector<valander *> *canongrads;
vector<valander *> studgrads(HELPEQSZ,0L);
int numindysets;
vector<indyset> *listofsets;
int numvars;
vector<vector<int> > * listsetrefs;
vector<int> *lasttriedeq;

extern unitabrs unittable;

void constsfill();

//
//#include <fstream>
#define fLog(c) 
/*{ \
  std::ofstream zlog("C:/lz.log", std::ios::out | std::ios::app); \
  if (zlog) { \
    zlog << c << std::endl; \
    zlog.close(); \
  } \
}
*/

#ifdef BACKDOOR
#include "backdoor.cpp"
#endif

/************************************************************************
 * indyEmpty   clears out all information on the current problem and 	*
 *   prepares for solving a new problem. If it is the first time it     *
 *   has been called (setupdone == false) it initializes the data       *
 *   data structures:							*
 *     setupdone = true							*
 *     gotthevars = false   (to be set by indyDoneAddVar later)		*
 *     numparams = 0        (to be incremented by parameter statements) *
 *     unittable and table of constants get filled			*
 *     new empty vectors are created for 				*
 *       canonvars, canoneqf, canongrads, paramasgn, numsols, 		*
 *       listofsets, listsetrefs, lasttriedeq				*
 *     new fixed-size vectors with empty slots are created for		*
 *       studeqf, studgrads, studeqsorig				*
 *   if it is not the first time indyEmpty has been called, all of the  *
 *     above structures are restored to the state they would be in      *
 *     after a first-time indyEmpty.					*
 ************************************************************************/
void indyEmpty() {
  fLog("indyEmpty called,");
  int k;
  gotthevars = false;
  numparams = 0;
  if (setupdone) {
    DBG(cout << "IndyEmpty called again" << endl; );
    for (k = ((int)canonvars->size()) - 1; k >= 0; k--) {
      delete (*canonvars)[k];
      canonvars->pop_back();
    }
    DBG(cout << "IndyEmpty emptied canonvars" << endl; );
    for (k = ((int)canoneqf->size()) - 1; k >= 0; k--) {
      (*canoneqf)[k]->destroy();
      canoneqf->pop_back();
    }
    DBG(cout << "IndyEmpty emptied canoneqf" << endl);
    if ((canongrads != 0L) && (canongrads != (vector<valander *> *)NULL)) {
      for (k = ((int)canongrads->size()) - 1; k >= 0; k--) {
        delete (*canongrads)[k];
        canongrads->pop_back();
      }
      DBG(cout << "IndyEmpty emptied canongrads" << endl; );
    }
    for (k = ((int)paramasgn->size()) - 1; k >= 0; k--) {
      (*paramasgn)[k]->destroy();
      paramasgn->pop_back();
    }
    for (k=0; k<HELPEQSZ; k++) {
      if (studeqf[k]) {
        studeqf[k]->destroy();
        studeqf[k] = 0L;
        studeqsorig[k]->erase();
        if (studgrads[k]) {
          delete studgrads[k];
          studgrads[k] = 0L;
        }
      }
    }
    DBG(cout << "IndyEmpty may have emptied studgrads and stueqsorig" <<endl;);
    // check for nonexistence of numsols added due to bad interaction
    // with solveClear = clearTheProblem, which deletes it
    if (numsols) delete numsols;
      //numsols->empty(); // this does not do what one thinks it does
    numsols = new vector<double>;
    DBG(cout << "IndyEmpty made sure numsols is existent empty vector"<<endl;);
    for (k = ((int)listofsets->size()) - 1; k >= 0; k--) {
      indyKeepN(k,0);
      DBG( cout << "IndyEmpty emptied set " << k << endl; );
      listofsets->pop_back();        // this ought to delete indyset k, right?
      DBG( cout << "IndyEmpty popped listofsets, now has " 
                << listofsets->size() << endl; );
      listsetrefs->pop_back(); 
      DBG( cout << "IndyEmpty popped listsetrefs, now has " 
                << listsetrefs->size() << endl; );
      lasttriedeq->pop_back(); 
      DBG( cout << "IndyEmpty popped lasttriedeq, now has " 
                << lasttriedeq->size() << endl; );
    }
  } else {// end of recalled indyEmpty
    DBG(cout << "IndyEmpty called to initialize everything" << endl; );
    constsfill();
    canonvars = new vector<physvar *>;
    canoneqf = new vector<binopexp *>;
    canongrads = new vector<valander *>;
    paramasgn = new vector<binopexp *>;
    studeqf.assign(HELPEQSZ, (binopexp*)NULL);
    studgrads.assign(HELPEQSZ, (valander*)NULL);
    studeqsorig.assign(HELPEQSZ, (string*)NULL);
    for (k=0; k<HELPEQSZ; k++) {
      studeqsorig[k] = new string();
    }
    numsols = new vector<double>;
    listofsets = new vector<indyset>;
    listsetrefs = new vector<vector<int> >;
    lasttriedeq = new vector<int>;
    setupdone = true;
  }
  DBG(cout << "indyEmpty done, returning" << endl;);
  fLog("Done indyEmpty");
}

/************************************************************************
 *  indyAddVar( name,  value, unitstr)                                  *
 *        name is the canonical name of the variable                    *
 *        value is the value in whichever units are given by unitstr    *
 *        unitstr is the preferred units for expressing this variable   *
 ************************************************************************/
void indyAddVar(const char* const name, double value,
                const char* const unitstr)
{
  int k;
  string thename(name);
  DBG(cout << "indyAddVar asked to add " << name << " with value " 
           << value << endl; );
  for (k = 0; k < canonvars->size(); k++) {
    if (thename == (*canonvars)[k]->clipsname) {
      throw(string("indyAddVar got duplicate name") + thename);
    }
  }
  physvar *pv = new physvar(thename);
  pv->prefUnit = unitstr;
  pv->value = value;
  canonvars->push_back(pv);
  numvalexp * nv = getfromunits(unitstr);
  value *= nv->value;
  pv->MKS = nv->MKS;
  nv->destroy();
  
  numsols->push_back(value);
}

/************************************************************************
 * indyDoneAddVar   is called to indicate that all variables for the    *
 *    problem have been declared by indyAddVar.				*
 * it sets numvars. Note this must be complete before			*
 *     indyAddCanonEq or indyAddStudEq can be called			*
 ************************************************************************/
void indyDoneAddVar() {
  DBG(cout << "indyDoneAddVar called" << endl;);
  if (gotthevars) {
    throw(string("Two indyDoneAddVar without intervening indyEmpty"));
  }
  gotthevars = true;
  numvars = canonvars->size();
}

/************************************************************************
 * indyAddCanonEq(int eqnID, const char* const equation) 		*
 *     places the canonical equation given in Lisp form in canonical    *
 *     variables by the string equation, in the list of canonical       *
 *     equations with index eqnID. We are agreed that these calls will  *
 *     occur with eqnID starting from 0 incrementing by 1, so eqnID     *
 *     is really just a check that the help and algebra system agree on *
 *     the index of each equation					*
 *   Each equation is converted to expr form and placed in 		*
 *     canoneqf[eqnID], by a call to getCanonEqn, and its gradient	*
 *     at the solution point is calculated and stored in 		*
 *     canongrads[eqnID]						*
 ************************************************************************/
void indyAddCanonEq(int eqnID, const char* const equation) {
  DBG(cout << "indyAddCanonEq asked to add with index " << eqnID 
      << " the equation" << endl;);

  // ensure that any variables to be added have been (as well as we can <g>)
  if (! gotthevars) {
    throw(string("indyAddCanonEq called before indyDoneAddVar"));
  }
  // make sure that no other equation has been added
  if (eqnID < canoneqf->size()) {
    throw(string("indyAddCanonEq called for already filled slot"));
  }
  // assume that equations com in order
  if (eqnID > canoneqf->size()) {
    throw(string("indyAddCanonEq promised to fill slots in order, didn't"));
  }
  // check that equation is parseable ??? should never happen
  if (! getCanonEqn(equation)) {
    throw(string("Couldn't parse equation ") + string(equation));
  }
  // ???? ensure that equations come in order ????
  if (eqnID +1 != canoneqf->size()) {
    throw(string("can't deal with equations presented except in sequence"));
  }
  DBG(cout << eqnID << ": " << (*canoneqf)[eqnID]->getInfix() << endl);
  DBG(cout << "Ready to push equation gradient" << endl);
  // must be okay so record and quit
  canongrads->push_back(getvnd((*canoneqf)[eqnID], canonvars, numsols));
}


/************************************************************************
 * indyIsCanonIndy(int setID, int eqnID) 				*
 *    checks whether the linear expansion of the canonical equation 	*
 *    with index eqnID is independent of the linear expansions of the   *
 *    equations already placed in set setID				*
 * This is now expected to be used only as part of the more robust	*
 *    question indyCanonHowIndy in newindy.cpp				*
 ************************************************************************/
bool indyIsCanonIndy(int setID, int eqnID) { 
  DBG(cout << "indyIsCanonIndy started" << endl);
  if (!gotthevars) 
    throw(string("indyIsCanonIndy called before indyDoneAddVar"));
  if ((eqnID >= canoneqf->size()) || (eqnID < 0))
    throw(string(
     "indyIsCanonIndy called for undefined equation"));
  if ((setID >= listofsets->size()) || (setID < 0)) throw(string(
     "indyIsCanonIndy called for undefined set"));
  (*lasttriedeq)[setID] = eqnID;
  bool answer=(*listofsets)[setID].isindy((*canongrads)[eqnID]);
  DBG(cout << "indyIsCanonIndy with eqnID=" << eqnID << ", setID=" 
      << setID << ", determined that " << endl << "    "
      << (*canoneqf)[eqnID]->getInfix() << endl << "     is "
      << (answer?"independent":"dependent") << " of the "
      << (*listofsets)[setID].size() << " equations with " 
      << listofsets->size() - 1 << " sets."<< endl);
  return(answer);
}

/************************************************************************
 * indyIsStudIndy(int setID, int eqnID) 				*
 *    checks whether the linear expansion of the equation in student 	*
 *    slot eqnID is independent of the linear expansions of the         *
 *    equations already placed in set setID				*
 * This is now expected to be used only as part of the more robust	*
 *    question indyStudHowIndy in newindy.cpp				*
 ************************************************************************/
bool indyIsStudIndy(int setID, int eqnID)
{ 
  if (!gotthevars) 
    throw(string("indyIsStudIndy called before indyDoneAddVar"));
  if ((eqnID >= HELPEQSZ) || (eqnID < 0))
    throw(string(
     "indyIsStudIndy called for undefined equation"));
  if (studeqf[eqnID] == (binopexp *)NULL)
    throw(string("Student Equation ") + itostr(eqnID) + 
          " is blank, can't be checked for independence");
  if ((setID >= listofsets->size()) || (setID < 0)) throw(string(
     "indyIsStudIndy called for undefined set"));
  (*lasttriedeq)[setID] = eqnID;
  DBG(cout << studeqf[eqnID]->getInfix() << " is independent of the "
      << (*listofsets)[setID].size()<< " equations in set " << setID << endl);
  return((*listofsets)[setID].isindy(studgrads[eqnID]));
}

/************************************************************************
 * indyAddEq2CanSet(int setID, int eqnID)				*
 *    adds the canonical equation with index eqnID to the set setID	*
 *    if setID is one larger than the largest existing set number, a    *
 *    new set is created with just eqnID. If setID is larger than that  *
 *    an exception is thrown. 						*
 * Note the equation to be added must be INDEPENDENT IN LINEAR 		*
 *    APPROXIMATION of the equations already in the set, or an		*
 *    exception is thrown.						*
 ************************************************************************/
void indyAddEq2CanSet(int setID, int eqnID)
{
  if (setID > listofsets->size())
    throw(string("tried to add to set neither defined nor next"));
  if (setID == listofsets->size())
    {
      listofsets->push_back(indyset(numvars));
      vector<int> * temp = new vector<int>;
      listsetrefs->push_back(*temp);
      lasttriedeq->push_back(-1);
    }
  if (!indyIsCanonIndy(setID,eqnID))
    throw(string("Equation ") + itostr(eqnID) + 
          " not independent of what is already in set " + itostr(setID));
  (*listofsets)[setID].placelast();
  (*listsetrefs)[setID].push_back(eqnID);
}

/************************************************************************
 *  indyKeepN(int setID, int numberToKeep)				*
 *     remove all but the first numberToKeep equations from the 	*
 *     set setID. It is not possible to remove random equations. 	*
 *  I believe this is only being used with numberToKeep = 0 to clear	*
 *     the set.								*
 ************************************************************************/
void indyKeepN(int setID, int numberToKeep)
{
  if ((setID >= listofsets->size()) || (setID < 0)) throw(string(
     "indyKeepN called for undefined set"));
  if (!((*listofsets)[setID]).keepn(numberToKeep))
    throw(string("error in call to indyKeepN"));
  while ((*listsetrefs)[setID].size() > numberToKeep)
    (*listsetrefs)[setID].pop_back(); // added 10/23/01 JaS
  return;
}


/************************************************************************
 *  closeupshop()  deletes all the structures created by indyEmpty, to  *
 *    be used when closing down the system (not between problems)	*
 ************************************************************************/
void closeupshop() 
{
  indyEmpty();
  delete canonvars;
  delete canoneqf;
  delete canongrads;
  delete numsols;
  delete listofsets;
  delete listsetrefs;
  delete lasttriedeq;
  delete constnames;
  delete constnumvals;
  // should we output something?
}

