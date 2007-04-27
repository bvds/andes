#ifndef BINOPFUNCTIONS_INCLUDED
#define BINOPFUNCTIONS_INCLUDED
#include "expr.h"
#include "decl.h"
//
//  Associated function prototypes
// 

void recassign( vector<binopexp *> * eqn, vector<varindx> * & vars,
		vector<binopexp *> * soleqs);		       // recassign.cpp
vector<binopexp *> * dopurelin(vector<binopexp *> *eqn, 	// purelin.cpp
       vector<varindx> * & vars, vector<binopexp *> * soleqs,
			       int & doagain );
bool dofactor(vector<binopexp *> * eqn, vector<varindx> * & vars);
bool donlsolv(vector<binopexp *> * eqn);			// donlsolv.cpp
int dotrig(vector<binopexp *> * eqn);			 // dotrig.cpp
#endif
