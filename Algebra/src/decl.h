// decl.h  declarations for lists.cc etc.
#ifdef _WINDOWS
#pragma warning (disable: 4786)
#endif
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

#include <string>
#include <iostream>
#include <vector>
#include <stack>
#include <fstream>
#ifdef _WINDOWS
#pragma warning (disable: 4786)
#endif
using namespace std;
#include "expr.h"

typedef int varindx;

							// in file (*=not done)
double addnum(double a, double b);			      // solveknownvar
void apluskb(expr * & a1, const expr * const a2,		// moreexpr
	     double const coef);
void apluskb(expr * & a1, const expr * const a2,		// moreexpr 
	     numvalexp * nv);
void checkeqs( vector<binopexp *> * & , vector<varindx> * &, 	// checkeqs
	       ostream &);
int cleanup(n_opexp * &);					// cleanup
expr * copyexpr(const expr *);					// copyexpr
expr * dimenchk(const bool fix, expr * & ex);			// dimenchk
void delminus (string * p);					// parseclipseq
bool desperate(vector<binopexp *> * eqn, 			// desperate
	       vector<varindx> * & vars);
bool doesnthave(const expr * ex, const varindx var);		// slvlinonev
string dtostr(double val);					// utils
void eqnumsimp(expr * &, const bool);				// eqnumsimp
bool equaleqs(const expr * exp1, const expr * exp2);		// equaleqs
double evalpoly(const vector<double> * poly, const double x);	// polysolve
bool exprcontains(expr * e,varindx var);			// subexpin
bool factorout(const expr * factor,int n, expr * & expression); // factorout
vector<double> * findallroots(vector<double> *poly);		// polysolve
double findroot(const vector<double> * poly, 			// polysolve
		const vector<double> * polyderiv, 
		const double low, const double high);
void findtrigvars(expr * & ex, vector<expr *> * &trigvars);	// solvetrig
bool fixupforpls(binopexp * & eq);				// fixupforpls
bool flatten(expr * &);						// flatten
string getaline(istream &instr);				// getaline
bool getall(string bufst);					// getall
bool getallfile(istream &);			  	        // getallfile
int getclipsvar(string token,int start);			// parse
vector<double> *getsols(ifstream & solfile);			// getnewsols

void gointeract(vector<binopexp *> * & eqexpr, 			// interact
		vector<varindx> * & vars);
bool hasjustonevar(const expr * e, varindx & pv);		// justonev
bool isanum(string token);					// parse
bool isclean( n_opexp *);					// cleanup
bool isnonneg(string name);					// ispos
bool isnonneg(const expr *);					// ispos
bool ispositive(string name);					// ispos
bool ispositive(const expr *);					// ispos
string itostr(int val);						// utils
void kmult(expr * & ex, const double k);   // ex <-   k * ex	   normexpr
void kmult(expr * & ex, numvalexp * nv);   // ex <-   *nv * ex	   normexpr
bool linvarcoefs(const expr * ex, const varindx var, 		// slvlinonev
	    expr * & coef, expr * & numer);
bool listchk(vector<int> * vchk);				// treechk
bool lookslikeint(double, int &);				// lookslikeint
void maketrigvars(vector<binopexp *> * eqexpr,			// solvetrig
		  vector<expr *> * & trigvars);			// solvetrig
vector<physvar *> *makevarlist(vector<string> *eqs,		// makevarl
			       vector<binopexp *> *);
void minuseq(expr * & a1, const expr * const a2);    //a1 -= a2	   moreexpr
bool nlsolvov(binopexp * & bineq);				// nlsolvov
numvalexp * normexpr(expr * & ex);  // normalizes ex & retns factor// normexpr
int numfactorsof(const expr * factor,const expr * expression); // numfactorsof
int numunknowns(expr * eq, vector<varindx> & varl, 		// numunknowns
		const bool chkknown);
bool ordinvars(const expr * ex, const vector<varindx> * vars, // ordinvars
	       vector<int> * & orders);
int ordunknowns(const expr * eq, const bool chkknown);		// ordunknowns
int parseanum(string token,int start);				// parse
stack<string>* parseclipseq(const string &);			// parseclipseq
stack<string>* parseEqWUnits(const string &);		// parseeqwunits
bool plussort(expr * & ex);					// plussort
vector<double> *polyadd(const vector<double> * poly1, 		// polysolve
			const vector<double> * poly2);
bool polyexpand(const expr * ex,varindx var,			// polysolve
		vector<double> * & coefs);
vector<double> *polymult(const vector<double> * poly1, 		// polysolve
			 const vector<double> * poly2); 
bool polysolve(vector<binopexp *> * eqn, 			// polysolve
	       const vector<varindx> *vars);
vector<double> *polytopow(const vector<double> * poly, 		// polysolve
			  const int pow); 		
int powonev(const expr * eq, const varindx var);		// powonev
void printdv(const vector<double> & vec);			// utils
bool purelinsolv(const vector<binopexp *> * const eqs,		// purelin
	const vector<varindx> * const vars, 
	vector<binopexp *> * sols);
void qsrtexpr(vector<expr *> *Vptr);				// qsrtexpr
bool rationalize(binopexp * & eq);				// rationalize
void remove_duplicates (vector<binopexp *> *eqn, int & doagain);
bool signisknown(const expr * const ex);			// solvetrig
bool slvlinonev(binopexp * & eq, const varindx var);		// slvlinonev
bool slvlinvar(const expr * ex, const varindx var, 		// slvlinvar
	       expr * & numer, expr * & denom);
vector<double> *slvpolyexpr(const expr * eq, varindx var);	// polysolve
bool solveknownvar(expr * & eq);			      // solveknownvar
bool solvetrigvar(const expr * const var, 	  		// solvetrig
		  vector<binopexp *> * & eqn);
int solvetwoquads(binopexp * & ,  binopexp * & , 		// despquad
                  const varindx , const varindx );
bool subexpin(expr * & , const binopexp *);			// subexpin
bool substin(expr * & target, const binopexp * assign);		// substin
bool treechk(const expr * const ex, vector<int> * vchk);	// treechk
bool trigsearch(const expr * const arg, expr *& coef,		// solvetrig
		const expr * const ex, bool & iscos,
		expr * & oside);	  
void unnop(expr * & e);						// flatten
bool undotrigvar(const expr * const arg, 			// solvetrigb
		  vector<binopexp *> * & eqn);			// solvetrigb
bool uptonum(const expr * const ans, const expr * const term,   // normexpr
             numvalexp * & coef);
// stupid windows seems to have its own private min and max - call Joel Klein!
#ifdef NO_DLL
inline int max(int a, int b) { return (a > b) ? a : b; }
inline int min(int a, int b) { return (a > b) ? b : a; }
#endif
