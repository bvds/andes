// indysgg.h	functions defined in indysgg.cpp
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

using namespace std;

void indyEmpty();
void indyAddVar(const char* const name, double value, const char* const units);
void indyDoneAddVar();
void indyKeepN(int setID, int numberToKeep);
void indyAddEq2CanSet(int setID, int eqnID);
bool indyIsCanonIndy(int setID, int eqnID);
bool indyIsStudIndy(int setID, int eqnID);
std::string indyExpSetCanEq(int setID, int eqnID);
std::string indyExpSetStudEq(int setID, int eqnID);
void indyAddCanonEq(int eqnID, const char* const equation);
int indyAddStudEq(int slot, const char* const equation);
int indyIsStudEqnOkay(const char* const equation);
std::string solveOneEqn(const char * const varName, const int sourceSlot, 
		   const int destSlot);
std::string simplifyEqn(const int sourceSlot, const int destSlot);
std::string subInOneEqn(int sourceSlot,int targetSlot,int destSlot);
int indyCanonHowIndy(int setID, int eqnID, vector<int>* linexpand, vector<int>* mightdepend);
int indyStudHowIndy(int setID, int eqnID, vector<int>* linexpand, vector<int>* mightdepend);
void closeupshop();

enum OkayAns {
  OKAY,         // 0 all is okay
  NOSUCH,       // 1 no such student equation slot
  NOPARSE,      // 2 could not be parsed
  NOTANEQ,      // 3 parsed but not an equation
  SINGULAR,     // 4 added but not diferentiable at solution point
  UNITSNG,      // 5 inconsistant units
  WRONG,        // 6 consistant but wrong
  INACCUR,      // 7 consistant to default student accur but not 100 RELERR
  SLOTEMPTIED   // 8 emptied as asked
};

