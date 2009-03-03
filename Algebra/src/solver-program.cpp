//////////////////////////////////////////////////////////////////////////////
// solver-program.cpp -- run solver as executable using stdio
// Author(s): Brett van de Sande, 2009
// Copyright 2009 by Kurt Vanlehn and Brett van de Sande
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
//////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include <string>
#include "decl.h"
#include "extstruct.h"
#include "Solver.h"
RETURN_CSTRING solverDoLog(const char* const src);
#include "indysgg.h"
#include "indyset.h"
#include "dbg.h"
using namespace std; 

#define LOGn(s) cout << s << endl
extern vector<indyset> *listofsets;

int indyCanonHowIndy(int setID, int eqnID, vector<int> * & linexpand,
		     vector<int> * & mightdepend );
int indyStudHowIndy(int setID, int eqnID, vector<int> * & linexpand,
		     vector<int> * & mightdepend );
string powersolve(const int howstrong, const string varname, 
		  const int destslot);

extern vector<valander *> *canongrads;
extern vector<valander *> studgrads;
void doinitinit();

//////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[]) {
  string buf;
  
  if (argc > 2) { cerr << "Usage: " << argv[0] << " [dbgmask]" << endl; 
    exit(1) ; }
  if (argc == 2) dbglevel = strtoul(argv[1],0,0);
  doinitinit();
#if 0
  solverDoLog("t");  // make log file
#endif
  // Loop through stdin until "exit" and post result into stdout
  while (getline(std::cin,buf)) {
    int space;
    string command, action;
    char* result;
    space=buf.find_first_of(" ");
    if(space != string::npos){  
      command=buf.substr(0,space);
      action=buf.substr(space);
      cout << "solver got action" << action << endl;
    } else {
      command=buf;
      action="";
    }
    
    if(command == "solverDoLog"){
      result=solverDoLog(action.c_str());
    }
    else if(command == "solverStartLog"){
      result=solverStartLog(action.c_str());
    }
    else if(command == "solverDebugLevel"){
      result=solverDebugLevel(atoi(action.c_str()));
    }
    else if(command == "solveBubble"){
      result=solveBubble();
    }
    else if(command == "solveMoreBubble"){
      result=solveMoreBubble();
    }
    else if(command == "solveAdd"){
      result=solveAdd(action.c_str());
    }
    else if(command == "solveClear"){
      result=solveClear();
    }
    else if(command == "c_indyCanonHowIndy"){
      result=c_indyCanonHowIndy(action.c_str());
    }
    else if(command == "c_indyAddVariable"){
      result=c_indyAddVariable(action.c_str());
    }
    else if(command == "c_indyDoneAddVariable"){
      result=c_indyDoneAddVariable();
    }
    else if(command == "c_indyAddEquation"){
      result=c_indyAddEquation(action.c_str());
    }
    else if(command == "c_indyEmpty"){
      result=c_indyEmpty();
    }
    else if(command == "c_indyAddEq2Set"){
      result=c_indyAddEq2Set(action.c_str());
    }
    else if(command == "c_indyKeepNOfSet"){
      result=c_indyKeepNOfSet(action.c_str());
    }
    else if(command == "c_indyStudHowIndy"){
      result=c_indyStudHowIndy(action.c_str());
    }
    else if(command == "c_indyStudentAddEquationOkay"){
      result=c_indyStudentAddEquationOkay(action.c_str());
    }
    else if(command == "c_indyIsStudentEquationOkay"){
      result=c_indyIsStudentEquationOkay(action.c_str());
    }
    else if(command == "c_subInOneEqn"){
      result=c_subInOneEqn(action.c_str());
    }
    else if(command == "c_powersolve"){
      result=c_powersolve(action.c_str());
    }
    else if(command == "c_solveOneEqn"){
      result=c_solveOneEqn(action.c_str());
    }
    else if(command == "c_simplifyEqn"){
      result=c_simplifyEqn(action.c_str());
    }     
    else if(command == "exit"){
      break;
    } else {
      // error handling for bad command
    }
    // The solver has a lot of print statements, so we mark
    // the actual result as a line beginning with two slashes.
    cout << "//" << result << endl;
  }
  closeupshop();
  return 0;
}
