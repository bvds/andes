//////////////////////////////////////////////////////////////////////////////
// solver.cpp -- solver code
// Copyright (C) 2001 by <Linwood H. Taylor's Employer> - All Rights Reserved.
// Author(s): Linwood H. Taylor <lht@lzri.com>
// Modified:
//      31 January 2001 - lht -- created
//      19 February 2001 - lht -- modified for additional functionality
//////////////////////////////////////////////////////////////////////////////
#include "Solver.h"
#include "coldriver.h"
#include <string>
#include <vector>
#include <cstdio>
#include <iostream>
#include "indysgg.h"
#include "dbg.h"
using namespace std;

//////////////////////////////////////////////////////////////////////////////
#define SOLVER_IS_DEBUGGING
#ifdef SOLVER_IS_DEBUGGING
#include <fstream>
static ofstream joelLog;
static string joelFileName = "Solver.log";
static bool joelLogOn = false;
#define SLog(s) if (joelLogOn) { \
      joelLog.open(joelFileName.c_str(), ios::app); \
      if (joelLog) {joelLog << s << ";" << endl; joelLog.close();} \
      else cout << "Can't open log file " << joelFileName << endl; } 
#define NewLog() if (joelLogOn) { \
   joelLog.open(joelFileName.c_str()); \
   if (joelLog) {joelLog << "Begin" << endl; joelLog.close();} \
   else cout << "Can't open new log file " << joelFileName << endl; }
#else // ifndef SOLVER_IS_DEBUGGING
#define SLog(s)
#define NewLog()
#endif

//////////////////////////////////////////////////////////////////////////////
// added to fix tan(90, 270, (2n+1)90 degrees) problems
#include <cmath>
// below is pi / 180

#define DEG2RAD 0.0174532925199432957692369076848861

#define tanWINDOW 0.0000001
double myTan(double x) {
  int sf = (int)floor((x/DEG2RAD) + tanWINDOW);
  // get closest integer to multiple of 90 deg minus allowance

  int ef = (int)ceil((x/DEG2RAD) - tanWINDOW);
  // get closest integer to multiple of 90 deg plus allowance

  if ((sf == ef) && (sf % 90 == 0) && (((sf / 90) % 2) == 1)) {
    // if the same integer and it's a multiple of (2n+1)90 then
    throw(string("Illegal Argument to trig function")); // throw the eexception
  }
  return tan(x); // otherwise it is okay
}

//////////////////////////////////////////////////////////////////////////////
// catch all for code in coldriver.cpp (should enter in .h)
string powersolve(const int howstrong, const string varname, 
		       const int destslot);
bool handleInput(string& aLine);
bool clearTheProblem();
string itostr(int val);

//////////////////////////////////////////////////////////////////////////////
// local routines and variables not directly accessible from outside this file
//////////////////////////////////////////////////////////////////////////////

/************************************************************************
 *  remove0A0Ds(const char* const str) 					*
 *     removes carriage returns and line feeds from the interchanges    *
 *     from the help system to the algebra system. This is necessary    *
 *     so that the Solver.log file can be compiled			*
 ************************************************************************/
char* remove0A0Ds(const char* const str) {
  char* bfr = const_cast<char*>(str);
  int i = 0;
  while (bfr[i]) {
    if ((bfr[i] == 0x0d) || (bfr[i] == 0x0a)) bfr[i] = ' ';
    i++;
  }
  return bfr;
}

//////////////////////////////////////////////////////////////////////////////
// result is buffer used:
//  1) primary storage for values returned to lisp
//  2) temp workspace for evaluation of data passed by lisp
// size is chosen by the 'make it big enough rule' ... 4K should suffice
//////////////////////////////////////////////////////////////////////////////
static char result[4096];

//////////////////////////////////////////////////////////////////////////////
// setResult - used to copy a message to the result buffer
// argument(s):
//    m - text to be placed in buffer
// returns:
//    NOTHING - no return value
// note(s):
//    mostly just a layer on top of sprintf 'cause I may need to alter the
//    implementation without altering the action
//////////////////////////////////////////////////////////////////////////////
static void setResult(const char* const message) {
  sprintf(result, "%s", message);
}

//////////////////////////////////////////////////////////////////////////////
// copyToResult - used to copy a portion of data to the result buffer
// argument(s):
//    d - source buffer
//    s - index in start of first item to copy
//    e - index in start of last item to copy
//    sp - index to place in result to copy to (defaults to 0)
// returns:
//    NOTHING - no return value
// note(s):
//    used to get things into buffer for mangling before handing off to calls
//////////////////////////////////////////////////////////////////////////////
static void copyToResult(const char* const d, int s, int e, int sp = 0) {
  int j = 0;
  for (j=0; s<=e; s++, j++) {
    result[sp+j] = d[s];
  }
  result[sp+j] = '\0';
}

//////////////////////////////////////////////////////////////////////////////
// makeError - used to form erros to return to lisp
// argument(s):
//    m - message/description of error
//    r - is name of routine/function that error caught in
//    a - arguments passed to function when error occurred
// returns:
//    NOTHING - no return value
// note(s):
//////////////////////////////////////////////////////////////////////////////
static void makeError(const char* const m, const char* const r, const char* const a) {
  sprintf(result, "(Error: <%s(%s)> \"%s\")", r, a, m);
}

//////////////////////////////////////////////////////////////////////////////
// findChar - find wher first occurrence of a char is in a string
// argument(s):
//    toFind - character to search for
//    data - data to search
//    beginAt - index in data to begin search from
// returns:
//    index in data of first encountered occurrence of toFind or -1 if not
// note(s):
//////////////////////////////////////////////////////////////////////////////
static int findChar(char toFind, const char* const data, int beginAt) {
  int l = strlen(data);
  while (beginAt < l) {
    if (data[beginAt] == toFind) {
      return beginAt;
    }
    beginAt++;
  }
  return -1;
}

//////////////////////////////////////////////////////////////////////////////
// routines supplied through interface (solver.h)
//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING solverStartLog(const char* const src) {
  string tmp = src;
  joelFileName = tmp;
  NewLog();
  setResult("t");
  return result;
}

RETURN_CSTRING solverDoLog(const char* const src) {
  string flag = src;
  if ((flag == "T") || (flag == "t")) {
    joelLogOn = true;
  } else {
    joelLogOn = false;
  }
  setResult("t");
  return result;
}

RETURN_CSTRING solverDebugLevel(const unsigned long int x) {
  dbglevel = x;
  setResult("t");
  return result;
}

//////////////////////////////////////////////////////////////////////////////
// solver routines
//////////////////////////////////////////////////////////////////////////////

RETURN_CSTRING solveBubble() {
  SLog("solveBubble()");
  try {
    setResult(solveTheProblem());
  } catch (string message) {
    makeError(message.c_str(), "solveBubble", "");
  } catch(...) {
    makeError("unexpected and unhandled exception", "solveBubble", "");
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING solveMoreBubble() {
  SLog("solveMoreBubble()");
  try {
    setResult(solveMoreOfTheProblem());
  } catch(string message) {
    makeError(message.c_str(), "solveMoreBubble", "");
  } catch(...) {
    makeError("unexpected and unhandled exception", "solveMoreBubble", "");
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING solveAdd(const char* const lispExpression) {
  SLog("solveAdd(\"" << remove0A0Ds(lispExpression) << "\")");
  try {
    string bfr = lispExpression;
    if (handleInput(bfr)) {
      setResult("t");
    } else {
      setResult("nil");
    }
  } catch(string message) {
    makeError(message.c_str(), "solveAdd", lispExpression);
  } catch(...) {
    makeError("unexpected and unhandled exception", "solveAdd", lispExpression);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING solveClear() {
  SLog("solveClear()");
  try {
    if (clearTheProblem()) {
      setResult("t");
    } else {
      setResult("nil");
    }
  } catch(string message) {
    makeError(message.c_str(), "solveClear", "");
  } catch(...) {
    makeError("unexpected and unhandled exception", "solveClear", "");
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
// independence routines
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyAddVariable(const char* const data) {
  SLog("c_indyAddVariable(\"" << remove0A0Ds(data) << "\")");
  try {
    int p = findChar(' ', data, 1);
    int q = findChar(' ', data, p + 1);
    copyToResult(data, p + 1, q - 1);
    double value = atof(result);
    copyToResult(data, 1, p - 1);
    copyToResult(data, q + 1, strlen(data) - 2, p);
    indyAddVar(result, value, &result[p]);
    setResult("t");
  } catch (string message) {
    makeError(message.c_str(), "indyAddVariable", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "indyAddVariable", data);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyDoneAddVariable() {
  SLog("c_indyDoneAddVariable()");
  try {
    indyDoneAddVar();
    setResult("t");
  } catch (string message) {
    makeError(message.c_str(), "indyDoneAddVariable", "");
  } catch (...) {
    makeError("unexpected and unhandled exception", "indyDoneAddVariable", "");
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyAddEquation(const char* const data) {
  SLog("c_indyAddEquation(\"" << remove0A0Ds(data) << "\")");
  string tmp = "";
  try {
    int p = findChar(' ', data, 1);
    copyToResult(data, 1, p - 1);
    tmp += result;
    int equationID = atoi(result);
    copyToResult(data, p + 1, strlen(data) - 2);
    tmp += " ";
    tmp += result;
    indyAddCanonEq(equationID, result);
    setResult("t");
  } catch (string message) {
    makeError(message.c_str(), "indyAddEquation", tmp.c_str()); //data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "indyAddEquation", data);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyEmpty() {
  SLog("c_indyEmpty()");
  try {
    indyEmpty();
    setResult("t");
  } catch (string message) {
    makeError(message.c_str(), "indyEmpty", "");
  } catch (...) {
    makeError("unexpected and unhandled exception", "indyEmpty", "");
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyAddEq2Set(const char* const data) {
  SLog("c_indyAddEq2Set(\"" << remove0A0Ds(data) << "\")");
  try {
    int p = findChar(' ', data, 1);
    copyToResult(data, 1, p - 1);
    int setID = atoi(result);
    copyToResult(data, p + 1, strlen(data) - 2);
    indyAddEq2CanSet(setID, atoi(result));
    setResult("t");
  } catch (string message) {
    makeError(message.c_str(), "indyAddEq2CanSet", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "indyAddEq2CanSet", data);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyKeepNOfSet(const char* const data) {
  SLog("c_indyKeepNOfSet(\"" << remove0A0Ds(data) << "\")");
  try {
    int p = findChar(' ', data, 1);
    copyToResult(data, 1, p - 1);
    int setID = atoi(result);
    copyToResult(data, p + 1, strlen(data) - 2);
    indyKeepN(setID, atoi(result));
    setResult("t");
  } catch (string message) {
    makeError(message.c_str(), "indyKeepNOfSet", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "indyKeepNOfSet", data);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyHowIndy(const int which, const char* const data) {
  try {
    int p = findChar(' ', data, 1);
    copyToResult(data, p + 1, strlen(data) - 2);
    int equationID = atoi(result);
    copyToResult(data, 1, p - 1);
    vector<int> linexpand;
    vector<int> mightdepend;
    switch (which) {
    case 0:
      p = indyCanonHowIndy(atoi(result), equationID, &linexpand, &mightdepend);
      break;
    case 1:
      p = indyStudHowIndy(atoi(result), equationID, &linexpand, &mightdepend);
      break;
    default:
      throw string("No third option in indyHowIndy");
    }
    string retstr("(");
    retstr += (itostr(p) + " (");
    if (p != 0) {
      for (int k=0; k<linexpand.size(); k++) {
        retstr += (itostr(linexpand[k]) + " ");
      }
    }
    retstr += ") (";
    for (int k=0; k<mightdepend.size(); k++) {
      retstr += (itostr(mightdepend[k]) + " ");
    }
    retstr += "))";
    setResult(retstr.c_str());
  } catch (string message) {
    makeError(message.c_str(), "indyHowIndy", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "indyHowIndy", data);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyCanonHowIndy(const char* const data) {
  SLog("c_indyCanonHowIndy(\"" << remove0A0Ds(data) << "\")");
  return c_indyHowIndy(0, data);
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyStudHowIndy(const char* const data) {
  SLog("c_indyStudHowIndy(\"" << remove0A0Ds(data) << "\")");
  return c_indyHowIndy(1, data);
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyStudentAddEquationOkay(const char* const data) {
  SLog("c_indyStudentAddEquationOkay(\"" << remove0A0Ds(data) << "\")");
  try {
    int p = findChar(' ', data, 1);
    copyToResult(data, 1, p - 1);
    int equationID = atoi(result);
    copyToResult(data, p + 1, strlen(data) - 2);
    sprintf(result, "%d", indyAddStudEq(equationID, result));
  } catch (string message) {
    makeError(message.c_str(), "indyStudentAddEquationOkay", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "indyStudentAddEquationOkay", data);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyIsStudentEquationOkay(const char* const data) {
  SLog("c_indyIsStudentEquationOkay(\"" << remove0A0Ds(data) << "\")");
  try {
    copyToResult(data, 1, strlen(data) - 2);
    sprintf(result, "%d", indyIsStudEqnOkay(result));
  } catch (string message) {
    makeError(message.c_str(), "indyIsStudentEquationOkay", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "indyIsStudentEquationOkay", data);
  }

  SLog("// " << result);
  return result;
}


//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_powersolve(const char* const data) {
  SLog("c_powersolve(\"" << remove0A0Ds(data) << "\")");
  try {
    int p = findChar(' ', data, 1);
    int q = findChar(' ', data, p + 1);
    copyToResult(data, 1, p - 1);
    int strength = atoi(result);
    copyToResult(data, q + 1, strlen(data) - 2);
    int slot = atoi(result);
    copyToResult(data, p + 1, q - 1);
    setResult(powersolve(strength, result, slot).c_str());
  } catch (string message) {
    makeError(message.c_str(), "powersolve", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "powersolve", data);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_simplifyEqn(const char* const data) {
  SLog("c_simplifyEqn(\"" << remove0A0Ds(data) << "\")");
  try {
    int p = findChar(' ', data, 1);
    copyToResult(data, 1, p - 1);
    int sourceSlot = atoi(result);
    copyToResult(data, p + 1, strlen(data) - 2);
    int destSlot = atoi(result);
    setResult(simplifyEqn(sourceSlot,destSlot).c_str());
  } catch (string message) {
    makeError(message.c_str(), "simplifyEqn", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "simplifyEqn", data);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_solveOneEqn(const char* const data) {
  SLog("c_solveOneEqn(\"" << remove0A0Ds(data) << "\")");
  try {
    int p = findChar(' ', data, 1);
    int q = findChar(' ', data, p + 1);
    copyToResult(data, q + 1, strlen(data) - 2);
    int destSlot = atoi(result);
    copyToResult(data, p + 1, q - 1);
    int sourceSlot = atoi(result);
    copyToResult(data, 1, p - 1);
    setResult(solveOneEqn(result,sourceSlot,destSlot).c_str());
  } catch (string message) {
    makeError(message.c_str(), "solveOneEqn", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "solveOneEqn", data);
  }

  SLog("// " << result);
  return result;
}

//////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_subInOneEqn(const char* const data) {
  SLog("c_subInOneEqn(\"" << remove0A0Ds(data) << "\")");
  try {
    int p = findChar(' ', data, 1);
    int q = findChar(' ', data, p + 1);
    copyToResult(data, 1, p - 1);
    int sourceSlot = atoi(result);
    copyToResult(data, q + 1, strlen(data) - 2);
    int destSlot = atoi(result);
    copyToResult(data, p + 1, q - 1);
    int targetSlot = atoi(result);
    setResult(subInOneEqn(sourceSlot,targetSlot,destSlot).c_str());
  } catch (string message) {
    makeError(message.c_str(), "subInOneEqn", data);
  } catch (...) {
    makeError("unexpected and unhandled exception", "subInOneEqn", data);
  }

  SLog("// " << result);
  return result;
}

#ifdef _USRDLL
//////////////////////////////////////////////////////////////////////////////
// DllMain -- the entry point for dll management
//////////////////////////////////////////////////////////////////////////////
BOOL APIENTRY DllMain(HANDLE module, DWORD reasonCalled, LPVOID reserved) {
//  NewLog();
  switch (reasonCalled) {
    case DLL_PROCESS_ATTACH:
#ifdef TRACE_OUTPUT    // building version that always prints trace output   
      // init debuglevel to print trace output messages (requires WITHDBG code)
      // dbglevel = 0xFFFFFFFF; // show all trace info 
      // turn off low-level detail which interrupts high-level solver trace:
	dbglevel = ~(EXPRDB | GETEQS | ORDUNK | DIMCHK);
	break;
#endif 
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
    case DLL_PROCESS_DETACH:
      break;
  }

  return TRUE;
//  CloseLog();
}
#endif // def _USRDLL

//////////////////////////////////////////////////////////////////////////////
// End of file solver.cpp
// Copyright (C) 2001 by <Linwood H. Taylor's Employer> - All Rights Reserved.
//////////////////////////////////////////////////////////////////////////////
