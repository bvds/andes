// indysgg.h	functions defined in indysgg.cpp

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

