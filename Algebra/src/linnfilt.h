// linnfilt.h	functions defined in convert.cpp
// solver routines
//////////////////////////////////////////////////////////////////////////////
char * solveBubbleFile(const char* const src, const char* const dst);
char * solveBubble();
char * solveMoreBubble();
char * solveAdd(const char* const lispExpression);
char * solveClear();
// char * c_indyIsIndependent(const char* const data);
char * c_indyAddVariable(const char* const data);
char * c_indyDoneAddVariable();
char * c_indyAddEquation(const char* const data);
char * c_indyEmpty();
char * c_indyAddEq2Set(const char* const data);
char * c_indyKeepNOfSet(const char* const data);
// char * c_indyExpandInSet(const char* const data);
// char * c_indyStudentExpandInSet(const char* const data);
// char * c_indyStudentIsIndependent(const char* const data);
char * c_indyStudHowIndy(const char* const data);
char * c_indyCanonHowIndy(const char* const data);
char * c_indyStudentAddEquationOkay(const char* const data);
