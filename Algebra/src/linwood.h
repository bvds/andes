From lht@lzri.com Tue Mar 06 08:40:13 2001
Date: Mon, 05 Mar 2001 15:45:21 -0500
From: Linwood H. Taylor <lht@lzri.com>
To: Joel A. Shapiro <jshapiro@imap.pitt.edu>
Subject: Indy specs (pass one)

// as i said sort of .h file

// the equations sent/received will be lisp type formed

//////////////////////////////////////////////////////////////////////////////

// indyIsIndependant -- determines if equation is independent of current

//  equation set
// argument(s):
//  char* - null-terminated c-string containg equation to check
independence
// returns:
//  char* - "t" all is well or "(Error <string of description>)
// notes:
//
//////////////////////////////////////////////////////////////////////////////

char* indyIsIndependant(const char* const equation); // t / nil

//////////////////////////////////////////////////////////////////////////////

// indyAddData -- adds equation/variable to database
// argument(s):
//  char* - null-terminated c-string containg equation to add as well as
id
// returns:
//  char* - "t" all is well or "(Error <string of description>)
// notes:
//  data will be in a lisp readable form (pre-fix)
//////////////////////////////////////////////////////////////////////////////

char* indyAddData(const char* const data); // t/ nil

//////////////////////////////////////////////////////////////////////////////

// indyDefineSet -- list of indeces defining current set of equations to
use
//  when checking independence
// argument(s):
//  char* - null-terminated c-string containing list of indeces
// returns:
//  char* - "t" all is well or "(Error <string of description>)
// notes:
//  indices will be a string of the form (12 13 14 89 23 ... 43 45)
//////////////////////////////////////////////////////////////////////////////

char* indyDefineSet(const char* const indeces); // t/ nil

//////////////////////////////////////////////////////////////////////////////

// indyEmpty -- clear equations from set of independent equations
// argument(s):
//  NONE
// returns:
//  char* - "t" all is well or "(Error <string of description>)
// notes:
//  should never return failed (out of memory only issue)
//////////////////////////////////////////////////////////////////////////////

char* indyEmpty(); // t / nil



