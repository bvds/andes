// parse.cc
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
// contains
//    parseanum
//    isanum
//    getclipsvar

#pragma warning (disable: 4786)
#include <string>
using namespace std;
// no diagnostics

/************************************************************************
 *  parseanum:	interpret as much as possible of string as a number	*
 *  int parseanum(string token, int start) takes as much of 		*
 *  token[start,stop] as possible to have a integer or floating point 	*
 *  number, including possible exponent with e				*
 *  returns stop+1, so if start=stop, no number was found		*
 ************************************************************************/
int parseanum(string token,int start)
{
  enum gotsofar { nothing, justdot, justsign, integ, floatnum, uexp, exp };
  int i;
  
  /**********************************************************************
   *  what we have so far. start with nothing. 				*
   *  justsign: so far have only + or - . If nothing else comes, not a	*
   *		number							*
   *  justdot:  so far have "." or "-." or "+." . Again, if nothing	*
   *  		else comes, not a number				*
   *  integ:    what we have is a legitimate integer.			*
   *  floatnum: what we have is a legitimate float without an exponent	*
   *  uexp:     just read the E of a float with exponent		*
   *  exp:      have read E and number or sign				*
   *									*
   *  bugs***   no error checking on exp having no digits. 		*
   **********************************************************************/
  gotsofar sofar=nothing;
  for (i=start;i<token.size();i++)
    {
      switch (sofar) {
      case nothing:
	if (isdigit(token[i]))
	  {  
	    sofar = integ;   
	    break;; 
	  }
	if ((token[i]=='+')||(token[i]=='-'))
	  {  
	    sofar = justsign; 
	    break;;
	  }
	if (token[i]=='.')
	  {
	    sofar = justdot;
	    break;;
	  }
	else 	return(i);

      case justsign:
	if (token[i]=='.')
	  {
	    sofar = justdot;
	    break;;
	  }
	if (isdigit(token[i]))
	  {
	    sofar = integ;
	    break;;
	  }
	else return(i-1);	// sign was not part of number
      case justdot:		// justdot is "." or "+." or "-."
	if (isdigit(token[i]))
	  {
	    sofar = floatnum; 
	    break;;
	  }
	else 
	  return(start);	// was not a number
      case integ:
	if (isdigit(token[i])) break;;
	if (token[i]=='.')
	  {
	    sofar = floatnum;
	    break;;
	  }
	if ((token[i]=='E')||(token[i]=='e'))
	  { sofar = uexp;  break;; }
	else return(i);
      case floatnum:
	if (isdigit(token[i])) break;;
	if ((token[i]=='E')||(token[i]=='e'))
	  { sofar = uexp;  break;; }
	else return(i);
      case uexp:
	if ((token[i]=='+')||(token[i]=='-')||isdigit(token[i]))
	  {  sofar = exp;   break;; }
	else return(i-1);	// probably error - got E but no num
      case exp:
	if (! isdigit(token[i])) return(i);
      }
    }
  if ((sofar==justsign) || (sofar==justdot)) return(i-1);
  else return(i);
}

bool isanum(string token)	// returns true if token is an number
{ 
  return (token.size()==parseanum(token,0));
}

/************************************************************************
 *  finds the longest string which starts with an alphabetic char or $	*
 *  and continues with '-', '_', '$', '&', or alphanumeric chars.	*
 *  returns the index of first character not in string			*
 ************************************************************************/
int getclipsvar(string token, int start)
{
  int j;
  if (!isalpha(token[start]) && (token[start] != '$')) return (start);
  for (j = start+1; j<token.size();j++)
    if ((!isalnum(token[j]))
	&&(token[j] != '-') 
	&&(token[j] != '_')
	&&(token[j] != '&')
	&&(token[j] != '$'))
      return(j);
	throw(string("getclipsvar ran off end of token string"));
}
