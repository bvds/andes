// LispReader.cpp: implementation of the CLispReader class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "fbd.h"
#include "LispReader.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLispReader::CLispReader(FILE* fp)
:m_fp(fp)
{
}

CLispReader::~CLispReader()
{
	// !!! Need to hold on to allocated LispObjs and free them when done !!!	
}

// return next token as a string. Empty string on EOF
// !!! Can't distinguish empty Lisp string "" from EOF!
CString CLispReader::GetToken()
{
	CString strToken;

   // skip white space. We don't handle comments
   int ch;
   while ((ch=getc(m_fp)) != EOF && isspace(ch))
	   ;
  
   switch (ch)
   {
   case EOF: 
	   break;

   case '(': 
	   strToken= "(";
	   break;

   case ')': 
		strToken = ")";
		break;

   case '"': // quote delimited string
	   // scan string until \"
	   while ((ch=getc(m_fp)) != EOF && ch != '"') 
	   {
		   if (ch == '\\')  // backslash escapes next char
		   {
			   ch = getc(m_fp);
			   if (ch == EOF) 
				   break; // out of while loop
		   }
		   strToken += ch;
	   }
	   break;
   
   case '|':  // vbar delimited symbol
	   // scan token until '|'
	   while (ch=getc(m_fp) != EOF && ch != '|') {
		   strToken += ch;
	   }
	   break;
   
   // anything else: aggregate into symbol until break character
   default:
	   do {
			strToken += ch;
	   } while ( (ch=getc(m_fp))!=EOF && !isspace(ch) 
		          && ch != ')' && ch != '(' && ch!= '"');
	   // Push back terminating character so it will get
	   // processed. (needless but harmless for space or EOF).
	   ungetc(ch, m_fp);
	   
	   break;
   }

   //TRACE("GetToken: %s\n", strToken);
   return strToken;
}



CLispReader::Obj* CLispReader::GetObject()
{
   CString strToken = GetToken();

   // might have hit EOF:
   if (strToken.IsEmpty()) 
	   return NULL;

   if (strToken != "(") {
	   TRACE("GetObject: Atom %s\n", strToken);
	   return new Atom(strToken); // need to hold onto it for deletion!
   }
    // else we've got an lparen:
    TRACE("GetObject: BEGIN LIST\n");
    List* pList = new List();	  // need to hold onto it for deletion!
	Obj* pObj;
	while  ((pObj = GetObject()) 
		    && ! (pObj->IsAtom() && ((Atom*)pObj)->m_strValue == ")")){
		 pList->Append(pObj);
	}
	if (! pObj) { // hit EOF without list end
		// throw error?
	}
	TRACE("GetObject: END LIST\n");
	return pList;
}




