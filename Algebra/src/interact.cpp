// interact.cpp		interactive manipulation of equations
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
#pragma warning (disable: 4786)

#include "decl.h"
#include "extoper.h"
#include "extstruct.h"
using namespace std;

// no diagnostics
void printcoms();
void listcoms();

void gointeract(vector<binopexp *> * & eqn, vector<varindx> * & vars)
{
  int k, q;
  char ansch;
  string command;
  int num, current;
  expr *cureq;
  cerr << "Interact with equations" << endl;
  cerr << "Here is the list of equations" << endl;
  cout << "Interact with equations" << endl;
  cout << "Here is the list of equations" << endl;
  for (k=0; k < eqn->size(); k++)
    {
      cerr << k << ": " << (*eqn)[k]->getInfix() << endl;
      cout << k << ": " << (*eqn)[k]->getInfix() << endl;
    }
  cerr << endl << "Here is the list of variables" << endl;
  cout << endl << "Here is the list of variables" << endl;
  for (k=0; k < vars->size(); k++)
    {
      cerr << k << ": " << (*canonvars)[(*vars)[k]]->clipsname << endl;
      cout << k << ": " << (*canonvars)[(*vars)[k]]->clipsname << endl;
    }
  printcoms();
  for (;;)
    {
      cerr << "Command: ";
      cin >> command;
      cout << command << endl;
      if (command.compare("exit") == 0) return;
      if (command.compare("help") == 0) {printcoms(); continue;}
      if (command.compare("commands") == 0) {listcoms(); continue;}
      if (command.compare("infix") == 0) 
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= eqn->size())
	    {
	      cerr << "no such equation numbered " << num << endl;
	      cout << "no such equation numbered " << num << endl;
	    }
	  else
	    {
	      cerr << (*eqn)[num]->getInfix() << endl;
	      cout << (*eqn)[num]->getInfix() << endl;
	      current = num;
	    }
	  continue;
	}
      if (command.compare("dbg") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= eqn->size())
	    {
	      cerr << "no such equation numbered " << num << endl;
	      cout << "no such equation numbered " << num << endl;
	    }
	  else 
	    {
	      (*eqn)[num]->dbgprint(0);
	      current = num;
	    }
	  continue;
	}
      if (command.compare("solvefor") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= vars->size())

	    cerr << "no such variables numbered " << num << endl;
	  else 
	    {
	      cerr << "sorry, dont have solveeqforvar yet" << endl;
	    }
	  continue;
	}
      if (command.compare("substin") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= eqn->size())
	    {
	      cerr << "no such equation numbered " << num << endl;
	      cout << "no such equation numbered " << num << endl;
	    }
	  else 
	    { 
	      expr *temp = (binopexp *) copyexpr((*eqn)[num]);
	      cout << "about to use " 
		   << (*eqn)[current]->getInfix() << endl; // diag
	      cout << "to substitute into " << temp->getInfix() << endl;// diag
	      subexpin(temp,(*eqn)[current]);
	      cout << "result is " << temp->getInfix() << endl; // diag

	      if (temp->etype != binop) throw(string("subexpin killed binop"));
	      eqn->push_back((binopexp *)temp);
	      current = eqn->size() - 1 ;
	      cerr << current << ": "
		   << (*eqn)[current]->getInfix() << endl;
	      cout << current << ": "
		   << (*eqn)[current]->getInfix() << endl;
	    }
	  continue;
	}
      if (command.compare("flatten") == 0)
	{
	  cin >> num; cout << num;
	  if ((num < 0) || num >= eqn->size())
	    {
	      cerr << "no such equation numbered " << num << endl;
	      cout << "no such equation numbered " << num << endl;
	    }
	  else 
	    { 
	      cureq = copyexpr((*eqn)[num]);
	      if (flatten(cureq))
		{
		  expr * eqexpr = cureq;
		  if (cureq->etype != binop) 
		    throw(string("flatten killed binop"));
		  eqn->push_back((binopexp *)cureq);
		  current = eqn->size() - 1 ;
		  cerr << current << ": "
		       << (*eqn)[current]->getInfix() << endl;
		  cout << current << ": "
		       << (*eqn)[current]->getInfix() << endl;
		}
	      else 
		{
		  cerr << "flatten made no change" << endl;
		  cout << "flatten made no change" << endl;
		}
	    }
	  continue;
	}
      if (command.compare("eqnumsimp") == 0)
	{
	  cin >> num; cout << num;
	  cerr << "about to eqnumsimp " << num << endl;	// diag
	  if ((num < 0) || num >= eqn->size())
	    {
	      cerr << "no such equation numbered " << num << endl;
	      cout << "no such equation numbered " << num << endl;
	    }
	  else 
	    { 
	      cureq = copyexpr((*eqn)[num]);
	      eqnumsimp(cureq, false);
	      if (cureq->etype != binop) 
		throw(string("eqnumsimp killed binop"));
		  eqn->push_back((binopexp *)cureq);
	      current = eqn->size() - 1 ;
	      cerr << current << ": "
		   << (*eqn)[current]->getInfix() << endl;
	      cout << current << ": "
		   << (*eqn)[current]->getInfix() << endl;
	    }
	  continue;
	}
      if (command.compare("factorout") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= vars->size())
	    {
	      cerr << "no such variable numbered " << num << endl;
	      cout << "no such variable numbered " << num << endl;
	    }
	  else 
	    {
	      expr * temp = copyexpr((*eqn)[current]);
	      string reply("Numfactorsof " + (*canonvars)[(*vars)[num]]->clipsname);
	      reply = reply + " in " + temp->getInfix() + " is ";
	      physvarptr * varnum = new physvarptr((*vars)[num]);
	      k = numfactorsof(varnum,temp);
	      cout << reply << k << endl;
	      cerr << reply << k << endl;
	      factorout(varnum,k,temp);
	      cerr << "result is " << endl;
	      cerr << temp->getInfix() << endl;
	      cout << "result is " << endl;
	      cout << temp->getInfix() << endl;
	      cerr << "Would you like enter this in equation list? [y/n]"
		   << endl;
	      cin >> ansch;
	      if (ansch == 'n') temp->destroy();
	      else 
		{
		  if (temp->etype != binop) 
		    throw(string("factorout killed binop"));
		  eqn->push_back((binopexp *)temp);
		}
	      delete varnum;
	    }
	  continue;
	}
      if (command.compare("nlsolv") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= eqn->size())
	    {
	      cerr << "no such equation numbered " << num << endl;
	      cout << "no such equation numbered " << num << endl;
	    }
	  else 
	    {
	      binopexp * temp = (binopexp *)copyexpr((*eqn)[current]);
	      if (nlsolvov(temp))
		{
		  cout << "nlsolvov declared success" << endl;
		  cout << temp->getInfix() << endl;
		  cerr << "nlsolvov declared success" << endl;
		  cerr << temp->getInfix() << endl;

		  cerr << "Would you like enter this in equation list? [y/n]"
		       << endl;
		  cin >> ansch;
		  if (ansch == 'n') temp->destroy();
		  else eqn->push_back(temp);
		}
	      else 
		{
		  cout << "nlsolvov failed to solve" << endl
		       << (*eqn)[num]->getInfix() << endl;
		  cerr << "nlsolvov failed to solve" << endl
		       << (*eqn)[num]->getInfix() << endl;
		  temp->destroy();
		}
	    }
	  continue;
	}
      if (command.compare("powonev") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= vars->size())

	    cerr << "no such variables numbered " << num << endl;
	  else 
	    cerr << powonev((*eqn)[current],(*vars)[num]) << endl;
	  continue;
	}
      if (command.compare("slvlinonev") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= vars->size())

	    cerr << "no such variables numbered " << num << endl;
	  else 
	    {
	      binopexp * temp = (binopexp *)copyexpr((*eqn)[current]);
	      bool answer = slvlinonev(temp,(*vars)[num]);
	      cerr << "Slvlinonev returns " << ((answer) ? "true" : "false");
	      if (answer) {
		cerr << "Returns solution " << endl
		     << temp->getInfix() << endl;
		cerr << "Would you like enter this in equation list? [y/n]"
		     << endl;
		cin >> ansch;
		if (ansch == 'n') temp->destroy();
		else eqn->push_back(temp);
	      }
	      else cerr << endl;
	    }
	  continue;
	}
      if (command.compare("ordinvars") == 0) 
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= eqn->size())
	    {
	      cerr << "no such equation numbered " << num << endl;
	      cout << "no such equation numbered " << num << endl;
	    }
	  else
	    {
	      vector<int> *ansvec = NULL;
	      if (ordinvars((*eqn)[num],vars,ansvec))
		{
		  cerr << "true, : ";
		  cout << "true, : ";
		  for (k = 0; k < vars->size(); k++)
		    {
		      cerr << (*ansvec)[k] << ", ";
		      cout << (*ansvec)[k] << ", ";
		    }
		  cout << endl;
		  cerr << endl;
		}
	      else
		{
		  cerr << "false" << endl;
		  cout << "false" << endl;
		}
	      current = num;
	    }
	  continue;
	}
      if (command.compare("doesnthave") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= vars->size())

	    cerr << "no such variables numbered " << num << endl;
	  else 
	    cerr << doesnthave((*eqn)[current],(*vars)[num]) << endl;
	  continue;
	}
      if (command.compare("linvarcoefs") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= vars->size())

	    cerr << "no such variables numbered " << num << endl;
	  else 
	    {
	      expr * coef = NULL;
	      expr * numer = NULL;
	      if (!linvarcoefs((*eqn)[current],(*vars)[num],coef,numer))
		{ cerr << "linvarcoefs returned false" << endl; continue; }
	      cerr << "Coef is " << coef->getInfix() << endl;
	      cerr << "constant term is " << numer->getInfix() << endl;
	      cerr << "would you like this substituted in an equation n/y/a]?";
	      cin >> ansch;
	      if (ansch == 'n') { coef->destroy(); numer->destroy();continue;}
	      kmult(numer,-1.);
	      binopexp * neweq = new binopexp(&equals,
					      new physvarptr((*vars)[num]),
					      new binopexp(&divby,numer,coef));
	      if (ansch == 'a')
		{
		  q = eqn->size(); //keep from doing on equations just added
		  for (k=0;k < q; k++)
		    {
		      if (!doesnthave((*eqn)[k],(*vars)[num]))
			{
			  expr * temp =  copyexpr((*eqn)[k]);
			  if (subexpin(temp,neweq))
			    {
			      if (temp->etype != binop) 
				throw(string("subexpin killed binop"));
			      eqn->push_back((binopexp *)temp);
			      cerr << eqn->size()-1 << ": "
				   << temp->getInfix() << endl;
			    }
			}
		    }
		  continue;
		} // end of answer = a
	      if (ansch == 'y')
		{
		  cerr << "which one? " << endl;
		  cin >> k;
		  if ((k < 0) || k >= eqn->size())
		    {
		      cerr << "no such equation numbered " << k << endl;
		      cout << "no such equation numbered " << k << endl;
		    }
		  else
		    {
		      if (!doesnthave((*eqn)[k],(*vars)[num]))
			{
			  expr * temp = (binopexp *)copyexpr((*eqn)[k]);
			  if (subexpin(temp,neweq))
			    {
			      if (temp->etype != binop) 
				throw(string("subexpin killed binop"));
			      eqn->push_back((binopexp *)temp);
			      cerr << eqn->size()-1 << ": "
				   << temp->getInfix() << endl;
			    }
			}
		    }
		} // end of answer = y
	      neweq->destroy();
	    } // end of else from variable check
	  continue;
	}
      if (command.compare("slvpolyexpr") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= vars->size())

	    cerr << "no such variables numbered " << num << endl;
	  else 
	    {
	      vector<double> *roots
		= slvpolyexpr((*eqn)[current],(*vars)[num]);
	      cerr << "roots are: ";
	      for (k=0; k < roots->size(); k++) cerr << (*roots)[k] << ", ";
	      cerr << endl;
	      delete roots;
	    } // end of var check
	  continue;
	} // end of slvpolyexpr
      if (command.compare("polyexpand") == 0)
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= vars->size())

	    cerr << "no such variables numbered " << num << endl;
	  else 
	    {
	      vector<double> *coefs = NULL;
	      if (!polyexpand(((binopexp *)(*eqn)[current])->lhs,
			      (*vars)[num],coefs))
		{ cerr << "polyexpand returned false" << endl;
		delete coefs;
		continue;
		}
	      for (k=0; k < coefs->size(); k++) cerr << (*coefs)[k] << ", ";
	      cerr << endl;
	      cerr << "roots are: ";
	      vector<double> *roots = findallroots(coefs);
	      for (k=0; k < roots->size(); k++) cerr << (*roots)[k] << ", ";
	      cerr << endl;
	      delete coefs;
	      delete roots;
	    } // end of var check
	  continue;
	}
      if (command.compare("rationalize") == 0) 
	{
	  cin >> num; cout << num << endl;
	  if ((num < 0) || num >= eqn->size())
	    {
	      cerr << "no such equation numbered " << num << endl;
	      cout << "no such equation numbered " << num << endl;
	    }
	  else
	    {
	      current = num;
	      if (!rationalize((*eqn)[num]))
		cerr << "rationalize returned false" << endl;
	      cerr << num << ": " << (*eqn)[num]->getInfix() << endl;
	    }
	  continue;
	}

      cerr << "I didn't understand command "<< command << " try again" << endl;
      cout << "I didn't understand command "<< command << " try again" << endl;
    }
}

void listcoms()
{
  cerr << "help, commands, exit, infix eqn, dbg eqnn, solvefor var," << endl
       << "substin target, flatten, eqnumsimp, factorout var" 
       << "nlsolv eqn, powonev var, slvlinonev var, ordinvars eqn"<< endl
       << "doesnthave var, linvarcoefs var, slvpolyexpr var," << endl
       << "polyexpand var, rationalize eqn" << endl;
  cout << "help, commands, exit, infix eqn, dbg eqnn, solvefor var," << endl
       << "substin target, flatten, eqnumsimp, factorout var" 
       << "nlsolv eqn, powonev var, slvlinonev var, ordinvars eqn"<< endl
       << "doesnthave var, linvarcoefs var, slvpolyexpr var," << endl
       << "polyexpand var, rationalize eqn" << endl;
}

void printcoms()
{
  cerr << endl << "Here is the list of commands" << endl
       << "help   - print this list" << endl
       << "commands   - print list of commands" << endl
       << "exit  - exit from program" << endl
       << "infix n  - print to cerr infix form the equation number n" << endl
       << "   n becomes current equation" << endl
       << "dbg n - print to stdout dbg form the equation number n" << endl
       << "   n becomes current equation" << endl
       << "solvefor m - solve current equation for variable number m" << endl
       << "   solution added to equations, and becomes current equation" 
       << endl
       << "substin n  - take current equation (must be in solution form"
       << endl
       << "   and substitute in equation n" << endl
       << "flatten  - flatten the current equation" << endl
       << "eqnumsimp  - flatten the current equation" << endl
       << "factorout n  - count factors of physvar n in current equation "
       << endl
       << "nlsolv n  - try nonlinear solve of equation n in one var " << endl
       << "powonev n  - count order of physvar n in current equation "
       << endl
       << "slvlinonev n  - solve current eqn which is linear in var n"
       << endl
       << "ordinvars n  - give orders of vars in eqn n" << endl
       << "doesnthave n - check that current equation independent of var" 
       << endl
       << "linvarcoefs var - current eq linear in var, return coef and const"
       << endl
       << "slvpolyexpr var - current eq poly in var and only var, solve"
       << endl
       << "polyexpand var - current eq poly in var, expand coefs "
       << "and give options" << endl
       << "rationalize eqn - remove denominators" << endl;
}


