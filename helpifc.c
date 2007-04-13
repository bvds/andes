/******************************************************************************
 *
 *  Helpifc.c --  DLL interface to the Andes help system in Lisp
 *
 * Uses Allegro Common Lisp functions from the LNKACL 
 * dll and import library provided by Allegro. Requires that a runtime image
 * of type :dll has been built using Allegro.
 ******************************************************************************/
#include <windows.h>
#include <string.h>

#define Dllexport _declspec(dllexport)

static int lisp_initialized = 0;    // flag if we have initialized		

// Following is type of the main help system interface function
// we will call.
static int (*lisp_helpsys_fn)(char *, char*, int) = 0;

static int (*cmd_handler_fn) (char*) = 0;

// Callbacks exported to Lisp:
 
/*
 * The custom initialization function on the Lisp side will call back
 * to this function in order to pass us back a function pointer 
 * we can use as an entry point for subsequent calls.
 * */
Dllexport void c_set_lisp_entrypoint( int (*pfi) (char *, char *, int) )
{
  lisp_helpsys_fn = pfi;
}

/* Export to Lisp:
 * Lisp side will use this function to fill result string buffer */
int Dllexport
c_copy_result( char *str1, char *str2, int n )
{
  strncpy( str1, str2, n - 1 );
  *(str2 + n) = '\0';
  return 1;
}

/* Export to Lisp:
 * Lisp side will use this function to send commands to client */
int Dllexport
c_execute_cmd( char *str)
{
  if (cmd_handler_fn)
     return (*cmd_handler_fn)(str);
  else return 0;
}

// Export to clients of the Lisp help system ifc library:
// Initialize everything
/* returns 1 if successful or already done
   returns 0 if initialization failed
   */
Dllexport int helpsys_initialize( int bConsole, char* szImageFile, int (*pfi) (char *))
{
  if (lisp_initialized) 
	return 1;
 
  /* save client's command string handler */
  cmd_handler_fn = pfi;

  /* start Lisp and load our image */
  if (InitializeLisp(szImageFile, 0, bConsole ) == 0) 
	return 0;

  /* don't return until lisp can process commands */
  if (RemoteCommand( "(helpsys-initialize)", 1 ) != 1)
  	return 0;
  
  return lisp_initialized = 1;
}

// Export to clients of the help sys ifc library
/* Users of the helpsys ifc library call this */
/* returns 1 if successful; 0 otherwise */
Dllexport int helpsys_execute ( char * cmdstr, char *resultbuf, int buf_size )
{
  if (!lisp_initialized || !lisp_helpsys_fn) {
    return -1;
  }
  switch ( (*lisp_helpsys_fn) ( cmdstr, resultbuf, buf_size ))
  {
    case 1:
      return 1;
    case 0:
    default:
      *resultbuf = '\0';
      return 0;
  }
}


