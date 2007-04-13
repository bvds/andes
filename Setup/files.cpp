///////////////////////////////////////////////////////////////////////////////
//
// Files.cpp -- defines list of files, directories to install for ANDES 
//              setup/uninstall programs.
//
///////////////////////////////////////////////////////////////////////////////
#include <stdafx.h>

//
// Following globals will store the master lists of all files, dirs to install:
//
CStringArray directories;
CStringArray files;

//
// Internal tables hold compiled-in static data used to build exported lists at 
// run-time. We add the per-problem files via code mainly so adding a new problem
// only requires one new entry to the problem table. Also to enable us to move to 
// using an external list of problems so don't have to recompile setup program 
// to add new ones. 
//
// Files deleted from earlier version install lists are included in the UNINSTALL 
// version of the list, so the uninstaller can still remove them if operating on an 
// upgraded installation. This is safe since the uninstaller silently ignores any 
// files on the list that don't exist in the installation.
//
#define TBL_SIZE(array) (sizeof(array)/sizeof(array[0]))

static const char * s_directories[] =
{
	// Created in list order, so parents must go before subdirs. 
	// Cleanup/Uninstall should remove in reverse order.
	// "students",
	"Problems",
	"Problems\\Solutions",
	// "Examples",
	// "pData",
	// Log directory
	"log",
	// runtime knowledge-base directory
	"kb",
	// Mini-lesson directory
	"Review",
	"Review\\Graphics",
	"Review\\Videos",		// demo videos
	// "Configuration-Scripts",
#ifdef EXPERIMENT
	"Textbook",
	"Textbook\\section2-8_files",
#endif
};
#define N_STATIC_DIRS TBL_SIZE(s_directories)

static const char * s_files[] = 
{
#ifdef PATCH
//
// For a patch-only install, list files to be installed here
// we use wildcards since some or all of these may be missing from patch
//
	// new executables
	"fbd-*.exe",
	"Andes*.dxl",
	"solve*.dll",
	// any new problems/problem sets
	"Problems\\*.fbd",
	"Problems\\*.prb",
	"Problems\\*.aps",
	"Problems\\*.gif",
	// New review pages:
	"Review\\*.html",
	"Review\\*.htm",
	"Review\\*.jpg",
	"Review\\*.gif",
	// Minilesson graphics subdirectory
	"Review\\Graphics\\*.gif",
	"Review\\Graphics\\*.jpg",
	"Review\\Graphics\\*.htm",
	// Updated kb files
	"kb\\*.tsv",


#else // !PATCH  -- whole rest of files list is within this case

	// Program files
	"fbd-tcp.exe",		// workbench executable for Andes2
	"fbd-tcp.hlp",		// workbench help file for Andes2
	"fbd.cnt",			// workbench help contents file, name compiled into .hlp file

	//"andes2.dxl",		// helpsys Lisp image
	//"andes2.exe",		// renamed lisp driver program for helpsys
	//"acl5016.dll",		// Allegro Lisp dll -- for now just leave in exe dir

	// New DLL-based helpsys using ACL 8.0.1
	"HelpSys.dxl",
	"HelpSys.lic",
	"helpifc.dll",
	"acli8010.dll",
	"lnkacl.dll",

	"solver.dll",		// Solver dll
	
#ifdef USNA_EVAL
	"upload.exe",		// upload utility
#endif 
	"Uninst.exe",		// uninstaller
	"config.cl",		// helpsys config option settings.
	"AndesLicense.txt",      // our license agreement
	// No longer include GS tutorial (requires GS problem files!) in all distributions
	// "GettingStarted.doc", // tutorial documentation
#ifdef EXPERIMENT
	//"SaveData.bat",		// batch file utility to save log contents to floppy disk
#endif 

#ifdef UNINSTALL		// include old files in list if removing

	// Andes2 exe-based files:
	"andes2.dxl",		// helpsys Lisp image
	"andes2.exe",		// renamed lisp driver program for helpsys
	"acl5016.dll",		// Allegro Lisp dll -- for now just leave in exe dir

	// Andes1 helpsys files
	"andes.exe",
	"andes.dxl",
	"fbd.exe",
	"assessor.dll",
	// Really ancient: acl3.0 helpsys files from Andes99
	lisp.exe",
	"andesrt.img", 
	"upload.exe",
#endif UNINSTALL		// old files		
	
	// Problem set files:

#ifdef EXPERIMENT		// only include sets used in experiment
	// "Problems\\Getting Started.aps",
	// "Problems\\Andes Experiment Problems.aps",

	// For experiment install, just copy all relevant files in Problems:
	"Problems\\*.aps",
	"Problems\\*.prb",
	"Problems\\*.gif",

	// Copy all of textbook directory
	"Textbook\\*.*",
	"Textbook\\section2-8_files\\*.*",

#else // ! EXPERIMENT, i.e. normal ANDES

#ifndef OLI	// no problems in OLI installation
/*
	"Problems\\Angular Momentum.aps",
	"Problems\\Circular Motion.aps",
	"Problems\\Energy-Work.aps",
	"Problems\\Free Body Diagrams.aps",
	// "Problems\\Getting Started.aps",
	"Problems\\Graphical Kinematics.aps",
	"Problems\\Linear Momentum.aps",
	"Problems\\Power.aps",
	"Problems\\Rotational Dynamics.aps",
	"Problems\\Rotational Kinematics.aps",
	"Problems\\Statics.aps",
	"Problems\\Translational Dynamics.aps",
	"Problems\\Translational Kinematics.aps",
	"Problems\\Vectors.aps",
	"Problems\\Electric Field.aps",
	"Problems\\Electric Potential.aps",
	"Problems\\DC Circuits.aps",
	"Problems\\Capacitance.aps",
	"Problems\\Resistance.aps",
	"Problems\\Magnetic Field.aps",
	"Problems\\Electromagnetic Induction.aps",
	"Problems\\Inductance.aps",
	"Problems\\Optics.aps",
	"Problems\\Fluids.aps",
	"Problems\\Oscillations.aps",
	"Problems\\Waves.aps",
	"Problems\\Work_Energy.aps",
*/
	"Problems\\*.aps",

	// Now just copy all of these
	"Problems\\*.fbd",
	"Problems\\*.prb",
	"Problems\\*.aps",
	"Problems\\*.gif",
	"Problems\\*.jpg",
	"Problems\\*.html", // for index.html

	// Video pseudo-problem for preview image
	"Problems\\video.fbd",
#endif

	// knowledge base files
	// "kb\\*.cl",			// uncompiled lisp files
	// "kb\\*.fasl",		// compiled lisp files
	"kb\\*.tsv",		// Tables used by workbench
  

#endif // ! EXPERIMENT, i.e. normal ANDES

#ifdef UNINSTALL	// old stuff to remove if still there
	// no longer used in Andes2:
	"Problems\\Qualitative Motion.aps",
	// obsolete problem set:
	"Problems\\kinematics+net-force.aps",
	// old hyphenated names.
	"Problems\\forces-static.aps",
	"Problems\\rotational-dynamics.aps",
	"Problems\\translational-dynamics.aps",
	"Problems\\translational-kinematics.aps",
	"Problems\\qualitative-motion.APS",
	"Problems\\rotational-kinematics.aps",
#endif UNINSTALL
	
//********************************
// Minilesson templates and rule dialogs:
// just install everything in these directories.
// NB: !! Could be dangerous to use wildcards here!! Uninstaller uses same code to get 
// list of files to remove, so will uninstall every matching file existing in these 
// directories at uninstall time, even if we didn't install them! Better would be for
// installer to write an install log that uninstall could pick up, but that is more work. 
	"Review\\*.html",
	"Review\\*.htm",
	"Review\\*.jpg",
	"Review\\*.gif",
	"Review\\*.png",
	// Minilesson graphics subdirectory
	"Review\\Graphics\\*.gif",
	"Review\\Graphics\\*.jpg",
	"Review\\Graphics\\*.htm",
	"Review\\Videos\\*.wmv",
	"Review\\Videos\\*.html", 

#endif // ! PATCH -- end of whole big section
};
#define N_STATIC_FILES TBL_SIZE(s_files)



//-------------------
// Problems to install
//-------------------
static const char *s_problems[] =
{   
#if PATCH||OLI
// list fbd-based problems in the patch here
// Need NULL marker if list is empty, urgh
	"NULL",
#else // ! PATCH -- whole rest of list is in this condition

#ifdef EXPERIMENT	// small set of problems for use in experiment	
	
	// these have .fbd file
	"4times",
	"castle",
	"missle",
	"motorboat",
	"swimmer",

#else // ! EXPERIMENT, i.e. standard full ANDES installation

	"NULL",

/* just install everything in zip for ease of changes
  // "gs00",
  // "gs01",
  "dr1a",
  "dr2a",
  "dr2b",
  "dr3a",
  "dr4a",
  "dr5a",
  "dr6a",
  "dr6b",
  "dr7a",
  "dr8a", 
  "dt1a",
  "dt1b",
  "dt1c",
  "dt2a",
  "dt3a",
  "dt3b",
  "dt3c",
  "dt4a",
  "dt4b",
  "dt5a",
  "dt6a",
  "dt7a",
  "dt7b",
  "dt8a",
  "dt9a",
  "dt10a",
  "dt11a",
  "dt11b",
  "dt12a",
  "dt13a",
  "dt13b",
  "dt14a",
  "dt14b",
 // "dt15a",  not working, don't install
  "e1a",
  "e1b",
  "e1c",
  "e2a",
  "e2b",
  "e2c",
  "e3a",
  "e4a",
  "e4b",
  "e4c",
  "e5a",
  "e5b",
  "e6a",
  "e7a",
  "e7b",
  "e8a",
  "e8b",
  "e9a",
  "e9b",
  "e10a",
  "e11a",
  "e12a",
  "fbd1a",
  "fbd1b",
  "fbd2a",
  "fbd3a",
  "fbd4a",
  "fbd5a",
  "fbd6a",
  "kr1a",
  "kr1b",
  "kr2a",
  "kr2b",
  "kr3a",
  "kr3b",
  "kr4a",
  "kr5a",
  "kr6a",
  "kr7a",
  "kt1a",
  "kt1b",
  "kt2a",
  "kt3a",
  "kt3b",
  "kt4a",
  "kt5a",
  "kt6a",
  "kt6b",
  "kt7a",
  "kt8a",
  "kt8b",
  "kt9a",
  "kt9b",
  "kt10a",
  "kt10b",
  "kt10c",
  "kt10d",
  "kt11a",
  "kt11b",
  "kt12a",
  "kt12b",
  "kt12c",
  "kt13a",
  "kt13b",
  "kt13c",
  "lmom1a",
  "lmom1b",
  "lmom2a",
  "lmom2b",
  "lmom3a",
  "lmom4a",
  "lmom5a",
  "momr1a",
  "momr1b",
  "momr2a",
  "momr2b",
  "momr3a",
  "momr4a",
  "pow1a",
  "pow1b",
  "pow2a",
  "pow3a",
  "pow4a",
  "pow4b",
  "pow5a",
  "relvel1a",
  "relvel2a",
  "relvel3a",
  "rots1a",
  "rots1b",
  "rots1c",
 // "rots1d",  // not working
  "rots2a",
  "rots3a",
  "rots4a",
  "rots4b",
  "rots4c",
  "rots5a",
  "rots6a",
  "rots6b",
  "rots6c",
  "rots7a",
  "rots8a",
  "rots8b",
  "s1a",
  "s1b",
  "s1c",
  "s1d",
  "s1e",
  "s1f",
  "s2a",
  "s2b",
  "s2c",
  "s2d",
  "s2e",
  "s3a",
  "s3b",
  "s3c",
  "s4a",
  "s4b",
  "s5a",
  "s6a",
  "s7a",
  "s8a",
  "s9a",
  "s10a",
  "s11a",
  "s11b",
  "vec1a",
  "vec1b",
  "vec1c",
  "vec1d",
  "vec2a",
  "vec2b",
  "vec2c",
  "vec2d",
  "vec3a",
  "vec3b",
  "vec3c",
  "vec4a",
  "vec4b",
  "vec4c",
  "vec4d",
  "vec5a",
  "vec5b",
  "vec5c",
  "vec5d",
  "vec6a",
  "vec6b",
  "vec6c",
  "vec6d",
  "vec7a",
  "vec8a",
  "vec8b",
  "vec8c",
  "vec9a",
  "vec10a",
  "vec11a",


  "cap1a",
  "cap1b",
  "cap2a",
  "cap2b",
  "cap3a",
  "cap4a",
  "cap5a",
  "cir1",
  "cir2",
  "cir3",
  "cir6",
  "cir7",
  "cir10",
  "cir17",
  "cir18",
  "cir30",
  "cir31",
  "cir32b",
  "cir40",
  "eqcap1a",
  "eqcap1b",
  "eqcap1c",
  "eqcap1d",
  "eqcap2a",
  "eqcap2b",
  "eqcap3a",
  "eqcap3b",
  "eqcap4a",
  "eqcap4b",
  "eqcap5a",
  "eqcap6a",
  "eqres1a",
  "eqres1b",
  "eqres1c",
  "eqres2a",
  "eqres2b",
  "eqres3a",
  "eqres3b",
  "eqres4a",
  "eqres4b",
  "eqres5a",
  "eqres6a",
  "kir1a",
  "kir1b",
  "kir2a",
  //"kir2b",
  "kir2c",
  "kir2d",
  "kir3a",
  "kir3b",
  "kir3c",
  "kir4a",
  "kir5a",
  //"kir6a",
  "kir7a",
  "rc1a",
  "rc1b",
  "rc1c",
  "rc2a",
  "rc3a",
  "rc3b",

  "mag1a",   // this one has fbd file
  // Faraday's Laws
  "fara*",
  // Inductance, LR, LC, LRC circuits
  "ind*",
  "LR*",
  "LC*",
  "LRC*",
  // Electric power
  "epow*",
*/ 
#endif // ! EXPERIMENT, i.e. standard Andes

#ifdef UNINSTALL
	// old trial circuits problems
	"Excir3a",
	"Excir4a",
	"Excir5a",
	"Excir8a",
	"Excir9a",
	"Excir10a",
	"Excir16a",
	"Excir17a",

	// old ATMS problems:
	"P2-1",
	"P10-1",
	"P10-2",
	"P10-3",
	"P11-1",
	"P11-2",
	"P11-3",
	"P11-4",
//	"P15-1",
//	"P15-2",
//	"P15-3",
	"P5-1",
	"P5-2",
	"P5-3",
	"P5-4",
	"P6-1",
	"P6-2",
	"P6-3",
	"P7-1",
	"P7-2",
	"P7-3",
	"P8-1",
	"P9-1",
	"P9-2",
	"P9-3",
	
	// old Qualitative
	"Qex4-1",
	"Qex4-2",
	"Qex5-1",
	"Qex5-2",
	"Qex5-3",
	"Qex6-1",
	"Qex6-3",
	"Qex6-4",
	"Qex7-1",
	"qex7-2",
	"Qex8-1",
	"qex8-2",
	"Qex8-3",
	"qex8-4",
	"qex8-5",
	"qex9-1",
	"qex9-2",
	"qex9-3",
	"qex9-4",
	"qex9-5",
	"qex9-6",
#endif UNINSTALL
#endif // ! PATCH -- normal lists
};


#define N_PROBLEMS TBL_SIZE(s_problems)

// Installed in Andes1: examples for study
static const char* s_examples[] = {
	"Se1", "Se2", "Se3", 
};
#define N_EXAMPLES TBL_SIZE(s_examples)

// 
// For building up arrays by adding:
// We allocate big arrays and maintain our own fill pointer.
//

// we know size of directory list in advance, so index should always be within bounds
static int nDirs = 0;					// counts dirs as added
void ADD_DIR(LPCSTR d)  
{
	directories[nDirs++] = d;
}

// Because file specs may contain wildcards, we need to use function that will grow
// the array as needed. We get exact size at end.
static int nFiles = 0;					// counts files as added
void ADD_FILE(CString strFileName)	// add single item to file list, updating count of real elements.
{
	files.SetAtGrow(nFiles++, strFileName);
	TRACE("Added file %d: %s\n", nFiles-1, files[nFiles-1]);
}

// add file to file list expanding wildcards in spec
// if optional, treats single filename as wildcard so won't add if doesn't exist.
void AddFile(CString strSpec, BOOL bOptional=FALSE)  
{ 
	// if no wildcard, just add single name to list. We could do with wildcard code but
	// by always adding specified files to the install list we will be sure to get an 
	// error if they don't exist in the install image directory. For wildcard files,
	// there will be no error, so have to be careful on these. Usually means including 
	// a whole directory so less likely to miss something here when assembling image.
	if (strSpec.Find('*') == -1 && !bOptional) {
		ADD_FILE(strSpec);
	} 
	else // loop to add all regular files matching wildcard
	{
		// Need to save relative path from spec to prepend to added file name
		CString strSpecPath;
		int iLastSep = strSpec.ReverseFind('\\');		// NB: must use backslash
		if (iLastSep != - 1)
			strSpecPath = strSpec.Left(iLastSep + 1);	// include trailing separator

		CFileFind finder;
	    BOOL bFilesRemaining = finder.FindFile(strSpec);
		while (bFilesRemaining)
		{
			bFilesRemaining = finder.FindNextFile();
     
			// skip directories which can be matched with *.*
			CString strFileName = finder.GetFileName();
			if (! (finder.IsDirectory() || finder.IsDots() || strFileName.IsEmpty())){
				ADD_FILE(strSpecPath + strFileName);
			}
		}
	}
}

//
// BuildFileList -- build master lists of directories to create and files to install
//
// Expands wildcards in file lists and 
// Expands problem list adding appropriate files to  file and directory lists.
// [Andes1 created one directory per problem so did this with code from problem list.
// This is no longer needed in Andes2 so we could do without the directory step.]
//
void BuildFileList()
{
	CString strProblemPath = "Problems\\";
	int i;

	//--- Build up directory list ---//

	// pre-allocate exact array size we need to avoid multiple reallocs.
	directories.SetSize(N_STATIC_DIRS);

	// init with dirs from static list
	for (i = 0; i < N_STATIC_DIRS; i++) 
		ADD_DIR(s_directories[i]);

	// CStringArray size should match actual added file count.
	ASSERT(nDirs == directories.GetSize());	

	// --- Build up file list ---//

	// pre-allocate big array to avoid multiple reallocs. extra space for wildcards
	files.SetSize(N_STATIC_FILES + N_PROBLEMS*2 + 200, /*grow by:*/500);
	
	// init with files from static list
	for (i = 0; i < N_STATIC_FILES; i++) 
		AddFile(s_files[i]);

	// add files needed for each problem on problem list
	for (i = 0; i < N_PROBLEMS; i++) 
	{
		CString strProblem = s_problems[i];
		if (strProblem == "NULL")  // marker for empty list
			break;

		// the fbd problem file itself:
		AddFile(strProblemPath + strProblem + ".fbd");
		// the helpsys files for this problem. OK if missing, run without help
		AddFile(strProblemPath + strProblem + ".prb", /*bOptional=*/ TRUE);
	}

	// reduce array size to reflect actual added file count
	files.SetSize(nFiles);
	TRACE("BuildFileList done: nFiles = %d\n", nFiles);
}
