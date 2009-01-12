#!/bin/bash
############################################################################
# MakeDist.sh -- shell script to build an Andes installer image
#
#    Usage: MakeDist.sh [-oli] [-m index.html]
#
# -oli flag builds the OLI client installer.
# Relies on list of problem sets to include via tools/copyprbs.pl script
# Default is tools/modules.lst
#
# Assumes it is being run from root of an Andes source directory
#
# Does not rebuild executables, just packages up the system as currently 
# built in the source directory
#
# Requires Winzip with command line add-on and Winzip Self-Extractor
# Assumes these are installed in C:\Program Files\
#
# Default name for the installer will be AndesNNNN.exe where NNNN
# are the components of the workbench file version number.

set -o errexit	# set to exit script on any command error 

# shell magic to process switches in command line args
# OLI flag will be zero-length for false, detected with [ -z "$OLI" ]
#     non-zero-length tests true in [ "$OLI" ]
OLI="";		
MODULE_FILE="index.html";
while [ $# -gt 0 ]
do
    case "$1" in
      -oli) OLI=true;;  # any non-zero-length value will do 
	-m) MODULE_FILE=$2; shift;;
	-*)
            echo >&2 "usage: $0 [-oli] [-m index.html]"
	    exit 1;;
	*)  break;;	# terminate while loop
    esac
    shift
done
echo "MakeDist with OLI = '$OLI', MODULE_FILE= $MODULE_FILE"

#------------------------------------------------------------------------
#  PHASE 1: copy all needed distribution files into a temp directory
#------------------------------------------------------------------------

# Make temp directory to hold the distribution files. Delete any 
# previous version to ensure we use only the freshest ingredients
dstdir=TempDir
if [ -e $dstdir ]; then rm -rf $dstdir; fi
mkdir -p $dstdir

# Copy all the files we need for a distribution
echo "MakeDist: Copying distribution files into $dstdir ..."
# !!! No visible feedback until copyprbs.pl starts
#    executable files and support
# rename OLI version workbench to fbd-tcp.exe
if [ $OLI ] ; then
     cp fbd-tcp-oli.exe $dstdir/fbd-tcp.exe 
else 
     cp fbd-dll.exe $dstdir/fbd-tcp.exe 
fi
cp fbd-tcp.hlp $dstdir
cp fbd.cnt $dstdir
cp Helpsys.dxl $dstdir
cp Helpsys.lic $dstdir
cp helpifc.dll $dstdir
cp /cygdrive/c/Program\ Files/acl81/lnkacl.dll $dstdir
cp /cygdrive/c/Program\ Files/acl81/acli817.dll $dstdir
cp solver.dll $dstdir
cp Config.cl $dstdir
#  rename appropriate Setup version to Setup.exe
if [ $OLI ] ; then
     cp SetupOLI.exe $dstdir/Setup.exe
else
     cp SetupStudent.exe $dstdir/Setup.exe
fi
cp Uninst.exe $dstdir
# Other files needed by setup program: Andes License
cp Setup/AndesLicense.txt $dstdir
# redistributable files: ACELP.net codec installers
cp Setup/redist/sl_anet.acm  $dstdir
cp Setup/redist/acelpacm.inf $dstdir
cp Setup/redist/Vista_Install_AcelpNet.exe $dstdir
# MSVC runtime libraries we used. Never needed these days since newer ones
# installed on more recent versions of Windows, but installer expects them.
cp Setup/redist/mfc42.dll $dstdir
cp Setup/redist/msvcrt.dll $dstdir
#    KB files
mkdir -p $dstdir/KB
cp KB/features.tsv $dstdir/KB
cp KB/principles.tsv $dstdir/KB
cp KB/scalars.tsv $dstdir/KB
#    Review files
# Note use of wildcards here could overgenerate if there are junk files in the
# source directories. Including extra files is usually harmless, but they do 
# get installed by setup program, which also does a wildcard copy on these.
mkdir -p $dstdir/Review
cp Review/*.html $dstdir/Review
cp Review/*.htm $dstdir/Review
cp Review/*.gif $dstdir/Review
cp Review/*.GIF $dstdir/Review
cp Review/*.JPG $dstdir/Review
# currently no jpgs in use
# cp Review/*.jpg $dstdir/Review
#    minilesson graphics subdirectory
mkdir -p $dstdir/Review/Graphics
cp Review/Graphics/*.gif $dstdir/Review/Graphics
cp Review/Graphics/*.htm $dstdir/Review/Graphics
#    essential introductory video 
if [ -z "$OLI" ]; then
   mkdir -p $dstdir/Review/Videos
    cp Videos/Intro_to_Andes.wmv $dstdir/Review/Videos
    cp Videos/Intro_to_Andes.html $dstdir/Review/Videos
    # videos for a "full" installation including videos would go here.
    # These videos would also require the following html wrappers:
    # cp Videos/wrappers/*.html  $dstdir/Review/Videos
fi

# Andes problem files
mkdir -p $dstdir/Problems
if [ -z "$OLI" ]; then
   cp Problems/index.html $dstdir/Problems
   cp Problems/video.fbd $dstdir/Problems
   # Copy the required APS's and all needed problem files. 
   perl tools/copyprbs.pl -m $MODULE_FILE $dstdir 
fi

#------------------------------------------------------------------------
#  PHASE 2: zip up distribution tree and make self-extracting installer
#------------------------------------------------------------------------

# Extract Andes version number from FBD executable for inclusion
# in our dialog messages and in naming our installer file
# Following uses a simple vbscript to get this. 
# Version number resources have four fields, but by convention we only 
# include a non-zero fourth digit for internal test or experimental builds.
# The workbench will also not display the fourth digit if it is zero. 
# So we apply sed command to strip fourth field if it is zero
cscript /Nologo tools/FileVersion.vbs $dstdir/fbd-tcp.exe | sed 's/\.0$//' > version.txt
# Following is an alternate method that pulls version out of
# workbench source, changing commas used in source to periods. 
# Finding version this way is less reliable because workbench might
# not be up to date with the source. But it avoids reliance on vbscript
# grep FILEVERSION FBD\fbd.rc | cut -f3 -d' ' | tr ',' '.' > version.txt


VERSION=`cat version.txt | tr -d [:cntrl:]` # remove trailing newline 
E_NOVERSION=65
if [ -n "$VERSION" ]	# length of string is non-zero
then
	echo "Makedist: showing version number $VERSION"
else
	echo "Makedist: Failed to get version number"
	exit $E_NOVERSION
fi

# figure out which filename to use for the installer
if [ $OLI ]; then
    INSTALLERNAME="andes_installer"
    FOR_OLI=" for OLI"  # optional suffix, include initial space
else
    VERSION_NODOTS=`cat version.txt | tr -Cd [:alnum:]`
    INSTALLERNAME=Andes$VERSION_NODOTS
    FOR_OLI=""
fi

# create the zip
echo "MakeDist: Zipping $dstdir into $INSTALLERNAME.zip"
rm -f $INSTALLERNAME.zip
C:/Program\ Files/WinZip/wzzip -rp $INSTALLERNAME.zip $dstdir
if [ ! $? ]; then echo "MakeDist: Zip operation failed"; exit 1; fi

# Make installer message files containing version number
echo "Installing ANDES Physics Tutor version $VERSION $FOR_OLI
Please wait ..." > DistDlg.txt

# create the self-extractor
echo "MakeDist: Creating self-extractor"
C:/Program\ Files/WinZip\ Self-Extractor/WZIPSE32 $INSTALLERNAME -setup -auto -t DistDlg.txt -c ./Setup.exe
if [ ! $? ]; then echo "MakeDist: self-extractor creation failed"; exit 1; fi

# OK, done. clean up temporary files
#rm DistMsg.txt DistDlg.txt version.txt
echo "MakeDist: Done"
exit 0
