#!/usr/bin/perl 
#
# copyprbs -- copy all Andes problem files for a given list of problem sets
#
# Usage:  copyprbs.pl module.lst dstdir [verbose]
#
# module.lst file should contain a list of Andes problem set names 
# without extension, one to a line
#
# dstdir is the root of the destination Andes directory. 
#
# verbose can be set to any non-zero string (e.g. -verbose or 1)
# to enable tracing of all file copying operations.
#
# Assumes it will run in the root of an Andes source directory.
# APS, prb, and graphic files will be copied into Problems subdirectory of 
# dstdir, which must already exist
#

#
# docopy (srcfile, dstroot) -- copy named file into directory
# srcfile should be relative to Andes root
#
$verbose = 0;		# set this flag for verbose trace of copying
use File::Copy;
sub docopy()
{
    my($srcfile, $dstroot) = @_;
    my $dstfile = "$dstroot/$srcfile";
    copy ($srcfile, $dstfile) or die "Copy $srcfile $dstfile failed:$!";
    if ($verbose) { print STDERR "copied $srcfile to $dstroot\n"; }
}

# get command line arguments 
($module_lst, $dstdir, $verbose) = @ARGV;
if (!$module_lst || !$dstdir) { die "usage: copyprbs module.lst dstdir:" };

# ensure we have destination directory
if (! -d $dstdir) { die "$dstdir is not a directory."; }
$problemdir = $dstdir . "/Problems";
if (! -e $problemdir) { mkdir $problemdir or die "Couldn't create $problemdir. $!"; }
if (! -d $problemdir) { die "$problemdir is not a directory."; }

# do for each module listed
open (MODULES, "$module_lst") or die "Couldn't open $module_lst. $!";
while ($module = (<MODULES>))
{
    $module =~ s/[\r]\n//;  #  cygwin perl on Windows may include \r
   
    # open next aps file to process
    print STDERR "copyprbs: Copying files for $module\n";
    $apsfile = "Problems/$module" . ".aps";
    open (APS, "$apsfile") or die "Couldn't open $apsfile. $!";

    # copy the APS file itself
    &docopy($apsfile, $dstdir);
   
    # do for each line in APS file
    while ($line = <APS>)
    {
	  # skip non-problem lines
	  next if $line =~ /ANDES Problem Set/;
	  next if $line =~ /\.wmv/;    # demo video line

	  if ($line =~ /\S+/) { # non-space sequence = problem id
	      $problemid = $&;
	      # copy prb file
	      $prbfile = "Problems/$problemid" . ".prb";
	      if (! -e $prbfile) {
		  # assume this is stub problem, so keep going
		  print STDERR "copyprbs: no prb for $problemid -- ignored\n"; 
		  next;
	      }
	      &docopy($prbfile, $dstdir);
	 
	      # scan inside prb file for graphic and copy it also
              open (PRB, "$prbfile") or die "Couldn't open $prbfile. $!";
	      while (<PRB>) {
	         if (/^Graphic +"([^"]+)"/) {
		     $graphicfile = "Problems/$1";
		     &docopy($graphicfile, $dstdir);
		     last;
	          }
	      }
	      close PRB;
	  }
    }
    close APS;
}
close MODULES;
