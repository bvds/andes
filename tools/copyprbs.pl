#!/usr/bin/perl 
#
# copyprbs -- copy all Andes problem files for a given list of problem sets
#
# Usage:  copyprbs.pl -v -m index-file.html dstdir
#
# dstdir is the root of the destination Andes directory. 
#
# -v verbose mode enables tracing of all file copying operations.
# -i index-file.html is the file containing a list of homework sets, default
#    is index.html
#
# Assumes it will run in the root of an Andes source directory.
# APS, prb, and graphic files will be copied into Problems subdirectory of 
# dstdir, which must already exist
#
use Getopt::Long;
my $index="index.html";
&GetOptions("v" => \$verbose,"m=s" => \$index);
#
# docopy (srcfile, dstroot) -- copy named file into directory
# srcfile should be relative to Andes root
#
use File::Copy;
sub docopy()
{
    my($srcfile, $dstroot) = @_;
    my $dstfile = "$dstroot/$srcfile";
    copy ($srcfile, $dstfile) or die "Copy $srcfile $dstfile failed:$!";
    print "     copy $srcfile to $dstroot\n" if $verbose;
}

# get command line arguments 
my $dstdir = $ARGV[0] or die "usage: copyprbs dstdir";

# ensure we have destination directory
unless (-d $dstdir){ die "$dstdir is not a directory.";}
$problemdir = $dstdir . "/Problems";
unless (-e $problemdir) { 
  mkdir $problemdir or die "Couldn't create $problemdir. $!"; 
}
unless (-d $problemdir){ die "$problemdir is not a directory."; }

# do for each module listed
open (MODULES, "problems/" . $index) or 
  die "Couldn't open Problems/" . $index;
while (<MODULES>)
  {
    if(m/"(.*?\.aps)">(.*?)</){
      $apsfile="problems/" . $1;
      $module=$2;   
      # open next aps file to process
      print "Copying files for $module\n";
      open (APS, "$apsfile") or die "Couldn't open $apsfile";
      
      # copy the APS file itself
      &docopy($apsfile, $dstdir);
      
      # do for each line in APS file
      while (<APS>)
	{
	  # skip non-problem lines
	  next if m/ANDES Problem Set/;
	  next if m/\.wmv/;    # demo video line
	  # delete trailing spaces. Includes CR on Windows
	  s/[\s]+$//;     
	  next unless $_; # skip blank lines
	  # copy prb file
	  $prbfile = "solutions/" . uc($_) . ".prb";
	  if (! -e $prbfile) {
	    # assume this is a stub problem, so keep going
	    print "  no file $prbfile, skipping\n"; 
	    next;
	  }
	  &docopy($prbfile, $dstdir);
	 
	  # scan inside prb file for graphic and copy it also
	  open (PRB, "$prbfile") or die "Couldn't open $prbfile. $!";
	  while (<PRB>) {
	    if (/^Graphic +"([^"]+)"/) {
	      $graphicfile = "images/$1";
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
