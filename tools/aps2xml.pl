#!/usr/bin/perl 
# aps2xml.pl -- perl script to take Andes problem sets and generate OLI 
# learning pages for them.
#
# usage:  perl aps2xml *.aps
#
# Also generates the little OLI problem descriptor file for a problem if it 
# doesn't already exist and copies needed problem files.
#
# Expects to run in the root of the OLI source directory into which we
# will copy, e.g. Andes2/oli/intro_physics-1_8_patches
$root = ".";
# expects to find current Andes development files here:
# $ANDES = "C:/cygwin/home/andersw/Andes2";
$ANDES = "../..";
# Will fetch problem statements from problem statement files written into
#     $ANDES/Statements/<problem-id>.txt 
# Lisp code to do this from the knowledge base is in kb/makeprob.cl

# List of problem set ids that belong in em subtree. All others
# go in mechanics. If any new problem sets added to E&M, must update
# this list. Note id format is lower-case w/underscores instead of spaces.
%emsets= (			
"resistance", "em",
"capacitance", "em",
"dc_circuits", "em",
"electric_field", "em",
"electric_potential", "em",
"electromagnetic_induction", "em",
"inductance", "em",
"magnetic_field", "em",
"optics", "em",
"electromagnetic_waves", "em",
"ac_circuits", "em",
"em-extras", "em"
);

# IMPORTANT: remember to set this switch correctly! 
# Set to 1 if building a version that shouldn't show any stubs (e.g. for open and free)
# Set to 0 to include stub processing
$ignore_stubs = 0; # set to 1 to ignore stub problems entirely

use File::Copy;

sub sync # copy src to dst if it is newer
{
    my($src, $dst) = @_;
    if (! -e $src) {
	  print STDERR "Source file not found!: $src\n";
    }elsif (! (-e $dst)) {
          print "adding $dst\n";
	  copy($src, $dst);
	  # set mod time? see below
    } else {
    	$mtime_src = (stat $src)[9];
    	$mtime_dst = (stat $dst)[9];
    	if ($mtime_src <= $mtime_dst) {
	  	print "$dst is up to date\n";
    	} else {
          	print "updating $dst\n";
	  	copy($src, $dst);
	  # set modification date on copy??
	  # utime call applies accesstime and modtime from src:
	  # utime (stat($src)[8], stat($src)[9], $dst);
	}
    }
}

while (<>) {
	if ($ARGV ne $current)  # started new .aps file from command line
	{
	     # finish up previous file, if any
	     if ($current) {
		     if ($open_section) {
			     print LP "		</body></section>\n";
			     $open_section = 0;
		     }
		     print LP "     </body>\n</workbook_page>";
		     close LP;
	     }
	     $current = $ARGV;
             print "Processing $current:\n";
             ($setname, $ext) = split(/\./, $current);	# E.g Translational Kinematics
	     # in case aps filenames have already had underscores put in for spaces
	     # (may be done for convenience as shell script arguments), put spaces
	     # back in for use in friendly titles
	     $setname =~ tr/_/ /;
	     # form id for set -- all lower case, underscores for spaces
             $setid = $setname;
             $setid =~ tr/A-Z /a-z_/; 	
	     # now that we have gotten the internal id from the filename,  fix up the human-readable title for 
	     # case of experimental problem set names of form treacy-NAME[-exp|-control]
	     #$setname =~ s/treacy-//;
	     #$setname =~ s/-exp//;
	     #$setname =~ s/-control//;
	     $setname =~ s/skatz-//;
	     $setname =~ s/-short-kcd//;
	     $setname =~ s/-long-kcd//;
	     $setname =~ s/-control//;
	     # decide if this goes into em or mechanics.
	     if ($emsets{$setid}) { 
		      $module = "_m02_em"; 
	     } else { 
		      $module = "_m01_mechanics";
	     }
             print "setname = $setname, setid = $setid, module = $module\n";
	     

	     # start writing the learning page for this set
	     $filename = "$root/content/_u1_intro_physics/$module/x-oli-workbook_page/$setid.xml";
	     open (LP, ">$filename") or die("Couldn't open $filename. $!");
	     print "Now writing LP $filename\n";
             print LP <<EOH;
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE workbook_page
PUBLIC "-//Carnegie Mellon University//DTD Workbook Page 3.0//EN"
"http://oli.web.cmu.edu/dtd/oli_workbook_page_3_0.dtd">
<!--
==============================================================================

Copyright (c) 2003-2004 Carnegie Mellon University.

Bill Jerome (wjj@andrew.cmu.edu)

==============================================================================
-->
<workbook_page id="$setid">
<head>
	<title>$setname</title>
</head>
		<body>

<pullout type="note"> 
	<p>Please <link title="Andes installer" href="../../../webcontent/andes_installer.exe">install Andes</link> before doing any exercises.  See the <activity_link idref="intro">Introduction</activity_link> for details.</p>
</pullout>

EOH
	}
	# now do for each problem line in .aps file
	next if (/ANDES Problem Set/);
	# look for special section heading lines used in custom sets used in gated experiment
	if (/^DEMOS/) {
		if ($open_section) { print LP "		</body></section>\n"; }
		print LP "		<section><title>Demonstration Videos</title><body>\n\n";
		$open_section = 1;
		next;
	} elsif (/^HOMEWORK/) {
		if ($open_section) { print LP "		</body></section>\n"; }
		print LP "		<section><title>Homework Problems</title><body>\n\n";
		$open_section = 1;
		next;
	} elsif (/^SEQUENCE/) {
		print LP "<p>The following problems must be completed in order.</p>\n\n";
		next;
	} elsif (/^ENDSEQUENCE/) {
		next; # just like EXTRA, but no section heading
	} elsif (/^EXTRA/) {
		if ($open_section) { print LP "		</body></section>\n"; }
		print LP "		<section><title>Additional Problems</title><body>\n\n";
		$open_section = 1;
		next;
	}

	if (/(\S+)-DEMO\.wmv/) { 	# this line specifies a demo video
	    # write a section for the video link
	    $video = $videoUC = $1;
	    $video =~ tr[A-Z][a-z];	# video file names should be all lower-case
	    $videoUC =~ tr[a-z][A-Z];	# video problem ids upper case, to match problem ids
	    print "    Video name:  $video\n";
            print LP <<EOV;
<section><title>$setname - $videoUC Demo Video</title><body>
<p><link href=\"../webcontent/$video.wmv\">Video</link> showing how to solve problems like $videoUC.  See the <link href=\"../webcontent/videotips.html\"> instructions </link>for viewing videos.</p>
</body></section>

EOV
	} elsif (/\S+/) {	# match non-space characters => problem id
	    $problemid = $&;
	    $problemid =~ tr[a-z][A-Z];	# make it all upper-case
	    print "   problemid = [$problemid]\n";
	    # if prb file does not exist, assume this is a stub
	    $stub = ! (-e "$ANDES/Problems/$problemid.prb");

	    # skip over stubs if flag is set
	    if ($stub && $ignore_stubs) {
		    print "[stub problem - ignored]\n";
		    next;
	    }

	    $title = "$setname - $problemid";
            print LP "<section><title>$title</title><body>\n<p>\n";

	    # make sure we get the problem statement text
	    if (! (open (STMT, "$ANDES/Statements/$problemid.txt"))) {
		   if (! $stub) {
			   print STDERR "ERROR! Couldn't open statement file for non-stub $problemid\n";
		   }
		   print "[no problem statement found]\n";
	    } else {
		   while ($line = <STMT>) {
			 # for consistent Unix style output, change DOS CRLF to LF alone
			 $line =~ s/\r//;   
			 print LP "$line";
		   }
		   close STMT;
		   print "Copied statement for $problemid\n";
	    }
	    print LP "</p>\n";


	    if ($stub) {
		    print "Creating stub for $problemid\n";
		    print LP "<p><em>This problem is not ready for use.</em></p>"
	    }
	    else # not a stub now
	    {
	    	# if WAS a stub -- as marked by presence in some previous versions' "Stubs" dir -- but no 
		# longer is,  note that it requires upgrade. !!! Have to a new clause here each time we 
		# update an existing version, and build appropriate stub directory. Should make this
		# automatic
	
		# Set following to current version suffix (for stub copying below). Should mkdir!
		$currentstubs = "stubs-2.0.0";
		if (-e "Stubs1203/$problemid.prb" ) {
 		    print LP "<p>This problem requires <link href=\"../../../webcontent/andes_installer.exe\">upgrade to Andes 12.0.4</link> or higher.</p>";
		    print "Unstubbed in 12.0.4: $problemid\n";
		}
		elsif (-e "Stubs1202/$problemid.prb" ) {
		    print LP "<p>This problem requires <link href=\"../../../webcontent/andes_installer.exe\">upgrade to Andes 12.0.3</link> or higher.</p>";
		    print "Unstubbed in 12.0.3: $problemid\n";
 		} 
		elsif (-e "Stubs1201/$problemid.prb") {
		    print LP "<p>This problem requires <link href=\"../../../webcontent/andes_installer.exe\">upgrade to Andes 12.0.2</link> or higher.</p>";
		    print "Unstubbed in 12.0.2: $problemid\n";
		}   else {
		  #print "No stub file found: Stubs/$problemid.prb\n";
		}
	    }

	    # print LP "<activity purpose='practice' type='x-pitt-lrdc-andes1-assignment' idref='$problemid' />\n";
	    print LP "<activity purpose='learnbydoing' idref='$problemid' />\n";
            print LP "</body></section>\n\n";

	    # copy the latest problem files, reporting additions
	    # This will change the modified date even if the files haven't changed. That's OK, but makes it
	    # hard to tell what version of files we are using, e.g. very old fbds that haven't changed.
	    $prbdst = "$root/content/_u1_intro_physics/$module/webcontent/andes/$problemid.prb";
	    if (! (-e $prbdst)) {
		    print "Add new problem file $problemid.prb in $module\n";
	    }
	    if ($stub) {
	       &sync("$ANDES/Problems/DUMMY.prb", $prbdst) or warn "couldn't copy DUMMY.prb for $problemid.prb";
	       &sync("$ANDES/Problems/DUMMY.prb", "$currentstubs/$problemid.prb") or warn "couldn't copy DUMMY.prb to current stub Dir $problemid.prb";
	    } else {
	       &sync("$ANDES/Problems/$problemid.prb", $prbdst) or warn "couldnt copy $problemid.prb";
	    }
	    # copy other files used by problem
	    $problemname = $problemid;
	    $problemname =~ tr[A-Z][a-z];  # lower case
	    if (-e "$ANDES/Problems/$problemname.fbd") { # check for fbd file -- now obsolete
                 $fbddst = "$root/content/_u1_intro_physics/$module/webcontent/andes/$problemname.fbd";
		 if (! (-e $fbddst)) {
		    print "Add new problem file $problemname.fbd in $module\n";
		 }
                  &sync("$ANDES/Problems/$problemname.fbd", $fbddst) or die "couldn't copy $problemname.fbd";
	    } elsif (! $stub) { # no fbd file but not a stub.
		    # scan inside prb to see if it needs a graphic file 
		    open (PRB, "$ANDES/Problems/$problemid.prb") or warn "Couldn't open $problemid.prb";
		    while (<PRB>) {
			if (/^Graphic +"([^"]+)"/) {
				$graphic = $1;
				$graphicdst ="$root/content/_u1_intro_physics/$module/webcontent/andes/$graphic" ;
				if (! (-e $graphicdst)) {
				    print "Add new graphic file $graphic in $module\n";
				}
			        &sync("$ANDES/Problems/$graphic", $graphicdst) or warn "couldn't copy $graphicdst";
				last;
			}
			last if (/^Graphic +NIL/); 
		    }
	    }

	    # write a new xml problem descriptor file for this problem, if one doesn't exit. Otherwise leave
	    # existing one, since they almost never change. 
	    $xmldst = "$root/content/_u1_intro_physics/$module/x-pitt-lrdc-andes1-assignment/$problemid.xml";
	    next if (-e $xmldst); 
	    if (! open (PRBXML, ">$xmldst")) {
		    print STDERR "ERROR! Couldn't write problem file $xmldst. $!\n"
	    } else {
              print PRBXML <<EOP;
<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE andes_assignment PUBLIC '-//Carnegie Mellon University//DTD Andes Assignment 1.0b1//EN' 'http://oli.web.cmu.edu/dtd/pitt_lrdc_andes1_assignment_1_0b1.dtd'>

<andes_assignment id=\"$problemid\">
	<title>$title</title>
	<path>_u1_intro_physics/$module/webcontent/andes/</path>
</andes_assignment>
EOP
   	       close PRBXML;
	    } 
            
	} # end problem line in APS file
} # end main loop reading from successive APS file args

# close last file arg:
if ($open_section) {
	print LP "		</body></section>\n";
	$open_section = 0;
}
print LP "         </body>\n</workbook_page>\n";
close LP;
