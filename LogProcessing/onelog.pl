#!/usr/bin/perl -w
#
# onelog --- analyze a concatonated sequence of Andes log files
# as retrieved from OLI database query
#
# Usage:     onelog.pl access.csv
#
# OLI's data extraction tool returns the logs as database records in comma 
# separated values form. Which columns are included depends on the query.
# The entire contents of each andes session log file will be included as 
# the last column in one of these database records.  The log file text itself 
# includes CRLFs delimiting lines within it.
#
# So if we asked for date and info fields from the database, the
# combined log file will start with a header column line:
#
# 	time,info
# 
# followed by record lines which look like this:
#
#    	2006-05-04 11:30:04.0,# Log of Andes session begun Thursday, May 04, 2006 11:11:46 by m094530 on 09MOORE-CJ30A
# 	# Version 1
# 	# Help-flags 01f Procedural Conceptual Example
# 	0:00	Andes-Version 10.1.3
# 	0:00	FBD-Version 04 05 06^M
#         ... rest of log1 ...
# 	18:19	END-LOG 
#	
# 	2006-05-04 11:31:34.0,# Log of Andes session begun Thursday, May 04, 2006 11:30:13 by m094530 on 09MOORE-CJ30A
#         ... rest of log2 ...
#        4:40 END-LOG
#
# We pull out contents between the Andes header line and the END-LOG line.


while (<>) { # loop over andes sessions
    # find (and discard) database header
    unless(/^.* Log of Andes session begun/){next;}  
    
    while (<>) {   #loop over lines in Andes Session
	if(/END-LOG/) {last;}    
	unless(/DDE/) {next;} # skip non DDE lines
	if (/read-student-info .(\w+)/) {
	    $student = $1;  # session label should start with student id
	}
	elsif (/set-session-id .(\w+)-([a-zA-Z0-9-]+)/) {
	    $session_userid = $1;
	    $date = $2;
	}
	elsif (/read-problem-info .(\w+)/) {
	    $problem = $1;
	    $problem  =~ tr/A-Z/a-z/;  #set to lower case
	}
	elsif (/^(\d+):(\d+)\s+DDE.*close-problem/) {
	    $time_used = $1*60+$2; #total time in seconds
	}
    }

    if ($student ne $session_userid) {
	warn "warning: session label $session_userid doesn't match $student\n";
    }
    $prob{$student}{$problem} += $time_used;
    push @{ $sessions{$student}{$problem}}, $date;
 #   print "student $student problem $problem at $date for $time_used s\n";

}

foreach $student (keys %prob) {
    foreach $problem (keys %{$prob{$student}}) {
	print "$student $problem $prob{$student}{$problem} @{ $sessions{$student}{$problem}}";
    }
}
