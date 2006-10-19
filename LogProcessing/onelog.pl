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

$max_pause=0;

while (<>) { # loop over andes sessions
    # find (and discard) database header
    next unless /^.* Log of Andes session begun/;  
    
    $last_time=0;
    $dt_max=0;
    $score=0;
    while (<>) {   # loop over lines in Andes session
	last if /\tEND-LOG/;  # end of Andes session
	
	if (/^(\d+):(\d+)\t/) {
	    $this_time = $1*60+$2; # total time in seconds
	    $dt = $this_time - $last_time;
	    $last_time = $this_time;
	    
	    next if /\tApp-activate/;  # ignore pauses associated with sleep

	    if($dt > $dt_max) { $dt_max = $dt; }
	    if($dt>$max_pause) { $max_pause=$dt;}
             # make a histogram of the pauses
             #	    @hist_pause[]++;
	}

	next unless /\tDDE/;  # skip non DDE lines
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
	elsif (/^(\d+):(\d+)\tDDE.*close-problem/) {
	    $time_used = $1*60+$2; #total time in seconds
	}
	elsif (/\t DDE-COMMAND set-score (\d+)/) {
	    $score = $1; # we want the final score
	}
    }

    unless ($_) {
	warn "End of file encountered during Andes session\n";
	last;
    }
    if ($student ne $session_userid) {
	warn "warning: session label $session_userid doesn't match $student\n";
    }

    $times{$student}{$problem} += $time_used; # accumulate time used
    $scores{$student}{$problem} = $score; # want final score

    unless($pause{$student}{$problem} and 
	   $dt_max < $pause{$student}{$problem}) {
	$pause{$student}{$problem}=$dt_max; # record largest pause
    }
    push @{ $sessions{$student}{$problem}}, $date; # accumulate session times

}

foreach $student (keys %times) {
    foreach $problem (keys %{$times{$student}}) {
#	print "$student $problem $times{$student}{$problem} @{ $sessions{$student}{$problem}}\n";
#	print "$student $problem $times{$student}{$problem} $scores{$student}{$problem} $pause{$student}{$problem}\n";
	if($pause{$student}{$problem} > 0.5*$max_pause) {
	    print "$student $problem $times{$student}{$problem} $pause{$student}{$problem} @{ $sessions{$student}{$problem}}\n";
	}
    }
}

print "\nMaximum pause found is $max_pause s\n";


