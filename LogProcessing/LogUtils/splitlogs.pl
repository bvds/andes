#!/usr/bin/perl
#
# splitlogs -- split concatenated sequence of Andes logs files from a single
# file as retrieved from OLI database query into individual log files
#
# Usage:     splitlogs.pl andes-log-records.csv
#
# Creates one file per log in the current directory, with name like:
#       userid-date-problem.log
#
# OLI's data extraction tool returns the logs as database records in comma 
# separated values form. Which columns are included depends on the query.
# The entire contents of each andes session log file will be included as the last 
# column in one of these database records. Note the log file text itself includes CRLFs
# delimiting lines within it.
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


# For each log we encounter, we create a file with a temporary name and copy 
# log lines into it, renaming the output file at the end of each log
$TEMPNAME = "Temp.log";   # temp name for current log's output file 

while (<>) 
{
if (/^.*# Log of Andes session begun/) {
    # begin writing to a new file
    open(OUTF, ">$TEMPNAME") || die "Failed to create output file $TEMPNAME: $!";
    # set flag to start copying lines into current output file
    $inlog = 1;
    # delete any database fields preceding log header on this line
    s/^.*#/#/;
}
if (/set-session-id "([^"]*)"/) {
    $session = $1;
    ($session_userid, $date) = split(/-/, $session, 2);
}
if (/read-student-info "([^"]*)"/) { 
    $student = $1;  # session label should start with student id
    if ($student ne $session_userid) {
	  warn "warning: session label $session_userid doesn't begin with student id $student\n";
    }
}
if (/read-problem-info "([^"]*)"/) {
    $problem = $1;
}
# while within logfile contents, copy all lines to output
if ($inlog) {
    print OUTF;
}
if (/END-LOG/) {
    # rename output file to show session and problem.
    close OUTF;
    # adjust this line to format the output file name:
    $newname = "$problem-$student-$date.log";  # by problem first
    # $newname = "$student-$problem-$date.log";    # by student first
    rename($TEMPNAME, $newname) || warn "Couldn't rename $TEMPNAME to $newname: $!";
    print STDERR "Finished log $newname\n";
    $inlog = 0;
}

} # end while (<>)
