#!/usr/bin/perl
#
# splitlogs -- split concatenated sequence of Andes logs files from a single
# file (as retrieved from OLI database query) into individual log files
#
# Usage:     splitlogs.pl combined-log-file 
#
# Creates one file per log in the current directory, with name like:
#       userid-date-problem.log
#

# For each log we encounter, we create a file with a temporary name and copy 
# log lines into it, renaming the output file at the end of each log
$TEMPNAME = "Temp.log";   # temp name for current log's output file 

while (<>) 
{
if (/^# Log of Andes session begun/) {
    # begin writing to a new file
    open(OUTF, ">$TEMPNAME") || die "Failed to create output file $TEMPNAME: $!";
    $inlog = 1;
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
