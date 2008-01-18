#!/usr/bin/perl -w
#
#  files containing Andes logs, as gotten from OLI, 
#  are not sorted by time.
#  This script sorts sessions in a log file by time
$this="";       # Any lines before the first session 
while (<>) {
    if(/^.* Log of Andes session begun/) {$this=$_;}
    $sessions{$this} .= $_;
}
foreach $session (sort keys %sessions) {
    print "$sessions{$session}";
}
