#!/usr/bin/perl -w
#
#  files containing Andes logs, as gotten from OLI, 
#  are not sorted by time.
#  This script sorts sessions in a log file by time
# Had to install this on FC6.
# On OS X, run existing cpan and install DateTime::Format::Strptime
use DateTime::Format::Strptime;
my $dateformat =
    DateTime::Format::Strptime->new( pattern => '%B %d, %Y %T' );

$date=DateTime->new( year => 1000 );    # Any lines before the first session 

while (<>) {
    if(/^.* Log of Andes session begun \w, (.+) by /) {
      $date = $dateformat->parse_datetime($1);
    }
    $sessions{$date} .= $_;
}
foreach $session (sort keys %sessions) {
    print "$sessions{$session}";
}
