#!/usr/bin/perl -w
#
#  files containing Andes logs, as gotten from OLI, 
#  are not sorted by time.
#  This script sorts sessions in a log file by time

# Had to install this on FC6.
# On OS X, run existing cpan and install DateTime::Format::Strptime
use DateTime::Format::Strptime;
my $dateformat1 = DateTime::Format::Strptime->new( pattern => '%F %T' );
my $dateformat2 = DateTime::Format::Strptime->new( pattern => '%Y/%m/%d %T' );
my $dateformat3 = DateTime::Format::Strptime->new( pattern => '%B %d, %Y %T' );

my $date=DateTime->new(year=>1000);       # Any lines before the first session 

while (<>) {
  if(/^(.*)# Log of Andes session begun \w+, (.+) by /) {
    if ($1) {
      my $tmp=$1; 
      chop $tmp; # remove trailing comma
      $date = ($dateformat1->parse_datetime($tmp)
	       or $dateformat2->parse_datetime($tmp));
    } else {
      $date=$dateformat3->parse_datetime($2);
      warn "Using local computer time $date for timestamp";
    } 
  }
    $sessions{$date} .= $_;
}
foreach $session (sort keys %sessions) {
  print "$sessions{$session}";
}
