#!/usr/bin/perl -w
# Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to convert online Andes Questionnaires 
# into csv format.
# Usage: questionnaires_to_excel.plx andes_q.txt > andes_q.csv

my @categories = (
'Name','Section','School','Semester','Year','CourseID','Date','Time',
'1','2','3','4','5','6','7','8A','8B','9','10','11A','11B','11C','11D','11E','11F','11G','11H','11I','11J','11K','11L','12A','12B','12C','12D','12E','13A','13B','13C','13D','14','15','16A','16B','16C','16D','16E','17','18A','18B','18C','18D','19','20','21','22','23A','23B','23C','23D','23E','24','25','26','27','28','29','30','30A','30B','30C','30D'
);

# for sanity test later on
my %categories_hash;
foreach (@categories) { $categories_hash{$_}=1; }
 
# print out header line
{
  local $"=",";  # comma formatted lists
  my @quoted_categories =  map {"\"" . $_ . "\""} (@categories);
  print "@quoted_categories\n";
}

while (<>) {  #loop through mails
  my %answers=();
  my $last=0;

  while (<>) {  # loop through questionnaire lines
    last if /^.From andes2\@pitt.edu/;
    
    0 and warn "working on $_";

    chomp;  # remove CR; in unix, we don't have \r
    # Find the answers to the close-ended questions.
    if (/^Question_(\w+)=(.*)/) { 
      $answers{$1} = $2;
      $last=$1; 
    }
    # throw away Open-Ended fields
    elsif (/^Open-Ended_(\w+)=(.*)/) {
      $last=0;
    }
    # get non-question fields (Name, School, etc.)
    elsif (/^(\w+)=(.*)/) {
      $answers{$1} = $2;
      $last=0;
    }
    #  Sometimes essay answers take up multiple lines, paste to most recent. 
    elsif ($last) {
       $answers{$last} .= ' ' . $_;
    }
    # Date: Tue, 27 Nov 2007 13:39:02 -0500 (EST)
    elsif (/^Date: \w+, (\d+ \w+ \d+) (\w+) /) {
      $answers{'date'} = $1;
      $answers{'time'} = $2;
    } else {
      $last=0;
    }
  }

  # Test that all keys are matched
  foreach my $key (keys %answers) {
    warn "unmatched category $key" unless $categories_hash{$key};
  }
  
  my $count=0;
  foreach (@categories) {
    if ($count++) { print ",";}
    if ($answers{$_}) {
      if ($answers{$_} =~ /^[+-]?[\d.]+$/) {     # match a number
	print "$answers{$_}";
      }
      else {  #quote everything else
	$answers{$_} =~ s/"/\\"/g; # escape any quotes
	print "\"$answers{$_}\"";
      }
    }
  }
  print "\n";

  last if eof;  # in case inner loop already reached eof
}
