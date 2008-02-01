#!/usr/bin/perl -w
# Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to convert online Andes Questionnaires into an Excel-readable format.
# Usage: questionnaires_to_excel.plx Andes_Questionnaire_Fall07.txt > junk.out
use strict;

my @catagories = (
'Name','Section','School','Semester','CourseID','Date','Time',
'1','2','3','4','5','6','7','8A','8B','9','10','11A','11B','11C','11D','11E','11F','11G','11H','11I','11J','11K','11L','12A','12B','12C','12D','12E','13A','13B','13C','13D','14','15','16A','16B','16C','16D','16E','17','18A','18B','18C','18D','19','20','21','22','23A','23B','23C','23D','23E','24','25','26','27','28','29','30','30A','30B','30C','30D'
);

{
  local $"=",";  # comma formatted lists
  print "@catagories\n";
}

while (<>) {  #loop through sessions
  my %answers=();

  while (<>) {  # loop through questionnaire lines
    last if /^.From andes2\@pitt.edu/;
    
    # Find the answers to the close-ended questions.
    if (/^Question_(.+)=([^\r\n]+)/) {  #don't include CR in name
      $answers{$1} = $2;
    }
    elsif (/^Open-Ended_(.+)=([^\r\n]+)/) {  #don't include CR in name
      #throw away
    }
    # get non-question entries
    elsif (/^(\w+)=([^\r\n]+)/) { # use ^\r so we don't include CR in names
      $answers{$1} = $2;
    }
    # Date: Tue, 27 Nov 2007 13:39:02 -0500 (EST)
    elsif (/^Date: \w+, (\d+ \w+ \d+) (\w+) /) {
      $answers{'date'} = $1;
      $answers{'time'} = $2;
    }
  }
  
  my $count=0;
  foreach (@catagories) {
    if ($count++) { print ",";}
    if ($answers{$_} and $answers{$_} =~ /^[+\-\.\d]+$/) {     # match a number
      print "$answers{$_}";
    }
    elsif ($answers{$_}) {  #quote everything else
      print "\"$answers{$_}\"";
    }
  }
  print "\n";
}  
 

