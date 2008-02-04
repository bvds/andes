#!/usr/bin/perl -w
# Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to convert online Andes Questionnaires 
# into csv format.
# Usage: 
#    questionnaires_to_excel.plx andes_q.txt > andes_q.csv
#    In the spreadsheet, do a regexp find and replace to remove single
#    quote at beginning of line (used to force text format),
#    then sort first 3 columns.  Make School Section CourseID Name columns
#    explcitly text format.  (I could only get this to work in 
#    openoffice and then export to excel.)
# Mathematica format:
#    questionnaires_to_excel.plx -m andes_q.txt > andes_q.m

use Getopt::Long;
my $mma=0;
GetOptions ('math|m|mma' => \$mma);

my @categories = (
'Year','Semester','School','Section','CourseID','Name','Date','Time',
'1','2','3','4','5','6','7','8A','8B','9','10','11A','11B','11C','11D','11E','11F','11G','11H','11I','11J','11K','11L','12A','12B','12C','12D','12E','13A','13B','13C','13D','14','15','16A','16B','16C','16D','16E','17','18A','18B','18C','18D','19','20','21','22','23A','23B','23C','23D','23E','24','25','26','27','28','29','30','30A','30B','30C','30D'
);

# for sanity test later on
my %categories_hash;
foreach (@categories) { $categories_hash{$_}=1; }
 
# print out header line
{
  local $"=",";  # comma formatted lists
  my @quoted_categories =  map {"\"" . $_ . "\""} (@categories);
  print "questions={{" if $mma;
  print "@quoted_categories";
  print "}," if $mma;
  print "\n";
}

while (<>) {  #loop through mails
  my %answers=();
  my $last=0;

  while (<>) {  # loop through questionnaire lines
    last if /^.From andes2\@pitt.edu/;
    
    0 and warn "working on $_";

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
       $answers{$last} .= $_;
    }
    # Date: Tue, 27 Nov 2007 13:39:02 -0500 (EST)
    elsif (/^Date: \w+, (\d+ \w+) (\d+) (\d+:\d+:\d+) /) {
      $answers{'Date'} = $1 . ' ' .$2;
      $answers{'Year'} = $2;
      $answers{'Time'} = $3;
    } else {
      $last=0;
    }
  }

  # Test that all keys are matched
  foreach my $key (keys %answers) {
    warn "unmatched category $key" unless $categories_hash{$key};
  }
  
  my $count=0;
  print "{" if $mma;
  foreach (@categories) {
    if ($count++) { print ",";}
    my $x=$answers{$_};
    next unless $x;
    chomp $x;  # remove CR; in unix, we don't have \r to worry about
    # match a number unless it is a student name
    if ($x =~ /^[+-]?[\d.]+$/ and not /Name/) {
      print "$x";
    }
    else {  #quote everything else
      if ($mma) {
	$x =~ s/\n/\\n/g; # escape newline
	$x =~ s/"/\\"/g;  # escape quote
      } else {
	$x =~ s/"/""/g; # for csv, double any quotes
	# force text format in spreadsheet
	$x = "'" . $x unless /Date/ or /Time/;
      }
      print "\"$x\"";
    }
  }
  print "}" if $mma;
  last if eof;  # when inner loop reaches eof
  print "," if $mma;
  print "\n";

}

print "};" if $mma;
print "\n";
