# !/usr/bin/perl
# Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to convert online Andes Questionnaires into an Excel-readable format.
# Usage: perl questionnaires_to_excel.plx Andes_Questionnaire_Fall07.txt
use strict;

# Variables for looping through log file
my @questionnaire;
my @answer_array;

# Variables for printing the results.
my $name;
my $school;
my $year;
my $semester;
my $courseID;
my $section;
my $date;
my $time;

# Open the file containing the questionnaire and build an array from it.
open (FILENAMES, @ARGV[0]) || die ("Could not open file: $!");
while (<FILENAMES>) { chomp; push @questionnaire, $_; }

# Open a file to store the scores.
my $file = "Andes_Questionnaire_Spreadsheet.txt";
open(INFO, ">>$file");

# Print a header in the first row in Excel.
# First semester header:
print INFO "Name\tSection\tSchool\tYear\tSemester\tCourseID\tDate\tTime\tQuestion_1\tQuestion_2\tQuestion_3\tQuestion_4\tQuestion_5\tQuestion_6\tQuestion_7\tQuestion_8A\tQuestion_8B\tQuestion_9\tQuestion_10\tQuestion_11A\tQuestion_11B\tQuestion_11C\tQuestion_11D\tQuestion_11E\tQuestion_11F\tQuestion_11G\tQuestion_11H\tQuestion_11I\tQuestion_11J\tQuestion_11K\tQuestion_11L\tQuestion_12A\tQuestion_12B\tQuestion_12C\tQuestion_12D\tQuestion_12E\tQuestion_13A\tQuestion_13B\tQuestion_13C\tQuestion_13D\tQuestion_14\tQuestion_15\tQuestion_16A\tQuestion_16B\tQuestion_16C\tQuestion_16D\tQuestion_16E\tQuestion_17\tQuestion_18A\tQuestion_18B\tQuestion_18C\tQuestion_18D\tQuestion_19\tQuestion_20\tQuestion_21\tQuestion_22\tQuestion_23A\tQuestion_23B\tQuestion_23C\tQuestion_23D\tQuestion_23E\tQuestion_24\tQuestion_25\tQuestion_26\tQuestion_27\tQuestion_28\tQuestion_29\tQuestion_30\tQuestion_30A\tQuestion_30B\tQuestion_30C\tQuestion_30D";
# Second semester header:
#print INFO "Name\tSection\tSchool\tYear\tSemester\tCourseID\tDate\tTime\tQuestion_1\tQuestion_2\tQuestion_3\tQuestion_4\tQuestion_5\tQuestion_6\tQuestion_7\tQuestion_8A\tQuestion_8B\tQuestion_9\tQuestion_10\tQuestion_11A\tQuestion_11B\tQuestion_11C\tQuestion_11D\tQuestion_11E\tQuestion_11F\tQuestion_11G\tQuestion_11H\tQuestion_11I\tQuestion_11J\tQuestion_11K\tQuestion_12A\tQuestion_12B\tQuestion_12C\tQuestion_12D\tQuestion_12E\tQuestion_13A\tQuestion_13B\tQuestion_13C\tQuestion_13D\tQuestion_14\tQuestion_15\tQuestion_16A\tQuestion_16B\tQuestion_16C\tQuestion_16D\tQuestion_16E\tQuestion_17\tQuestion_18A\tQuestion_18B\tQuestion_18C\tQuestion_18D\tQuestion_19\tQuestion_20\tQuestion_21\tQuestion_22\tQuestion_23A\tQuestion_23B\tQuestion_23C\tQuestion_23D\tQuestion_23E\tQuestion_24\tQuestion_25\tQuestion_26\tQuestion_27\tQuestion_28\tQuestion_29\tQuestion_30\tQuestion_30A\tQuestion_30B\tQuestion_30C\tQuestion_30D";

# loop through array of questionnaire lines, and process each one.
foreach (@questionnaire) {
  
  if ($_ =~ /Name\=(.*)/) {
    $name = $1;
  }

  if ($_ =~ /Section\=(.*)/) {
    $section = $1;
  }

  if ($_ =~ /School\=(.*)/) {
    $school = $1;
  }

  if ($_ =~ /Semester\=(.*)/) {
    $semester = $1;
  }

  if ($_ =~ /CourseID\=(.*)/) {
    $courseID = $1;
  }

  # Date: Tue, 27 Nov 2007 13:39:02 -0500 (EST)
  if ($_ =~ /Date: /) {
    my @temp_array = split (/\s/, $_);
    $date = $temp_array[2] . " " . $temp_array[3] . " " . $temp_array[4];
    $year = $temp_array[4];
    $time = $temp_array[5];
  }
  
  # Find the answers to the close-ended questions.
  if ($_ =~ /Question\_(.*)\=(.*)/) {
    push (@answer_array, $2);
  }
  
  # Find the last line of the survey & Print the results.
  if ($_ =~ /From andes2\@pitt.edu/) { 
  
    print INFO "\n$name\t$section\t$school\t$year\t$semester\t$courseID\t$date\t$time\t";
  
    foreach my $ans (@answer_array) {
      chomp $ans;
      print INFO "$ans\t";
    }
    
    # Initialize variables
    @answer_array = ();
    $name = 0;
    $school = 0;
    $year = 0;
    $semester = 0;
    $courseID = 0;
    $section = 0;
    $date = 0;
    $time = 0;
  }
  
} # next line

print "\ndone!\n";