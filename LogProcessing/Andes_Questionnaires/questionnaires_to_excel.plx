# !/usr/bin/perl
# Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to convert online Andes Questionnaires into an Excel-readable format.
# Usage: perl questionnaire_to_excel.plx file.txt
use strict;

# Variables for looping through log file
my @filenames;
my @line_array;
my $j = 0;
my @tmp_array;
my @new_array;
my $single_line;

# Variables for printing the results.
my $name;
my $school;
my $year;
my $semester;
my $courseID;
my $section;
my $date;
my $time;

# Open the list of files and build an array from them.
open (FILENAMES, @ARGV[0]) || die ("Could not open file: $!");
while (<FILENAMES>) { chomp; push @filenames, $_; }

# Open a file to store the scores.
my $file = "Andes_Questionnaire4.xls";
open(INFO, ">>$file");

# Print a header in the first row in Excel.
# First semester header:
print INFO "Name\tSection\tSchool\tYear\tSemester\CourseID\tDate\tTime\tQuestion_1\tQuestion_2\tQuestion_3\tQuestion_4\tQuestion_5\tQuestion_6\tQuestion_7\tQuestion_8A\tQuestion_8B\tQuestion_9\tQuestion_10\tQuestion_11A\tQuestion_11B\tQuestion_11C\tQuestion_11D\tQuestion_11E\tQuestion_11F\tQuestion_11G\tQuestion_11H\tQuestion_11I\tQuestion_11J\tQuestion_11K\tQuestion_11L\tQuestion_12A\tQuestion_12B\tQuestion_12C\tQuestion_12D\tQuestion_12E\tQuestion_13A\tQuestion_13B\tQuestion_13C\tQuestion_13D\tQuestion_14\tQuestion_15\tQuestion_16A\tQuestion_16B\tQuestion_16C\tQuestion_16D\tQuestion_16E\tQuestion_17\tQuestion_18A\tQuestion_18B\tQuestion_18C\tQuestion_18D\tQuestion_19\tQuestion_20\tQuestion_21\tQuestion_22\tQuestion_23A\tQuestion_23B\tQuestion_23C\tQuestion_23D\tQuestion_23E\tQuestion_24\tQuestion_25\tQuestion_26\tQuestion_27\tQuestion_28\tQuestion_29\tQuestion_30\tQuestion_30A\tQuestion_30B\tQuestion_30C\tQuestion_30D";
# Second semester header:
print INFO "Name\tSection\tSchool\tYear\tSemester\CourseID\tDate\tTime\tQuestion_1\tQuestion_2\tQuestion_3\tQuestion_4\tQuestion_5\tQuestion_6\tQuestion_7\tQuestion_8A\tQuestion_8B\tQuestion_9\tQuestion_10\tQuestion_11A\tQuestion_11B\tQuestion_11C\tQuestion_11D\tQuestion_11E\tQuestion_11F\tQuestion_11G\tQuestion_11H\tQuestion_11I\tQuestion_11J\tQuestion_11K\tQuestion_12A\tQuestion_12B\tQuestion_12C\tQuestion_12D\tQuestion_12E\tQuestion_13A\tQuestion_13B\tQuestion_13C\tQuestion_13D\tQuestion_14\tQuestion_15\tQuestion_16A\tQuestion_16B\tQuestion_16C\tQuestion_16D\tQuestion_16E\tQuestion_17\tQuestion_18A\tQuestion_18B\tQuestion_18C\tQuestion_18D\tQuestion_19\tQuestion_20\tQuestion_21\tQuestion_22\tQuestion_23A\tQuestion_23B\tQuestion_23C\tQuestion_23D\tQuestion_23E\tQuestion_24\tQuestion_25\tQuestion_26\tQuestion_27\tQuestion_28\tQuestion_29\tQuestion_30\tQuestion_30A\tQuestion_30B\tQuestion_30C\tQuestion_30D";

# loop through array of file names, putting their contents in an array.
foreach (@filenames) {
  open (FILES, $_) || die ("Could not open file: $!");
  while (<FILES>) { chomp; push @line_array, $_; }

  foreach (@line_array) {
    # Date: December 5, 2006 10:12:29 AM EST
    if ($_ =~ /Date: /) {
      my @temp_array = split (/ /, $_);
      $date = $temp_array[1] . " " . $temp_array[2] . " " . $temp_array[3];
      $time = $temp_array[4] . " " . $temp_array[5] . " " . $temp_array[6];
    }

    if ($_ =~ /Name\=/) {
      my @temp_array = split (/=/, $_);
      $name = $temp_array[1];
    }

    if ($_ =~ /Section\=/) {
      my @temp_array = split (/=/, $_);
      $section = $temp_array[1];
    }

    if ($_ =~ /School\=/) {
      my @temp_array = split (/=/, $_);
      $section = $temp_array[1];
    }

    if ($_ =~ /Section\=/) {
      my @temp_array = split (/=/, $_);
      $section = $temp_array[1];
    }

    $_ =~ s/\n/ /g;
    $single_line .= $_;
  }

  #print "$single_line\n";
  #@new_array  = split (/(Question_.+?=.*?\s)[Open\-Ended_|Question_]|(Open-Ended_.+?=.*?\s)[Open\-Ended_|Question_]/, $single_line);
  @new_array  = split (/\s/, $single_line);

  print INFO "\n$name\t$section\t$date\t$time\t";

  line: foreach my $line (@new_array) {
    if ($line =~ /Question_/) {
      #print "$line\n";
      @tmp_array = split (/=/, $line);
      chomp $tmp_array[1];
      print INFO "$tmp_array[1]\t";
    }
  }

#print INFO "\n";
@new_array  = ();
@line_array = ();
$single_line = "";

} # next file

print "\ndone!\n";