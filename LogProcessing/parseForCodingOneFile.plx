#!/usr/bin/perl
#  Copyright (c) 2007 Robert G.M. Hausmann
#
# The purpose of this script is to segment andes log files for import into excel.
# Once the script is run, it will create a single file with all the condensed tranactions from the log
# files specified in "files.txt". The goal is to represent student entries in a way that can be coded
# in excel.
#
# Usage: perl parseForCoding.plx files.txt
use strict;

# Variables for looping through log file
my @filenames;
my @line_array;
my $line_num;
my $i;
my @tmp_array;
my $current_problem = "null";
my $condensed_file_name = "Condensed_Files_Exper2";
my $subjid = "null";
my $transaction_ID = 0;
my $check_entries = "false";

# Open the log file & store in an array.
open (FILENAMES, @ARGV[0]) || die ("Could not open file: $!");
while (<FILENAMES>) { chomp; push @filenames, $_; }

# loop through array of file names, putting their contents in an array.
foreach (@filenames) {
  open FILES, $_;
  @line_array = <FILES>;

  foreach (@line_array) {

    # Create a name for the output file.
    if ($_ =~ /DDE-POST \(set-session-id \"(.+)\"\)/) {
      #$condensed_file_name = substr($1, 0, 11);
      $subjid = substr($1, 0, 5);
    }

    # Establish which problem the student is currently working on.
    # If no problems are currently open, then null.
    if ($_ =~ /read-problem-info/) {
      my @temp_array = split (/"/, $_);
      $current_problem = $temp_array[1];
    }
    if ($_ =~ /close-problem/) {
      $current_problem = "null";
    }

    # Condense log file.
    if ($_ =~ /DDE|AndesDemo/) {
      $line_num = $i + 1;
      $tmp_array[$i] = $subjid . "\t" . $current_problem . "\t" . $line_num . "\t" . $transaction_ID . "\t" . $_;
    }

    # Set Check-Entries mode to avoid counting them as transactions
    if ($_ =~ /Check-Entries\sT/) {
      $check_entries = "true";
      print "$line_num check entries: $check_entries\n";
    }
    if ($_ =~ /Check-Entries\sNIL/) {
      $check_entries = "false";
      print "$line_num check entries: $check_entries\n";
    }

    # Print a carriage return if a user action is performed in the interface.
    # Bump the transaction counter for entries and non-events (i.e., Andes housekeeping)
    if ( ($_ =~ /DDE\s|DDE-POST/) && ($_ !~ /Check-Entries|DDE\s\(handle-student-response|DDE\s\(Explain-More/) && ($check_entries =~ "false") ) {
      $line_num = $i + 1;
      $transaction_ID++;
      $tmp_array[$i] = "\n" . $subjid . "\t" . $current_problem . "\t" . $line_num . "\t" . $transaction_ID . "\t" . $_;
    }

    $i++;

  } # next line

  $i=0;

  # Open a file to store the scores.
  my $file1 = $condensed_file_name . ".txt";
  open(CONDENSE, ">>$file1");

  foreach (@tmp_array) {
    print CONDENSE $_;
  }
  
  close CONDENSE;
  @tmp_array = ();

} # next file

print "\ndone!\n";