#!/usr/bin/perl
#  Copyright (c) 2006 Robert G.M. Hausmann
# The purpose of this script is to segment andes log files.
# Usage: perl parseForCoding.plx files.txt
use strict;

# Variables for looping through log file
my @filenames;
my @line_array;
my $line_num;
my $i;
my @tmp_array;
my $current_problem = "null";
my $condensed_file_name;
my $transaction_ID = 0;

# Open the log file & store in an array.
open (FILENAMES, @ARGV[0]) || die ("Could not open file: $!");
while (<FILENAMES>) { chomp; push @filenames, $_; }

# loop through array of file names, putting their contents in an array.
foreach (@filenames) {
  open FILES, $_;
  @line_array = <FILES>;

  foreach (@line_array) {

    # Create a name for the output file.
    if ($_ =~ /set-session-id/) {
      my @temp_array = split (/"/, $_);
      $condensed_file_name = $temp_array[1];
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
      $tmp_array[$i] = $current_problem . "\t" . $line_num . "\t" . $transaction_ID . "\t" . $_;
    }

    # Print a carriage return if a user action is performed in the interface.
    # Bump the transaction counter for entries and non-events (i.e., Andes housekeeping)
    if ( ($_ =~ /DDE\s|DDE-POST/) && ($_ !~ /Check-Entries/) ) {
      $line_num = $i + 1;
      $transaction_ID++;
      $tmp_array[$i] = "\n" . $current_problem . "\t" . $line_num . "\t" . $transaction_ID . "\t" . $_;
    }

    $i++;

  } # next line

  $i=0;

  # Open a file to store the scores.
  my $file1 = "ParsedFileToCode" . $condensed_file_name . ".txt";
  open(CONDENSE, ">$file1");

  foreach (@tmp_array) {
    print CONDENSE $_;
  }
  
  close CONDENSE;
  @tmp_array = ();

} # next file

print "\ndone!\n";