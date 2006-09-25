#!/usr/bin/perl
#  Copyright (c) 2006 Robert G.M. Hausmann
#
# This perl script was written as a tool to help experimenters and instructors extract useful information
# from Andes log files. To run the script, you must have perl installed on your computer. If you are running
# MacOS X, perl is already installed. If you are running Windows XP, I recommend installing
# ActivePerl:
#
# http://www.activestate.com/Products/ActivePerl/?psbx=1
#
# The purpose of this script is to extract problem-level information from Andes homework files.
# Several different measures are summarized in a tab-delimited file, including:
# 
# subjid = the participant's id number.
# section = the class section at the usna.
# cond = the experimental condition.
# date = the date the problem was opened.
# time = the time the problem was opened.
# probid = the name of the problem.
# nsh = the number of next step help requests.
# wwh = the number of what's wrong help requests.
# unsol_help = the number of hints that Andes provides for small errors.
# errors = the number of incorrect entries (i.e., entries that turn red).
# corr_ent = the number of incorrect entries (i.e., entries that turn green).
# tot_prob_time = the total number of seconds the problem is open. 
# num_entries = the total number of entries in the interface, including scalars, vectors, and equations.
# suc_sol_entry = the total number of correct entries in the answer box.
# bottom = the total number of bottom-out hints requested (does not distinguish between nsh or wwh boh).
# skill_estimate_score = (SES) found at the bottom-right of the Andes interface.
# 
# The condition and section variables are experiment-specific. Therefore, they are not filled in by 
# this script. Instead, create a table in Excel and use the VLOOKUP command:
#
# http://office.microsoft.com/en-us/assistance/HA010563201033.aspx
#
# To use this script, the <files> variable refers to a text file that holds the name of your files, 
# which can also include directory information. If the directory information is omitted, then the script 
# needs to be run in the same directory as the files.
#
# Usage: perl GetHomeworkScores1.plx files.txt

use Date::Calc qw(Add_Delta_DHMS);
use strict;

# Variables for identification
my $subjid = "null";
my $section = "null";
my $condition = "null";
my $current_problem = "null";
my @close_problem;

# Variables for global performance measures
my $num_of_entries = 0;
my $reduce_count = 0;
my $num_of_correct_entries = 0;
my $num_of_errors = 0;
my $num_of_successful_sol_entries = 0;
my $skill_estimate_score = 0;

# Variables for hints & errors
my $num_of_wwh_requests = 0;
my $num_of_nsh_requests = 0;
my $help_mode;
my $num_of_unsolicited_help = 0;
my $num_of_bottom_out_hints = 0;

# Variables for time
my $open_problem_time;
my $close_problem_time;
my $problem_diff;
my $total_problem_time;
my $problem_date;
my $problem_time;

# Variables for looping through log file
my @filenames;
my $line;
my $buffer;
my @line_array;
my $line_count;
my $i;
my $j;
my @tmp_array;

# Variables for parsing time
my $size;
my $seconds;
my $minutes;
my $hours;
my $tot_seconds;
my @parsed_time;

# Open the list of files and build an array from them.
open (FILENAMES, @ARGV[0]) || die ("Could not open file: $!");
while (<FILENAMES>) { chomp; push @filenames, $_; }

# Open a file to store the scores.
my $file = "GetHomeworkScores4.xls";
open(INFO, ">>$file");

# Print a header in the first row in Excel.
print INFO "subjid\tsection\tcond\tdate\ttime\tprobid\tnsh\twwh\tunsol_help\terrors\tcorr_ent\ttot_prob_time\tnum_entries\tsuc_sol_entry\tbottom\tses\n";

# Loop through array of file names, putting their contents into one large array.
foreach (@filenames) {
  open FILES, $_;
  print "Open file: $_ ";
  @line_array = <FILES>;
  $line_count = @line_array;
  print "Number of lines: $line_count\n";

  # Loop through each line of the log file and generate counts for each variable.
  foreach my $line (@line_array) {

    # Establish when the problem was opened.
    if ($line =~ /Open-problem/i) {
      my @tmp_array = split (/\t| /, $line_array[$i]);
      $open_problem_time = convert_sec(@tmp_array[0]);
    }

    # Get the name of the problem.
    if ($line =~ /read-problem-info/i) {
      my @tmp_array = split (/"/, $line);
      $current_problem = @tmp_array[1];
    }

    # Get the id of the student.
    if ($line =~ /read-student-info/i) {
      my @tmp_array = split (/"/, $line);
      $subjid = @tmp_array[1];
    }

    # Get Subject ID, the date, and the time of the problem.
    if ($line =~ /Log of Andes session/) {
      my @tmp_array = split (/ /, $line);
      #$subjid = $tmp_array[12];
      $problem_date = $tmp_array[7]. " " . $tmp_array[8] . " " . $tmp_array[9];
      $problem_time = $tmp_array[10];
    }

    # Count the number of Next Step Help requests.
    if ($line =~ /DDE \(Get-Proc-Help\)/) {
      $help_mode = "NSH";
      $num_of_nsh_requests++;
    }

    # Count the number of What's Wrong Help requests.
    if ($line =~ /DDE \(Why-wrong/) {
      $help_mode = "WWH";
      $num_of_wwh_requests++;
    }

    # Count the number of unsolicited hints provided by Andes.
    if ($line =~ /DDE-RESULT \|NIL!show-hint|DDE-RESULT \|T!show-hint/) {
      #print "$num_of_unsolicited_help\t$current_problem \t$line";
      $help_mode = "USH";
      $num_of_unsolicited_help++;
    }

    # Update the count if the student requests a further explanation.
    if ($line =~ /DDE \(Explain-More\)/) {
      if ($help_mode =~ /NSH/) {
        $num_of_nsh_requests++;
      }
      if ($help_mode =~ /WWH/) {
        $num_of_wwh_requests++;
      }
      if ($help_mode =~ /USH/) {
        $num_of_unsolicited_help++;
      }
    }

    # Count the number of errors made by the student.
    if ($line =~ /DDE-RESULT \|NIL/) {
      $num_of_errors++;
    }
    # Handle cases when the help system is unable to reply.
    if (($line =~ /handle-student-response/) & ($line_array[$i+1] =~ /DDE-RESULT \|NIL\|/)) {
      $num_of_errors--;
    }

    # Count the number of correct entries.
    if ($line =~ /DDE-RESULT \|T/) {
      $num_of_correct_entries++;
      #print "$num_of_correct_entries\t$current_problem \t$line";
    }
    # Handle Andes' housekeeping procedures.
    if ($line =~ /read-problem-info|DDE-COMMAND set-score 0/) {
      $num_of_correct_entries--;
    }
    # Update entries, errors, and unsolicited help in cases where the student reopens the problem.
    if ($line =~ /DDE-POST \(Check-Entries T\)/) {
      my ($reduce_true_count, $reduce_false_count, $reduce_showhint_count) = &count_previous_entries($i);

      $num_of_correct_entries = $num_of_correct_entries - $reduce_true_count;
      $num_of_errors = $num_of_errors - $reduce_false_count;
      $num_of_unsolicited_help = $num_of_unsolicited_help - $reduce_showhint_count;
    }

    # Count the number of entries, including modifications to existing objects.
    if ($line =~ /Edit-props|Select-tool|Modify-Variable|New-Variable|EQ-SUBMIT|Begin-Resize|Ans-submit/) {
      #print "$num_of_entries\t$current_problem \t$line";
      my $interrupt = "true";
      $interrupt = &transaction_interrupt($i);
      if ($interrupt =~ /false/) {
        $num_of_entries++;
      }
    }

    # Count the number of corrent entries in the answer field.
    if (($line =~ /Ans-exit/) & ($line_array[$i+1] =~ /DDE-RESULT \|T\|/)) {
      $num_of_successful_sol_entries++;
    }

    # For NSH, bottom-out hints are labeled as such.
    if ($line =~ /BOTTOM-OUT/) {
      $num_of_bottom_out_hints++;
    }

    # For WWH, the bottom-out hints are not labeled.
    if ( ($help_mode =~ /WWH/) && ($line =~ /~\|/) ) {
      $num_of_bottom_out_hints++;
    }

    # Get the current value of the skill estimate score (SES).
    if ($line =~ /DDE-COMMAND set-score/) {
      my @tmp_array = split (/set-score/, $line);
      $skill_estimate_score = @tmp_array[1];
      chomp $skill_estimate_score;
    }

    # Print the counts from the current problem.
    if ($line_array[$i] =~ /close-problem/i) {

      # Establish when the problem was closed.
      @close_problem = split (/\t| /, $line_array[$i]);
      $close_problem_time = convert_sec(@close_problem[0]);
      $problem_diff = $close_problem_time - $open_problem_time;
      $total_problem_time =+ $problem_diff;
  
      # Print variables for identification to file.
      print INFO "$subjid\t";
      print INFO "$section\t";
      print INFO "$condition\t";
      print INFO "$problem_date\t";
      print INFO "$problem_time\t";
      print INFO "$current_problem\t";
  
      # Print global performance measures to file.
      print INFO "$num_of_nsh_requests\t";
      print INFO "$num_of_wwh_requests\t";
      print INFO "$num_of_unsolicited_help\t";
      print INFO "$num_of_errors\t";
      print INFO "$num_of_correct_entries\t";
      print INFO "$problem_diff\t";
      print INFO "$num_of_entries\t";
      print INFO "$num_of_successful_sol_entries\t";
      print INFO "$num_of_bottom_out_hints\t";
      print INFO "$skill_estimate_score\n";
  
      # Reset scores for next problem.
      $num_of_nsh_requests = 0;
      $num_of_wwh_requests = 0;
      $num_of_unsolicited_help = 0;
      $num_of_errors = 0;
      $num_of_correct_entries = 0;
      $num_of_entries = 0;
      $num_of_successful_sol_entries = 0;
      $num_of_bottom_out_hints = 0;
      $skill_estimate_score = 0;

      $open_problem_time = 0;
      $close_problem_time = 0;
      $problem_diff = 0;
      $total_problem_time = 0;

    }
  
    $i++;

  } # next line

  $i=0;
  $j=0;
  $line_count = 0;
  @line_array = ();
  $num_of_correct_entries = 0;
  my $subjid;
  my $condition;
  close(FILES);

} # next file

sub count_previous_entries {
  # The purpose of this subroutine is to count the number of 
  # correct, incorrect, and entries with hints attached for
  # problems that have been previously opened.

  my @temp_array = @line_array;
  my $j = @_[0];
  my $stop_line = @temp_array;
  my $count_true_entries = 0;
  my $count_false_entries = 0;
  my $count_showhint_entries = 0;

  foreach ($j; $j < $stop_line; $j++) {
    if ($temp_array[$j] =~ /DDE-RESULT \|T/) {
      $count_true_entries++;
    }
    if ($temp_array[$j] =~ /DDE-RESULT \|NIL/) {
      $count_false_entries++;
    }
    if ($temp_array[$j] =~ /show-hint/) {
      $count_showhint_entries++;
    }
    if ($temp_array[$j] =~ /DDE-POST \(Check-Entries NIL/) {
      return ($count_true_entries, $count_false_entries, $count_showhint_entries);
    }
  }
}

sub transaction_interrupt {
  # The purpose of this subroutine is to catch occurances where students
  # interrupt themselves while making an entry. This is particularly true
  # when drawing in the FBD.

  my @temp_array = @line_array;
  my $j = @_[0] + 1;
  my $stop_line = @temp_array;
  my $interrupt = "true";

  interrupt: foreach ($j; $j < $stop_line; $j++) {
    if ($temp_array[$j] =~ /DDE-RESULT \|T|DDE-RESULT \|NIL/) {
      #print "feedback found $temp_array[$j]";
      return $interrupt = "false";
      last interrupt;
    }
    if ($temp_array[$j] =~ /Edit-props|Select-tool|Modify-Variable|New-Variable|EQ-SUBMIT|Begin-Resize|Ans-submit|Get-Proc-Help|Why-wrong/) {
      #print "interrupt $temp_array[$j]";
      return $interrupt = "true";
      last interrupt;
    }
  }
}

sub convert_sec {
  my $time = shift(@_);
  $tot_seconds = (ss($time)) + (mm($time)*60) + (hh($time)*3600);
  return $tot_seconds;
}

sub ss {
  my $time = shift(@_);
  @parsed_time = split (/:/, $time);
  $size = @parsed_time;
  if ($size == 2) {
    $seconds = $parsed_time[1];
    return $seconds;
  }
  else {
    $seconds = $parsed_time[2];
    return $seconds;
  }
}

sub mm {
  my $time = shift(@_);
  @parsed_time = split (/:/, $time);
  $size = @parsed_time;
  if ($size == 2) {
    $minutes = $parsed_time[0];
    return $minutes;
  }
  else {
    $minutes = $parsed_time[1];
    return $minutes;
  }
}

sub hh {
  my $time = shift(@_);
  @parsed_time = split (/:/, $time);
  $size = @parsed_time;
  if ($size == 2) {
    $hours = 0;
    return $hours;
  }
  else {
    $hours = $parsed_time[0];
    return $hours;
  }
}

close INFO;