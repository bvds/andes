#!/usr/bin/perl
#  Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to filter Andes log files into an Excel-readable format. 
# The program can be run from any location, but the "files.txt" should contain the complete
# path and file name. Use ">" to output the print commands to an output file (i.e., "output.xls").
#
# usage: perl GetKCScores.plx files.txt>output.xls

# Open the list of files and build an array from them.
open (FILENAMES, @ARGV[0]) || die ("Could not open file: $!");
while (<FILENAMES>) { chomp; push @filenames, $_; }

# Open a file to store the scores.
#my $file = "GetKCScores2.xls";
#open(INFO, ">>$file");

# Print a header in the first row in Excel.
#print INFO "subjid\tsection\tcond\tdate\ttime\tprobid\tKC\tscore\n";
print "Transaction\tsubjid\tprobid\tdate\ttime\tline_number\tscore\tKC\tIndex\tAction\n";

# Loop through array of file names, putting their contents into one large array.
foreach (@filenames) {
  open FILES, $_;
  #print "Open file: $_ ";
  @line_array = <FILES>;
  $line_count = @line_array;
  #print "Number of lines: $line_count\n";

  my $curr_line = 1;
  my $current_KC = "null-kc";
  my $current_problem = "null-probid";
  my $transaction_number = 0;

  # Loop through each line of the log file and generate counts for each variable.
  foreach my $line (@line_array) {

    ($time0, $rest) = split (/\t/, $line, 2);

    # Get the name of the problem.
    if ($line =~ /read-problem-info\s"(.+)"/) {
      $current_problem = $1;
    }

    # Get the name of the student.
    if ($line =~ /read-student-info\s"(.+)"/) {
      $subjid = ($1);
      #$subjid = deidentify($1);
    }

    # Get the date of the solution.
    if ($line =~ /Log\sof\sAndes\ssession\sbegun\s(.+?),\s(.+?),\s(.+?)\s/) {
      $date = $2 . ", " . $3;
    }

    # Find a help-request transaction.
    if ($line =~ /Why-wrong-equation|Why-wrong-object|NSH\s|Explain-More|DDE-RESULT \|NIL!|DDE-RESULT \|T!|handle-student-response/) {

      $transaction_number++;

      # Get the type of help being requested.
      $help = &get_Help_Type($line, $curr_line);

      # Give the transaction a score: 0 = help request; 1 = correct; -1 = incorrect
      $KC_score = &score_updater($curr_line);

      print "$transaction_number\t$subjid\t$current_problem\t$date\t$time0\t$curr_line\t$KC_score\t$KC_step\t$help\n";
      
    }

    # Find a sol-entry transaction.
    if ( ($line =~ /DDE \(/) && ($line !~ /read-student-info|read-problem-info|get-stats persist|close-problem|Get-Proc-Help|Why-wrong-equation|Why-wrong-object|Explain-More|handle-student-response/) ) {

      $transaction_number++;

      # Give the transaction a score: 0 = help request; 1 = correct; -1 = incorrect
      $KC_score = &score_updater($curr_line);

      # Get the step associated with transaction, if it can be found.
      $KC_step = &get_step($curr_line);

      # Save Andes's index of the entry.
      $entry_name = $current_problem . "-" . &get_Entry_Name($line, $curr_line);

      print "$transaction_number\t$subjid\t$current_problem\t$date\t$time0\t$curr_line\t$KC_score\t$KC_step\t$entry_name\t$rest";

      # GOAL: Use entry name for entry array to keep track of statistics on each transaction: help request + error
    }

    # Find other transactions, such as looking at the "Review Andes Equations" menu.

    $curr_line++;

  } #next line within a log

} #next log file

sub get_NSH_step {
  # Takes one argument: present line
  # Returns the next step defined by Andes.
  # Returns "NSH-KC-not-found" if feedback is given before step is found.

  my $j = $_[0];
  my @tmp_array = @line_array;
  my $max_size = @line_array;
  my $op = "NSH-KC-not-found";
  my $input_bool = 0;

  for (my $count = $j; $count<$max_size; $count++) {
    if ( ($tmp_array[$j] =~ /DDE \(/) && ($tmp_array[$j] !~ /DDE \(handle-student-response/) ) {
      $input_bool = 1;
    }

    if ( ($tmp_array[$j] =~ /DDE-COMMAND assoc op (.+)/) & ($input_bool == 1) ) {
      # DDE-COMMAND assoc op DRAW-BODY
      $op = $1;
      return $op;
    }

    if ( ($tmp_array[$j] =~ /DDE-RESULT \|T\||DDE-RESULT \|NIL/) & ($input_bool == 1) ) {
      return $op;
    }
    $j++;
  }
} # end sub &get_NSH_step()

sub get_Help_Type {
  my $get_Help_Type = $_[0];
  my $curr_line_num = $_[1];

  ($time1, $rest) = split (/\t/, $get_Help_Type, 2);
  chomp $rest;
  
  if ($get_Help_Type =~ /Explain-More/ ) {
    # DDE (Explain-More)
    if ($help_object !~ /Explain-More/ ) {
      $help_object = $help_object . "\t" . $rest;
    }
    else { $help_object }
  }

  # Tag bottom-out hints.
  if ($get_Help_Type =~ /DDE-RESULT \|!show-hint(.+)\~\|/ ) {
    # DDE-RESULT |NIL!show-hint Units are inconsistent.~| => unsolicited
    # DDE-RESULT |!show-hint Units are inconsistent.~|    => WWH/NSH

    $hint_type = "BOH-";

    if ($help_object =~ /BOH-/ ) {
      $help_object = substr ($help_object, 0, 4);
    }
    else {
     $help_object = $hint_type . $help_object . "\t" . $rest;
    }
  }

  if ($get_Help_Type =~ /Why-wrong-equation\s(.+)\)/ ) {
    # DDE (Why-wrong-equation 0)
    $hint_type = "WWH";
    $help_object = "$hint_type-EQN-$1";
  }
  
  if ($get_Help_Type =~ /Why-wrong-object\s(.+)\s(.+)-(\d|\d\d)\)/ ) {
    # DDE (Why-wrong-object Ft Vector-18)
    $hint_type = "WWH";
    $help_object = "$hint_type-$2-$3";
  }
  
  if ($get_Help_Type =~ /DDE-FAILED \(Why-wrong/ ) {
    # DDE-FAILED (Why-wrong-equation 0)
    $help_object = $help_object . "\t" . $rest;
  }

  if ($get_Help_Type =~ /DDE-COMMAND assoc \(NSH\s(.+?)(\)|\s)(.+)/ ) {
    # DDE-COMMAND assoc (NSH CONTINUE-SOLUTIONS)
    $hint_type = "NSH";
    $help_object = "$hint_type-$1";
    $NSH_type = $3;

    # Get the next step the student is attempting.
    $KC_step = &get_NSH_step($curr_line_num);
  }

  if ($get_Help_Type =~ /handle-student-response/ ) {
    $help_object = $help_object . "\t" . $rest;
  }
  else { $help_object }

  if ($get_Help_Type =~ /DDE-RESULT \|NIL!/ ) {
    # DDE-RESULT |NIL!show-hint Units are inconsistent.~|
    $hint_type = "Unsol";
    $help_object = "$hint_type-Incorrect-$current_KC";
  }

  if ($get_Help_Type =~ /DDE-RESULT \|T!/ ) {
    # DDE-RESULT |NIL!show-hint Units are inconsistent.~|
    $hint_type = "Unsol";
    $help_object = "$hint_type-Incomplete-$current_KC";
  }

  return $help_object;

} # end sub &get_Help_Type()

sub get_Entry_Name {
  # Takes two args: 1-the contents of the current line; 2-the current line number
  my $get_Input_Type = $_[0];
  my $input_Name = "unknown entry";

  if ($get_Input_Type !~ /handle-student-response/) {

    ($time, $line) = split (/\t/, $get_Input_Type, 2);

    if ($get_Input_Type =~ /assert-x-axis/) {
      my @temp_array2 = split (/\sAxes-(\d|\d\d)\s"/, $get_Input_Type);
      $input_Name = "Axis-" . $temp_array2[1];
    }

    if ($get_Input_Type =~ /assert-object/) {
      my @temp_array2 = split (/\|\sSystem-(\d|\d\d)\)/, $get_Input_Type);
      $input_Name = "Body-" . $temp_array2[1];
    }

    if ($get_Input_Type =~ /lookup-force|lookup-vector/) {
      my @temp_array2 = split (/\|\sVector-(\d|\d\d)\)/, $get_Input_Type);
      $input_Name = "Vector-" . $temp_array2[1];
    }
  
    if ($get_Input_Type =~ /define-variable/) {
      my @temp_array2 = split (/\|\sVar-(\d|\d\d)/, $get_Input_Type);
      $input_Name = "Var-" . $temp_array2[1];
    }
  
    if ($get_Input_Type =~ /lookup-eqn-string/) {
      my @temp_array2 = split (/"\s(\d|\d\d)\)/, $get_Input_Type);
      $input_Name = "EQN-" . $temp_array2[1];
    }

    if ($get_Input_Type =~ /DDE\s\(solve-for-var\s"(.+)"\s(\d|\d\d)/) {
      # DDE (solve-for-var "F" 2)
      $input_Name = "SOL-$2";
    }

    if ($get_Input_Type =~ /DDE\s\(check-answer\s"(.+)"\sAnswer-(\d|\d\d)/) {
      # DDE (check-answer "205 N" Answer-1)
      $input_Name = "ANS-$2";
    }

  }

  $current_KC = $input_Name;

} # end sub &get_Entry_Name()

sub get_step {
  # Takes one arg: the current line number
  my $j = $_[0];
  my $step_name = "No-Andes-interpretation-step";
  my $op_name = "No-Andes-interpretation-op";
  my @tmp_array = @line_array;
  my $max_size = @line_array;

  step: for (my $count = $j; $count<$max_size; $count++) {

    # Handle action interrupts
    if ($tmp_array[$j] =~ /DDE \(/) {
      return $op_name;
    }

    if ($tmp_array[$j-1] =~ /solve-for-var/) {
      return $op_name = "SOLVE-FOR-VAR";
    }

    if ($tmp_array[$j-1] =~ /check-answer/) {
      return $op_name = "CHECK-ANSWER";
    }

    # Return op if found before next entry.
    if ($tmp_array[$j] =~ /DDE-COMMAND assoc op (.+)/) {
      # DDE-COMMAND assoc op DRAW-BODY
      $op_name = $1;
      chomp $op_name;
      return $op_name;
    }

    $j++;
  }
} # end sub &get_step()

sub score_updater {
  # Takes one arg: the current line
  my $j = $_[0];
  my @tmp_array = @line_array;
  my $max_size = @line_array;

  rightwrong: for (my $count = $j; $count<$max_size; $count++) {
    # Handle action interrupts
    if ($tmp_array[$j] =~ /DDE \(/) {
      return 0;
      last rightwrong;
    }

    # Handle if "assoc error" comes before "assoc step"
    if ($tmp_array[$j] =~ /assoc error|DDE-RESULT \|NIL/)  {
      return -1;
      last rightwrong;
    }
    if ($tmp_array[$j] =~ /DDE-RESULT \|T/) {
      return +1;
      last rightwrong;
    }
    $j++;
  }
} # end sub &score_updater()

sub deidentify {
  $char_count = 0;
  @alphabet = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K');

  for (split //, $_[0]) {
    if ($char_count == 0 ) { $alpha_char[0] = "" }
    if ($char_count == 1 ) { $alpha_char[1] = "$alphabet[0]" }
    if ($char_count == 2 & $_ == 7) { $alpha_char[2] = "$alphabet[0]" }
    if ($char_count == 2 & $_ == 8) { $alpha_char[2] = "$alphabet[1]" }
    if ($char_count == 2 & $_ == 9) { $alpha_char[2] = "$alphabet[2]" }
    if ($char_count == 3 || $char_count == 4 || $char_count == 5 || $char_count == 6) {
      $long_num = $_ + 2;
      $num_length = length($long_num) - 1;
      $short_num = substr($long_num, $num_length, 1);
      $alpha_char[$char_count] = $short_num;
    }
    $char_count++;
  }

  # Combine the modified characters into a single string.
  $new_code = $alpha_char[1].$alpha_char[2].$alpha_char[3].$alpha_char[4].$alpha_char[5].$alpha_char[6];
  return $new_code;
} # end sub &deidentify()