#!/usr/bin/perl
#
# ds-student-summary.pl -- extract per-student summary report from
#                          exported Andes dataset
#
# This reads the DataShop's exported tab-delimited text format.
# That is convenient because it is already in transaction form.
# It produces a summary report on a per-student, per-problem basis.

use Time::Local;
%month2num = ( # map 3-letter month names to 0-based month numbers
  "Jan"=>0, "Feb"=>1, "Mar"=>2, "Apr"=>3, "May"=>4, "Jun"=>5, 
  "Jul"=>6, "Aug"=>7, "Sep"=>8, "Oct"=>9, "Nov"=>10, "Dec"=>11); 

# list of per-problem statistic names
# In addition to heading the output columns, the statistic name is 
# also used as the hash key for that entry in a per student 
# per-problem hash. So these are like variable names, if you 
# change the name here, must change it in code below to match.
my @stats = ( "Ordinal", "Sessions", "Time", "To Answer", 
	      "Entries", "Before Answer", "Errors",
	      "Bodies", "Axes", "Vectors", "Variables", "Equations", 
	      "Deletes", "Help Requests", "Solves", "Answers", 
	      "Parts Answered", "Score");
while (<>) {
   next if ($. <= 2); # skip first two header lines 
   chomp();

   # remember last items seen before updating globals. (null on first line OK)
   $prev_time = $time;
   $prev_student = $Student;
   $prev_problem = $Problem;

   # Datashop currently has 29 fields in each row. Follwing names taken from 
   # the datashop column headings, modified in a few places to fit perl syntax.
   # Note all variables with capitalized names refer to fields on the most 
   # recently read line.
($Student, $Session_Id, $Time, $Time_Zone, $Student_Response_Type, $Student_Response_Subtype, 
 $Tutor_Response_Type, $Tutor_Response_Subtype, $Module, $Group, $Problem, $Step, $Attempt_At_Step, 
 $Outcome, $Selection, $Action, $Input, $Feedback_Text, $Feedback_Classification, $Help_Level, $Total_Hints, 
 $Condition_Name, $Condition_Type, $KC, $KC_Category, $School, $Class, $Score, $Parse)
     = split ('\t', $_, -1); # -1 preserves trailing blank fields

   # $Time field is ISO 8601 string as YYYY-MM-DD HH:MM:SS.0
   # lower-case $time is current event's time in seconds from the epoch. 
   ($t_year, $t_month, $t_mday, $t_hour, $t_min, $t_sec, $t_tenth) = split(/-| |:|\./, $Time);
   $time = timelocal($t_sec, $t_min, $t_hour, $t_mday, $t_month - 1, $t_year-1900);

   # detect end of one problem session and beginning of new one
   if ($Session_Id ne $current_session_id) {
           # First finish up current session, if any:
	   # add current session time based on remembered last entry time
	   if ($current_session_id) {
	   	$data{$prev_student}{$prev_problem}{"Time"} 
		 	+= ($prev_time - $session_start_time);
	   }
           #
	   # Now start new session
	   #
	   $current_session_id = $Session_Id;
           $data{$Student}{$Problem}{"Sessions"} += 1;
	   # if this is a new problem for this student, record its ordinal for 
	   # student. If so, we just added new problem key to hold "Sessions" 
	   # count above, so ordinal = number of keys.
           if (! ($data{$Student}{$Problem}{"Ordinal"})) {
                   $data{$Student}{$Problem}{"Ordinal"} =
		      (keys %{$data{$Student}}); 
	   }
	   # extract session start time from session id, since not marked in 
	   # transactions
	   ($session_user, $monthdate, $hours, $min, $sec) = split('-', $Session_Id);
           ($monthabbr, $mday) = ($monthdate =~ m/([A-Z][a-z]+)([\d]+)/); 
	   $session_start_time = timelocal($sec,$min,$hours,$mday,$month2num{$monthabbr},$t_year-1900); 
   }

   # just ignore blank answer submissions = clears. 
   # NB: don't test for non-blankness with if ($Input), value may be '0'
   next if ($Student_Response_Subtype eq 'Answer' && $Input eq '');

   # handle entries, recorded as "ATTEMPTS" with subtype
   if ($Student_Response_Type eq 'ATTEMPT') 
   {
	   # For any entry: update count of entries and errors 
	   $data{$Student}{$Problem}{"Entries"} += 1;
	   if ($Outcome eq 'INCORRECT') 
	           { $data{$Student}{$Problem}{"Errors"} += 1; }

	   # special processing for Answer entries
	   if ($Student_Response_Subtype eq 'Answer') 
	   {
	      $data{$Student}{$Problem}{"Answers"} += 1; 
	      if ($Outcome eq "CORRECT") {
		if (! $data{$Student}{$Problem}{"Saw-Correct-Answer"}) {
	     	# remember time to first correct answer = any time accumulated 
		# in prior sessions plus time to this event in current session
		$data{$Student}{$Problem}{"To Answer"} = 
	             $data{$Student}{$Problem}{"Time"} + $time - $session_start_time;

		# don't do this again for this (Student, Problem)
		$data{$Student}{$Problem}{"Saw-Correct-Answer"} = 1;
	      }

	      # update set of correct answers seen, for completion checking. 
	      # This is a hash whose keys are ids "Answer-1", "Answer-2"
	      $data{$Student}{$Problem}{"Correct-Answers"}{$Selection} = 1;
	     }else { # incorrect answer 
	      # delete it from the Correct-Answer set for this problem
	      delete $data{$Student}{$Problem}{"Correct-Answers"}{$Selection};
	     }
	     # update current count of distinct correct answers
	     $data{$Student}{$Problem}{"Parts Answered"} =  
	            keys %{$data{$Student}{$Problem}{"Correct-Answers"}};
           } 
	   else # non-Answer entry: 
	   { 
		# count work entries made before first correct answer
	        if (! $data{$Student}{$Problem}{"Saw-Correct-Answer"}) {
		      $data{$Student}{$Problem}{"Before Answer"} += 1;
	        }
		# count subtypes
	        if ($Student_Response_Subtype eq 'Body') 
	   		{ $data{$Student}{$Problem}{"Bodies"} += 1; }
	   	if ($Student_Response_Subtype eq 'Axes') 
	     		{  $data{$Student}{$Problem}{"Axes"} += 1; }
	   	if ($Student_Response_Subtype eq 'Vector') 
	     		{  $data{$Student}{$Problem}{"Vectors"} += 1; }
	   	if ($Student_Response_Subtype eq 'Variable') 
	     		{  $data{$Student}{$Problem}{"Variables"} += 1; }
	   	if ($Student_Response_Subtype eq 'Equation') 
	     		# !!! might want to ignore empty submissions or treat as deletions
	     		{  $data{$Student}{$Problem}{"Equations"} += 1; }   
	   }
   }
   if ($Student_Response_Type eq 'DELETION') 
          { $data{$Student}{$Problem}{"Deletes"} += 1; }
   if ($Student_Response_Type eq 'CALC_REQUEST') 
          { $data{$Student}{$Problem}{"Solves"} += 1; }
   if ($Student_Response_Type eq 'HINT_REQUEST') {
	   $data{$Student}{$Problem}{"Help Requests"} += 1;
	   # could break out Whats-Wrong, NextStepHelp
   }
   # Score included only when changed. Remember last score recorded
   # !!! here assuming records sorted by time -- should verify! 
   if ($Score) { $data{$Student}{$Problem}{"Score"} = $Score; }
}
   # EOF: finish last session
   if ($current_session_id) {
	$data{$prev_student}{$prev_problem}{"Time"} += $prev_time - $session_start_time;
   }

   # print results. 
   # column headings
   print "Student\tProblem\t";
   foreach $stat (@stats) { print "$stat\t"; }
   print "\n";

   # 1 data row for each student-problem recorded
   foreach $student (sort keys %data) {
     foreach $problem (sort keys %{$data{$student}}) {
	   print "$student\t$problem\t" ;
           foreach $stat (@stats) {
	   	print "$data{$student}{$problem}{$stat}\t";
	   }
	   print "\n";
     }
   }
