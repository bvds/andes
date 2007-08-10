#!/usr/bin/perl -w
#  
#  This is a modified version of onelog.pl that looks at errors.  
#
#  We catagorize errors by multiple choice, equation entries, 
#  and quantity entry (everything else).
#  We are interested in rate of correct response, and for incorrect
#  response, do they self-correct (no intervening hints
#
#  This analysis does not re
#

my $last_header="";  #first line of the most recent Andes session.

my $all_correct=0;

while (<>) { # loop over andes sessions
  
  # beginning of new file
  if (/^time,info/) {$last_header="";}
  # find (and discard) database header
  next unless /^.* Log of Andes session begun/;  
  
  # Test that sessions have been sorted by date
  # If the beginning of the file was not marked, could have a
  # false error.
  # use sort-by-time.pl to create a sorted version of the log file.
  $last_header le $_ or die "Sessions in log file are not sorted.\n";
  $last_header = $_;
  
  my $last_time=0;  #timestamp (seconds) of most recent line in session
  my $score=0;
  my $loss_of_focus=0;  #accumulate pauses associated with loss of focus
  my $error_interp=0;
  my $intervening_hints=0;
  my $last_adjusted_time=0;
  my $check_entries=0;
  
  while (<>) {   # loop over lines within an Andes session
    
    # skip evaluation of any pre-defined quantities/equations
    if (/\tCheck-Entries (\d)/) {$check_entries=$1;}
    next if $check_entries;
    
    if(/^(\d+):(\d+)\t/ or /^(\d+):(\d+):(\d+)\t/) {
      # total time in seconds
      $this_time = $3 ? $1*3600+$2*60+$3 : $1*60+$2; 
      $dt = $this_time - $last_time;
      $last_time = $this_time;
      
      if (/\tApp-activate/ or /\tApp-deactivate/) {
	$loss_of_focus += $dt;
      }
    } else {
      # silently ignore header lines
      warn "Time stamp missing for $_\n" unless /^#/;
      next;
    }
    
    last if /\tEND-LOG/;  # end of Andes session
    
    next unless /\tDDE/;  # skip non DDE lines
    # reset operator & step lists and record time
    if (/\tDDE /) { # student action sent to help system
      $#step_list = -1;
      $#operator_list = -1;
      $error_name = 0;  #delete any old error names
    }
    if (/read-student-info .(\w+)/) {
      $student = $1;  # session label should start with student id
    }
    elsif (/set-session-id .(\w+)-([a-zA-Z0-9-]+)/) {
      $session_userid = $1;
    }
    elsif (/read-problem-info .(\w+)/) {
      $problem = $1;
      $problem  =~ tr/A-Z/a-z/;  #set to lower case
    }
    elsif (/\tDDE-COMMAND set-score (\d+)/) {
      $score = $1; 
    }
    # use ^\r so we don't include CR in names
    elsif (/\tDDE-COMMAND assoc step ([^\r]+)/) {
      @step_list = split /,/,$1;
    }
    elsif (/\tDDE-COMMAND assoc op ([^\r]+)/) {
      @operator_list = split /,/,$1;
    }
    elsif (/\tDDE-COMMAND assoc error ([^\r]+)/) {
      $error_name = $1;
    }
    # This way of doing things does not address the case where 
    # a student tries one thing, fails, and then does (successfully)
    # something else.
    # Sometimes Andes has a guess for the associated operator, but
    # it is not very reliable.
    elsif (/\tDDE-RESULT \|NIL\|/ or /\tDDE-RESULT \|NIL!show-hint .*\|/) {
      #if previous entry was also an error, record as uncorrected.
      if ($error_interp) {
	$dt = $this_time-$error_time;
	push @{$all_not_corrected{$intervening_hints}}, $dt;
	foreach $operator (@error_op or @operator_list or 
			   ("unknown-operator")) {
	  push @{$not_corrected{$error_interp}{$operator}
		  {$intervening_hints}}, $dt;
	}
      }
      if (/\tDDE-RESULT \|NIL\|/ ) {
	$error_interp = ($error_name or "assoc-error");
	$intervening_hints = 0;
      } else {
	$error_interp = ($error_name or "assoc-error-with-hint");
	$intervening_hints = 1;
      }
      $error_time = $this_time;
      @error_op = @operator_list;
    }
    elsif (/\tDDE-RESULT \|!show-hint .*\|/) {
      $intervening_hints++;
    }
    # Correct with unsolicited hint arises when a student does not 
    # express a vector equation in terms of its components or 
    # combines equations pre-maturely.  Many students go and do something 
    # else, some ignore the hint, and a few try to fix the equation. 
    #
    # We count any previous incorrect entry as being corrected.
    #
    # Helpsystem returns a "green" for this entry, now we record
    # correct for this entry and self-corrected for any
    # preceeding incorrect entry. 
    elsif (/\tDDE-RESULT \|T\|/ or /\tDDE-RESULT \|T!show-hint .*\|/) { 
      if (@step_list) {$all_correct++;}  #don't spread over multiple operators
      # Sanity test:  these should have the same length
      @operator_list == @step_list or
	die "assoc op and assoc step don't match\n";
      
      while (@operator_list) {
	my $operator = pop @operator_list;
	my $step = pop @step_list;
	# skip implicit eqn's unless associated operator is 
	# WRITE-IMPLICIT-EQN.  In a few cases, it is the only
	# operator (and is redundant with a previous step) in
	# which case, we associate it with that operator
	next if @step_list > 1 and $step =~ /^\(IMPLICIT-EQN / 
	    and $operator !~ /^WRITE-IMPLICIT-EQN/;
	$correct{$operator} += 1;
      }
      # If previous step was an error, record as self-corrected.
      if($error_interp) {
	# if previous step was wrong, and no entry was determined,
	# assume they were trying to do this entry.
	$dt = $this_time - $error_time;
	push @{$all_eventually_corrected{$intervening_hints}}, $dt;
	foreach $operator (@error_op or @operator_list) {
	  push @{$eventually_corrected{$error_interp}{$operator}
	    {$intervening_hints}}, $dt;
	}
	$error_interp=0;  #reset flag
      }
    }
  }
  
  unless ($_) {
    warn "End of file encountered during Andes session\n";
    last;
  }
  # next unless $time_used and $final_score and $student and $problem;
  if ($student ne $session_userid) {
    warn "warning: session label $session_userid doesn't match $student\n";
  }
}

#############################################################################
#
#                     Analysis of accumulated data
#
#############################################################################


# Print out times, aggregating over operators and error interpretations

if (1) {
  local $"=",";  # for Mathematica formatted lists
  print "correct=$all_correct;\n";

  foreach $nhint (keys %all_eventually_corrected) {
    foreach $t (@{$all_eventually_corrected{$nhint}}) {
      push @{$time_histogram{$nhint}}, $t;
    }
  }
  print "eventually={";
  my $sep="";
  foreach $nhint (sort {$a <=> $b} keys %time_histogram) {
    print "${sep}{$nhint,{@{$time_histogram{$nhint}}}}";
    $sep=",";
  }
  print "};\n";
  
  foreach $nhint (keys %all_not_corrected) {
    foreach $t (@{$all_not_corrected{$nhint}}) {
      push @{$not_histogram{$nhint}}, $t;
    }
  }
  print "notcorrected={";
  $sep="";
  foreach $nhint (sort {$a <=> $b} keys %not_histogram) {
    print "${sep}{$nhint,{@{$not_histogram{$nhint}}}}";
    $sep=",";
  }
  print "};\n";
  
}

# For each problem solution, print problem times and a list of operators
# applied.  This can be used to construct a linear model.
# Include student cutoff, score cut-off, but not problem cut-off.
# Express in Mathematica format as a list containing each 
# problem solution.  Each member of the list is of the format:
#  {student, problem, time, {{op1,n1},{op2,n2}, ...}}
# where ni is the number of distinct instances of opi in that solution.
if (0) {
    local $"=",";  # for Mathematica formatted lists
    my @each_student=();
    my @each_time=();
    my @op_list=sort keys %correct;
    my @op_quoted = map {"\"" . $_ . "\""} @op_list;
    #my @error_list=union( (keys %correct) (keys %not_corrected));
    #my @error_quoted = map {"\"" . $_ . "\""} @error_list;
    print "operators={@op_quoted};\n";
    print "correct={";
    my $sep="";
    foreach $op (sort keys %correct) {
	print $sep,"$correct{$op}";
	$sep=",";
      }
    print "}\n";

    print "corrected={";
    $sep="";
    foreach $op (sort keys %correct) {
	print $sep,"$correct{$op}";
	$sep=",";
      }
    print "}\n";

    print "operatorinstances={";
    my $count=0;
    my $stprob=1;  # Mathematica style indexing
    # %times includes the cut-off on the list of students
    foreach $student (sort keys %times) {
	# no cut-off on problems
	foreach $problem (sort keys %{$op_inst{$student}}) {
	    if ($op_inst{$student}{$problem} and
		$times{$student}{$problem}) {
		push @each_student, $student;
		push @each_time, $times{$student}{$problem};
		if($count++){ print ",\n";}		
		print "{";
		if (1) {
		    # sparse version
		    my $count2=0;
		    foreach $op (sort keys %{$op_inst{$student}{$problem}}) {
			my $inst=scalar(keys 
					%{$op_inst{$student}{$problem}{$op}});
			if($count2++){ print ",";}
		    }
		} else {
		    # dense version
		    my @inst_list = map {scalar keys 
					     %{$op_inst{$student}
					       {$problem}{$_}}} @op_list;
		    print "@inst_list";
		}
		print "}";
		$stprob++;
	    }
	}
    }
    print "};\n";
    print "eachstudent={@each_student};\n";
    print "eachtime={@each_time};\n";
}
