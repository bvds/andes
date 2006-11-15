#!/usr/bin/perl -w
#
# onelog --- analyze a concatonated sequence of Andes log files
# as retrieved from OLI database query
#
# Usage:     onelog.pl access.csv
#
# OLI's data extraction tool returns the logs as database records in comma 
# separated values form. Which columns are included depends on the query.
# The entire contents of each andes session log file will be included as 
# the last column in one of these database records.  The log file text itself 
# includes CRLFs delimiting lines within it.
#
# So if we asked for date and info fields from the database, the
# combined log file will start with a header column line:
#
# 	time,info
# 
# followed by record lines which look like this:
#
#    	2006-05-04 11:30:04.0,# Log of Andes session begun Thursday, May 04, 2006 11:11:46 by m094530 on 09MOORE-CJ30A
# 	# Version 1
# 	# Help-flags 01f Procedural Conceptual Example
# 	0:00	Andes-Version 10.1.3
# 	0:00	FBD-Version 04 05 06^M
#         ... rest of log1 ...
# 	18:19	END-LOG 
#	
# 	2006-05-04 11:31:34.0,# Log of Andes session begun Thursday, May 04, 2006 11:30:13 by m094530 on 09MOORE-CJ30A
#         ... rest of log2 ...
#        4:40 END-LOG
#
# We pull out contents between the Andes header line and the END-LOG line.

#
#  Impose cutoffs on accepted data points.
#
$score_cut_off=10;  # minimum score to count a problem as attempted
# doesn't contribute
$minimum_problem_attempts=0.5;   # cutoff on list of students
# there is a significant number of problems (not assigned)
# that were only attempted by a few students.  Including these
# might introduce a selection bias.
$minimum_student_attempts=20.5;  # cutoff on list of problems


# read in file with meta-operators
# the format of the file is that each line begins with the meta-operator 
# and then all regular operators that it contains 
use Getopt::Long;
&GetOptions("meta=s" => \$meta_operator_file);
if ($meta_operator_file) {
    open(META,$meta_operator_file) or die "Can't open $meta_operators.\n";
    while (chomp($line=<META>)) {
	my @normals = reverse split / /,$line;
        my $meta = pop @normals;
	foreach $normal (@normals) {
	    push @{$meta_operators{$normal}}, $meta;
	}
    }
    close(META);
}

while (<>) { # loop over andes sessions
    #  unless ($ARGV eq $last_ARGV) {
    #    print "Start reading $ARGV\n";
    #    $last_ARGV=$ARGV;
    #  }
    
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

    my $last_time=0;
    my $score=0;
    my $loss_of_focus=0;  #accumulate pauses associated with loss of focus
    my $intervening_errors=0;
    my @error_interp=();
    my $intervening_hints=0;
    my $last_adjusted_time=0;
    my $check_entries=0;
    while (<>) {   # loop over lines in Andes session
	last if /\tEND-LOG/;  # end of Andes session

	# skip evaluation of any pre-defined quantities/equations
	if (/\tCheck-Entries (\d)/) {$check_entries=$1;}
	next if $check_entries;
	
	if(/^(\d+):(\d+)\t/ or /^(\d+):(\d+):(\d+)\t/) {
	    # total time in seconds
	    $this_time = $3 ? $1*3600+$2*60+$3 : $1*60+$2; 
	    $dt = $this_time - $last_time;
	    $last_time = $this_time;
	    
            # in pause histogram, might ignore pauses associated with 
	    # loss of focus:
	    # next if /\tApp-activate/ or /\tApp-deactivate/;  
	    $dt_histogram{$dt}++;
	    if (/\tApp-activate/ or /\tApp-deactivate/) {
		$loss_of_focus += $dt;
	    }
	} else {
            # silently ignore header lines
	    warn "Time stamp missing for $_\n" unless /^#/;
	    next;
	}

	next unless /\tDDE/;  # skip non DDE lines
	# reset operator list and record time
	if (/\tDDE /) { # student action sent to help system
	    $#step_list = -1;
	    $#operator_list = -1;
	    $adjusted_time = $this_time - $loss_of_focus;
	    $error_name = 0;  #delete any old error names
	}
	if (/read-student-info .(\w+)/) {
	    $student = $1;  # session label should start with student id
	}
	elsif (/set-session-id .(\w+)-([a-zA-Z0-9-]+)/) {
	    $session_userid = $1;
	    $date = $2;
	}
	elsif (/read-problem-info .(\w+)/) {
	    $problem = $1;
	    $problem  =~ tr/A-Z/a-z/;  #set to lower case
	}
	elsif (/\tDDE .close-problem/) {
	    $time_used = $this_time;   # calculated above
	    $final_score = $score;     # want most recent score
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
	elsif (/\tDDE-RESULT \|NIL\|/) {
	    $intervening_errors++;
	    push @error_interp,  ($error_name or "assoc-error");
	}
	elsif (/\tDDE-RESULT \|!show-hint .*\|/) {
	    $intervening_hints++;
	}
	# This condition arises from either not expressing a vector
	# equation in terms of its components or from combining equations
	# pre-maturely.  Many students go and do something else, some
        # ignore the hint, and a few try to fix the equation. 
	#
	# Even though the equation turns green, we will treat it 
	# as an error since the student is not given full credit for 
	# the entry.  We will ignore the guesses Andes gives
	# for the operator because the list is often too large
	elsif (1 and /\tDDE-RESULT \|T!show-hint .*\|/) {
	    $intervening_hints++;
	    $intervening_errors++;
	    push @error_interp, ($error_name or "assoc-error-t-show-hint");
	  }
	# error with unsolicited hint.  
	# It is not clear how to count this
	elsif (/\tDDE-RESULT \|NIL!show-hint .*\|/) {
	    $intervening_hints++;
	    $intervening_errors++;
	    push @error_interp, ($error_name or "assoc-error-nil-show-hint");
	}
	elsif (/\tDDE-RESULT \|T\|/ or 
	       (0 and /\tDDE-RESULT \|T!show-hint .*\|/)) { 
	    if (/\tDDE-RESULT \|T!show-hint .*\|/) {
		$intervening_hints++;
		$intervening_errors++;
		push @error_interp, ($error_name or "assoc-error-t-show-hint");
	    }
	    my $facts={errors => $intervening_errors,
		       error_names => [@error_interp],
		       hints => $intervening_hints,
		       time => $adjusted_time-$last_adjusted_time,
                       problem => $problem};

	    unless(@operator_list == @step_list) {
		die "assoc op and assoc step don't match for @operator_list\n";
	    }

	    if (@step_list == 1 and $step_list[0] =~ /\(IMPLICIT-EQN / 
		and ! $operator_list[0] =~ /WRITE-IMPLICIT-EQN/) {
		die "implicit-eqn is only step @step_list and @operator_list\n";
	    }

	    while (@operator_list) {
		my $operator = pop @operator_list;
		my $step = pop @step_list;
		# skip implicit eqn's unless associated operator is 
		# WRITE-IMPLICIT-EQN.  There are always other operators that
		# are associated with the step.
		next if $step =~ /^\(IMPLICIT-EQN / 
		    and $operator !~ /^WRITE-IMPLICIT-EQN/;

		push @{$op_inst{$student}{$problem}{$operator}}, $step;
	        push @{$mastery{$operator}{$student}}, $facts;
		foreach $meta_op (@{$meta_operators{$operator}}) {
	            push @{$mastery{$meta_op}{$student}}, $facts;
	        }
	    }
	    $intervening_errors=0;
	    $intervening_hints=0;
	    $last_adjusted_time = $adjusted_time;
	    @error_interp=();
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

    # Can't do too much analysis here since a problem might
    # be solved over multiple sessions.
    # Accumulate time used, throwing out time where Andes is not in focus.
    $times{$student}{$problem} += $time_used-$loss_of_focus; 
    $scores{$student}{$problem} = $final_score;
    # problems attempted. Don't bother counting here because
    # a problem might be solved over multiple sessions.
    if($final_score > $score_cut_off) {$problems{$problem}=1}; 
    push @{ $sessions{$student}{$problem}}, $date; # accumulate sessions
}

#
#          Analysis of accumulated data
#
#


# print out score histogram in Mathematica notation
# This is not affected by any of the cutoffs
if(0) {
    foreach $student (keys %times) {
	foreach $problem (keys %{$times{$student}}) {
	    $score_histogram{$scores{$student}{$problem}}++;
	}
    }
    print "\nscorehistogram={";
    foreach $score (sort {$a <=> $b} (keys %score_histogram)) {
	print "{$score,$score_histogram{$score}}",$score<100?",":"";
    }
    print "};\n";
}    
 
# printout histogram of pauses
# This is not affected by any of the cutoffs

if(0) {
    print "pausehistogram={";
    $sep="";
    foreach $delay (sort {$a <=> $b} (keys %dt_histogram)) {
	print $sep,"{$delay,$dt_histogram{$delay}}";
	$sep=",";
    }
    print "};\n";
    
    # make a log-log histogram of the pauses
    $step = 10;  #number of steps per decade
    foreach $delay (keys %dt_histogram) {
	next if $delay<1;
	$log_dt_hist{int (0.5 + log($delay)*$step/log(10.0))} += 
	    $dt_histogram{$delay};
    }
    # find bin centers and renormalize to compensate for bin widths
    foreach $i (keys %log_dt_hist) {
	$log_dt_bin{$i}=exp($i*log(10.0)/$step);
	# calculate the number of bins that have been merged.
	$log_dt_hist{$i} /= int(exp(($i+0.5)*log(10.0)/$step))-
	    int(exp(($i-0.5)*log(10.0)/$step));
    }
    # print out log binned pause histogram in Mathematica notation
    print "\nlogpausehistogram={";
    $count=0;
    foreach $i (sort {$a <=> $b} (keys %log_dt_hist)) {
        if ($count++) { print ",";}
	print "{$log_dt_bin{$i},$log_dt_hist{$i}}";
    }
    print "};\n";
}

#    
#  Remove students that solved only a few problems.
#  This should be done semester-wise
foreach $student (sort keys %times) {
    $i=0;
    foreach $problem (sort keys %problems) {
	 if ($times{$student}{$problem} and 
	     $scores{$student}{$problem} > $score_cut_off) {$i++;}
     }
    $problem_attempts{$i}++;
    if($i<=$minimum_problem_attempts) {delete $times{$student};}
}
#  Print number of students that attempted to solve a given number of problems.
if (0) {
    print "problemattempts={";
    $count=0;
    foreach $i (sort {$a <=> $b} keys %problem_attempts) {
        if ($count++) {print ",";}
	print "{$i,$problem_attempts{$i}}";}
    print "};\n";
}
#
#     Remove problems that were solved by only a few students
#  This should be done semester-wise
foreach $problem (sort keys %problems) {
    $i=0;
    foreach $student (sort keys %times) {
	 if ($times{$student}{$problem} and 
	     $scores{$student}{$problem} > $score_cut_off) {$i++;}
     }
    $student_attempts{$i}++;
    if($i<=$minimum_student_attempts) {delete $problems{$problem};}
}
#  Print number of problems solved by a given number of students
if (0) {
    print "studentattempts={";
    $count=0;
    foreach $i (sort {$a <=> $b} keys %student_attempts) {
        if ($count++) { print ",";}
	print "{$i,$student_attempts{$i}}";}
    print "};\n";
}
#
#                        Problem times
#
# print out problem time matrix in CSV format
#
# I used:
#   onelog.pl Fall2005-treacy.dat Fall2005-wintersgill.dat > times-Fall2005.csv
#   onelog.pl Spring2006-treacy.dat Spring2006-wintersgill.dat > times-Spring2006.csv
#
if (0) {
    print " ";
    foreach $problem (sort keys %problems) {print ",$problem";}
    print "\n";
    foreach $student (sort keys %times) {
	print "$student";
	foreach $problem (sort keys %problems) {
	    if ($times{$student}{$problem} and 
		$scores{$student}{$problem} > $score_cut_off) {
		print ",$times{$student}{$problem}";
	    } else {
		print ",";
	    } 
	}
	print "\n";
    }
}

# Print out time, errors, hints for each application of a principle.

if(0) {
    local $"=",";  # for Mathematica formatted lists
    print "(* In the following, FNA (first no assistance) means that\n",
    "the student has, for the first time, successfully applied\n",
    "the principle without any assistance (hints given or errors made\n", 
    "since the last successful entry. *)\n";
    @op_quoted = map {"\"" . $_ . "\""} (sort keys %mastery);
    print "operators={@op_quoted};\n";
    foreach $operator (sort keys %mastery) {
	print "(* $operator  *)\n";  # print as mathematica comment
	@op_time=();
	@op_hints=();
	@op_errors=();
	@op_error_free=();
	@op_students=();
	@op_problems=();
	@op_error_names=();
	$nevermastery=0;
	%first_mastery_attempts=();
	@first_mastery_times=();
	# using %times includes any cutoff in students
	foreach $student (keys %times) {
	    next unless $mastery{$operator}{$student};
	    
	    # Debug print of raw data
	    if(0) {
		print "  $student\n";
		for $application (@{$mastery{$operator}{$student}}) {
		    print "    @$application\n";
		}
	    }
	    
	    $previous_error_free=0;
	    $total_time_spent=0;
	    for ($i=0; $i<@{$mastery{$operator}{$student}}; $i++) {
                # Include any cutoff on set of allowed problems.  However,
		# removing certain problems can cause a "hole" in the arrays.
		# next unless $problems{$mastery{$operator}{$student}[$i]{problem}};
		#push @{$op_problems[$i]}, $mastery{$operator}{$student}[$i]{problem};
		$op_problems[$i]{$mastery{$operator}{$student}[$i]{problem}}++;
		$op_errors[$i]+=$mastery{$operator}{$student}[$i]{errors};
		if (@{$mastery{$operator}{$student}[$i]{error_names}}) {
		    foreach $err 
			(@{$mastery{$operator}{$student}[$i]{error_names}}) {
			$op_error_names[$i]{$err}++;
		    }
		}
		$op_hints[$i]+=$mastery{$operator}{$student}[$i]{hints};
		# make sure there are no "holes" in the array
		$op_error_free[$i] or $op_error_free[$i]=0;
		$total_time_spent += $mastery{$operator}{$student}[$i]{time};
		if($mastery{$operator}{$student}[$i]{errors} == 0 and
		   $mastery{$operator}{$student}[$i]{hints} == 0) {
		    $op_error_free[$i]++;
		    unless ($previous_error_free) {
		      $first_mastery_attempts{$i}++;
		      push @first_mastery_times, $total_time_spent;
		      $previous_error_free=1;
		    }
		} 
		$op_time[$i] += $mastery{$operator}{$student}[$i]{time};
		$op_students[$i]++;
	    }
	    unless ($previous_error_free) {
	      $nevermastery++; # student never gets it right
	    }
	}
	for ($i=0; $i<@op_students; $i++) {
	    $op_errors[$i] /= $op_students[$i];
	    $op_hints[$i] /= $op_students[$i];
	    $op_error_free[$i] /= $op_students[$i]; 
	    $op_time[$i] /= $op_students[$i]; 
	}

	$op_arg="\"" . $operator . "\"";
	print " problemlist[$op_arg]={";
	for($count=0; $count<@op_problems; $count++) {
	    if ($count) {print ",";}
	    print "{";
	    $count2=0;
	    foreach $prob (sort keys %{$op_problems[$count]}) {
		if($count2++) {print ",";}
		print "{\"$prob\",$op_problems[$count]{$prob}}";
	    }
	    print "}";
	}
	print "};\n";

	print " errornames[$op_arg]={";
	for ($count=0; $count<@op_error_names; $count++) {
	    if ($count) {print ",";}
	    print "{";
	    $count2=0;
	    foreach $error_name (sort keys %{$op_error_names[$count]}) {
		if($count2++) {print ",";}
		print "{\"$error_name\",$op_error_names[$count]{$error_name}}";
	    }
	    print "}";
	}
	print "};\n";

	print " avgerrors[$op_arg]={@op_errors};\n";
	print " avghints[$op_arg]={@op_hints};\n";
	print " avgtime[$op_arg]={@op_time};\n";
	# fraction of students who completed this step without using
	# hints or making errors
	print " noassistance[$op_arg]={@op_error_free};\n";
	print " numberstudents[$op_arg]={@op_students};\n";
	# print out histogram for first no assistance
	print " neverFNA[$op_arg]=$nevermastery;\n";
	print " attemptsbeforeFNA[$op_arg]={";
	$count=0;
	foreach $attempt (sort {$a <=> $b} (keys %first_mastery_attempts)) {
	  if ($count++) {print ",";}
	  print "{$attempt,$first_mastery_attempts{$attempt}}";
	}
	print "};\n";
	# Map[{Mean[N[timebeforeFNA[#]]],#}&,operators]
	print " timebeforeFNA[$op_arg]={@first_mastery_times};\n";
    }
}

# print problem times and an operator list for linear model.
# Include student cutoff, score cut-off, but not problem cut-off.
# Express in Mathematica format.
if (0) {
    print linear
    foreach $student (sort keys %times) {
	print "
	foreach $problem (sort keys %problems) {
	    if ($times{$student}{$problem} and 
		$scores{$student}{$problem} > $score_cut_off) {
		print ",$times{$student}{$problem}";
	    } else {
		print ",";
	    } 
	}
	print "\n";
    }
}
